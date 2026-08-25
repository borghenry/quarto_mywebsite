#!/usr/bin/env Rscript

# Generate publication pages from the canonical root references.bib file.
# Preview is the default; pass --write only after reviewing the proposed changes.

args <- commandArgs(trailingOnly = TRUE)
write_changes <- "--write" %in% args
adopt_existing <- "--adopt-existing" %in% args

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (length(x) == 1 && is.na(x))) y else x
}
root <- normalizePath(getwd())
if (!file.exists(file.path(root, "_quarto.yml"))) {
  stop("Run this command from the Quarto project root.")
}
bib_path <- file.path(root, "references.bib")
if (!file.exists(bib_path)) stop("Missing canonical bibliography: ", bib_path)

suppressPackageStartupMessages({
  library(bibtex)
  library(yaml)
})

clean_text <- function(x) {
  x <- paste(x %||% "", collapse = " ")
  x <- gsub("[{}]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

slugify <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("(^_+|_+$)", "", x)
}

normalise_title <- function(x) gsub("[^a-z0-9]", "", tolower(clean_text(x)))

entry_data <- function(entry) unclass(entry)[[1]]

entry_field <- function(entry, name, default = "") {
  fields <- entry_data(entry)
  if (!(name %in% names(fields))) return(default)
  value <- fields[[name]]
  if (is.null(value)) return(default)
  clean_text(value)
}

person_names <- function(x) {
  if (is.null(x)) return(character())
  vapply(x, function(p) {
    family <- paste(p$family %||% "", collapse = " ")
    given <- paste(p$given %||% "", collapse = " ")
    trimws(paste(given, family))
  }, character(1))
}

format_authors <- function(authors) {
  if (!length(authors)) return("")
  if (length(authors) == 1) return(authors)
  if (length(authors) == 2) return(paste(authors, collapse = " and "))
  paste0(paste(authors[-length(authors)], collapse = ", "), ", and ", authors[length(authors)])
}

publication_section <- function(entry) {
  override <- tolower(entry_field(entry, "site_type"))
  if (override %in% c("articles", "chapters", "otherpubs")) return(override)
  type <- tolower(attr(entry_data(entry), "bibtype") %||% "")
  if (type %in% c("article")) return("articles")
  if (type %in% c("incollection", "inbook", "inproceedings", "proceedings")) return("chapters")
  "otherpubs"
}

category_rules <- list(
  "COVID-19" = "covid|pandemic",
  "European Union" = "european union|\\beu\\b|next generation eu|ngeu|europeani[sz]ation",
  "Italy" = "italy|italian|meloni",
  "Portugal" = "portugal|portuguese",
  "Parliaments" = "parliament|parliamentary|legislature|legislative|law-making",
  "Agenda-Setting" = "agenda.setting|policy agenda|political attention",
  "Political Representation" = "representation|representative|constituency|responsiveness",
  "Political Parties" = "political part|party system|partisan|party competition|niche part",
  "Government and Coalitions" = "coalition|government|cabinet|executive|prime minister",
  "Public Policy" = "public policy|policy making|policy process|welfare|unemployment",
  "Populism" = "populis",
  "Democracy" = "democra",
  "Economic Crisis" = "economic crisis|great recession|financial crisis|recovery plan|recession",
  "Comparative Politics" = "comparative|cross-national|across countries|six western european",
  "Digital Politics" = "digital|virtual|online"
)

inferred_categories <- function(entry) {
  text <- tolower(paste(
    entry_field(entry, "title"),
    entry_field(entry, "abstract"),
    entry_field(entry, "journal"),
    entry_field(entry, "booktitle"),
    collapse = " "
  ))
  names(category_rules)[vapply(category_rules, function(pattern) grepl(pattern, text, perl = TRUE), logical(1))]
}

publication_categories <- function(entry) {
  # site_categories is the manual override; otherwise use a consistent controlled vocabulary.
  explicit <- entry_field(entry, "site_categories")
  values <- trimws(unlist(strsplit(explicit, "[,;]")))
  values <- values[nzchar(values) & !grepl("^\\\\?#", values)]
  categories <- unique(c(values, inferred_categories(entry)))
  if (!length(categories)) categories <- "Political Science"
  head(categories, 5)
}

read_qmd_frontmatter <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) < 3 || lines[1] != "---") return(list())
  closing <- which(lines[-1] == "---")[1]
  if (is.na(closing)) return(list())
  yaml::yaml.load(paste(lines[2:closing], collapse = "\n")) %||% list()
}

existing_pages <- list.files(file.path(root, "publication"), pattern = "index\\.qmd$", recursive = TRUE, full.names = TRUE)
existing_meta <- lapply(existing_pages, function(path) list(path = path, meta = read_qmd_frontmatter(path)))

find_existing_path <- function(key, title) {
  keyed <- vapply(existing_meta, function(x) identical(as.character(x$meta$`citation-key` %||% ""), key), logical(1))
  if (any(keyed)) return(existing_meta[[which(keyed)[1]]]$path)
  if (!adopt_existing) return(NA_character_)

  wanted <- normalise_title(title)
  candidates <- vapply(existing_meta, function(x) {
    candidate <- normalise_title(x$meta$title %||% "")
    nzchar(candidate) && nchar(wanted) > 12 && grepl(wanted, candidate, fixed = TRUE)
  }, logical(1))
  if (sum(candidates) == 1) existing_meta[[which(candidates)[1]]]$path else NA_character_
}

yaml_block <- function(name, values) {
  if (!length(values)) return(character())
  c(paste0(name, ":"), paste0("  - ", vapply(values, function(x) shQuote(x, type = "sh"), character(1))))
}

indent_block <- function(text) {
  text <- clean_text(text)
  if (!nzchar(text)) return("  ")
  paste0("  ", strsplit(text, "\n", fixed = TRUE)[[1]])
}

format_citation <- function(entry, include_doi = TRUE) {
  authors <- person_names(entry_data(entry)$author)
  author_text <- format_authors(authors)
  year <- entry_field(entry, "year", "n.d.")
  title <- entry_field(entry, "title")
  container <- entry_field(entry, "journal", entry_field(entry, "booktitle", entry_field(entry, "publisher")))
  volume <- entry_field(entry, "volume")
  number <- entry_field(entry, "number")
  pages <- entry_field(entry, "pages")
  type <- tolower(attr(entry_data(entry), "bibtype") %||% "")
  editors <- format_authors(person_names(entry_data(entry)$editor))
  address <- entry_field(entry, "address")
  publisher <- entry_field(entry, "publisher")
  parts <- c(paste0(author_text, ". ", year, ". \u201c", title, ".\u201d"))
  if (type == "article" && nzchar(container)) {
    issue <- paste0(if (nzchar(volume)) paste0(" ", volume) else "", if (nzchar(number)) paste0("(", number, ")") else "")
    parts <- c(parts, paste0("*", container, "*", issue, if (nzchar(pages)) paste0(": ", pages) else "", "."))
  } else if (type %in% c("incollection", "inbook", "inproceedings") && nzchar(container)) {
    chapter_details <- c(paste0("In *", container, "*"))
    if (nzchar(editors)) chapter_details <- c(chapter_details, paste0("edited by ", editors))
    if (nzchar(pages)) chapter_details <- c(chapter_details, paste0("pp. ", pages))
    parts <- c(parts, paste0(paste(chapter_details, collapse = ", "), "."))
    imprint <- paste(c(address, publisher)[nzchar(c(address, publisher))], collapse = ": ")
    if (nzchar(imprint)) parts <- c(parts, paste0(imprint, "."))
  } else if (type == "book") {
    imprint <- paste(c(address, publisher)[nzchar(c(address, publisher))], collapse = ": ")
    if (nzchar(imprint)) parts <- c(parts, paste0(imprint, "."))
  } else if (nzchar(container)) {
    parts <- c(parts, paste0("In *", container, "*", if (nzchar(pages)) paste0(": ", pages) else "", "."))
  }
  doi <- entry_field(entry, "doi")
  if (include_doi && nzchar(doi)) parts <- c(parts, paste0("[https://doi.org/", doi, "](https://doi.org/", doi, ")"))
  paste(parts, collapse = " ")
}

render_page <- function(entry, key) {
  title <- format_citation(entry, include_doi = FALSE)
  authors <- person_names(entry_data(entry)$author)
  year <- entry_field(entry, "year")
  date <- entry_field(entry, "date", if (nzchar(year)) paste0(year, "-12-31") else "")
  abstract <- entry_field(entry, "abstract")
  doi <- entry_field(entry, "doi")
  url <- entry_field(entry, "url")
  front <- c(
    "---",
    "title: |-",
    indent_block(title),
    yaml_block("authors", authors),
    if (nzchar(date)) paste0("date: ", shQuote(date, type = "sh")) else character(),
    yaml_block("categories", publication_categories(entry)),
    paste0("citation-key: ", shQuote(key, type = "sh")),
    "description: |-",
    indent_block(format_citation(entry)),
    if (nzchar(abstract)) c("abstract: |-", indent_block(abstract)) else character(),
    if (nzchar(doi)) paste0("doi: ", shQuote(doi, type = "sh")) else character(),
    if (nzchar(url)) paste0("url: ", shQuote(url, type = "sh")) else character(),
    "---",
    "",
    "## Citation",
    "",
    format_citation(entry),
    ""
  )
  paste(front, collapse = "\n")
}

entries <- bibtex::read.bib(bib_path)
created <- updated <- adopted <- character()
for (key in names(entries)) {
  entry <- entries[[key]]
  section <- publication_section(entry)
  year <- entry_field(entry, "year", "undated")
  path <- find_existing_path(key, entry_field(entry, "title"))
  if (is.na(path)) path <- file.path(root, "publication", section, paste0(year, "_", slugify(key)), "index.qmd")
  if (adopt_existing && file.exists(path) && is.null(read_qmd_frontmatter(path)$`citation-key`)) adopted <- c(adopted, path)
  if (file.exists(path)) updated <- c(updated, path) else created <- c(created, path)
  if (write_changes) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(render_page(entry, key), path, useBytes = TRUE)
  } else {
    # Validate all generated YAML and Markdown before the user opts into writing.
    render_page(entry, key)
  }
}

cat("Canonical bibliography:", bib_path, "\n")
cat("Entries:", length(entries), "\n")
cat("Would create:", length(unique(created)), "publication pages\n")
cat("Would update:", length(unique(updated)), "publication pages\n")
if (adopt_existing) cat("Existing paths adopted:", length(unique(adopted)), "\n")
if (!write_changes) cat("\nPreview only. Run: Rscript scripts/sync_publications.R --write --adopt-existing\n")
