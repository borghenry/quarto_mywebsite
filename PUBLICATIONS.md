# Updating publications

`references.bib` in the project root is the intended single source for publication metadata.
Add or update entries there, then run:

```sh
Rscript scripts/sync_publications.R
```

The default command only reports the proposed changes. Once the report looks correct, apply them:

```sh
Rscript scripts/sync_publications.R --write --adopt-existing
quarto render
```

`--adopt-existing` performs the one-time migration: it links existing publication pages to citation keys where their titles match, preserving their current folders and URLs. Subsequent runs only need `--write`.

## Optional BibTeX fields

The script reads standard fields such as `author`, `year`, `title`, `abstract`, `doi`, `url`, `journal`, `booktitle`, and `keywords`.

Use these optional fields only when needed:

```bibtex
keywords = {representation, European politics},
site_categories = {Representation, European politics},
site_type = {articles}
```

`site_categories` overrides the automatic classification; `site_type` can be `articles`, `chapters`, or `otherpubs`. When no `site_categories` field is set, the script assigns up to five categories from a controlled vocabulary by reading the title, abstract, journal, and book title. This keeps classification close to the bibliography rather than spread across page files.

## Before switching the CV

The previous bibliography copies in `publication/` and `cv/` currently contain different citation keys. Reconcile those records in root `references.bib` first, then change the CV front matter to:

```yaml
bibliography: ../references.bib
```

This avoids silently dropping legacy CV entries during the migration.
