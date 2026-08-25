# bio package AI context

This document maps the package structure and the main responsibilities of the code so AI tools can reason about the project without reading generated content.

## Scope and conventions

- Source package code lives in `R/`.
- Package metadata and project config live at the repo root.
- Generated website/build artifacts are excluded from this map, including `docs/`, `man/*.Rd`, and the temporary `_tmp/` area.
- Prefer package-qualified calls like `pkg::fun()` and keep examples short and interactive-safe.

## Top-level package layout

- `DESCRIPTION` — package metadata, dependencies, and roxygen settings.
- `NAMESPACE` — generated exports; do not hand-edit unless intentionally updating roxygen output.
- `AGENTS.md` — repository-specific working rules.
- `.github/copilot-instructions.md` — package-level AI guidance for Copilot.
- `README.md` and `README.Rmd` — user-facing overview and examples.
- `_pkgdown.yml` — pkgdown website config.
- `.Rproj` — RStudio project file.
- `LICENSE` and `LICENSE.md` — package licensing.
- `codecov.yml` — coverage config.
- `tests/` — testthat suite.
- `inst/` — bundled support files, config snapshots, and RStudio settings assets.
- `R/` — package implementation source.
- `docs/` — generated website output.
- `_tmp/` — temporary scratch and one-off development scripts; not part of the maintained package logic.
- `man/` — generated roxygen documentation; not part of the hand-written source.

## Source code map

### Core package helpers

- `R/bio-package.R` — package-level metadata and exports.
- `R/helpers.R` — small reusable utility helpers used across the package.
- `R/helpers--make_unique_obj_names.R` — naming helper for avoiding duplicate object names.
- `R/reexport.R` — re-exported functions from imported packages.
- `R/load_update.R` — update/install logic and package loading support.
- `R/open_in_rstudio.R` — RStudio opening helpers.
- `R/restart_reload.R` — reload/restart workflow helpers.
- `R/get_os.R` and `R/get_os--NEW.R` — OS detection helpers.
- `R/paths-and-files.R` — file/path utilities.

### Project and environment lifecycle

- `R/projects.R` — project management and project-scoped helper functions.
- `R/packages--check.R` — installed package/version checks.
- `R/packages--find.R` — package lookup and discovery helpers.
- `R/programs.R` — installed software checks, version comparisons, and availability reporting.
- `R/dictionaries.R` — spellcheck/dictionary management.

### RStudio and settings management

- `R/settings.R` — higher-level reset workflows and RStudio housekeeping.
- `R/settings--preferences.R` — user preferences reset utilities.
- `R/settings--keybindings.R` — keybinding reset helpers.
- `R/bio-related.R` — package-specific RStudio/bio helper integration points.

### Package entry points

- `R/bio-package.R` and `R/reexport.R` are the main entry points for public API exposure.
- Public user-facing helpers should be documented in roxygen comments and exported through the package namespace.

## Test structure

- `tests/testthat/test-programs.R` — current behavior checks for version-reporting logic and helper safety.
- `tests/testthat.R` — testthat bootstrap entry file.
- Add new tests beside the functional area they cover; keep them focused and runnable without network-dependent metadata unless explicitly mocked or skipped.

## Bundled resources

- `inst/rs-settings/` — packaged RStudio settings, keybinding and preference JSON payloads.
- `inst/WORDLIST` — spelling word list for package documentation and tests.

## Suggested traversal order for AI work

1. Start with `DESCRIPTION` and `AGENTS.md`.
2. Review the public API in `R/bio-package.R` and `R/reexport.R`.
3. Inspect the domain area: `programs.R`, `settings.R`, `projects.R`, `packages--check.R`, etc.
4. Add or update tests in `tests/testthat/` for behavior changes.
5. Avoid editing generated artifacts such as `man/*.Rd`, `docs/`, or `_tmp/`.

## Package review notes

- Review runtime dependencies vs developer-only dependencies before broad refactors.
- Favor small, targeted maintenance changes over unnecessary cleanup.
- Keep `R` code modern, scalar-safe, and non-destructive in tests.
- For this package, prioritize review of RStudio integration, program/version checks, and package installation helper paths.
- Keep dependency trimming focused on tools used only for developer convenience or optional workflows.

## Cross-reference map

- `AGENTS.md` contains repo working rules.
- `.github/copilot-instructions.md` contains package-level Copilot guidance.
- `README.md` is the human-facing overview.
- `R/settings.R` is the main reset and cleanup implementation.
- `R/programs.R` contains installation/version checks and scalar validation helpers.
- `tests/testthat/test-programs.R` covers the active regression suite.
