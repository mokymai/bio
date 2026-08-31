# bio package AI context

This document maps the package structure and the main responsibilities of the code so AI tools can reason about the project without reading generated content.

## Scope and conventions

- Source package code lives in `R/`.
- Package metadata and project config live at the repo root.
- Generated website/build artifacts are excluded from this map, including `docs/`, `man/*.Rd`, and the temporary `_tmp/` area.
- Prefer package-qualified calls like `pkg::fun()` and keep examples short and interactive-safe.
- Do not retain disabled implementations in package source. Remove obsolete code;
  temporary historical notes belong outside the maintained package code.

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
- `R/open_in_rstudio.R` — RStudio opening helpers.
- `R/restart_reload.R` — reload/restart workflow helpers.
- `R/get_os.R` — OS detection helpers.
- `R/paths-and-files.R` — file/path utilities.

### Project and environment lifecycle

- `R/projects.R` — project management and project-scoped helper functions.
- `R/packages--check.R` — installed package/version checks.
- `R/packages--find.R` — package lookup and discovery helpers.
- `R/programs.R` — installed software checks, version comparisons, and availability reporting.
  Online R, RStudio, and Quarto version discovery is best-effort: connectivity,
  endpoint, parsing, empty-result, and unexpected-format failures warn and
  return `NULL` rather than aborting installed-program checks.
  Includes RStudio Desktop detection: `find_rstudio_install_dir()` (checks
  both per-user "just me" (`%LOCALAPPDATA%/Programs/RStudio`, `~/Applications/RStudio.app`)
  and system-wide "all users" (`%PROGRAMFILES%/RStudio`, `/Applications/RStudio.app`,
  `/usr/lib/rstudio`) install locations, including both `HKCU`/`HKLM` registry hives
  on Windows via `rstudio_registry_paths()` for `InstallLocation`/`InstallPath`) and
  `get_rstudio_install_scope()` (classifies a resolved install dir as
  `"user"`/`"system"`/`NA`). Note: this install-scope distinction only
  affects where the RStudio *application files* live (and where
  `find_rstudio_prefs_schema_file()` looks for `user-prefs-schema.json`) — RStudio's
  preferences/keybindings/config dirs (`R/paths-and-files.R`) are always
  per-OS-user regardless of install scope. The classifier normalizes Windows
  and Unix path separators and matches complete per-user path prefixes, so
  its behavior and tests are independent of the operating system running R.
  Includes Rtools detection (`get_installed_rtools_version()`): queries active
  toolchain via `pkgbuild::rtools_path()`, falling back to `RTOOLS*_HOME` env
  vars, registry, and `C:\rtools*` directories. Rtools releases (e.g., Rtools 4.5)
  span multiple R minor series (e.g., R 4.5.x and 4.6.x).
- `R/dictionaries.R` — spellcheck/dictionary management.

### RStudio and settings management

- `R/settings.R` — higher-level reset workflows and RStudio housekeeping.
- `R/settings--preferences.R` — user preferences reset utilities. Also has
  `rstudio_compare_user_settings(to, source = c("auto","live","file"), output = c("concise","minimal","verbose"))`:
  `source = "live"` reads "current" prefs via `rstudioapi` (requires a
  running RStudio session); `source = "file"` always reads the on-disk
  `rstudio-prefs.json`; `source = "auto"` (default) picks live vs. file
  based on whether RStudio is running. Since `rstudio-prefs.json` only
  stores values overridden from RStudio's built-in defaults, the file-based
  path fills in unset keys (when possible) from the local RStudio
  installation's `user-prefs-schema.json`
  (`<install dir>/resources/app/resources/schema/user-prefs-schema.json`,
  `properties.<pref_name>.default`).
  File-based preset application is transactional: failures restore the exact
  original preference-file bytes, or remove partial output if the file did not
  exist before the operation.
- `R/settings--keybindings.R` — keybinding reset helpers.
- Current RStudio Desktop locations (verified against Posit Support): user
  configuration is `%APPDATA%/RStudio` on Windows and `~/.config/rstudio` on
  Linux/macOS; internal state is `%LOCALAPPDATA%/RStudio` on Windows and
  `~/.local/share/rstudio` on Linux/macOS.
- `clear_r_history()` is an internal helper. In a running RStudio session it
  delegates to `rstudio_clear_history()` and RStudio's `clearHistory` command;
  outside RStudio it uses base R history functions.

### Package entry points

- `R/bio-package.R` and `R/reexport.R` are the main entry points for public API exposure.
- Public user-facing helpers should be documented in roxygen comments and exported through the package namespace.
- Before final validation, run Styler on affected R files or selected code only.
  The project `.Rprofile` configures
  `styler::tidyverse_style(strict = FALSE)`; avoid unrelated formatting churn.
- Generated documentation uses the current R release, roxygen2 8.1.0 (pinned
  by `Config/roxygen2/version`), and Pandoc 2.14 locally and in GitHub Actions.
  README version/date badges come from `DESCRIPTION`, not the render date or
  an installed package. After roxygen generation,
  `tools::checkDocFiles(dir = ".")` checks usage, arguments, and aliases.

## Test structure

- `tests/testthat/test-programs.R` — installed-version reporting and resilient
  online availability behavior.
- `tests/testthat/test-settings.R` — settings workflows, confirmation branches,
  transactional rollback, reset summaries, and bundled JSON validation.
- `tests/testthat/test-helpers.R` — deterministic helper contracts and public
  spellcheck dictionary installer exports.
- `tests/testthat.R` — testthat bootstrap entry file.
- Add new tests beside the functional area they cover; keep them focused and runnable without network-dependent metadata unless explicitly mocked or skipped.

## Bundled resources

- `inst/rs-settings/rstudio-prefs--bio-default.json` — the course preference
  overrides applied on top of RStudio defaults.
- `inst/rs-settings/rstudio-prefs--rstudio-default.json` — the package's
  maintained baseline of RStudio defaults; local schema defaults are preferred
  when settings are compared.
- `inst/rs-settings/keybindings--addins.json` and
  `inst/rs-settings/keybindings--rstudio_bindings.json` — packaged shortcut
  profiles copied by the keybinding reset helpers.
- `inst/WORDLIST` — spelling word list for package documentation and tests.

## Automation

- `.github/workflows/R-CMD-check.yaml` — cross-platform package checks.
- `.github/workflows/lint.yaml` — `lintr` and `styler` checks for maintained R
  and test code.
- `.github/workflows/test-coverage.yaml` — test coverage reporting.
- `.github/workflows/generated-docs.yaml` — read-only pull-request check that
  fails when `NAMESPACE`, `man/`, or `README.md` are stale.
- `.github/workflows/pkgdown.yaml` — serialized post-merge regeneration of
  `NAMESPACE`, `man/`, and `README.md`; commits those files when needed, then
  builds and deploys the GitHub Pages site.
- `.github/workflows/drat--publish-package.yaml` — sequential source and binary
  package publication to `mokymai/download`. The matrix must keep
  `max-parallel: 1`, workflow concurrency serializes separate runs, publication
  failures are not suppressed, and commit messages record the concrete runtime
  R version.

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
