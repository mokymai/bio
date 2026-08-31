# Package working rules for this repository

## Coding conventions
- Prefer package-qualified calls: `pkg::fun()` instead of `library()`/`require()`.
- Prefer the base pipe `|>` over `%>%` for new code.
- Keep tidyverse-style formatting but avoid rigid spacing rules; use readable, idiomatic R.
- Prefer small, explicit functions and early returns.
- Avoid unsupported metadata or network-based version gates unless they are actually implemented and tested.
- Do not leave obsolete implementations commented out in `R/`; preserve any
	short-lived historical notes outside maintained package source.
- Treat deletion of user preferences, histories, dictionaries, and course
	directories as destructive behavior: require an explicit guard, preserve a
	backup where practical, and test only against temporary paths.

## R package maintenance
- Keep roxygen2 documentation in sync with function behavior.
- Use `if (interactive()) { ... }` in examples instead of `\dontrun{\donttest{ ... }}` blocks.
- Keep examples short and safe for CRAN-friendly review.
- Export only intended user-facing functions and document them with `@export` and parameter details.

## Verification checklist
- Run targeted tests for changed behavior.
- Regenerate roxygen docs when function comments change.
- Review for broken version-check logic before merging.
- Keep drat matrix jobs sequential and serialize workflow runs that publish to
	the shared download repository. Publication pull/push failures must fail the
	workflow rather than be reported as "nothing to commit".
- Keep generated-documentation checks on pull requests read-only. Post-merge
  pkgdown runs may commit only `NAMESPACE`, `man/`, and `README.md`; serialize
  those runs and do not suppress commit, rebase, or push failures.
- Generate documentation with R 4.6.1, roxygen2 8.1.0, and Pandoc 2.14 locally
	and in GitHub Actions. Keep README output derived from repository metadata,
	and run `tools::checkDocFiles(dir = ".")` after roxygen generation.
- Validate bundled JSON presets and test headless RStudio operations without
	touching real user settings.

## Related files
- Package structure guide: [AI_CONTEXT.md](AI_CONTEXT.md)
- Copilot guidance: [.github/copilot-instructions.md](.github/copilot-instructions.md)
- Main package code: [R](R)
- Tests: [tests/testthat](tests/testthat)
