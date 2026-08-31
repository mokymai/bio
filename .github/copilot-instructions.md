# Copilot instructions for the bio package

## Preferred R patterns
- Use `pkg::fun()` for external functions.
- Prefer `|>` over `%>%` in new code.
- Use tidyverse-style readability, not strict spacing dogma.
- Run Styler on affected R files or selected code before final validation. Use
	the project `.Rprofile` style (`styler::tidyverse_style(strict = FALSE)`) and
	avoid formatting unrelated files or lines.
- Keep examples in roxygen blocks as `if (interactive())` instead of `\dontrun{\donttest{ ... }}`.

## Package-specific rules
- Do not reintroduce unsupported required-version lookups or external metadata files.
- Keep program checks focused on installed software and online availability checks.
- Document user-facing functions with roxygen2 comments and realistic examples.
- Guard destructive changes to user files and directories, preserve backups
	where practical, and keep tests confined to temporary paths.
- Keep package-publication jobs sequential both within a matrix and across
	workflow runs; do not hide deployment pull or push failures.
- Keep pull-request generated-documentation checks read-only. Let serialized
	post-merge pkgdown runs commit scoped generated files when contributors omit
	them, with deployment failures remaining visible.
- Keep local and CI documentation generation aligned on the current R release,
	roxygen2 8.1.0, and Pandoc 2.14. Derive README badges from `DESCRIPTION`, and
	validate generated Rd contracts with `tools::checkDocFiles(dir = ".")`.

## Future maintenance
- Update this file when adding new package conventions or shared workflows.
- Prefer small, verifiable fixes and targeted tests over broad refactors.
- Keep dependency reviews focused on runtime essentials vs helper-only tooling.
- For this package, review RStudio integration, update/install helpers, and external tool checks before broad cleanup.
- Validate the JSON files under `inst/rs-settings/` when changing bundled
	preferences or keybindings.

## Related context files
- Package overview: [AI_CONTEXT.md](../AI_CONTEXT.md)
- Repository rules: [AGENTS.md](../AGENTS.md)
- Main source directory: [R](../R)
- Test suite: [tests/testthat](../tests/testthat)
