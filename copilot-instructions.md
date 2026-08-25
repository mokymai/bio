# Copilot instructions for the bio package

## Preferred R patterns

- Use `pkg::fun()` for external functions.
- Prefer `|>` over `%>%` in new code.
- Use tidyverse-style readability, not strict spacing dogma.
- Keep examples in roxygen blocks as `if (interactive())` instead of
  `\dontrun{\donttest{ ... }}`.

## Package-specific rules

- Do not reintroduce unsupported required-version lookups or external
  metadata files.
- Keep program checks focused on installed software and online
  availability checks.
- Document user-facing functions with roxygen2 comments and realistic
  examples.

## Future maintenance

- Update this file when adding new package conventions or shared
  workflows.
- Prefer small, verifiable fixes and targeted tests over broad
  refactors.
- Keep dependency reviews focused on runtime essentials vs helper-only
  tooling.
- For this package, review RStudio integration, update/install helpers,
  and external tool checks before broad cleanup.

## Related context files

- Package overview:
  [AI_CONTEXT.md](https://mokymai.github.io/AI_CONTEXT.md)
- Repository rules: [AGENTS.md](https://mokymai.github.io/AGENTS.md)
- Main source directory: [R](https://mokymai.github.io/R)
- Test suite: [tests/testthat](https://mokymai.github.io/tests/testthat)
