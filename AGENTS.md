# Package working rules for this repository

## Coding conventions

- Prefer package-qualified calls: `pkg::fun()` instead of
  [`library()`](https://rdrr.io/r/base/library.html)/[`require()`](https://rdrr.io/r/base/library.html).
- Prefer the base pipe `|>` over `%>%` for new code.
- Keep tidyverse-style formatting but avoid rigid spacing rules; use
  readable, idiomatic R.
- Prefer small, explicit functions and early returns.
- Avoid unsupported metadata or network-based version gates unless they
  are actually implemented and tested.
- Do not leave obsolete implementations commented out in `R/`; preserve
  any short-lived historical notes outside maintained package source.

## R package maintenance

- Keep roxygen2 documentation in sync with function behavior.
- Use `if (interactive()) { ... }` in examples instead of
  `\dontrun{\donttest{ ... }}` blocks.
- Keep examples short and safe for CRAN-friendly review.
- Export only intended user-facing functions and document them with
  `@export` and parameter details.

## Verification checklist

- Run targeted tests for changed behavior.
- Regenerate roxygen docs when function comments change.
- Review for broken version-check logic before merging.

## Related files

- Package structure guide:
  [AI_CONTEXT.md](https://mokymai.github.io/bio/AI_CONTEXT.md)
- Copilot guidance:
  [.github/copilot-instructions.md](https://mokymai.github.io/bio/copilot-instructions.md)
- Main package code: [R](https://mokymai.github.io/bio/R)
- Tests: [tests/testthat](https://mokymai.github.io/bio/tests/testthat)
