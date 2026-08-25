# List Packages Used in Directory

Scans a directory for R, Rmd, and Qmd files and returns a vector of
unique package names used across those files.

## Usage

``` r
list_pkgs_used_in_dir(path = ".", exclude_base = TRUE, progress = FALSE, ...)
```

## Arguments

- path:

  Path to directory. Defaults to `"."`.

- exclude_base:

  Logical. If `TRUE` (default), base R packages (e.g., `stats`, `utils`,
  `graphics`) are excluded from the output.

- progress:

  Logical. Whether to show a progress bar. Defaults to `FALSE`.

- ...:

  Further arguments passed to
  [`renv::dependencies()`](https://rstudio.github.io/renv/reference/dependencies.html).

## Value

Character vector of unique, sorted package names.
