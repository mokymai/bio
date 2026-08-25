# List Packages Used in Specific File(s)

Scans specific R, Rmd, or Qmd file(s) and returns a vector of unique
package names used inside them.

## Usage

``` r
list_pkgs_used_in_files(files, exclude_base = TRUE, progress = FALSE, ...)
```

## Arguments

- files:

  Character vector of file paths (R, Rmd, Qmd).

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
