<div id="main" class="col-md-9" role="main">

# List Packages Used in Specific File(s)

<div class="ref-description section level2">

Scans specific R, Rmd, or Qmd file(s) and returns a vector of unique
package names used inside them.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
list_pkgs_used_in_files(files, exclude_base = TRUE, progress = FALSE, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   files:

    Character vector of file paths (R, Rmd, Qmd).

-   exclude_base:

    Logical. If `TRUE` (default), base R packages (e.g., `stats`,
    `utils`, `graphics`) are excluded from the output.

-   progress:

    Logical. Whether to show a progress bar. Defaults to `FALSE`.

-   ...:

    Further arguments passed to `renv::dependencies()`.

</div>

<div class="section level2">

## Value

Character vector of unique, sorted package names.

</div>

</div>
