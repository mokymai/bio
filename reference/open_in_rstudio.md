<div id="main" class="col-md-9" role="main">

# Open file in RStudio

<div class="ref-description section level2">

Function tries opening a file in RStudio.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
open_in_rstudio(path, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   path:

    (string) Path to file.

-   ...:

    Further arguments to `rstudioapi::navigateToFile()`.

</div>

<div class="section level2">

## See also

<div class="dont-index">

-   `rstudioapi::navigateToFile()`,

-   `fs::file_show()`, `browseURL()`,

-   `utils::file.edit()`

-   `usethis::edit_file()`

</div>

</div>

</div>
