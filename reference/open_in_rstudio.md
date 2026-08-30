# Open file in RStudio

Function tries opening a file in RStudio.

## Usage

``` r
open_in_rstudio(path, ...)
```

## Arguments

- path:

  (string) Path to file.

- ...:

  Further arguments to
  [`rstudioapi::navigateToFile()`](https://rstudio.github.io/rstudioapi/reference/navigateToFile.html).

## See also

- [`rstudioapi::navigateToFile()`](https://rstudio.github.io/rstudioapi/reference/navigateToFile.html),

- [`fs::file_show()`](https://fs.r-lib.org/reference/file_show.html),
  [`browseURL()`](https://rdrr.io/r/utils/browseURL.html),

- [`utils::file.edit()`](https://rdrr.io/r/utils/file.edit.html)

- [`usethis::edit_file()`](https://usethis.r-lib.org/reference/edit_file.html)
