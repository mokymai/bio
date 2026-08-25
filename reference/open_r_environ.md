# Open `.Renviron` File

Functions to get path to and open `.Renviron` file that contains
definitions of R environment variables.

Compared to
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html),
`open_r_environ()` does not create file if it does not exist.

## Usage

``` r
get_path_r_environ(scope = c("user", "project"))

open_r_environ()
```

## Arguments

- scope:

  (character) The scope of file. One of "user" or "project".

## See also

- [`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)

## Examples

``` r
get_path_r_environ()
#> /home/runner/.Renviron
```
