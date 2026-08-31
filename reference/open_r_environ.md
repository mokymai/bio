<div id="main" class="col-md-9" role="main">

# Open `.Renviron` File

<div class="ref-description section level2">

Functions to get path to and open `.Renviron` file that contains
definitions of R environment variables.

Compared to `usethis::edit_r_environ()`, `open_r_environ()` does not
create file if it does not exist.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_path_r_environ(scope = c("user", "project"))

open_r_environ()
```

</div>

</div>

<div class="section level2">

## Arguments

-   scope:

    (character) The scope of file. One of "user" or "project".

</div>

<div class="section level2">

## See also

<div class="dont-index">

-   `usethis::edit_r_environ()`

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
get_path_r_environ()
#> /home/runner/.Renviron
```

</div>

</div>

</div>
