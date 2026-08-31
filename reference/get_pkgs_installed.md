<div id="main" class="col-md-9" role="main">

# List packages installed on this computer

<div class="ref-description section level2">

List packages installed on this computer

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_pkgs_installed(rm_duplicates = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   rm_duplicates:

    (logical) Should duplicated names of packages be removed? If `TRUE`,
    when several packages are found, only the one with the highest
    version is returned. If `FALSE`, no packages are removed from the
    list.

</div>

<div class="section level2">

## Value

Data frame with columns `"package"` and `"current_version"`.

</div>

<div class="section level2">

## See also

<div class="dont-index">

Other R-packages-related functions: `compare_version()`

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
head(get_pkgs_installed())
#>      package current_version
#> 1 KernSmooth         2.23-26
#> 2       MASS          7.3-65
#> 3     Matrix           1.7-5
#> 4         R6           2.6.1
#> 5       Rcpp           1.1.2
#> 6    askpass           1.2.1

nrow(get_pkgs_installed(rm_duplicates = TRUE))
#> [1] 150
nrow(get_pkgs_installed(rm_duplicates = FALSE))
#> [1] 151
```

</div>

</div>

</div>
