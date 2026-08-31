<div id="main" class="col-md-9" role="main">

# Compare Version Numbers

<div class="ref-description section level2">

Compare Version Numbers

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
compare_version(v_installed, v_required)
```

</div>

</div>

<div class="section level2">

## Arguments

-   v_installed:

    vector with installed version numbers

-   v_required:

    vector with required version numbers

</div>

<div class="section level2">

## Value

The same as in `utils::compareVersion()`, just a vector.

</div>

<div class="section level2">

## See also

<div class="dont-index">

Other R-packages-related functions: `get_pkgs_installed()`

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
compare_version("2.4", "2")
#> [1] 1

compare_version("2.3", "2.3")
#> [1] 0

compare_version("2.3", "2.3.1")
#> [1] -1
```

</div>

</div>

</div>
