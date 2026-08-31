<div id="main" class="col-md-9" role="main">

# Check if package is installed

<div class="ref-description section level2">

Check if package is installed

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
is_pkg_installed(pkgs)
```

</div>

</div>

<div class="section level2">

## Arguments

-   pkgs:

    (character) A list of installed packages.

</div>

<div class="section level2">

## Value

A logical vector for each input element.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
is_pkg_installed("bio")
#> [1] TRUE

is_pkg_installed(c("bio", "utils", "grugru"))
#> [1]  TRUE  TRUE FALSE
```

</div>

</div>

</div>
