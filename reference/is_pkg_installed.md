# Check if packae is installed

Check if packae is installed

## Usage

``` r
is_pkg_installed(pkgs)
```

## Arguments

- pkgs:

  (character) A list of installed packages.

## Value

A logical vector for each input element.

## Examples

``` r

is_pkg_installed("bio")
#> [1] TRUE

is_pkg_installed(c("bio", "utils", "grugru"))
#> [1]  TRUE  TRUE FALSE
```
