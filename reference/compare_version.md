# Compare Version Numbers

Compare Version Numbers

## Usage

``` r
compare_version(v_installed, v_required)
```

## Arguments

- v_installed:

  vector with installed version numbers

- v_required:

  vector with required version numbers

## Value

The same as in
[`utils::compareVersion()`](https://rdrr.io/r/utils/compareVersion.html),
just a vector.

## See also

Other R-packages-related functions:
[`get_pkgs_installed()`](https://mokymai.github.io/bio/reference/get_pkgs_installed.md)

## Examples

``` r

compare_version("2.4", "2")
#> [1] 1

compare_version("2.3", "2.3")
#> [1] 0

compare_version("2.3", "2.3.1")
#> [1] -1
```
