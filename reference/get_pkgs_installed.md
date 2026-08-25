# List packages installed on this computer

List packages installed on this computer

## Usage

``` r
get_pkgs_installed(rm_duplicates = TRUE)
```

## Arguments

- rm_duplicates:

  (logical) Should duplicated names of packages be removed? If `TRUE`,
  when several packages are found, only the one with the highest version
  is returned. If `FALSE`, no packages are removed from the list.

## Value

Data frame with columns `"package"` and `"current_version"`.

## See also

Other R-packages-related functions:
[`compare_version()`](https://mokymai.github.io/bio/reference/compare_version.md)

## Examples

``` r

head(get_pkgs_installed())
#>       package current_version
#> 1  KernSmooth         2.23-26
#> 2        MASS          7.3-65
#> 3      Matrix           1.7-5
#> 4     R.cache          0.17.0
#> 5 R.methodsS3           1.8.2
#> 6        R.oo          1.27.1

nrow(get_pkgs_installed(rm_duplicates = TRUE))
#> [1] 157
nrow(get_pkgs_installed(rm_duplicates = FALSE))
#> [1] 158
```
