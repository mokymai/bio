# Get previous package versions available on CRAN

Function to scrape the CRAN website and retrieve archived (old) package
versions

## Usage

``` r
pkg_list_archived_versions(package)
```

## Arguments

- package:

  (character) Package name.

## Value

Vector with version numbers (the current version is not present).

## Examples

``` r
pkg_list_archived_versions("ggplot2")
#>  [1] ‘4.0.2’   ‘4.0.1’   ‘4.0.0’   ‘3.5.2’   ‘3.5.1’   ‘3.5.0’   ‘3.4.4’  
#>  [8] ‘3.4.3’   ‘3.4.2’   ‘3.4.1’   ‘3.4.0’   ‘3.3.6’   ‘3.3.5’   ‘3.3.4’  
#> [15] ‘3.3.3’   ‘3.3.2’   ‘3.3.1’   ‘3.3.0’   ‘3.2.1’   ‘3.2.0’   ‘3.1.1’  
#> [22] ‘3.1.0’   ‘3.0.0’   ‘2.2.1’   ‘2.2.0’   ‘2.1.0’   ‘2.0.0’   ‘1.0.1’  
#> [29] ‘1.0.0’   ‘0.9.3.1’ ‘0.9.3’   ‘0.9.2.1’ ‘0.9.2’   ‘0.9.1’   ‘0.9.0’  
#> [36] ‘0.8.9’   ‘0.8.8’   ‘0.8.7’   ‘0.8.6’   ‘0.8.5’   ‘0.8.4’   ‘0.8.3’  
#> [43] ‘0.8.2’   ‘0.8.1’   ‘0.8’     ‘0.7’     ‘0.6’     ‘0.5.7’   ‘0.5.6’  
#> [50] ‘0.5.5’   ‘0.5.4’   ‘0.5.2’   ‘0.5.1’   ‘0.5’    

pkg_list_archived_versions("none")
#> <0 elements>
```
