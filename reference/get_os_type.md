# Detect the current operating system

Returns a normalized operating-system label for the current R session.
The result is a single lowercase string such as "windows", "mac", or
"linux"; other Unix-like systems are normalized to their platform name.

## Usage

``` r
get_os_type()

is_64bit_os()

is_32bit_os()
```

## Value

A length-1 character string with the current OS name in lowercase.

## Examples

``` r
get_os_type()
#> [1] "linux"
```
