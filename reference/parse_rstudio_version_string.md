# Parse an RStudio calendar-version string into a [`numeric_version()`](https://rdrr.io/r/base/numeric_version.html)

RStudio version strings look like `"2026.08.2+200"`.
[`numeric_version()`](https://rdrr.io/r/base/numeric_version.html)
accepts `-` (like `.`) as a component separator but not `+`, so `+` is
normalized to `-` to keep the build number as a 4th version component.

## Usage

``` r
parse_rstudio_version_string(x)
```

## Arguments

- x:

  Character scalar containing (or surrounded by) a version string.

## Value

A [`numeric_version()`](https://rdrr.io/r/base/numeric_version.html)
object, or `NULL` if `x` has no match.
