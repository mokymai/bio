<div id="main" class="col-md-9" role="main">

# Parse an RStudio calendar-version string into a `numeric_version()`

<div class="ref-description section level2">

RStudio version strings look like `"2026.08.2+200"`. `numeric_version()`
accepts `-` (like `.`) as a component separator but not `+`, so `+` is
normalized to `-` to keep the build number as a 4th version component.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
parse_rstudio_version_string(x)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    Character scalar containing (or surrounded by) a version string.

</div>

<div class="section level2">

## Value

A `numeric_version()` object, or `NULL` if `x` has no match.

</div>

</div>
