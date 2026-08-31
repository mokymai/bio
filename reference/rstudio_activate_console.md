<div id="main" class="col-md-9" role="main">

# Activate the console in RStudio when available.

<div class="ref-description section level2">

Activate the console in RStudio when available.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_activate_console()
```

</div>

</div>

<div class="section level2">

## Value

Invisibly returns `NULL` when RStudio is unavailable.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  rstudio_activate_console()
}
```

</div>

</div>

</div>
