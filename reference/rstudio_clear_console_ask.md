<div id="main" class="col-md-9" role="main">

# Ask before clearing the RStudio console.

<div class="ref-description section level2">

Ask before clearing the RStudio console.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_clear_console_ask()
```

</div>

</div>

<div class="section level2">

## Value

Invisibly returns `NULL` if RStudio is unavailable or the user says no.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  rstudio_clear_console_ask()
}
```

</div>

</div>

</div>
