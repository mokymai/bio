<div id="main" class="col-md-9" role="main">

# Reset the RStudio pane layout.

<div class="ref-description section level2">

Reset the RStudio pane layout.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_reset_layout(rs_layout = "left")
```

</div>

</div>

<div class="section level2">

## Arguments

-   rs_layout:

    Character scalar: either `"left"` or `"right"`.

</div>

<div class="section level2">

## Value

Invisibly returns `NULL`.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  rstudio_reset_layout("left")
}
```

</div>

</div>

</div>
