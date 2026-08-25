# Reset the RStudio pane layout.

Reset the RStudio pane layout.

## Usage

``` r
rstudio_reset_layout(rs_layout = "left")
```

## Arguments

- rs_layout:

  Character scalar: either `"left"` or `"right"`.

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (interactive()) {
  rstudio_reset_layout("left")
}
```
