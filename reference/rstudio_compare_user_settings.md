# Show differences in sets of settings

Show differences in sets of settings

## Usage

``` r
rstudio_compare_user_settings(to = "bio-default")
```

## Arguments

- to:

  One of: "bio-default", "rstudio-default" (or an unambiguous
  abbreviation of these).

## Value

Nothing. But prints the set differences between `to` list and current
settings. Settings, which are not in `to` list, will not be displayed at
all.

## Examples

``` r
if (interactive()) {
  rstudio_compare_user_settings(to = "bio-default")
  rstudio_compare_user_settings(to = "rstudio-default")
}
```
