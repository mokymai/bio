# Reset RStudio keybindings to a packaged preset.

This helper copies the packaged keybindings files into the user-level
RStudio folder, or removes the current keybinding files when the preset
is `"rstudio-default"`.

## Usage

``` r
rstudio_reset_keybindings(to, backup = TRUE)
```

## Arguments

- to:

  String scalar. Supported values are `"bio-default"` and
  `"rstudio-default"`.

- backup:

  Logical scalar. If `TRUE`, a backup copy of the current keybinding
  files is created before resetting.

## Value

Invisibly returns `NULL` after resetting the keybindings.

## Examples

``` r
if (interactive()) {
  bio::rstudio_reset_keybindings(to = "bio-default")
  bio::rstudio_reload_ui()
}
```
