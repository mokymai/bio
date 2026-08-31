<div id="main" class="col-md-9" role="main">

# Reset RStudio keybindings to a packaged preset.

<div class="ref-description section level2">

This helper copies the packaged keybindings files into the user-level
RStudio folder, or removes the current keybinding files when the preset
is `"rstudio-default"`.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_reset_keybindings(to, backup = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   to:

    String scalar. Supported values are `"bio-default"` and
    `"rstudio-default"`.

-   backup:

    Logical scalar. If `TRUE`, a backup copy of the current keybinding
    files is created before resetting.

</div>

<div class="section level2">

## Value

Invisibly returns `NULL` after resetting the keybindings.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  bio::rstudio_reset_keybindings(to = "bio-default")
  bio::rstudio_reload_ui()
}
```

</div>

</div>

</div>
