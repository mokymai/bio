<div id="main" class="col-md-9" role="main">

# Directories of RStudio-Related Files

<div class="ref-description section level2">

Directories of RStudio (desktop) settings, preferences and other files.

-   \`get_path_rstudio_config_dir()“ - gets path to RStudio
    configuration directory (and its sub-directories).

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_path_rstudio_config_dir(..., .check = FALSE)

get_path_rstudio_internal_state_dir(..., .check = FALSE)

get_path_rstudio_keybindings_dir()

open_rstudio_config_dir()

open_rstudio_internal_state_dir()

open_rstudio_keybindings_dir()
```

</div>

</div>

<div class="section level2">

## Arguments

-   ...:

    (character) Parts of the path. Path to sub-directories.

-   .check:

    (logical) If `TRUE`, additionally checks for path existence.

</div>

<div class="section level2">

## Value

(string) path to RStudio configuration directory. When `.check = TRUE`,
returns an error if the path does not exist.

</div>

<div class="section level2">

## See also

<div class="dont-index">

-   `fs::file_show()`, `browseURL()`,

-   `rstudioapi::navigateToFile()`,

-   `utils::file.edit()`

&nbsp;

-   `get_path_rstudio_config_dir()`:
    https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  get_path_rstudio_config_dir()

  get_path_rstudio_config_dir("dictionaries")
}
if (interactive()) {
  get_path_rstudio_keybindings_dir()
}
```

</div>

</div>

</div>
