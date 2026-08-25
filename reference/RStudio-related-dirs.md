# Directories of RStudio-Related Files

Directories of RStudio (desktop) settings, preferences and other files.

- \`get_path_rstudio_config_dir()“ - gets path to RStudio configuration
  directory (and its sub-directories).

## Usage

``` r
get_path_rstudio_config_dir(..., .check = FALSE)

get_path_rstudio_internal_state_dir(..., .check = FALSE)

get_path_rstudio_keybindings_dir()

open_rstudio_config_dir()

open_rstudio_internal_state_dir()

open_rstudio_keybindings_dir()
```

## Arguments

- ...:

  (character) Parts of the path. Path to sub-directories.

- .check:

  (logical) If `TRUE`, additionally checks for path existence.

## Value

(string) path to RStudio configuration directory. When `.check = TRUE`,
renturns error, if the path does not exist.

## See also

- [`fs::file_show()`](https://fs.r-lib.org/reference/file_show.html),
  [`browseURL()`](https://rdrr.io/r/utils/browseURL.html),

- [`rstudioapi::navigateToFile()`](https://rstudio.github.io/rstudioapi/reference/navigateToFile.html),

- [`utils::file.edit()`](https://rdrr.io/r/utils/file.edit.html)

&nbsp;

- `get_path_rstudio_config_dir()`:
  https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State

## Examples

``` r
if (interactive()) {
  get_path_rstudio_config_dir()

  get_path_rstudio_config_dir("dictionaries")
}
if (interactive()) {
  get_path_rstudio_keybindings_dir()
}
```
