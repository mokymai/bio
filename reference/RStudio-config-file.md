# Manage RStudio Configuration (Preferences) File

Manage file with RStudio configuration (user preferences).

## Usage

``` r
get_path_rstudio_config_file(which = "current")

open_rstudio_config_file(which = "current")
```

## Arguments

- which:

  (character) type of settings:

  - "current": file with current RStudio settings (that differ from the
    defaults);

  - "bio-default": file with setting from "bio-default" list (except
    theme);

  - "rstudio-default": a preset compiled from the most recent RStudio
    settings documentation available when it was downloaded. For
    comparisons,
    [`rstudio_compare_user_settings()`](https://mokymai.github.io/bio/reference/rstudio_compare_user_settings.md)
    fills unset values from the local RStudio `user-prefs-schema.json`
    when available.

## See also

- [`get_path_rstudio_config_dir()`](https://mokymai.github.io/bio/reference/RStudio-related-dirs.md)

## Examples

``` r
if (interactive()) {
  get_path_rstudio_config_file()

  get_path_rstudio_config_file("bio-default")
}
```
