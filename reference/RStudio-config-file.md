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

  - "rstudio-default": file with most of default RStudio settings listed
    at
    https://docs.rstudio.com/ide/server-pro/session_user_settings/session_user_settings.html
    (downloaded on 2022-08-03).

## See also

- [`get_path_rstudio_config_dir()`](https://mokymai.github.io/bio/reference/RStudio-related-dirs.md)

## Examples

``` r
if (interactive()) {
  get_path_rstudio_config_file()

  get_path_rstudio_config_file("bio-default")
}
```
