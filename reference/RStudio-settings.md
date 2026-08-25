# Reset RStudio settings

Reset RStudio to use predefined set of settings/preferences. Correctly
works only with RStudio 1.3 or newer. Recommended to use with RStudio
2022.07.1 or newer.

## Usage

``` r
rstudio_reset_user_settings(to, backup = TRUE, ask = TRUE)
```

## Arguments

- to:

  The name of pre-defined set of RStudio settings/preferences. Options:
  "rstudio-default", "bio-default", "bio-dark-blue", "bio-black".

- backup:

  (logical) If `TRUE`, a backup copy of files with settings is created.

- ask:

  (logical) If `TRUE`, additional confirmation to reset settings is
  required.

## Details

Settings that can be used in `rstudio-prefs.json` file:
https://docs.rstudio.com/ide/server-pro/session-user-settings.html

## See also

[`get_path_rstudio_config_file()`](https://mokymai.github.io/bio/reference/RStudio-config-file.md)

On [Customizing
RStudio](https://support.rstudio.com/hc/en-us/articles/200549016-Customizing-the-RStudio-IDE)
using point-and-click method.

On [Configuration and
Settings](https://www.rstudio.com/blog/rstudio-1-3-preview-configuration/).

A list of [Session User
Settings](https://docs.rstudio.com/ide/server-pro/session_user_settings/session_user_settings.html)
to be used with
[`rstudioapi::writeRStudioPreference()`](https://rstudio.github.io/rstudioapi/reference/writeRStudioPreference.html).

On [RStudio setting
locations](https://docs.rstudio.com/ide/desktop-pro/settings/settings.html).

On [Resetting RStudio Desktop's
State](https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State).

StackOverflow threads on export/import RStudio of user preferences:

- https://stackoverflow.com/a/55940249/4783029

- https://stackoverflow.com/a/54982341/4783029

## Examples

``` r
if (interactive()) {

  rstudio_reset_user_settings(to = "rstudio-default")
  rstudio_reset_user_settings(to = "bio-default")
  rstudio_reset_user_settings(to = "bio-dark-blue")
  rstudio_reset_user_settings(to = "bio-black")

}
```
