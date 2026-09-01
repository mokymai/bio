<div id="main" class="col-md-9" role="main">

# Reset RStudio settings

<div class="ref-description section level2">

Reset RStudio to use predefined set of settings/preferences. Correctly
works only with RStudio 1.3 or newer. Recommended to use with RStudio
2022.07.1 or newer.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_reset_user_settings(to, backup = TRUE, ask = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   to:

    The name of pre-defined set of RStudio settings/preferences.
    Options: "rstudio-default", "bio-default", "bio-dark-blue",
    "bio-black".

-   backup:

    (logical) If `TRUE`, a backup copy of files with settings is
    created.

-   ask:

    (logical) If `TRUE`, additional confirmation to reset settings is
    required.

</div>

<div class="section level2">

## Details

Posit's [Custom Settings
guide](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html)
documents the point-and-click interface for user preferences, preference
files, and configuration directories.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`get_path_rstudio_config_file()`

[Session User
Settings](https://docs.posit.co/ide/server-pro/session_user_settings/session_user_settings.html)
lists settings accepted by `rstudioapi::writeRStudioPreference()`.

On [RStudio setting
locations](https://docs.posit.co/ide/desktop-pro/settings/settings.html).

On [Resetting RStudio Desktop's
State](https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State).

For a broader interface to preference files and addin shortcuts, see the
[rstudio.prefs](https://CRAN.R-project.org/package=rstudio.prefs)
package by S.A. van der Wulp and Daniel D. Sjoberg.

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {

  rstudio_reset_user_settings(to = "rstudio-default")
  rstudio_reset_user_settings(to = "bio-default")
  rstudio_reset_user_settings(to = "bio-dark-blue")
  rstudio_reset_user_settings(to = "bio-black")

}
```

</div>

</div>

</div>
