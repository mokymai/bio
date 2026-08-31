<div id="main" class="col-md-9" role="main">

# Manage RStudio Configuration (Preferences) File

<div class="ref-description section level2">

Manage file with RStudio configuration (user preferences).

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_path_rstudio_config_file(which = "current")

open_rstudio_config_file(which = "current")
```

</div>

</div>

<div class="section level2">

## Arguments

-   which:

    (character) type of settings:

    -   "current": file with current RStudio settings (that differ from
        the defaults);

    -   "bio-default": file with setting from "bio-default" list (except
        theme);

    -   "rstudio-default": a preset compiled from the most recent
        RStudio settings documentation available when it was downloaded.
        For comparisons, `rstudio_compare_user_settings()` fills unset
        values from the local RStudio `user-prefs-schema.json` when
        available.

</div>

<div class="section level2">

## See also

<div class="dont-index">

-   `get_path_rstudio_config_dir()`

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  get_path_rstudio_config_file()

  get_path_rstudio_config_file("bio-default")
}
```

</div>

</div>

</div>
