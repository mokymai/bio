<div id="main" class="col-md-9" role="main">

# Merge a preset preferences JSON file straight into `rstudio-prefs.json`.

<div class="ref-description section level2">

Headless fallback used by `rstudio_set_preferences()` when no RStudio
session is available (e.g. run via `Rscript`). Values are merged
directly into the preferences file instead of going through
`rstudioapi::writeRStudioPreference()`, which requires a live session.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_merge_preferences_file(file)
```

</div>

</div>

<div class="section level2">

## Arguments

-   file:

    Path to a JSON file with preferences to merge in.

</div>

<div class="section level2">

## Value

Logical scalar, `TRUE` on success.

</div>

</div>
