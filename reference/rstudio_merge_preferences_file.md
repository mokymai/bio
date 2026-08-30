# Merge a preset preferences JSON file straight into `rstudio-prefs.json`.

Headless fallback used by `rstudio_set_preferences()` when no RStudio
session is available (e.g. run via `Rscript`). Values are merged
directly into the preferences file instead of going through
[`rstudioapi::writeRStudioPreference()`](https://rstudio.github.io/rstudioapi/reference/writeRStudioPreference.html),
which requires a live session.

## Usage

``` r
rstudio_merge_preferences_file(file)
```

## Arguments

- file:

  Path to a JSON file with preferences to merge in.

## Value

Logical scalar, `TRUE` on success.
