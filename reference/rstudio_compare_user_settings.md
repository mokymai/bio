<div id="main" class="col-md-9" role="main">

# Show differences in sets of settings

<div class="ref-description section level2">

Show differences in sets of settings

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_compare_user_settings(
  to = "bio-default",
  source = "auto",
  output = "concise"
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   to:

    One of: "bio-default", "rstudio-default" (or an unambiguous
    abbreviation of these).

-   source:

    One of:

    -   `"auto"` (default): use a live RStudio session if one is
        running, otherwise fall back to the saved preferences file.

    -   `"live"`: read "current" settings live via
        `rstudioapi::readRStudioPreference()`; fails gracefully if
        RStudio is not running.

    -   `"file"`: always read "current" settings from the saved
        `rstudio-prefs.json` file on disk, even if RStudio is running.

-   output:

    One of:

    -   `"minimal"`: print only the match/difference counts.

    -   `"concise"` (default): print how many settings match, plus a
        short list of what differs.

    -   `"verbose"`: fall back to the full `waldo::compare()` output
        (useful for deep debugging, but can be very verbose for large
        preference sets).

</div>

<div class="section level2">

## Value

Invisibly, a data frame of per-key comparison results
(`"concise"`/`"minimal"`), or the `waldo::compare()` result
(`"verbose"`). Settings, which are not in `to` list, will not be
displayed at all. Returns `invisible(NULL)` if the requested `source` is
unavailable (e.g. `"live"` without a running RStudio session, or
`"file"`/`"auto"` with no saved preferences file).

</div>

<div class="section level2">

## Details

`source = "live"` (or `"auto"` with RStudio running) reads "current"
settings live via `rstudioapi::readRStudioPreference()`.
`source = "file"` (or `"auto"` without RStudio running) reads the saved
`rstudio-prefs.json` file on disk (see
`get_path_rstudio_config_file()`). Since that file only stores values
overridden from RStudio's built-in defaults, keys left at their default
are also filled in (when possible) from the local RStudio installation's
`user-prefs-schema.json`, so they aren't misreported as "missing". The
file-based comparison may still not reflect unsaved, in-memory session
state.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  rstudio_compare_user_settings(to = "bio-default")
  rstudio_compare_user_settings(to = "rstudio-default")
  rstudio_compare_user_settings(to = "bio-default", source = "file")
  rstudio_compare_user_settings(to = "bio-default", output = "minimal")
  rstudio_compare_user_settings(to = "bio-default", output = "verbose")
}
```

</div>

</div>

</div>
