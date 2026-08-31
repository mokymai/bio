<div id="main" class="col-md-9" role="main">

# Install classroom/lab default configuration for RStudio and R

<div class="ref-description section level2">

Installs the "bio-default" preferences, keybindings and snippets,
updates spellcheck dictionaries and TinyTeX. Unlike
`rstudio_reset_session_state()`, every step here can run outside a live
RStudio session. Settings and keybindings are written directly to disk,
and dictionary installation falls back to downloading the archive
directly.

Clears everything that only exists while RStudio is running: open tabs
(files/plots/help/viewer/projects/terminals/documents), the R workspace,
pane layout and zoom, theme, and, as the very last steps, the R console
and the command history. This function needs a live RStudio session (it
is built entirely on `rstudioapi::executeCommand()`).

A thin wrapper that runs `rstudio_configure_defaults()` (file-based
configuration, works even outside RStudio) followed by
`rstudio_reset_session_state()` (runtime state, requires a live RStudio
session), kept for internal backward compatibility.

Clear and Reset R and RStudio settings and preferences.

Uses `utils::savehistory()` and `utils::loadhistory()` to back up and
clear the R command history outside RStudio. In a running RStudio
session, it delegates to `rstudio_clear_history()` so RStudio clears its
active history.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_configure_defaults(force_update_dictionaries = FALSE)

rstudio_reset_session_state(...)

rstudio_reset_gmc(..., force_update_dictionaries = FALSE)

clear_r_history(backup = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   force_update_dictionaries:

    Logical scalar. If `TRUE`, the dictionaries are refreshed even when
    the current locale is present.

-   ...:

    Further arguments used by `restriction_status()` for compatibility.

-   backup:

    Logical scalar. If `TRUE`, save a timestamped backup before clearing
    the active history.

</div>

<div class="section level2">

## Value

Invisibly returns a data frame with one row per step, its `ok` status,
and an error `message` (if any).

Invisibly returns a data frame with one row per step, its `ok` status,
and an error `message` (if any).

Invisibly returns a list with `configure` and `session_state` summary
data frames.

Invisibly returns `NULL`.

</div>

<div class="section level2">

## Details

Every step is run through `run_reset_step()`: a failure in one step is
reported but does not prevent the remaining steps from running, and none
of the steps open interactive confirmation popups.

Every step is run through `run_reset_step()`, so a failure in one step
is reported but does not prevent the remaining steps from running, and
none of the steps open interactive confirmation popups.

This helper is intentionally conservative and protects destructive reset
actions behind a simple override flag (see `restriction_status()`). It
is meant to be run on classroom/lab computers, never on a developer's
own, non-standardized RStudio session.

</div>

</div>
