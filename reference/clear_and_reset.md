# Install classroom/lab default configuration for RStudio and R

Installs the "bio-default" preferences, keybindings and snippets,
updates spellcheck dictionaries and TinyTeX, and (re)creates the course
working directories. Unlike `rstudio_reset_session_state()`, every step
here is file-based (settings/keybindings are written straight to disk,
see `rstudio_set_preferences()`) or a plain package install. Steps that
do need a live RStudio session (such as downloading dictionaries via
`.rs.downloadAllDictionaries()`, or applying a theme) are attempted but
simply reported as failed/skipped when run headlessly, without stopping
the remaining steps.

Clears everything that only exists while RStudio is running: open tabs
(files/plots/help/viewer/projects/terminals/documents), the R workspace,
pane layout and zoom, theme, and, as the very last steps, the R console
and the command history. This function needs a live RStudio session (it
is built entirely on
[`rstudioapi::executeCommand()`](https://rstudio.github.io/rstudioapi/reference/executeCommand.html)).

A thin wrapper that runs `rstudio_configure_defaults()` (file-based
configuration, works even outside RStudio) followed by
`rstudio_reset_session_state()` (runtime state, requires a live RStudio
session), kept for internal backward compatibility.

Clear and Reset R and RStudio settings and preferences.

Uses [`utils::savehistory()`](https://rdrr.io/r/utils/savehistory.html)
and [`utils::loadhistory()`](https://rdrr.io/r/utils/savehistory.html)
to back up and clear the R command history outside RStudio. In a running
RStudio session, it delegates to `rstudio_clear_history()` so RStudio
clears its active history.

## Usage

``` r
rstudio_configure_defaults(force_update_dictionaries = FALSE)

rstudio_reset_session_state(...)

rstudio_reset_gmc(..., force_update_dictionaries = FALSE)

clear_r_history(backup = TRUE)
```

## Arguments

- force_update_dictionaries:

  Logical scalar. If `TRUE`, the dictionaries are refreshed even when
  the current locale is present.

- ...:

  Further arguments used by
  [`restriction_status()`](https://mokymai.github.io/bio/reference/restriction_status.md)
  for compatibility.

- backup:

  Logical scalar. If `TRUE`, save a timestamped backup before clearing
  the active history.

## Value

Invisibly returns a data frame with one row per step, its `ok` status,
and an error `message` (if any).

Invisibly returns a data frame with one row per step, its `ok` status,
and an error `message` (if any).

Invisibly returns a list with `configure` and `session_state` summary
data frames.

Invisibly returns `NULL`.

## Details

Every step is run through
[`run_reset_step()`](https://mokymai.github.io/bio/reference/run_reset_step.md):
a failure in one step is reported but does not prevent the remaining
steps from running, and none of the steps open interactive confirmation
popups.

Every step is run through
[`run_reset_step()`](https://mokymai.github.io/bio/reference/run_reset_step.md),
so a failure in one step is reported but does not prevent the remaining
steps from running, and none of the steps open interactive confirmation
popups.

This helper is intentionally conservative and protects destructive reset
actions behind a simple override flag (see
[`restriction_status()`](https://mokymai.github.io/bio/reference/restriction_status.md)).
It is meant to be run on classroom/lab computers, never on a developer's
own, non-standardized RStudio session.
