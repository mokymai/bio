# bio 0.3.1

## `rstudio_compare_user_settings()` improvements

* `rstudio_compare_user_settings()` prints a short, readable summary by
  default (how many settings match, plus a grouped list of what differs)
  instead of a raw `waldo::compare()` dump. Use the new `output` argument
  to control verbosity: `"concise"` (default), `"minimal"` (counts only),
  or `"verbose"` (full `waldo::compare()` output for debugging).

* New `source` argument (`"auto"`, `"live"`, `"file"`) controls how
  "current" settings are read: live via `rstudioapi` (requires a running
  RStudio session), from the saved `rstudio-prefs.json` file on disk, or
  automatically picking whichever is available.

* `rstudio_compare_user_settings()` now works without a running RStudio
  session, comparing against the saved `rstudio-prefs.json` file instead of
  failing. Settings left at RStudio's built-in default (and therefore
  absent from that file) are backfilled from the locally installed
  RStudio's preference schema, so they're no longer misreported as
  "missing" — including nested settings like `panes$hiddenTabSet`.

## Headless RStudio support

* `rstudio_reset_user_settings()` and `rstudio_set_preferences()` now work
  when called outside RStudio (e.g. via `Rscript`), merging preference
  JSON files directly instead of requiring a live `rstudioapi` session.

* `rstudio_reset_gmc()` was split into `rstudio_configure_defaults()`
  (headless-capable: preferences, keybindings, snippets, directories,
  dictionaries) and `rstudio_reset_session_state()` (requires a live
  RStudio session: tabs, workspace, layout, theme, console, history).
  `rstudio_reset_gmc()` remains as a thin wrapper calling both, for
  backward compatibility.

* Added internal helpers for detecting an installed (not necessarily
  running) RStudio Desktop and its version, used to support the above.

* New `get_rstudio_install_scope()` reports whether the local RStudio
  Desktop install is system-wide ("all users") or per-user ("just me").
  `find_rstudio_install_dir()` now looks for both, checking the per-user
  registry hive on Windows that it previously missed.

## Other changes

* Documentation and AI-assistant context files updated to match current
  behavior.

* Fixed `rstudio_delete_spellcheck_dictionaries(ask = FALSE)`, which could
  previously fail before removing the selected dictionary directory.

* Fixed `open_project()` when called with a supplied `proj_list` and no
  `proj_list_path`.

* Updated RStudio Desktop support links to Posit's current support site and
  corrected current Linux/macOS internal-state paths to
  `~/.local/share/rstudio`.

* Updated remaining RStudio documentation links to `docs.posit.co` and
  documented the unverified Windows/RStudio command-history reset behavior.

* Fixed RStudio install-scope classification tests on non-Windows runners and
  made per-user path detection separator- and path-boundary-safe.

* Removed retired, commented-out implementation experiments from the package
  source.
