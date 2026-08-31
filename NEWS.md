# bio 0.3.2

## RStudio settings and dictionaries

* Preference-file resets are now transactional: failed preset reads or writes
  restore the original file exactly, or remove partial output when no original
  file existed. Errors encountered while applying preferences are no longer
  hidden.

* `rstudio_install_spellcheck_dictionaries()` now works outside RStudio by
  downloading and validating Posit's dictionary archive, retrying interrupted
  transfers, and falling back to the system `curl` command when available. It
  reports progress through UI messages and invisibly returns its success status.
  `rstudio_download_spellcheck_dictionaries()` remains a compatibility alias.

* Dictionary installation no longer reports success when extraction fails.
  `utils::unzip()` signals extraction problems as warnings rather than errors,
  so the result is now confirmed against the dictionary files that actually
  reached the target directory.

* RStudio Desktop detection no longer mistakes the local runtime state
  directory for an installation directory.

* `rstudio_configure_defaults()` no longer creates or deletes
  `~/Desktop/BS-pratybos/`. Deleting a student's course folder without
  confirmation was unsafe, so the step was dropped; create the folder manually
  if you need it. `~/R/main` is still created by
  `rstudio_reset_user_settings()` for the `bio-*` presets.

## Reliability and maintenance

* Expanded the package description and replaced the broad `backup.tools`
  namespace import with package-qualified calls and the single narrow import
  required for the `open_backup_dir()` re-export.

* Removed obsolete global-variable declarations, modernized interactive
  examples, and clarified the generated `..Rcheck/` ignore rule.

* Online R, RStudio, and Quarto version checks now treat connectivity,
  transport, parsing, and unexpected-response failures as unavailable version
  information instead of aborting program checks.

* GitHub Actions now tests installed-package behavior across current, devel,
  and older R releases. Package publication and generated-documentation jobs
  are serialized, deployment failures remain visible, and documentation jobs
  follow the current R release rather than a hardcoded patch version. Generated
  documentation is committed back to same-repository pull-request branches;
  read-only fork pull requests receive a patch artifact.

* Regression coverage was expanded for preference rollback, confirmation and
  reset summaries, malformed version responses, dictionary installation,
  bundled JSON assets, and deterministic internal helpers.

* The test suite no longer reaches the real network: the connectivity probe is
  mocked everywhere an online version check is exercised, so results no longer
  depend on runner connectivity.

* The lint workflow now fails when `lintr` reports findings
  (`LINTR_ERROR_ON_LINT`), and the package is lint-clean. `.lintr` documents
  why `object_usage_linter` is disabled (glue interpolation through
  `usethis::ui_*()`) and allows camelCase names required by external APIs.

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

* Fixed `get_path_rstudio_config_dir()` when `XDG_CONFIG_HOME` is set by
  appending the `rstudio` sub-directory per the XDG Base Directory specification.

* Added isolated regression coverage for dictionary deletion and canceled
  RStudio user-settings resets.

* Added mocked regression coverage for RStudio restart/reload commands and
  spellcheck dictionary installation.

* Expanded automated coverage across exported package, path, project, file
  opening, RStudio integration, and offline program-information helpers.

* Fixed `clear_r_history()` to use RStudio's history commands in a live
  RStudio session, including on Windows.

* Clarified that the bundled `rstudio-default` preset was compiled from the
  most recent official RStudio settings documentation available in 2022;
  setting comparisons use the locally installed RStudio schema when available.

* Added regression coverage for `open_project()` availability filtering and
  duplicate project names outside an interactive RStudio session.

* Updated RStudio Desktop support links to Posit's current support site and
  corrected current Linux/macOS internal-state paths to
  `~/.local/share/rstudio`.

* Updated remaining RStudio documentation links to `docs.posit.co` and
  documented the unverified Windows/RStudio command-history reset behavior.

* Fixed RStudio install-scope classification tests on non-Windows runners and
  made per-user path detection separator- and path-boundary-safe.

* Removed retired, commented-out implementation experiments from the package
  source.
