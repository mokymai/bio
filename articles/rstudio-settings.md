<div id="main" class="col-md-9" role="main">

# Managing RStudio settings

<div class="quarto-figure quarto-figure-center">

![Lifecycle: experimental](lifecycle-experimental.svg)

</div>

<div>

> **Lifecycle: experimental**
>
> The functions on this page **overwrite files that belong to you**:
> RStudio preferences, keybindings, and spellcheck dictionaries. They
> are experimental and their preset contents change between releases.
> Back-ups are made by default — keep it that way until you are sure.

</div>

<div class="section level2">

## Preferences

RStudio’s [Custom Settings
guide](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html)
documents the point-and-click interface for these preferences. The CRAN
package
[**rstudio.prefs**](https://CRAN.R-project.org/package=rstudio.prefs),
by S.A. van der Wulp and Daniel D. Sjoberg, is a broader alternative for
programmatic preference-file and addin-shortcut management.

Four presets are available:

| `to`                | What it does                  |
|---------------------|-------------------------------|
| `"rstudio-default"` | RStudio’s own defaults        |
| `"bio-default"`     | course defaults, light theme  |
| `"bio-dark-blue"`   | course defaults, Cobalt theme |
| `"bio-black"`       | course defaults, Chaos theme  |

<div class="cell">

<div id="cb1" class="sourceCode cell-code">

``` r
bio::rstudio_reset_user_settings(to = "bio-default")
```

</div>

</div>

`backup = TRUE` (the default) copies the current `rstudio-prefs.json`
into the backup directory first, and `ask = TRUE` asks for confirmation.
An automated run needs both switched off:

<div class="cell">

<div id="cb2" class="sourceCode cell-code">

``` r
bio::rstudio_reset_user_settings(to = "bio-default", backup = TRUE, ask = FALSE)
```

</div>

</div>

<div class="section level3">

### What happens inside RStudio versus outside it

The two situations genuinely differ, and it is worth knowing which one
you are in:

-   **Outside RStudio** (`Rscript`, a terminal, CI) the presets are
    merged straight into `rstudio-prefs.json`. The write is
    transactional: if any preset fails, the original file is restored
    byte-for-byte, or removed again if there was no original.
-   **Inside a running RStudio session** the presets go through
    `rstudioapi::writeRStudioPreference()`. RStudio owns the file and
    re-persists its in-memory state, so `bio` does not delete or roll
    back the file — doing so would have no effect anyway.

A preference key that your RStudio version does not recognise is
reported in a single warning and skipped; the remaining keys are still
applied. Only an unreadable preset file aborts the operation.

<div>

> **Note**
>
> `ask = TRUE` additionally issues RStudio’s `clearUserPrefs` command.
> That command opens its own confirmation dialog which cannot be
> suppressed, so it is deliberately limited to the interactive path.

</div>

</div>

<div class="section level3">

### Comparing before you change anything

<div class="cell">

<div id="cb3" class="sourceCode cell-code">

``` r
bio::rstudio_compare_user_settings(to = "bio-default")
bio::rstudio_compare_user_settings(to = "rstudio-default", output = "verbose")
```

</div>

</div>

`source` chooses where the “current” values come from:

-   `"auto"` (default) — live values inside RStudio, the file otherwise;
-   `"live"` — `rstudioapi`, and therefore a running session;
-   `"file"` — always `rstudio-prefs.json`.

Because that file only stores values you changed from RStudio’s built-in
defaults, the file-based comparison fills in unset keys from your local
RStudio installation’s preference schema when it can find it.

</div>

</div>

<div class="section level2">

## Keybindings

See Posit’s guide to [custom keyboard
shortcuts](https://docs.posit.co/ide/user/ide/guide/productivity/custom-shortcuts.html),
including conflict handling and the locations of the JSON binding files.

<div class="cell">

<div id="cb4" class="sourceCode cell-code">

``` r
bio::rstudio_reset_keybindings(to = "bio-default")
bio::rstudio_reset_keybindings(to = "rstudio-default", backup = TRUE)
```

</div>

</div>

This is a plain file copy, so it works without a running RStudio
session. Restart RStudio afterwards, or call `bio::rstudio_reload_ui()`.

</div>

<div class="section level2">

## Spellcheck dictionaries

Posit’s [Spelling
settings](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html#spelling)
guide explains how RStudio uses custom dictionaries. The GitHub-only
[**wellspell.addin**](https://github.com/nevrome/wellspell.addin)
provides interactive spellchecking that can use the dictionaries
installed here.

<div class="cell">

<div id="cb5" class="sourceCode cell-code">

``` r
bio::rstudio_install_spellcheck_dictionaries()
bio::rstudio_delete_spellcheck_dictionaries()
```

</div>

</div>

Inside RStudio this delegates to the IDE’s own downloader. Outside
RStudio it downloads Posit’s dictionary archive itself, retrying
interrupted transfers and falling back to the system `curl` command.
Before extracting, the archive is checked: it must contain the expected
dictionary files, and any entry that would write outside the target
directory is refused.

The function invisibly returns `TRUE` or `FALSE`, so you can branch on
it:

<div class="cell">

<div id="cb6" class="sourceCode cell-code">

``` r
ok <- bio::rstudio_install_spellcheck_dictionaries()
if (!ok) message("Dictionaries were not installed.")
```

</div>

</div>

<div>

> **Important**
>
> `secure = FALSE` fetches the archive over an unencrypted connection
> and extracts it into your configuration directory. It warns, and you
> should not need it.

</div>

</div>

<div class="section level2">

## Back-ups

Every destructive helper writes a timestamped copy first. To find them:

<div class="cell">

<div id="cb7" class="sourceCode cell-code">

``` r
bio::open_backup_dir()
```

</div>

</div>

Restoring is a manual file copy — deliberately, so that nothing
overwrites your current state without you asking.

</div>

<div class="section level2">

## The files themselves

<div class="cell">

<div id="cb8" class="sourceCode cell-code">

``` r
bio::get_path_rstudio_config_dir()
bio::get_path_rstudio_config_file(which = "current")
bio::get_path_rstudio_config_file(which = "bio-default")
bio::get_path_rstudio_keybindings_dir()
bio::get_path_rstudio_snippets_dir()
bio::get_path_rstudio_internal_state_dir()
bio::get_path_r_environ()
```

</div>

</div>

Open them instead with `bio::open_rstudio_config_file()`,
`bio::open_rstudio_keybindings_dir()`,
`bio::open_rstudio_snippets_dir()`, or `bio::open_r_environ()`.

The configuration directory (`%APPDATA%/RStudio` on Windows,
`~/.config/rstudio` elsewhere) holds preferences, keybindings, snippets,
and dictionaries. The internal state directory
(`%LOCALAPPDATA%/RStudio`, `~/.local/share/rstudio`) holds session state
and is not something `bio` rewrites.

See also Posit’s guides to [RStudio
settings](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html),
[code
snippets](https://docs.posit.co/ide/user/ide/guide/productivity/snippets.html),
and [editor
themes](https://docs.posit.co/ide/user/ide/guide/ui/appearance.html).

</div>

</div>
