# Package ***bio***

Convenience functions to facilitate installation and management of
required resources for course unit ***Biostatistics*** (*BS*).

  
This package is intended to be used with **RStudio 2026.08** or newer  
and **R 4.6.1** or newer.  
Functions may not work correctly with the previous versions of RStudio
and R.  
  
  

Bug reports and your feedback are welcome at
<https://github.com/mokymai/bio/issues>. In the report, indicate what
operating system you are using.

# Install

To install this package from CRAN-like repository, use code
(recommended):

``` r

repos <- c("https://mokymai.github.io/download/", getOption("repos"))
install.packages("bio", repos = repos)
```

Install from GitHub

To install this package from GitHub:

``` r

if (!require(remotes)) install.packages("remotes")
remotes::install_github("mokymai/bio", dependencies = TRUE)
```

Note: installation from *GitHub* may require *RTools* on *Windows*.

# Examples

## Check information about OS and user

Check information about the operating system (OS) and R-related user
information.

``` r

bio::check_user_info()
## # A tibble: 9 × 2
##   Setting          Value
##   <chr>            <fs::path>
## 1 Operating system Windows 10 x64 (build 19045)
## 2 Platform         x86_64-w64-mingw32/x64
## 3 LOGNAME
## 4 USERNAME         user
## 5 USERPROFILE      C:/Users/user
## 6 HOME             C:/Users/user
## 7 R_USER           C:/Users/user
## 8 R_HOME           C:/PROGRA~1/R/R-4.6.1
## 9 R_LIBS_USER      C:/Users/user/AppData/Local/R/win-library/4.6
```

## Check information about programs

Check the versions and availability of installed R-related and other
programs. Options:

- `"main"` (default) – checks for “R”, “RStudio”, “Quarto”, and “Rtools”
  (“Windows” only) or “R Build Tools” (non-“Windows” only).
- `"all"` – additionally checks for presence of “XQuartz” (“Mac” only),
  “Git” (all systems), and (sometimes) “Meld”.

``` r

bio::check_installed_programs()
## ✔ Program R (4.6.1) is installed (4.6.1 is available online).
## ✔ Program RStudio (2026.8.1.195) is installed (2026.8.1.195 is available online).
## ✔ Tool Quarto (1.10.18) is installed (1.10.18 is available online).
```

``` r

bio::check_installed_programs("all")
## ✔ Program R (4.6.1) is installed (4.6.1 is available online).
## ✔ Program RStudio (2026.8.1.195) is installed (2026.8.1.195 is available online).
## ✔ Tool Quarto (1.10.18) is installed (1.10.18 is available online).
## ✔ Tool Rtools is installed.
## ✔ Program Git is installed.
## ✔ Program Meld is installed.
```

## RStudio settings and user preferences

> Make sure that you understand what you are doing when using functions
> that reset RStudio key bindings (shortcut keys), settings, or
> snippets. **You may lose *all* your previous settings.**

### Dictionaries

This function downloads or updates **hunspell** spelling-checking
dictionaries (including Lithuanian) that can be used by RStudio or by
tools from the **wellspell.addin** package.

``` r

bio::rstudio_install_spellcheck_dictionaries()
```

### Shortcut keys

This function changes shortcut keys in RStudio.

Run the function without arguments to see the available options.

``` r

bio::rstudio_reset_keybindings()
## Error: The value of argument 'to' is missing.
## Possible options: 'bio-default', 'rstudio-default'.
```

Options:

- `"bio-default"` – shortcut keys recommended for BS course.
- `"rstudio-default"` – RStudio defaults.

``` r

bio::rstudio_reset_keybindings(to = "bio-default")
## ✔ Back up copy of shortcut keys was saved as  'C:/Users/User/.R/_backup/keybindings/addins__backup_260824_212734.json'
## ✔ Shortcut keys were reset to bio-default.
```

**Table 1.** Some examples of shortcut keys set with the `"bio-default"`
option.

| Shortcut | Action |
|----|----|
| Shift+Alt+S | Insert inline LaTeX equation (*Quarto*). |
| Shift+Alt+A | Use “Tidyverse” style on selected lines of R code. Package **styler** must be installed (*R code*). |
| Ctrl+Shift+Alt+S | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin) must be installed. |
| Ctrl+Shift+Alt+P | Align parts of selected lines at a custom pattern. |
| Ctrl+Shift+\] | Change heading one level down (*Quarto*) **\[!\]**. |
| Ctrl+Shift+\[ | Change heading one level up or remove heading (*Quarto*) **\[!\]**. |
| Ctrl+Shift+; | Insert lower opening quotes `„`. |
| Ctrl+Shift+’ | Insert upper closing quotes `“`. |
| Ctrl+R | **Insert/Enclose with R code block** (*Quarto*) **\[!\]** . |
| Ctrl+Alt+T | Insert inline code highlighted as R code (Quarto). |
| Ctrl+Alt+NumPad8 | Insert operator `%*%` (*R code*). |
| Ctrl+Alt+NumPad7 | Insert operator `%in%` (*R code*). |
| Alt+Shift+D | Document R package (that you are building). |

For more information, see [Customizing Keyboard
Shortcuts](https://docs.posit.co/ide/user/ide/guide/productivity/custom-shortcuts.html)
in RStudio.

### RStudio settings

This function resets RStudio user preferences and settings.

Run the function without arguments to see the available options.

``` r

bio::rstudio_reset_user_settings()
#> Error: The value of argument 'to' is missing.
#> Possible choices: 'bio-default', 'bio-dark-blue', 'bio-black', 'rstudio-default'.
```

The options:

- `"bio-default"` – recommended `bio` settings (and light theme
  “Textmate (default)”);
- `"bio-dark-blue"` – recommended `bio` settings (and dark blue theme
  “Cobalt”);
- `"bio-black"` – recommended `bio` settings (and black theme “Chaos”);
- `"rstudio-default"` – default RStudio settings.

``` r

bio::rstudio_reset_user_settings(to = "bio-default")
```

To check how your settings differ from the defaults in a particular
list, use:

``` r

bio::rstudio_compare_user_settings(to = "bio-default")
## ℹ Show differences between current and bio-default setting lists.
## ✔ No differences
```

Settings that are not in the list will not be displayed. The available
options for `to` are “bio-default” and “rstudio-default”. The default
`source = "auto"` reads settings from a running RStudio session when
available, otherwise it compares the saved `rstudio-prefs.json` file.
Use `source = "file"` to always inspect saved preferences, or
`source = "live"` to require a running RStudio session. The default
concise output summarizes differences; use `output = "minimal"` for
counts only or `output = "verbose"` for the complete technical
comparison.

### RStudio installation scope

Use
[`get_rstudio_install_scope()`](https://mokymai.github.io/bio/reference/get_rstudio_install_scope.md)
to determine whether the installed RStudio Desktop application is a
per-user or system-wide installation. This affects the location of
RStudio application files only; personal settings and keybindings remain
per-user in either case.

``` r

bio::get_rstudio_install_scope()
## [1] "user"
```

### Snippets

The function replaces the current R and Quarto snippets with those from
the “snippets” package.

``` r

snippets::install_snippets_from_package("snippets", backup = TRUE)
## ✔ File with markdown snippets was updated:
##   'C:/Users/User/AppData/Roaming/RStudio/snippets/markdown.snippets'
## ✔ File with r snippets was updated:
##   'C:/Users/User/AppData/Roaming/RStudio/snippets/r.snippets'
##
## ℹ You will be able to use the snippets after RStudio is closed and reopened.
```

For more information about [Code
Snippets](https://support.posit.co/hc/en-us/articles/204463668-Code-Snippets)
in RStudio and on package
[**snippets**](https://gegznav.github.io/snippets/).

### Restart R and RStudio

If you want to see the effect of the changed settings, reload RStudio.
You can use this function to reload RStudio without closing it:

``` r

bio::restart_rstudio()
```

However, in some cases, it is recommended that you close and reopen
RStudio.

To restart the R session without closing RStudio, use:

``` r

bio::restart_r()
```

Useful to apply before trying to install R packages.
