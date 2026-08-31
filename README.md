Package ***bio***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- # Package **_bio_** -->
<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.3.2-brightgreen.svg)](https://github.com/mokymai/bio)
[![R-CMD-check](https://github.com/mokymai/bio/workflows/R-CMD-check/badge.svg)](https://github.com/mokymai/bio/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2026--08--31-yellowgreen.svg)](/commits/master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/mokymai/bio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mokymai/bio/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/mokymai/bio/branch/master/graph/badge.svg)](https://app.codecov.io/gh/mokymai/bio)
<!-- badges: end -->

<!-- [![GitHub last commit](https://img.shields.io/github/last-commit/mokymai/bio)](https://github.com/mokymai/bio) -->

Convenience functions to facilitate installation and management of
required resources for course unit ***Biostatistics*** (*BS*).

<center>
<br> <font color="darkred" size=4> This package is intended to be used
with <b><a href="https://posit.co/download/rstudio-desktop/">RStudio
2026.08</a></b> or newer<br> and
<b><a href="https://www.r-project.org/">R 4.6.1</a></b> or newer.<br>
</font> <font color="darkred" size=3> Functions may not work correctly
with the previous versions of RStudio and R. </font> <br><br><br>
</center>
<!--
The package metadata permits installation on R 4.2.0 or newer. The current R
release and RStudio 2026.08 are the actively supported classroom baseline;
compatibility with selected older R versions is checked in CI but is not the
primary teaching configuration.
-->

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

<details>
<summary>
Install from GitHub
</summary>

To install this package from GitHub:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("mokymai/bio", dependencies = TRUE)
```

Note: installation from *GitHub* may require
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) on Windows.

</details>

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

-   `"main"` (default) – checks for “R”, “RStudio”, “Quarto”, and
    “Rtools” (“Windows” only) or “R Build Tools” (non-“Windows” only).
-   `"all"` – additionally checks for presence of “XQuartz” (“Mac”
    only), “Git” (all systems), and (sometimes) “Meld”.
    <!-- This check may not work properly on "Mac" and "Linux". -->

Installation and release information is maintained by the [R
Project](https://www.r-project.org/), [RStudio
IDE](https://docs.posit.co/ide/user/), and
[Quarto](https://quarto.org/docs/get-started/) projects. Windows users
who build packages from source should also consult the [Rtools
documentation](https://cran.r-project.org/bin/windows/Rtools/).

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
tools from the **wellspell.addin** package. See Posit’s guide to
[spelling
dictionaries](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html#spelling).

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

-   `"bio-default"` – shortcut keys recommended for BS course.
-   `"rstudio-default"` – RStudio defaults.

``` r
bio::rstudio_reset_keybindings(to = "bio-default")
## ✔ Back up copy of shortcut keys was saved as  'C:/Users/User/.R/_backup/keybindings/addins__backup_260824_212734.json'
## ✔ Shortcut keys were reset to bio-default.
```

**Table 1.** Some examples of shortcut keys set with the `"bio-default"`
option.

| Shortcut                                                     | Action                                                                                                                                           |
|--------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd>                 | Insert inline LaTeX equation (*Quarto*).                                                                                                         |
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>A</kbd>                 | Use “Tidyverse” style on selected lines of R code. Package [**styler**](https://CRAN.R-project.org/package=styler) must be installed (*R code*). |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd> | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin) must be installed.                    |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>P</kbd> | Align parts of selected lines at a custom pattern.                                                                                               |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\]</kbd>               | Change heading one level down (*Quarto*) **\[!\]**.                                                                                              |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\[</kbd>               | Change heading one level up or remove heading (*Quarto*) **\[!\]**.                                                                              |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>;</kbd>                | Insert lower opening quotes `„`.                                                                                                                 |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>’</kbd>                | Insert upper closing quotes `“`.                                                                                                                 |
| <kbd>Ctrl</kbd>+<kbd>R</kbd>                                 | **Insert/Enclose with R code block** (*Quarto*) **\[!\]** .                                                                                      |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>T</kbd>                  | Insert inline code highlighted as R code (Quarto).                                                                                               |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad8</kbd>            | Insert operator `%*%` (*R code*).                                                                                                                |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad7</kbd>            | Insert operator `%in%` (*R code*).                                                                                                               |
| <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>D</kbd>                 | Document R package (that you are building).                                                                                                      |

<!-- : **Table 1.** Some examples of shortcut keys set with option `"bio-default"`{.r}. -->
<!--  -->
<!-- Shortcut  | Action -->
<!-- --------- | ------------------------------------------------------------------------------------------------------------------------------------------- -->
<!-- Ctrl Shift Alt S  | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin){target="_blank"} must be installed. -->
<!-- Shift Alt A          | Use "Tidyverse" style on selected lines of R code. Package **styler** must be installed (*R code*). -->
<!-- Alt Shift D          | Document R package (that you are building). -->
<!-- Ctrl Shift Alt P  | Align parts of selected lines at a custom pattern. -->
<!-- Ctrl Shift ;         | Insert lower opening quotes `„`. -->
<!-- Ctrl Shift '         | Insert upper closing quotes `“`. -->
<!-- Ctrl R                  | **Insert/Enclose with R code block** (*Quarto*) **[!]** . -->
<!-- Ctrl Alt T           | Insert inline code highlighted as R code (Quarto). -->
<!-- Shift Alt S          | Insert inline LaTeX equation (*Quarto*). -->
<!-- Ctrl Alt NumPad0     | Remove heading/section name (*Quarto*). -->
<!-- Ctrl Alt NumPad1     | Replace line into/Insert heading (section name) of level 1 (*Quarto*). -->
<!-- Ctrl Alt NumPad2     | ... of level 2 (*Quarto*). -->
<!-- Ctrl Alt NumPad3     | ... of level 3 (*Quarto*). -->
<!-- Ctrl Alt NumPad4     | ... of level 4 (*Quarto*). -->
<!-- Ctrl Alt NumPad5     | ... of level 5 (*Quarto*). -->
<!-- Ctrl Alt NumPad6     | ... of level 6 (*Quarto*). -->
<!-- Ctrl Alt NumPad7     | Insert operator `%in%` (*R code*). -->
<!-- Ctrl Alt NumPad8     | Insert operator `%*%` (*R code*). -->

For more information, see
<a href="https://docs.posit.co/ide/user/ide/guide/productivity/custom-shortcuts.html" target="_blank">Customizing Keyboard Shortcuts</a>
in RStudio.

### RStudio settings

This function resets RStudio user preferences and settings.

RStudio documents the corresponding point-and-click options in its
[Custom Settings
guide](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html)
and explains editor themes in the [Themes
guide](https://docs.posit.co/ide/user/ide/guide/ui/appearance.html).

Run the function without arguments to see the available options.

``` r
bio::rstudio_reset_user_settings()
#> Error: The value of argument 'to' is missing.
#> Possible choices: 'bio-default', 'bio-dark-blue', 'bio-black', 'rstudio-default'.
```

The options:

-   `"bio-default"` – recommended `bio` settings (and light theme
    “Textmate (default)”);
-   `"bio-dark-blue"` – recommended `bio` settings (and dark blue theme
    “Cobalt”);
-   `"bio-black"` – recommended `bio` settings (and black theme
    “Chaos”);
-   `"rstudio-default"` – default RStudio settings.

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

Use `get_rstudio_install_scope()` to determine whether the installed
RStudio Desktop application is a per-user or system-wide installation.
This affects the location of RStudio application files only; personal
settings and keybindings remain per-user in either case.

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

For more information, see RStudio’s
<a href="https://docs.posit.co/ide/user/ide/guide/productivity/snippets.html" target="_blank">Code Snippets guide</a>
and the
<a href="https://gegznav.github.io/snippets/" target="_blank"><strong>snippets</strong> package website</a>.

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

## Related packages and tools

The following projects overlap with or complement parts of `bio`. They
are listed to help users choose a narrower or more general tool when
appropriate:

-   [**rstudio.prefs**](https://CRAN.R-project.org/package=rstudio.prefs),
    by S.A. van der Wulp and Daniel D. Sjoberg, is the closest CRAN
    alternative for managing RStudio preferences and addin shortcuts
    across devices.
-   [**rstudioapi**](https://CRAN.R-project.org/package=rstudioapi), by
    Kevin Ushey, JJ Allaire, Hadley Wickham, and Gary Ritchie, exposes
    the supported API for interacting with a running RStudio session.
    `bio` uses it for live IDE operations.
-   [**renv**](https://CRAN.R-project.org/package=renv), by Kevin Ushey
    and Hadley Wickham, manages reproducible project-local package
    libraries and lockfiles. This complements `bio`’s package discovery
    helpers.
-   [**styler**](https://CRAN.R-project.org/package=styler), by Kirill
    Müller, Lorenz Walthert, and Indrajeet Patil, formats R code and
    powers one of the optional course shortcuts.
-   [**drat**](https://CRAN.R-project.org/package=drat), by Dirk
    Eddelbuettel and Carl Boettiger, creates and manages CRAN-like
    repositories; the `mokymai` repository used to distribute `bio` is
    built with this approach.

The GitHub-only
[**wellspell.addin**](https://github.com/nevrome/wellspell.addin)
provides interactive spellchecking in RStudio. It is useful alongside
the dictionaries installed by `bio`, but is not currently a CRAN
package.

<!-- ## RStudio projects -->
<!-- Open (recently used) RStudio project. -->
<!-- ```{r README-11, eval=FALSE} -->
<!-- bio::open_project() -->
<!-- ##  -->
<!-- ## Choose the name of the project (press 0 to cancel):  -->
<!-- ##  -->
<!-- ##  1: project-1  -->
<!-- ##  2: _learn -->
<!-- ##  3: biostatistics -->
<!-- ##   -->
<!-- ##  Selection: 0 -->
<!-- ##  Cancelled by user. -->
<!-- ``` -->
<!-- Copy the recent projects to your personal project list. -->
<!-- ```{r README-12, eval=FALSE} -->
<!-- bio::update_rstudio_proj_list_user() -->
<!-- ``` -->
