Package ***bio***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- # Package **_bio_** -->

<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.0.8.3-brightgreen.svg)](https://github.com/mokymai/bio)
[![R-CMD-check](https://github.com/mokymai/bio/workflows/R-CMD-check/badge.svg)](https://github.com/mokymai/bio/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2020--09--16-yellowgreen.svg)](/commits/master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

<!-- [![GitHub last commit](https://img.shields.io/github/last-commit/mokymai/bio)](https://github.com/mokymai/bio) -->

Convenience functions to facilitate installation and management of
required resources for course units ***Biostatistics*** (*BS-2020*) and
***Introduction to data analysis with R*** (*R-2020*).

<!-- <center> -->

<!-- <font color="red" size=6> -->

<!-- <br> -->

<!-- Work is still in progress... -->

<!-- </font> -->

<!-- <br> -->

<!-- <font color="darkred" size=4> -->

<!-- This package is tested on <em>Windows 10</em> (64-bit) with RStudio <b>1.2</b>.5042.<br> -->

<!-- Some functions do not work correctly with RStudio <b>1.3</b>.0 or newer yet. -->

<!-- </font> -->

<!-- <br><br> -->

<!-- </center> -->

Bug reports and your feedback are welcome at
<https://github.com/mokymai/bio/issues>. In the report, indicate your
operating system.

# Install

To install this package from CRAN-like repository, use code
(recommended):

``` r
repos <- c("https://mokymai.github.io/download/", getOption("repos"))
install.packages("bio", repos = repos)
```

To install this package from GitHub:

``` r
if (!require(remotes)) {install.packages("remotes")}
remotes::install_github("mokymai/bio", dependencies = TRUE)
```

# Examples

## Check information about OS and user

Check information about operating system (OS) and R-related user
information.

``` r
bio::check_user_info()
## Operating system    Windows 10 x64 (build 18363)
## Platform            x86_64-w64-mingw32/x64 (64-bit)
## LOGNAME
## USERNAME            User
## USERPROFILE         C:/Users/User
## HOME                C:/Users/User/Documents
## R_USER              C:/Users/User/Documents
## R_HOME              C:/PROGRA~1/R/R-40~1.2
## R_LIBS_USER         C:/R/win-library/4.0
```

## Check information about programs

Check versions of presence of installed R-related and other programs.
Options:

  - `"main"` (default) – checks for “R”, “RStudio”, “Rtools” (“Windows”
    only) or “Compiler” (non-“Windows” only) and “XQuartz” (“Mac” only).
  - `"all"` – additionally checks for presence of “Atom”, “Git” and
    (sometimes) “Meld”.
    <!-- This check may not work properly on "Mac" and "Linux". -->

<!-- end list -->

``` r
bio::check_installed_programs()
## ✔ Program R (4.0.2) is installed (recommended 4.0.2, available 4.0.2)
## ✖ Program RStudio is not installed or is not running.
## ✔ Program Rtools is installed.
```

``` r
bio::check_installed_programs("all")
## ✔ Program R (4.0.2) is installed (recommended 4.0.2, available 4.0.2)
## ✔ Program RStudio (1.2.5033) is installed (recommended 1.3.1073, available 1.3.1073)
## ✔ Program Rtools is installed.
## ✖ Program Atom is either not detected or not configured incorrectly.
## ✔ Program Git is installed.
## ✔ Program Meld is installed.
```

## Check information about “R” packages

The following functions may be used to check installed packages by
topic. By default, only missing packages or packages with lower versions
than recommended are displayed. Currently available lists can be
returned by function `bio::get_pkg_lists_local()` and they include:

  - `'addins-rmd'`
  - `'bio'`
  - `'bs2020-initial'`
  - `'bs2020-s01'`
  - `'bs2020-s02'`
  - `'bs2020-u07'`
  - `'bs2020-u08'`
  - `'confidence-intervals'`
  - `'ggplot'`
  - `'gmc-r209'`
  - `'linear-regression'`
  - `'markdown'`
  - `'mini'`
  - `'r2020-initial'`
  - `'r2020-s01'`
  - `'r2020-s02'`
  - `'r2020-s03'`
  - `'rcmdr-biostat'`
  - `'rcmdr'`
  - `'snippets'`
  - `'spelling-grammar'`
  - `'statistical-tests'`
  - `'summary'`
  - `'tidyverse'`

<!-- end list -->

``` r
bio::check_packages_by_topic("mini")
## ✔ Minimal required versions of all 19 packages (from list 'mini') are already installed.
```

``` r
bio::check_packages_by_topic("mini", include = "newer_on_cran")
## ✔ Minimal required versions of all 19 packages (from list 'mini') are already installed.
## i Note: 1 package has newer version on CRAN.
```

## Update packages

These are the convenience functions to update certain R packages. The
functions restart R before installation if used in RStudio.

Update package **bio**.

``` r
bio::update_pkg_bio()
```

Check if there are updates for **bio** or its dependency packages.

``` r
bio::check_updates_pkg_bio()
```

    ✔ Minimal required versions of all 24 packages (from list 'bio') are already installed.

Update package **snippets**.

``` r
bio::update_pkg_snippets()
```

Update package **RcmdrPlugin.biostat**.

``` r
bio::update_pkg_rcmdr_biostat()
```

## RStudio settings and user preferences

> Make sure that you understand what you are doing when you try using
> the functions that reset RStudio key bindings (shortcut keys),
> settings or snippets. **You may loose *all* your previous settings.**

### Dictionaries

This function downloads or updates **hunspell** spelling checking
dictionaries dictionaries (including Lithuanian) that can be used by
RStudio or by tools from **wellspell.addin** package.

``` r
bio::rstudio_download_spellcheck_dictionaries()
```

### Shortcut keys

This function changes shortcut keys in RStudio.

Run function without arguments to see the available options.

``` r
bio::rstudio_reset_keybindings()
#> Error: The set of RStudio shortcut keys is not defined (argument 'to').
#> Possible options: 'bio-default', 'rstudio-default'.
```

Options:

  - `"bio-default"` – shortcut keys recommended for course BS-2020.
  - `"rstudio-default"` – RStudio defaults.

<!-- end list -->

``` r
bio::rstudio_reset_keybindings(to = "bio-default")
```

| Shortcut                                                     | Action                                                                                                                        |
| ------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd> | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin) must be installed. |
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>A</kbd>                 | Use “Tidyverse” style on selected lines of R code. Package **styler** must be installed (*R code*).                           |
| <kbd>Ctrl</kbd>+<kbd>ALt</kbd>+<kbd>D</kbd>                  | Document R package (that you are building).                                                                                   |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>D</kbd>                | Duplicate selected text.                                                                                                      |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>P</kbd> | Align patrs of selected lines at a custom pattern.                                                                            |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>;</kbd>                | Insert lower opening quotes `„`.                                                                                              |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>’</kbd>                | Insert upper closing quotes `“`.                                                                                              |
| <kbd>Ctrl</kbd>+<kbd>E</kbd>                                 | **Insert/Enclose with R code block** (*R Markdown*) **\[\!\]** .                                                              |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>T</kbd>                  | Insert inline code highlighted as R code (R Markdown).                                                                        |
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd>                 | Insert inline LaTeX equation (*R Markdown*).                                                                                  |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad0</kbd>            | Remove heading/section name (*R Markdown*).                                                                                   |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad1</kbd>            | Replace line into/Insert heading (section name) of level 1 (*R Markdown*).                                                    |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad2</kbd>            | … of level 2 (*R Markdown*).                                                                                                  |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad3</kbd>            | … of level 3 (*R Markdown*).                                                                                                  |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad4</kbd>            | … of level 4 (*R Markdown*).                                                                                                  |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad5</kbd>            | … of level 5 (*R Markdown*).                                                                                                  |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad6</kbd>            | … of level 6 (*R Markdown*).                                                                                                  |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad7</kbd>            | Insert operator `%in%` (*R code*).                                                                                            |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad8</kbd>            | Insert operator `%*%` (*R code*).                                                                                             |

**Table 1.** Some examples of shortcut keys set with option
`"bio-default"`.

<!-- "addins.rmd::rmd_list_numbered"                : "Ctrl+Shift+Alt+L", -->

<!-- "addins.rs::rs_align_code_at_arrow"            : "Ctrl+Shift+Alt+[", -->

<!-- "addins.rs::rs_align_code_at_equal"            : "Ctrl+Shift+Alt+]", -->

<!-- "addins.rs::rs_insert_exposition_pipe"         : "Ctrl+Alt+S", -->

<!-- "addins.rs::rs_insert_infix_in"                : "Ctrl+Alt+NumPad6", -->

<!-- "addins.rs::rs_insert_line_ds"                 : "Ctrl+Shift+Alt+Q", -->

<!-- "addins.rs::rs_insert_line_ss"                 : "Shift+Alt+Q", -->

<!-- "addins.rs::rs_insert_line_sw"                 : "Ctrl+Shift+Q", -->

<!-- "addins.rs::rs_insert_matrix_multiplication"   : "Ctrl+Alt+NumPad8", -->

<!-- "addins.rs::rs_insert_tee_pipe"                : "Ctrl+Alt+.", -->

<!-- "addins.rs::rs_insert_update_pipe"             : "Ctrl+Alt+,", -->

<!-- "addins.rs::rs_replace_slash_b2fw"             : "Ctrl+Alt+/", -->

<!-- "addins.rs::rs_replace_slash_bd2s"             : "Ctrl+Shift+Alt+\\", -->

<!-- "addins.rs::rs_replace_slash_bs2d"             : "Ctrl+Alt+\\", -->

<!-- "addins.rs::rs_replace_slash_fw2b"             : "Ctrl+Shift+Alt+/", -->

<!-- "bookdown::mathquill"                          : "Ctrl+Alt+NumPad9", -->

More on [Customizing Keyboard
Shortcuts](https://support.rstudio.com/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts)
in RStudio.

### RStudio settings

This function resets RStudio user preferences and settings.

Run function without arguments to see the available options.

``` r
bio::rstudio_reset_user_settings()
#> Error: The set of RStudio user settings is not defined (argument 'to').
#> Possible choices: 'bio-default', 'rstudio-default'.
```

The options:

  - `"bio-default"` – settings recommended for course BS-2020.
  - `"rstudio-default"` – default RStudio settings.

<!-- end list -->

``` r
bio::rstudio_reset_user_settings(to = "bio-default")
```

### Snippets

The function replaces current R code and R Markdown snippets with those
in package “snippets”.

``` r
snippets::install_snippets_from_package("snippets", type = c("r", "markdown"), backup = TRUE)
```

More information on [Code
Snippets](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets)
in RStudio and on package
[**snippets**](https://gegznav.github.io/snippets/).

### Restart R and RStudio

If you want to see the effect of changed settings, you should reload
RStudio. You may use this function, which reloads RStudio without
closing it:

``` r
bio::rstudio_reload_ui()
```

However, in some cases it is recommended to close and reopen RStudio.

Next, to restart R session without closing RStudio, use:

``` r
bio::rstudio_restart_r()
```

Useful to apply before trying to install R packages.

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

<!-- bio::update_personal_proj_list() -->

<!-- ``` -->
