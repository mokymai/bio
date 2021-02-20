Package ***bio***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- # Package **_bio_** -->

<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.1.1-brightgreen.svg)](https://github.com/mokymai/bio)
[![R-CMD-check](https://github.com/mokymai/bio/workflows/R-CMD-check/badge.svg)](https://github.com/mokymai/bio/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2021--02--19-yellowgreen.svg)](/commits/master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

<!-- [![GitHub last commit](https://img.shields.io/github/last-commit/mokymai/bio)](https://github.com/mokymai/bio) -->

Convenience functions to facilitate installation and management of
required resources for course units ***Biostatistics*** (*BS-2021*) and
***Introduction to data analysis with R*** (*R-2021*).

<center>

<br> <font color="darkred" size=4> This package is intended to be used
with <b>RStudio 1.4</b> or newer.<br> </font>
<font color="darkred" size=3> Functions do not work correctly with the
previous versions of RStudio. </font> <br><br><br>

</center>

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
## ✔ Program R (4.0.3) is installed (recommended 4.0.3, available 4.0.3)
## ✖ Program RStudio is not installed or is not running.
## ✔ Tool Rtools is installed.
```

``` r
bio::check_installed_programs("all")
## ✔ Program R (4.0.3) is installed (recommended 4.0.3, available 4.0.3)
## ✔ Program RStudio (1.2.5033) is installed (recommended 1.4.1103, available 1.4.1103)
## ✔ Tool Rtools is installed.
## ✖ Program Atom is either not detected or configured incorrectly.
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
  - `'bs2021-initial'`
  - `'confidence-intervals'`
  - `'ggplot-extra-tools'`
  - `'ggplot'`
  - `'gmc-r209'`
  - `'initial'`
  - `'linear-regression'`
  - `'markdown'`
  - `'mini'`
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

Install improved Lithuanian dictionary.

``` r
bio::rstudio_install_spellcheck_dictionary_lt()
```

### Shortcut keys

This function changes shortcut keys in RStudio.

Run function without arguments to see the available options.

``` r
bio::rstudio_reset_keybindings()
#> Error: The value of argument 'to' is missing.
#> Possible options: 'bio-default', 'rstudio-default'.
```

Options:

  - `"bio-default"` – shortcut keys recommended for course BS-2021.
  - `"rstudio-default"` – RStudio defaults.

<!-- end list -->

``` r
bio::rstudio_reset_keybindings(to = "bio-default")
```

| Shortcut                                                     | Action                                                                                                                        |
| ------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd> | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin) must be installed. |
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>A</kbd>                 | Use “Tidyverse” style on selected lines of R code. Package **styler** must be installed (*R code*).                           |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>D</kbd>                  | Document R package (that you are building).                                                                                   |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>D</kbd>                | Duplicate selected text.                                                                                                      |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>P</kbd> | Align parts of selected lines at a custom pattern.                                                                            |
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

<!-- : **Table 1.** Some examples of shortcut keys set with option `"bio-default"`{.r}. -->

<!--  -->

<!-- Shortcut  | Action -->

<!-- --------- | ------------------------------------------------------------------------------------------------------------------------------------------- -->

<!-- Ctrl Shift Alt S  | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin){target="_blank"} must be installed. -->

<!-- Shift Alt A          | Use "Tidyverse" style on selected lines of R code. Package **styler** must be installed (*R code*). -->

<!-- Ctrl Alt D           | Document R package (that you are building). -->

<!-- Ctrl Shift D         | Duplicate selected text. -->

<!-- Ctrl Shift Alt P  | Align parts of selected lines at a custom pattern. -->

<!-- Ctrl Shift ;         | Insert lower opening quotes `„`. -->

<!-- Ctrl Shift '         | Insert upper closing quotes `“`. -->

<!-- Ctrl E                  | **Insert/Enclose with R code block** (*R Markdown*) **[!]** . -->

<!-- Ctrl Alt T           | Insert inline code highlighted as R code (R Markdown). -->

<!-- Shift Alt S          | Insert inline LaTeX equation (*R Markdown*). -->

<!-- Ctrl Alt NumPad0     | Remove heading/section name (*R Markdown*). -->

<!-- Ctrl Alt NumPad1     | Replace line into/Insert heading (section name) of level 1 (*R Markdown*). -->

<!-- Ctrl Alt NumPad2     | ... of level 2 (*R Markdown*). -->

<!-- Ctrl Alt NumPad3     | ... of level 3 (*R Markdown*). -->

<!-- Ctrl Alt NumPad4     | ... of level 4 (*R Markdown*). -->

<!-- Ctrl Alt NumPad5     | ... of level 5 (*R Markdown*). -->

<!-- Ctrl Alt NumPad6     | ... of level 6 (*R Markdown*). -->

<!-- Ctrl Alt NumPad7     | Insert operator `%in%` (*R code*). -->

<!-- Ctrl Alt NumPad8     | Insert operator `%*%` (*R code*). -->

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
#> Error: The value of argument 'to' is missing.
#> Possible choices: 'bio-default', 'bio-dark-blue', 'bio-black', 'rstudio-default'.
```

The options:

  - `"bio-default"` – settings recommended for course BS-2021 (and light
    theme “Textmate (default)”);
  - `"bio-dark-blue"` – settings recommended for course BS-2021 (and
    dark blue theme “Cobalt”);
  - `"bio-black"` – settings recommended for course BS-2021 (and black
    theme “Chaos”);
  - `"rstudio-default"` – default RStudio settings.

<!-- end list -->

``` r
bio::rstudio_reset_user_settings(to = "bio-default")
```

### Snippets

The function replaces current R code and R Markdown snippets with those
in package “snippets”.

``` r
snippets::install_snippets_from_package("snippets", backup = TRUE)
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
