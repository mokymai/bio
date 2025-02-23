Package ***bio***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- # Package **_bio_** -->
<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.2.9-brightgreen.svg)](https://github.com/mokymai/bio)
[![R-CMD-check](https://github.com/mokymai/bio/workflows/R-CMD-check/badge.svg)](https://github.com/mokymai/bio/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2025--01--26-yellowgreen.svg)](/commits/master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/mokymai/bio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mokymai/bio/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- [![GitHub last commit](https://img.shields.io/github/last-commit/mokymai/bio)](https://github.com/mokymai/bio) -->

Convenience functions to facilitate installation and management of
required resources for course unit ***Biostatistics*** (*BS-2023*).

<center>
<br> <font color="darkred" size=4> This package is intended to be used
with <b>RStudio 2024.04</b> or newer<br> and <b>R 4.4.0</b> or
newer.<br> </font> <font color="darkred" size=3> Functions may not work
correctly with the previous versions of RStudio and R. </font>
<br><br><br>
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

<details>
<summary>
Install form GitHub
</summary>

To install this package from GitHub:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("mokymai/bio", dependencies = TRUE)
```

Note: installation from *GitHub* requires *RTools* on *Windows*.

</details>

# Examples

## Check information about OS and user

Check information about operating system (OS) and R-related user
information.

``` r
bio::check_user_info()
## Operating system    Windows 10 x64 (build 19044)
## Platform            x86_64-w64-mingw32/x64 (64-bit)
## LOGNAME
## USERNAME            User
## USERPROFILE         C:/Users/User
## HOME                C:/Users/User
## R_USER              C:/Users/User
## R_HOME              C:/PROGRA~1/R/R-4.2.2
## R_LIBS_USER         C:/Users/User/AppData/Local/R/win-library/4.2
```

## Check information about programs

Check versions of presence of installed R-related and other programs.
Options:

- `"main"` (default) – checks for “R”, “RStudio”, “Quarto”, and “Rtools”
  (“Windows” only) or “R Build Tools” (non-“Windows” only).
- `"all"` – additionally checks for presence of “XQuartz” (“Mac” only),
  “Git” (all systems), and (sometimes) “Meld”.
  <!-- This check may not work properly on "Mac" and "Linux". -->

``` r
bio::check_installed_programs()
## ✔ Program R (4.3.1) is installed (recommended >= 4.3.1).
## ✖ Program RStudio is not installed or is not running.
## ✔ Tool Quarto (1.4.268) is installed (recommended >= 1.3.336).
## ✔ Tool Rtools is installed.
```

``` r
bio::check_installed_programs("all")
## ✔ Program R (4.3.1) is installed (recommended >= 4.2.2, available 4.3.1).
## ✔ Program RStudio (2023.6.1.524) is installed (recommended >= 2023.3.0, available 2023.6.1.524).
## ✔ Tool Quarto (1.4.268) is installed (recommended >= 1.3.336).
## ✔ Tool Rtools is installed.
## ✔ Program Git is installed.
## ✔ Program Meld is installed.
```

## Check installation status of “R” packages

It is recommended to check installation status and version of required
packages by name:

``` r
bio::check_packages_by_name(c("bio", "remotes", "tidyverse"))
## ✔ Minimal required versions of all 3 packages are already installed.
```

There is a less recommended way (due to lower transparency) to check
installation status of packages by topic. By default, only missing
packages or packages with lower versions than recommended are displayed.
Currently available lists can be returned by function
`bio::get_pkg_lists_local()`.

<details>
<summary>
Details
</summary>

- `'addins-rmd'`
- `'bio'`
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

</details>

``` r
bio::check_packages_by_topic("mini")
## ℹ Reading list 'mini'
## ✔ Minimal required versions of all 19 packages (from list 'mini') are already installed.
```

``` r
bio::check_packages_by_topic("mini", include = "newer_on_cran")
## ℹ Reading list 'mini'
## ✔ Minimal required versions of all 19 packages (from list 'mini') are already installed.
## ℹ Note: 1 package has newer version on CRAN.
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
## ℹ Analyzing dependencies of package bio
##
## [... truncated ...]
##
## ✔ Minimal required versions of all 26 packages (from list 'bio') are already installed.
```

Update package **snippets**.

``` r
bio::update_pkg_snippets()
```

<!-- Update package **RcmdrPlugin.biostat**. -->
<!-- ```{r README-5a, eval=FALSE} -->
<!-- bio::update_pkg_rcmdr_biostat() -->
<!-- ``` -->

## RStudio settings and user preferences

> Make sure that you understand what you are doing when you try using
> the functions that reset RStudio key bindings (shortcut keys),
> settings or snippets. **You may loose *all* your previous settings.**

### Dictionaries

This function downloads or updates **hunspell** spelling checking
dictionaries dictionaries (including Lithuanian) that can be used by
RStudio or by tools from **wellspell.addin** package.

``` r
bio::rstudio_install_spellcheck_dictionaries()
```

Install improved Lithuanian dictionary.

``` r
bio::rstudio_install_spellcheck_dictionary_lt()
## ✔ lt_LT dictionary installed.
##   'C:/Users/ViG/AppData/Roaming/RStudio/dictionaries/languages-system/lt_LT.dic'
```

### Shortcut keys

This function changes shortcut keys in RStudio.

Run function without arguments to see the available options.

``` r
bio::rstudio_reset_keybindings()
## Error: The value of argument 'to' is missing.
## Possible options: 'bio-default', 'rstudio-default'.
```

Options:

- `"bio-default"` – shortcut keys recommended for course BS-2023.
- `"rstudio-default"` – RStudio defaults.

``` r
bio::rstudio_reset_keybindings(to = "bio-default")
## ✔ Back up copy of shortcut keys was created in 'C:/Users/User/.R/_backup/keybindings/'
## ✔ Shortcut keys were reset to bio-default.
```

| Shortcut                                                     | Action                                                                                                                        |
|--------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd>                 | Insert inline LaTeX equation (*R Markdown*).                                                                                  |
| <kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>A</kbd>                 | Use “Tidyverse” style on selected lines of R code. Package **styler** must be installed (*R code*).                           |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>D</kbd>                | Duplicate selected text.                                                                                                      |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>S</kbd> | Check spelling in selected text. Package [**wellspell.addin**](https://github.com/nevrome/wellspell.addin) must be installed. |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>P</kbd> | Align parts of selected lines at a custom pattern.                                                                            |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\]</kbd>               | Change heading one level down (*R Markdown*) **\[!\]**.                                                                       |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\[</kbd>               | Change heading one level up or remove heading (*R Markdown*) **\[!\]**.                                                       |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>;</kbd>                | Insert lower opening quotes `„`.                                                                                              |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>’</kbd>                | Insert upper closing quotes `“`.                                                                                              |
| <kbd>Ctrl</kbd>+<kbd>E</kbd>                                 | **Insert/Enclose with R code block** (*R Markdown*) **\[!\]** .                                                               |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>T</kbd>                  | Insert inline code highlighted as R code (R Markdown).                                                                        |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad8</kbd>            | Insert operator `%*%` (*R code*).                                                                                             |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>NumPad7</kbd>            | Insert operator `%in%` (*R code*).                                                                                            |
| <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>D</kbd>                  | Document R package (that you are building).                                                                                   |

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

More on <a
href="https://support.rstudio.com/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts"
target="_blank">Customizing Keyboard Shortcuts</a> in RStudio.

### RStudio settings

This function resets RStudio user preferences and settings.

Run function without arguments to see the available options.

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

To check, how your settings differ from the defaults in a certain list,
use:

``` r
bio::rstudio_compare_user_settings(to = "bio-default")
## ℹ Show differences between current and bio-default setting lists.
## ✔ No differences
```

The settings that are not in the list will not be displayed. Available
options of `to` are “bio-default” and “rstudio-default”.

### Snippets

The function replaces current R code and R Markdown snippets with those
in package “snippets”.

``` r
snippets::install_snippets_from_package("snippets", backup = TRUE)
## ✔ File with markdown snippets was updated:
##   'C:/Users/User/AppData/Roaming/RStudio/snippets/markdown.snippets'
## ✔ File with r snippets was updated:
##   'C:/Users/User/AppData/Roaming/RStudio/snippets/r.snippets'
##
## ℹ You will be able to use the snippets after RStudio is closed and reopened.
```

More information on <a
href="https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets"
target="_blank">Code Snippets</a> in RStudio and on package
<a href="https://gegznav.github.io/snippets/"
target="_blank"><strong>snippets</strong></a>.

### Restart R and RStudio

If you want to see the effect of changed settings, you should reload
RStudio. You may use this function, which reloads RStudio without
closing it:

``` r
bio::restart_rstudio()
```

However, in some cases it is recommended to close and reopen RStudio.

Next, to restart R session without closing RStudio, use:

``` r
bio::restart_r()
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
<!-- bio::update_rstudio_proj_list_user() -->
<!-- ``` -->
