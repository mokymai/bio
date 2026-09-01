<div id="main" class="col-md-9" role="main">

# Checking your setup

<div class="quarto-figure quarto-figure-center">

![Lifecycle: experimental](lifecycle-experimental.svg)

</div>

<div>

> **Lifecycle: experimental**
>
> These helpers are **experimental**. Output formatting and the set of
> programs that are checked may change between releases.

</div>

<div class="section level2">

## Installed programs

`check_installed_programs()` reports the installed version of R,
RStudio, Quarto, Rtools, and related tools, and — when the machine is
online — the newest version available:

Official installation and release information is available from the [R
Project](https://www.r-project.org/), [RStudio IDE User
Guide](https://docs.posit.co/ide/user/), [Quarto installation
guide](https://quarto.org/docs/get-started/), and [Rtools
documentation](https://cran.r-project.org/bin/windows/Rtools/).

<div class="cell">

<div id="cb1" class="sourceCode cell-code">

``` r
bio::check_installed_programs()
```

</div>

</div>

`type` selects which group of programs to report on, and
`skip_online_check = TRUE` makes the call purely local:

<div class="cell">

<div id="cb2" class="sourceCode cell-code">

``` r
bio::check_installed_programs(type = "main", skip_online_check = TRUE)
```

</div>

</div>

<div class="section level3">

### Behaviour without a network

Looking up “the newest available version” needs three things to work: a
reachable network, a reachable endpoint, and a response in the expected
shape. `bio` treats a failure in any of them as *“the available version
is unknown”* rather than as an error:

-   no connectivity → a message that the computer is offline;
-   the endpoint refuses or times out → a warning naming the tool;
-   the page or API answers in an unexpected format → a warning naming
    the tool.

In every case the installed versions are still reported. A broken or
blocked download never prevents you from seeing what you actually have
installed.

</div>

<div class="section level3">

### RStudio Desktop specifics

RStudio can be installed for one user or for all users, and `bio` looks
in both kinds of location:

<div class="cell">

<div id="cb3" class="sourceCode cell-code">

``` r
install_dir <- bio:::find_rstudio_install_dir()
bio::get_rstudio_install_scope(install_dir)
```

</div>

</div>

The scope only describes where the *application* lives. RStudio’s
preferences, keybindings, and internal state are always per
operating-system user. Posit’s guide documents the supported [RStudio
configuration
directories](https://docs.posit.co/ide/user/ide/guide/productivity/custom-settings.html).

</div>

</div>

<div class="section level2">

## Your session

`check_user_info()` collects the details worth pasting into a bug
report:

<div class="cell">

<div id="cb4" class="sourceCode cell-code">

``` r
bio::check_user_info()
```

</div>

</div>

Smaller helpers answer one question each:

<div class="cell">

<div id="cb5" class="sourceCode cell-code">

``` r
bio::get_os_type()
bio::is_64bit_os()
bio::is_32bit_os()
```

</div>

</div>

</div>

<div class="section level2">

## Packages

Check whether specific packages are installed, list everything that is,
or find out which packages a directory of scripts actually uses:

<div class="cell">

<div id="cb6" class="sourceCode cell-code">

``` r
bio::is_pkg_installed(c("dplyr", "ggplot2", "notapackage"))

bio::get_pkgs_installed()

bio::list_pkgs_used_in_dir(path = ".")
bio::list_pkgs_used_in_files("analysis.R")
```

</div>

</div>

`list_pkgs_used_in_dir()` scans `.R`, `.Rmd`, and `.qmd` files, so it is
a quick way to build the install list for a course folder. For
reproducible project libraries rather than discovery alone, see
[**renv**](https://rstudio.github.io/renv/).

</div>

<div class="section level2">

## Versions

Compare two versions, or list the archived versions of a CRAN package:

<div class="cell">

<div id="cb7" class="sourceCode cell-code">

``` r
bio::compare_version(v_installed = "4.5.0", v_required = "4.6.0")

bio::pkg_list_archived_versions("ggplot2")
```

</div>

</div>

`compare_version()` returns a comparison of two version strings; it is
the helper behind the “is available online” annotations in
`check_installed_programs()`.

</div>

</div>
