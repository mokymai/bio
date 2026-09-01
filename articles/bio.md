<div id="main" class="col-md-9" role="main">

# Get started with bio

<div class="quarto-figure quarto-figure-center">

![Lifecycle: experimental](lifecycle-experimental.svg)

</div>

<div>

> **Lifecycle: experimental**
>
> `bio` is **experimental**. It is developed for a specific
> Biostatistics course and its user interface may change without a
> deprecation cycle. Several functions rewrite files that RStudio owns,
> so read [Managing RStudio
> settings](https://mokymai.github.io/bio/articles/rstudio-settings.md)
> before running them on a machine you care about.

</div>

<div class="section level2">

## What `bio` is for

`bio` prepares and inspects the R and RStudio environment used in a
Biostatistics course. It groups into three areas:

| Area                  | Typical question                     | Vignette                                                                                |
|-----------------------|--------------------------------------|-----------------------------------------------------------------------------------------|
| Environment checks    | *Is my software new enough?*         | [Checking your setup](https://mokymai.github.io/bio/articles/checking-your-setup.md)    |
| RStudio configuration | *Can I get the classroom defaults?*  | [Managing RStudio settings](https://mokymai.github.io/bio/articles/rstudio-settings.md) |
| Projects and paths    | *Where does RStudio keep this file?* | this vignette                                                                           |

Nothing in `bio` is required to *use* R — it exists to make a room full
of laptops behave the same way.

</div>

<div class="section level2">

## Installation

`bio` and two of its dependencies are published through a
[drat](https://CRAN.R-project.org/package=drat) repository rather than
CRAN. The repository itself is available at
[mokymai/download](https://mokymai.github.io/download/):

<div class="cell">

<div id="cb1" class="sourceCode cell-code">

``` r
repos <- c(
  mokymai = "https://mokymai.github.io/download/",
  CRAN = "https://cran.rstudio.com"
)

install.packages("bio", repos = repos)
```

</div>

</div>

`DESCRIPTION` declares that repository in `Additional_repositories`, so
`install.packages()` resolves `backup.tools` and `snippets` from it too.
See the R manual for details about [installing
packages](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Installing-packages).

</div>

<div class="section level2">

## A short tour

Start by confirming that the surrounding software is present:

<div class="cell">

<div id="cb2" class="sourceCode cell-code">

``` r
bio::check_installed_programs()
```

</div>

</div>

Then look at where RStudio keeps the files `bio` may rewrite:

<div class="cell">

<div id="cb3" class="sourceCode cell-code">

``` r
bio::get_path_rstudio_config_dir()
bio::get_path_rstudio_config_file(which = "current")
bio::get_path_rstudio_keybindings_dir()
bio::get_path_rstudio_snippets_dir()
```

</div>

</div>

The `open_*()` counterparts open the same locations in a file manager:

<div class="cell">

<div id="cb4" class="sourceCode cell-code">

``` r
bio::open_rstudio_config_dir()
bio::open_rstudio_keybindings_dir()
bio::open_backup_dir()
```

</div>

</div>

</div>

<div class="section level2">

## Projects and project lists

RStudio keeps a list of recently opened projects. `bio` can read it,
refresh the user-editable copy, and open a project by a name pattern:

<div class="cell">

<div id="cb5" class="sourceCode cell-code">

``` r
bio::get_path_recent_proj_list()
bio::get_path_user_proj_list()

bio::update_rstudio_proj_list_user()
bio::read_projects(file = bio::get_path_user_proj_list())

bio::open_project("biostat", new_session = TRUE)
```

</div>

</div>

`open_project()` matches `pattern` against the project names. When
several projects match, it asks which one to open, so it is interactive
by design.

</div>

<div class="section level2">

## Restarting

<div class="cell">

<div id="cb6" class="sourceCode cell-code">

``` r
bio::restart_r()
bio::restart_rstudio()
bio::rstudio_reload_ui()
```

</div>

</div>

These require a running RStudio session; outside RStudio they report
that and return without doing anything.

</div>

<div class="section level2">

## Where to go next

-   [Checking your
    setup](https://mokymai.github.io/bio/articles/checking-your-setup.md)
    — installed versions, packages, and what happens when the machine is
    offline.
-   [Managing RStudio
    settings](https://mokymai.github.io/bio/articles/rstudio-settings.md)
    — presets, keybindings, dictionaries, backups, and the live-session
    caveats.

</div>

<div class="section level2">

## Related packages and tools

These projects overlap with or complement narrower parts of `bio`:

-   [**rstudio.prefs**](https://CRAN.R-project.org/package=rstudio.prefs),
    by S.A. van der Wulp and Daniel D. Sjoberg, is the closest CRAN
    alternative for RStudio preference files and addin shortcuts.
-   [**rstudioapi**](https://CRAN.R-project.org/package=rstudioapi), by
    Kevin Ushey, JJ Allaire, Hadley Wickham, and Gary Ritchie, provides
    the supported API for live RStudio integration.
-   [**renv**](https://CRAN.R-project.org/package=renv), by Kevin Ushey
    and Hadley Wickham, manages reproducible project-local libraries and
    lockfiles; it complements `bio::list_pkgs_used_in_dir()`.
-   [**styler**](https://CRAN.R-project.org/package=styler), by Kirill
    Müller, Lorenz Walthert, and Indrajeet Patil, formats R code.
-   [**drat**](https://CRAN.R-project.org/package=drat), by Dirk
    Eddelbuettel and Carl Boettiger, manages CRAN-like package
    repositories.

For the underlying platforms, use the official [R
manuals](https://cran.r-project.org/manuals.html), [RStudio IDE User
Guide](https://docs.posit.co/ide/user/), and [Quarto
guide](https://quarto.org/docs/guide/).

</div>

</div>
