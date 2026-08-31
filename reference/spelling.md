<div id="main" class="col-md-9" role="main">

# Dictionaries to check spelling

<div class="ref-description section level2">

-   `rstudio_install_spellcheck_dictionaries()` downloads and installs
    RStudio (system) spellchecking dictionaries.

-   `rstudio_download_spellcheck_dictionaries()` is a compatibility
    alias for the installer.

-   `rstudio_delete_spellcheck_dictionaries()` deletes RStudio (system)
    spellchecking dictionaries.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rstudio_install_spellcheck_dictionaries(secure = TRUE)

rstudio_download_spellcheck_dictionaries(secure = TRUE)

rstudio_delete_spellcheck_dictionaries(ask = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   secure:

    (logical) If `TRUE` (the default), uses "https", if `FALSE`, uses
    "http". `FALSE` downloads the dictionary archive over an
    unauthenticated connection and is not recommended: the archive is
    extracted into your RStudio configuration directory.

-   ask:

    (logical) If `TRUE`, user will have to confirm his/her choice
    interactively.

</div>

<div class="section level2">

## Value

For `rstudio_install_spellcheck_dictionaries()` and its download alias,
invisibly returns `TRUE` on success and `FALSE` on handled failure.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  rstudio_delete_spellcheck_dictionaries()
  rstudio_download_spellcheck_dictionaries()
}
```

</div>

</div>

</div>
