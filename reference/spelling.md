# Dictionaries to check spelling

- `rstudio_download_spellcheck_dictionaries()` downloads and updates
  RStudio (system) spellchecking dictionaries.

- `rstudio_delete_spellcheck_dictionaries()` deletes RStudio (system)
  spellchecking dictionaries.

## Usage

``` r
rstudio_install_spellcheck_dictionaries(secure = TRUE)

rstudio_download_spellcheck_dictionaries(secure = TRUE)

rstudio_delete_spellcheck_dictionaries(ask = TRUE)
```

## Arguments

- secure:

  (logical) If `TRUE`, uses "https", if `FALSE`, uses "http".

- ask:

  (logical) If `TRUE`, user will have to confirm his/her choice
  interactively.

## Examples

``` r
if (interactive()) {
  rstudio_delete_spellcheck_dictionaries()
  rstudio_download_spellcheck_dictionaries()
}
```
