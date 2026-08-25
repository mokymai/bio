# RStudio Dictionaries

Functions to work with RStudio dictionaries.

- [`rstudioapi::dictionariesPath()`](https://rstudio.github.io/rstudioapi/reference/dictionaries.html)

- `open_rstudio_system_dictionaries_dir()`

&nbsp;

- [`rstudioapi::userDictionariesPath()`](https://rstudio.github.io/rstudioapi/reference/dictionaries.html)

- `open_rstudio_user_dictionaries_dir()`

&nbsp;

- [`rstudioapi::dictionariesPath()`](https://rstudio.github.io/rstudioapi/reference/dictionaries.html)

- `open_rstudio_internal_dictionaries_dir()`

## Usage

``` r
open_rstudio_system_dictionaries_dir()

open_rstudio_user_dictionaries_dir()

open_rstudio_internal_dictionaries_dir()
```

## Value

String with path.

## See also

[rstudioapi::dictionaries](https://rstudio.github.io/rstudioapi/reference/dictionaries.html)

## Examples

``` r
if (interactive()) {
  rstudioapi::dictionariesPath()
  rstudioapi::userDictionariesPath()
  get_path_rstudio_config_dir("dictionaries")
}
```
