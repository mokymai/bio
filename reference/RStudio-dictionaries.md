<div id="main" class="col-md-9" role="main">

# RStudio Dictionaries

<div class="ref-description section level2">

Functions to work with RStudio dictionaries.

-   `rstudioapi::dictionariesPath()`

-   `open_rstudio_system_dictionaries_dir()`

&nbsp;

-   `rstudioapi::userDictionariesPath()`

-   `open_rstudio_user_dictionaries_dir()`

&nbsp;

-   `rstudioapi::dictionariesPath()`

-   `open_rstudio_internal_dictionaries_dir()`

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
open_rstudio_system_dictionaries_dir()

open_rstudio_user_dictionaries_dir()

open_rstudio_internal_dictionaries_dir()
```

</div>

</div>

<div class="section level2">

## Value

String with path.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[rstudioapi::dictionaries](https://rstudio.github.io/rstudioapi/reference/dictionaries.html)

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  rstudioapi::dictionariesPath()
  rstudioapi::userDictionariesPath()
  get_path_rstudio_config_dir("dictionaries")
}
```

</div>

</div>

</div>
