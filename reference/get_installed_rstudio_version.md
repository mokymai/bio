<div id="main" class="col-md-9" role="main">

# Get the version of an installed (but not necessarily running) RStudio

<div class="ref-description section level2">

On Windows, prefers the `Version` value written to the registry by the
installer (the `VERSION` file on disk can hold an unrelated Electron
shell build number on newer RStudio releases). Falls back to parsing the
`VERSION` file that RStudio Desktop places in its install directory.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_installed_rstudio_version()
```

</div>

</div>

<div class="section level2">

## Value

A `numeric_version()` object, or `NULL` if it could not be determined.

</div>

</div>
