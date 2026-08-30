# Get the version of an installed (but not necessarily running) RStudio

On Windows, prefers the `Version` value written to the registry by the
installer (the `VERSION` file on disk can hold an unrelated Electron
shell build number on newer RStudio releases). Falls back to parsing the
`VERSION` file that RStudio Desktop places in its install directory.

## Usage

``` r
get_installed_rstudio_version()
```

## Value

A [`numeric_version()`](https://rdrr.io/r/base/numeric_version.html)
object, or `NULL` if it could not be determined.
