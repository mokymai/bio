# Locate the RStudio Desktop installation directory

Searches common per-OS install locations (and, on Windows, the registry)
for an installed copy of RStudio Desktop. Does not require RStudio to be
running. Looks for both a system-wide ("all users") and a per-user
("just me") install, preferring the per-user one if both are found (see
[`get_rstudio_install_scope()`](https://mokymai.github.io/bio/reference/get_rstudio_install_scope.md)).

## Usage

``` r
find_rstudio_install_dir()
```

## Value

A length-1 character string with the install directory, or `NULL` if no
installation was found.
