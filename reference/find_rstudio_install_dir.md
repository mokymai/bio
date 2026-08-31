<div id="main" class="col-md-9" role="main">

# Locate the RStudio Desktop installation directory

<div class="ref-description section level2">

Searches common per-OS install locations (and, on Windows, the registry)
for an installed copy of RStudio Desktop. Does not require RStudio to be
running. Looks for both a system-wide ("all users") and a per-user
("just me") install, preferring the per-user one if both are found (see
`get_rstudio_install_scope()`).

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
find_rstudio_install_dir()
```

</div>

</div>

<div class="section level2">

## Value

A length-1 character string with the install directory, or `NULL` if no
installation was found.

</div>

</div>
