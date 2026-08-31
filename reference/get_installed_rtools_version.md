<div id="main" class="col-md-9" role="main">

# Get the version of an installed Rtools toolchain on Windows

<div class="ref-description section level2">

Evaluates the active toolchain selected by `pkgbuild::rtools_path()` for
the running R session, or searches environment variables
(`RTOOLS*_HOME`), the registry, and `C:\rtools*` directories for the
highest installed version. Rtools toolchains are released per compiler
toolchain update (e.g. Rtools 4.5 supports R 4.5.x and 4.6.x), so there
is no strict 1-to-1 major.minor version match requirement.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_installed_rtools_version()
```

</div>

</div>

<div class="section level2">

## Value

A `numeric_version()` object, or `NULL` if not on Windows or not found.

</div>

</div>
