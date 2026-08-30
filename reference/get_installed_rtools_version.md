# Get the version of an installed Rtools toolchain on Windows

Evaluates the active toolchain selected by
[`pkgbuild::rtools_path()`](https://pkgbuild.r-lib.org/reference/has_rtools.html)
for the running R session, or searches environment variables
(`RTOOLS*_HOME`), the registry, and `C:\rtools*` directories for the
highest installed version. Rtools toolchains are released per compiler
toolchain update (e.g. Rtools 4.5 supports R 4.5.x and 4.6.x), so there
is no strict 1-to-1 major.minor version match requirement.

## Usage

``` r
get_installed_rtools_version()
```

## Value

A [`numeric_version()`](https://rdrr.io/r/base/numeric_version.html)
object, or `NULL` if not on Windows or not found.
