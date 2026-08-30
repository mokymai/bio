# Determine whether the local RStudio Desktop install is system-wide or per-user

RStudio Desktop's Windows installer supports an "all users"
(system-wide, admin rights required) and a "just me" (per-user) install
mode; macOS installs are similarly either shared (`/Applications`) or
per-user (`~/Applications`). This distinction affects only where the
RStudio *application files* live (used e.g. to find the bundled
`user-prefs-schema.json`) — it does **not** affect where RStudio stores
preferences, keybindings, or other per-user state, which always lives
under the current OS user's profile regardless of install scope (see
[`get_path_rstudio_config_dir()`](https://mokymai.github.io/bio/reference/RStudio-related-dirs.md)).

## Usage

``` r
get_rstudio_install_scope(install_dir = find_rstudio_install_dir())
```

## Arguments

- install_dir:

  Character scalar, the resolved RStudio install directory. Defaults to
  [`find_rstudio_install_dir()`](https://mokymai.github.io/bio/reference/find_rstudio_install_dir.md).

## Value

`"system"`, `"user"`, or `NA_character_` if the install location is
unknown (e.g. RStudio isn't installed).

## Examples

``` r
if (interactive()) {
  get_rstudio_install_scope()
}
```
