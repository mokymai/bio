# Check installed programs and available versions

Checks whether key tools are installed and, when online, reports the
newest available versions for R, RStudio, and Quarto.

## Usage

``` r
check_installed_programs(type = "main", skip_online_check = FALSE)
```

## Arguments

- type:

  Character scalar selecting the tool group to check. Supported values
  are `"main"`, `"all"`, `"dev"`, `"gmc-bs"`, and `"gmc-r"`.

- skip_online_check:

  Logical. If `TRUE`, skips internet checks and does not attempt to
  fetch the newest available versions.

## Value

Invisibly returns `NULL`. The results are printed to the console.

## Examples

``` r
if (interactive()) {
  check_installed_programs()
  check_installed_programs("all")
}
```
