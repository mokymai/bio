<div id="main" class="col-md-9" role="main">

# Check installed programs and available versions

<div class="ref-description section level2">

Checks whether key tools are installed and, when online, reports the
newest available versions for R, RStudio, and Quarto.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
check_installed_programs(type = "main", skip_online_check = FALSE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   type:

    Character scalar selecting the tool group to check. Supported values
    are `"main"`, `"all"`, `"dev"`, `"gmc-bs"`, and `"gmc-r"`.

-   skip_online_check:

    Logical. If `TRUE`, skips internet checks and does not attempt to
    fetch the newest available versions.

</div>

<div class="section level2">

## Value

Invisibly returns `NULL`. The results are printed to the console.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  check_installed_programs()
  check_installed_programs("all")
}
```

</div>

</div>

</div>
