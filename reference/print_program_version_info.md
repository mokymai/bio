<div id="main" class="col-md-9" role="main">

# Print a version-status summary for a program.

<div class="ref-description section level2">

Formats the installed and latest available version for an application or
tool and reports whether the program is installed and whether an upgrade
is known.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
print_program_version_info(
  name = "",
  v_installed = NULL,
  v_available = NULL,
  type = "Program"
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   name:

    Name of the program.

-   v_installed:

    Installed version value, or `NULL` when missing.

-   v_available:

    Newest available version value, or `NULL` when unavailable.

-   type:

    Label used for the output, typically `"Program"` or `"Tool"`.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  bio:::print_program_version_info("R", getRversion(), "4.5.0")
  bio:::print_program_version_info("R", NULL, "4.5.0")
}
```

</div>

</div>

</div>
