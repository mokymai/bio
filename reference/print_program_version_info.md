# Print a version-status summary for a program.

Formats the installed and latest available version for an application or
tool and reports whether the program is installed and whether an upgrade
is known.

## Usage

``` r
print_program_version_info(
  name = "",
  v_installed = NULL,
  v_available = NULL,
  type = "Program"
)
```

## Arguments

- name:

  Name of the program.

- v_installed:

  Installed version value, or `NULL` when missing.

- v_available:

  Newest available version value, or `NULL` when unavailable.

- type:

  Label used for the output, typically `"Program"` or `"Tool"`.

## Examples

``` r
if (interactive()) {
  bio:::print_program_version_info("R", getRversion(), "4.5.0")
  bio:::print_program_version_info("R", NULL, "4.5.0")
}
```
