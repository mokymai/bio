# Validate that a function argument is a single value.

This helper keeps internal checks explicit and consistent when a
function is intentionally scalar-only.

## Usage

``` r
assert_single_value(x, arg_name, allow_null = TRUE, allow_na = TRUE)
```

## Arguments

- x:

  Value to validate.

- arg_name:

  Name of the argument for the error message.

- allow_null:

  Whether `NULL` is allowed.

- allow_na:

  Whether `NA` is allowed.

## Examples

``` r
bio:::assert_single_value("R", "name", allow_null = FALSE, allow_na = FALSE)
try(bio:::assert_single_value(c("R", "S"), "name", allow_null = FALSE, allow_na = FALSE))
#> Error : `name` must be a single value.
```
