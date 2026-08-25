# Check whether local reset safeguards are intentionally bypassed.

This helper keeps the legacy override semantics but avoids any
hard-coded network allow-list. It is intended as a small guard for
destructive local reset actions, and it can be bypassed explicitly when
a user opts in.

## Usage

``` r
restriction_status(ignore_ip = getOption("bio.ignore_ip", FALSE), ...)
```

## Arguments

- ignore_ip:

  Logical scalar that bypasses the local reset safeguard.

- ...:

  Additional arguments ignored for compatibility with older callers.

## Value

Logical scalar, `TRUE` when the safeguard is intentionally overridden.

## Examples

``` r
if (interactive()) {
  restriction_status(ignore_ip = TRUE)
  restriction_status(ignore_ip = FALSE)
}
```
