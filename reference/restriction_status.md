<div id="main" class="col-md-9" role="main">

# Check whether local reset safeguards are intentionally bypassed.

<div class="ref-description section level2">

This helper keeps the legacy override semantics but avoids any
hard-coded network allow-list. It is intended as a small guard for
destructive local reset actions, and it can be bypassed explicitly when
a user opts in.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
restriction_status(ignore_ip = getOption("bio.ignore_ip", FALSE), ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   ignore_ip:

    Logical scalar that bypasses the local reset safeguard.

-   ...:

    Additional arguments ignored for compatibility with older callers.

</div>

<div class="section level2">

## Value

Logical scalar, `TRUE` when the safeguard is intentionally overridden.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  restriction_status(ignore_ip = TRUE)
  restriction_status(ignore_ip = FALSE)
}
```

</div>

</div>

</div>
