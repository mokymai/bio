# Summarize `run_reset_step()` results and print a final message.

Summarize
[`run_reset_step()`](https://mokymai.github.io/bio/reference/run_reset_step.md)
results and print a final message.

## Usage

``` r
summarize_reset_steps(steps)
```

## Arguments

- steps:

  Named list of results returned by
  [`run_reset_step()`](https://mokymai.github.io/bio/reference/run_reset_step.md).

## Value

A data frame with one row per step (`step`, `ok`, `message`).
