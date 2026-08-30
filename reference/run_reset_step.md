# Run one reset step without letting it stop the rest of the workflow.

Evaluates `expr`, reports success or failure to the console, and turns
warnings into non-fatal notices. Used by
[`rstudio_reset_gmc()`](https://mokymai.github.io/bio/reference/clear_and_reset.md)
so a single failing step (e.g. no network for dictionaries) never blocks
later steps and never fails silently.

## Usage

``` r
run_reset_step(label, expr)
```

## Arguments

- label:

  Character scalar describing the step, used in progress output.

- expr:

  Expression to evaluate (wrap multiple statements in
  [`{ }`](https://rdrr.io/r/base/Paren.html)).

## Value

Invisibly returns a list with `label`, `ok` (logical), and `message`
(the error message, or `NA` on success).

## Examples

``` r
bio:::run_reset_step("A step that works", 1 + 1)
#> ✔ A step that works
bio:::run_reset_step("A step that fails", stop("boom"))
#> ✖ A step that fails failed: boom
```
