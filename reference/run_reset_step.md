<div id="main" class="col-md-9" role="main">

# Run one reset step without letting it stop the rest of the workflow.

<div class="ref-description section level2">

Evaluates `expr`, reports success or failure to the console, and turns
warnings into non-fatal notices. Used by `rstudio_reset_gmc()` so a
single failing step (e.g. no network for dictionaries) never blocks
later steps and never fails silently.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
run_reset_step(label, expr)
```

</div>

</div>

<div class="section level2">

## Arguments

-   label:

    Character scalar describing the step, used in progress output.

-   expr:

    Expression to evaluate (wrap multiple statements in `{ }`).

</div>

<div class="section level2">

## Value

Invisibly returns a list with `label`, `ok` (logical), and `message`
(the error message, or `NA` on success).

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
bio:::run_reset_step("A step that works", 1 + 1)
#> ✔ A step that works
bio:::run_reset_step("A step that fails", stop("boom"))
#> ✖ A step that fails failed: boom
```

</div>

</div>

</div>
