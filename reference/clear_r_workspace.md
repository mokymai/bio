<div id="main" class="col-md-9" role="main">

# Clear the global R workspace.

<div class="ref-description section level2">

Useful for the "reset" flows in RStudio when the user wants to remove
all objects from the global environment without removing attached
packages or environment state outside `.GlobalEnv`.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
clear_r_workspace(envir = .GlobalEnv)
```

</div>

</div>

<div class="section level2">

## Arguments

-   envir:

    Environment to clear. Defaults to `.GlobalEnv`.

</div>

<div class="section level2">

## Value

Invisibly returns the cleared environment.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
env <- new.env()
env$x <- 1
bio:::clear_r_workspace(env)
exists("x", envir = env)
#> [1] FALSE
```

</div>

</div>

</div>
