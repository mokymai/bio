# Clear the global R workspace.

Useful for the "reset" flows in RStudio when the user wants to remove
all objects from the global environment without removing attached
packages or environment state outside `.GlobalEnv`.

## Usage

``` r
clear_r_workspace(envir = .GlobalEnv)
```

## Arguments

- envir:

  Environment to clear. Defaults to `.GlobalEnv`.

## Value

Invisibly returns the cleared environment.

## Examples

``` r
env <- new.env()
env$x <- 1
bio:::clear_r_workspace(env)
exists("x", envir = env)
#> [1] FALSE
```
