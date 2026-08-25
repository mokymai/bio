# Reset the local RStudio session to a known-good classroom/lab state.

The function performs several destructive cleanup steps aimed at
restoring a consistent RStudio environment:

1.  clears history and recent-session state;

2.  resets user settings and keybindings;

3.  clears the current R workspace;

4.  restores the default snippets and layout;

5.  optionally updates spellcheck dictionaries; and

6.  restarts RStudio when the user confirms.

## Usage

``` r
rstudio_reset_gmc(..., force_update_dictionaries = FALSE)
```

## Arguments

- ...:

  Further arguments used by
  [`restriction_status()`](https://mokymai.github.io/bio/reference/restriction_status.md)
  for compatibility.

- force_update_dictionaries:

  Logical scalar. If `TRUE`, the dictionaries are refreshed even when
  the current locale is present.

## Value

Invisibly returns `NULL` after the reset workflow completes.

## Details

This helper is intentionally conservative and protects destructive reset
actions behind a simple override flag. The code does not rely on
external IP metadata or a hard-coded allow-list.

## Examples

``` r
if (interactive()) {
  options(bio.ignore_ip = TRUE)
  bio::rstudio_reset_gmc()
}
```
