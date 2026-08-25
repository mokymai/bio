# Manage RStudio Projects

- `read_projects()` reads a project-list file and returns the project
  names and paths.

## Usage

``` r
read_projects(file, sort_by = FALSE)
```

## Arguments

- file:

  (character) Path to the file with RStudio project names.

- sort_by:

  (`"name"`\|`"path"`\|`FALSE`)

## Examples

``` r
if (interactive()) {
  read_projects(get_path_recent_proj_list())
}
```
