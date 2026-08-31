<div id="main" class="col-md-9" role="main">

# Manage RStudio Projects

<div class="ref-description section level2">

-   `read_projects()` reads a project-list file and returns the project
    names and paths.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
read_projects(file, sort_by = FALSE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   file:

    (character) Path to the file with RStudio project names.

-   sort_by:

    (`"name"`\|`"path"`\|`FALSE`)

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  read_projects(get_path_recent_proj_list())
}
```

</div>

</div>

</div>
