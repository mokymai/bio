<div id="main" class="col-md-9" role="main">

# Open RStudio Project

<div class="ref-description section level2">

Open RStudio project by name or interactively. The projects list is read
from files that contain project lists.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
open_project(
  pattern = NULL,
  new_session = if (interactive()) NULL else TRUE,
  proj_list = NULL,
  proj_list_path = NULL,
  only_available = TRUE,
  name = NULL,
  negate = FALSE
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   pattern:

    (character) regular expression to narrow down the list of possible
    options.

-   new_session:

    (logical\|`NULL`) should the project be opened in a new session, or
    should the current RStudio session switch to that project? Note that
    `TRUE` values are only supported with RStudio Desktop and RStudio
    Server Pro. If `NULL`, user will have to choose interactively.

-   proj_list:

    (data frame) The result of `read_projects()` or `NULL`.

-   proj_list_path:

    (string) The path to the file with the list of project paths. When
    `proj_list` is also supplied, projects from both sources are
    combined.

-   only_available:

    (logical) If `TRUE`, non-existing projects and projects with broken
    paths are removed from the list of choices.

-   name:

    (string\|`NULL`) The name of the project or `NULL` to choose a
    project interactively.

-   negate:

    (logical) If `TRUE`, then the options defined by `pattern` are
    excluded.

</div>

<div class="section level2">

## Value

Opens the indicated project.

</div>

<div class="section level2">

## See also

<div class="dont-index">

-   `update_rstudio_proj_list_user()`

-   `rstudioapi::openProject()`

-   `rstudioapi::initializeProject()`

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  open_project()
  open_project("bio")
  open_project("R-2019-project")
}
```

</div>

</div>

</div>
