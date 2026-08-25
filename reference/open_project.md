# Open RStudio Project

Open RStudio project by name or interactively. The projects list is read
from files that contain project lists.

## Usage

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

## Arguments

- pattern:

  (character) regular expression to narrow down the list of possible
  options.

- new_session:

  (logical\|`NULL`) should the project be opened in a new session, or
  should the current RStudio session switch to that project? Note that
  `TRUE` values are only supported with RStudio Desktop and RStudio
  Server Pro. If `NULL`, user will have to choose interactively.

- proj_list:

  (data frame) The result of
  [`read_projects()`](https://mokymai.github.io/bio/reference/projects.md)
  or `NULL`.

- proj_list_path:

  (string) The path to the file with the list of project paths. If
  `proj_list` is not `NULL`, then `proj_list_path` is ignored.

- only_available:

  (logical) If `TRUE`, non-existing projects and projects with broken
  paths are removed from the list of choices.

- name:

  (string\|`NULL`) The name of the project or `NULL` to choose a project
  interactively.

- negate:

  (logical) If `TRUE`, then the options defined by `pattern` are
  excluded.

## Value

Opens the indicated project.

## See also

- [`update_rstudio_proj_list_user()`](https://mokymai.github.io/bio/reference/project-lists.md)

- [`rstudioapi::openProject()`](https://rstudio.github.io/rstudioapi/reference/projects.html)

- [`rstudioapi::initializeProject()`](https://rstudio.github.io/rstudioapi/reference/projects.html)

## Examples

``` r
if (interactive()) {
  open_project()
  open_project("bio")
  open_project("R-2019-project")
}
```
