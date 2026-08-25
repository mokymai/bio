# Manage Project Lists

Manage project lists.

- `get_path_recent_proj_list()` – gets path to the file with the list of
  recent RStudio projects.

- `get_path_user_proj_list()` – gets path to the file with the list of
  personal RStudio projects.

&nbsp;

- `open_recent_proj_list()` – opens the file with the list of recent
  RStudio projects.

## Usage

``` r
get_path_recent_proj_list()

get_path_user_proj_list(create = FALSE)

open_recent_proj_list()

open_user_proj_list()

update_rstudio_proj_list_user()
```

## Arguments

- create:

  (logical) If `TRUE` and file does not exist, the file is created.

## Examples

``` r
if (interactive()) {
  get_path_recent_proj_list()
  get_path_user_proj_list()
}
```
