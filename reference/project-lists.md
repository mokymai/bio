<div id="main" class="col-md-9" role="main">

# Manage Project Lists

<div class="ref-description section level2">

Manage project lists.

-   `get_path_recent_proj_list()` – gets path to the file with the list
    of recent RStudio projects.

-   `get_path_user_proj_list()` – gets path to the file with the list of
    personal RStudio projects.

&nbsp;

-   `open_recent_proj_list()` – opens the file with the list of recent
    RStudio projects.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_path_recent_proj_list()

get_path_user_proj_list(create = FALSE)

open_recent_proj_list()

open_user_proj_list()

update_rstudio_proj_list_user()
```

</div>

</div>

<div class="section level2">

## Arguments

-   create:

    (logical) If `TRUE` and file does not exist, the file is created.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (interactive()) {
  get_path_recent_proj_list()
  get_path_user_proj_list()
}
```

</div>

</div>

</div>
