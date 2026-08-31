<div id="main" class="col-md-9" role="main">

# Package index

<div class="section level2">

## Check

</div>

<div class="section level2">

-   `check_installed_programs()` : Check installed programs and
    available versions
-   `check_user_info()` : Get user-related information
-   `get_rstudio_install_scope()` : Determine whether the local RStudio
    Desktop install is system-wide or per-user

</div>

<div class="section level2">

## Change Settings

</div>

<div class="section level2">

-   `rstudio_reset_user_settings()` : Reset RStudio settings
-   `rstudio_compare_user_settings()` : Show differences in sets of
    settings
-   `rstudio_reset_keybindings()` : Reset RStudio keybindings to a
    packaged preset.
-   `rstudio_install_spellcheck_dictionaries()`
    `rstudio_download_spellcheck_dictionaries()`
    `rstudio_delete_spellcheck_dictionaries()` : Dictionaries to check
    spelling

</div>

<div class="section level2">

## Paths and Directories

</div>

<div class="section level2">

-   `get_path_rstudio_config_file()` `open_rstudio_config_file()` :
    Manage RStudio Configuration (Preferences) File

-   `open_rstudio_system_dictionaries_dir()`
    `open_rstudio_user_dictionaries_dir()`
    `open_rstudio_internal_dictionaries_dir()` : RStudio Dictionaries

-   `get_path_rstudio_config_dir()`
    `get_path_rstudio_internal_state_dir()`
    `get_path_rstudio_keybindings_dir()` `open_rstudio_config_dir()`
    `open_rstudio_internal_state_dir()` `open_rstudio_keybindings_dir()`
    : Directories of RStudio-Related Files

-   `get_path_desktop()` `open_desktop()` : Path to Desktop Folder

-   `open_in_rstudio()` : Open file in RStudio

-   `get_path_r_environ()` `open_r_environ()` :

    Open `.Renviron` File

</div>

<div class="section level2">

## RStudio Projects

</div>

<div class="section level2">

-   `open_project()` : Open RStudio Project
-   `get_path_recent_proj_list()` `get_path_user_proj_list()`
    `open_recent_proj_list()` `open_user_proj_list()`
    `update_rstudio_proj_list_user()` : Manage Project Lists
-   `read_projects()` : Manage RStudio Projects

</div>

<div class="section level2">

## Packages

</div>

<div class="section level2">

-   `get_pkgs_installed()` : List packages installed on this computer
-   `is_pkg_installed()` : Check if package is installed
-   `list_pkgs_used_in_dir()` : List Packages Used in Directory
-   `list_pkgs_used_in_files()` : List Packages Used in Specific File(s)
-   `pkg_list_archived_versions()` : Get previous package versions
    available on CRAN

</div>

<div class="section level2">

## Utilities and Helper Functions

</div>

<div class="section level2">

-   `compare_version()` : Compare Version Numbers
-   `get_os_type()` `is_64bit_os()` `is_32bit_os()` : Detect the current
    operating system
-   `rstudio_restart_r()` `rstudio_reload_ui()` `restart_r()`
    `restart_rstudio()` : Functions to Restart R and Reload RStudio

</div>

<div class="section level2">

## Other Functions

<div class="section-desc">

Functions not mentioned above.

</div>

</div>

<div class="section level2">

-   `assert_single_value()` : Validate that a function argument is a
    single value.

-   `find_rstudio_install_dir()` : Locate the RStudio Desktop
    installation directory

-   `get_installed_rstudio_version()` : Get the version of an installed
    (but not necessarily running) RStudio

-   `get_rstudio_version_from_registry()` : Get the RStudio version
    reported by the Windows installer registry entry

-   `is_rstudio_installed()` : Check whether RStudio Desktop is
    installed, even if it is not running

-   `parse_rstudio_version_string()` :

    Parse an RStudio calendar-version string into a `numeric_version()`

-   `print_program_version_info()` : Print a version-status summary for
    a program.

</div>

</div>
