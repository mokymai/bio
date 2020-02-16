
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package ***bio***

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bio)](https://CRAN.R-project.org/package=bio)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.0.9017-brightgreen.svg)](https://github.com/mokymai/bio)
[![Travis build
status](https://travis-ci.com/mokymai/bio.svg?branch=master)](https://travis-ci.com/mokymai/bio)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mokymai/bio?branch=master&svg=true)](https://ci.appveyor.com/project/mokymai/bio)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Documented
on](https://img.shields.io/badge/Documentation-2020--02--16-yellowgreen.svg)]()
<!-- badges: end -->

Convenience functions to facilitate installation and management of
required resources for ***Biostatistics*** course unit (*BS-2020*).

<center>

<font color="red" size=6> <br> Work is still in progress… <br><br>
</font>

</center>

# Install

To install this package from GitHub:

``` r
if (!require(remotes)) {install.packages("remotes")}
remotes::install_github("mokymai/bio")
```

# Examples

## Update packages

Update package **bio**.

``` r
bio::update_pkg_bio()
```

Update package **snippets**.

``` r
bio::update_pkg_snippets()
```

Update package **RcmdrPlugin.biostat**.

``` r
bio::update_pkg_rcmdr_biostat()
```

## Check information about OS, programs and packages

Check user information, installed R-related programs and R packages. (OS
– operating system.)

``` r
bio::check_user_info()
## Operating system    Windows 10 x64 (build 18362)
## Platform            x86_64-w64-mingw32/x64 (64-bit)
## USERNAME            User
## USERPROFILE         C:/Users/User
## HOME                C:/Users/User/Documents
## R_USER              C:/Users/User/Documents
## R_HOME              C:/PROGRA~1/R/R-36~1.2
## R_LIBS_USER         C:/R/win-library/3.6
```

``` r
bio::check_installed_programs("all")
## ✔ Program R (3.6.2) is installed (recommended 3.6.2, available 3.6.2)
## ✖ Program RStudio is not installed or is not running.
## ✔ Program Rtools is installed.
## ✔ Program Atom is installed.
## ✔ Program Git is installed.
## ✔ Program Meld is installed.
```

``` r
bio::check_installed_packages(list_name = "mini", include = "newer_on_cran")
## ✔ The required versions of all 19 packages (from list 'mini') are already installed.
```

## RStudio settings and user preferences

> You may use these functions **at your own risk**.

``` r
bio::set_rstudio_keybindings("bio-default")
```

``` r
bio::reset_rs_user_settings(to = "bio-default")
```

## RStudio projects

Open (recently used) RStudio project.

``` r
bio::open_project()
## 
## Choose the name of the project (press 0 to cancel): 
## 
##  1: project-1 
##  2: _learn
##  3: biostatistics
##  

##  Selection: 0
##  Cancelled by user.
```

``` r
bio::update_personal_proj_list()
```
