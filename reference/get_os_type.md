<div id="main" class="col-md-9" role="main">

# Detect the current operating system

<div class="ref-description section level2">

Returns a normalized operating-system label for the current R session.
The result is a single lowercase string such as "windows", "mac", or
"linux"; other Unix-like systems are normalized to their platform name.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
get_os_type()

is_64bit_os()

is_32bit_os()
```

</div>

</div>

<div class="section level2">

## Value

A length-1 character string with the current OS name in lowercase.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
get_os_type()
#> [1] "linux"
```

</div>

</div>

</div>
