# If a Value is NULL, Trigger the Parent Function's Return

If the focal input is `NULL`, return `value` from the parent function.
Should only be used within a function.

## Usage

``` r
return_if_null(x, value = NULL)
```

## Arguments

- x:

  Focal input.

- value:

  If `x` is `NULL`, [`return`](https://rdrr.io/r/base/function.html)
  this input from the parent function.

## Value

If `x` is not `NULL`, `NULL` is returned. If `x` is `NULL`, the result
of [`return`](https://rdrr.io/r/base/function.html) with `value` as its
input evaluated within the parent function's environment is returned.

## Examples

``` r
 ff <- function(x = 1, null_return = "hello"){
   return_if_null(x, null_return)
   x
 }
 ff()
#> [1] 1
 ff(NULL)
#> [1] "hello"
```
