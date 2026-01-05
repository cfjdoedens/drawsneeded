# remove trailing zeros from string containing at least one "."

Also removes "." if that is the last character. The idea is that we trim
zeros from a number containing decimal places.

## Usage

``` r
remove_trailing_zeros(s)
```

## Arguments

- s:

  The input string.

## Value

The input string with trailing zeros, after ".", if any, removed.

examples remove_trailing_zeros("37.5600")
