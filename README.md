
<!-- README.md is generated from README.Rmd. Please edit that file -->

# drawsneeded

<!-- badges: start -->

<!-- badges: end -->

The pacakage drawsneeded supports monetary unit sampling. In monetary
unit sampling we have a file of monetary statements. In general this
concerns money that has been spent. We want to estimate the percentage
of money from the file that was wrongfully spent, the error rate.

This very tiny package has one function, drawsneeded(). The function
gives an estimate of the number of monetary unit draws needed to
establish with some certainty that the error rate is below a certain
threshold.

This is only a good estimation. Due to randomness, the actual number of
monetary unit draws needed might be either to low or to high.

## Installation

You can install the development version of drawsneeded from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cfjdoedens/drawsneeded")
```

## Example: 0.1 percent error expected

Suppose you know from previous experience that a small error rate might
exist in the mass of monetary statements. You estimate the error rate to
be no more than 0.1 percent. So you set expected_error_rate to 0.001.
You need a result with 95% certainty. So you set certainty to 0.95, You
decide that the maximum number of samples you are willing to draw and
check is 500. So you set max_n to 500.

``` r
library(drawsneeded)
 drawsneeded(expected_error_rate = 0.001, certainty = 0.95, allowed_error_rate = 0.01, max_n = 500)
#> [1] 365
```

The conclusion is that you will need at least 365 samples.

## Example: no error expected

However, you might expect that to see no error at all. Then you could
calculate as follows:

``` r
library(drawsneeded)
 drawsneeded(expected_error_rate = 0.0, certainty = 0.95, allowed_error_rate = 0.01, max_n = 500)
#> [1] 298
```

So probably 298 samples is sufficient.

## Still TODO

- Make drawsneeded() work nicely with vector parameters that have length
  \> 1.
- Add graph that shows planned result: cdf with
  - vertical line for expected_error_rate
  - vertical line for allowed_error_rate
  - if possible, depict number of draws needed
  - if possible, depict situation when too many draws are needed
- Add extra margin, so extra draws, by taking into account the standard
  deviation.
