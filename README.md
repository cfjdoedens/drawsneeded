
<!--README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/cfjdoedens/drawsneeded/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cfjdoedens/drawsneeded/actions/workflows/R-CMD-check.yaml)

[![Codecov test
coverage](https://codecov.io/gh/cfjdoedens/drawsneeded/graph/badge.svg)](https://app.codecov.io/gh/cfjdoedens/drawsneeded)

<!-- badges: end -->

# drawsneeded

## Introduction

Supports planning of statistical sampling to audit the defect rate in a
set of like items. This package is useful, when the purpose of the audit
is to establish whether the defect rate of the total set with e.g. .95
certainty is below a threshold, the maximally allowed defect rate.
drawsneeded() assumes the defect rate of an individual item is expressed
on a scale of 0 tot 1.

An example application is the audit of a file of monetary statements. In
general this concerns money that has been spent. Here the defect rate is
the fraction of money from the file that was wrongly spent. And the
defect rate of an individual element typically varies from 0 to 1, with
probably most elements having 0 defect rate and some elements having
substantial defect rates.

Another example application is the audit of a file of administrative
decisions that have to be checked on their legality. Here the defect
rate is the fraction of decisions in the file that are not legally
correct. Typically the defect of an individual item is either 0 or 1.

This package has the function

- drawsneeded(posited_defect_rate, allowed_defect_rate, cert)

The function gives an estimate of the number of drawn items needed to
establish with some certainty, *cert*, that the defect rate is below a
certain threshold, *allowed_defect_rate*. The assumption of
drawsneeded() is that each drawn item exactly hase
*posited_defect_rate*. It might be that in practice the items have a
defect of either 0 or 1, or, when they have a defect on a scale from 0
to 1, most items have 0 defect and most items will contain no defects at
all, and some items will have a large defect rate, or are even totally
defective. Still, we suppose that this assumption leaves the estimates
of drawsneeded() realistic.

Due to randomness, the actual number of needed drawn items might be
smaller or bigger. Also, it might turn out, that the set has a higher
defect rate than allowed_defect_rate. In that case no amount of draws
will suffice to prove that the defect rate is below allowed_defect_rate.

The package has a couple of plotting functions:

- drawsneeded_plot()
- margin_plot_varying_posited_defect_rate()
- margin_plot_varying_allowed_defect_rate()
- margin_plot_varying_cert()
- combined_plot()

These should give the user insight in the relation between number of
draws taken and resulting estimates about the defect rate in the set of
items that is audited.

## When can drawsneeded() be useful?

drawsneeded() can be useful in the following situation:

1.  you want to audit the defect rate of a set of like items, and
2.  you consider doing this audit by means of statistical sampling on
    the set of items, and
3.  you want to establish whether or not the defect rate is below a
    certain treshold, and
4.  you want to estimate how many items you need to sample

## Role of posited_defect_rate

As said, the function drawsneeded(posited_defect_rate,
allowed_defect_rate, cert) gives an estimate of the minimum number of
drawn items needed to establish with some certainty, cert, e.g. 0.95,
that the defect rate is below allowed_defect_rate. For this estimation
it assumes that the actual defect rate in the file of items equals the
posited_defect_rate. It is up to the user of drawsneeded() to choose a
posited_defect_rate(). When the posited_defect_rate is chosen
conservative, i.e. on the high side, it can be used with some confidence
to produce a not too low number. And vice versa we can set the
posited_defect_rate on the low side to get a bottom estimate of the
number of monetary statements that need to be drawn and checked.

The function assumes that each drawn item has a defect rate of
posited_defect_rate. And it computes the minimum number of drawn items
necessary to establish with cert certainty that posited_defect_rate \<
allowed_defect_rate. Note that it might actually not be possible for any
of the items in the file to have exactly that defect rate.

The computation of this minimum is based on the binomial distribution.
This is extended with the beta distribution to allow for non integer
values of defects. (The binomial distribution assumes that there are
very many items to draw from, which all have equal chance of being
drawn, and there are so many items that when we have drawn some this
does not change in a substantial way the rate of erroneous items.)

Note that in this scheme the statistical procedure that is used to
estimate the defect rate in the file of items does *not* use the
posited_defect_rate! The posited_defect_rate is only used as means to
estimate the minimal number of items that need to be drawn to estimate
the defect rate with sufficient certainty. The posited_defect_rate is
used only for planning.

## Usage

### General

Using drawsneeded() is a simple and cheap way to estimate the effort,
needed to statistically establish the defect rate in a set of like
items.

### No need to stick to the planning for actual drawing and checking

The number that is produced by drawsneeded() gives an estimate of how
many drawn items are needed to establish the defect rate in a set of
like items. In order to have a statistical sound estimate of the defect
rate, it is not necessary to stick to that planned number for actual
drawing and evaluation! The number is just an indication of the effort
needed.

For example, when drawsneeded() estimates that there are 365 draws
needed (see the first example below), it might turn out that after 298
draws checked, not one defect was found. If the 298 items were chosen
randomly, over the total set of items, it can be safely concluded that
the defect rate is not more than 1 %. And there is no need to check the
other 67 items.

On the other hand, when it turns out that the defect rate of the drawn
and evaluated so far items is above the posited_defect_rate, one can do
a new call on drawsneeded() to establish a new, and forcibly higher,
number of draws that are needed. Care should be taken that all items in
the file had, and have, an equal chance of being selected.

### Combination with incremental drawing

In order to minimize the number of to be drawn and to be checked items
one can explicitly proceed by making an optimistic, i.e. low, guess of
the posited_defect_rate, and once this indeed turns out to be too
optimistic, plan anew as described above with the actual defect rate
found in the already checked items.

## Installation

You can install the development version of drawsneeded from
[GitHub](https://github.com/) with:

``` r
if (file.exists("/home/crist-jan/R/x86_64-pc-linux-gnu-library/4.5/drawsneeded")) {
  # We are executing on the author machine, use the development version available there.
  library("drawsneeded")
  print("using files directly from author of package; not from github")
} else {
  # Use the github version.
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  pak::pak("cfjdoedens/drawsneeded")
  print("using files from github")
}
#> [1] "using files directly from author of package; not from github"
```

## Example: 0.1 percent defect expected

### posited_defect_rate = *0.001*, allowed_defect_rate = 0.01, cert = 0.95

Suppose you know from previous experience that a small defect rate might
exist in the set of items. You guess the defect rate to be no more than
0.1 percent for the total set. So you set posited_defect_rate to 0.001.
You need a result with 95% certainty. So you set certainty to 0.95.

``` r
drawsneeded(posited_defect_rate = 0.001, allowed_defect_rate = 0.01, cert = 0.95)
#> [1] 365
```

The conclusion is that you will need at least 365 samples.

In a picture this looks like:

``` r
drawsneeded_plot(posited_defect_rate = 0.001,  allowed_defect_rate = 0.01, cert = 0.95)
```

<img src="man/figures/README-tiny-defects-plot-1.png" width="100%" /> We
can further analyse the situation by varying over posited_defect_rate,
or over allowed_defect_rate or over cert. See the following three plots.

### Varying over the posited defect rate

First we vary over the posited defect rate:

``` r
margin_plot_varying_posited_defect_rate(allowed_defect_rate = 0.01, cert = 0.95, max_n = 1000)
```

<img src="man/figures/README-tiny-defects-vary-eer-plot-1.png" width="100%" />

We see that as posited_defect_rate goes near to allowed_defect_rate, the
number of draws needed rises. In this graph only the results up to 1000
draws are shown. This limit can be made higher, but this does not really
give a more interesting picture:

``` r
margin_plot_varying_posited_defect_rate(allowed_defect_rate = 0.01, cert = 0.95, max_n = 100000)
```

<img src="man/figures/README-tiny-defects-vary-eer-plot-100000-1.png" width="100%" />

We can also study the variation of posited_defect_rate in a non
graphical way:

``` r
drawsneeded(posited_defect_rate = seq(from = 0.0, by = 0.001, to = 0.009), allowed_defect_rate = 0.01, cert = 0.95)
#>     0 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 
#>   298   365   458   594   801  1143  1767  3104  6894 27195
```

Again we see that for a posited defect rate of 0.001, we need 365 draws.

### Varying over the allowed defect rate

``` r
margin_plot_varying_allowed_defect_rate(posited_defect_rate = 0.001, cert = 0.95)
```

<img src="man/figures/README-tiny-defects-vary-aer-plot-1.png" width="100%" />

We see that as the allowed_defect_rate moves away from the
posited_defect_rate, the number of draws falls sharply.

Non graphically this looks like (note the 365 computed draws needed for
allowed defect rate 0.01):

``` r
drawsneeded(posited_defect_rate = 0.001, allowed_defect_rate = seq(from = 0.002, by = 0.001, to = 0.01), cert = 0.95)
#> 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009  0.01 
#>  5757  2190  1305   920   708   574   482   415   365
```

and,

``` r
drawsneeded(posited_defect_rate = 0.001, allowed_defect_rate = seq(from = 0.02, by = 0.01, to = 0.1), cert = 0.95)
#> 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09  0.1 
#>  163  104   76   60   49   42   36   32   28
```

and,

``` r
drawsneeded(posited_defect_rate = 0.001, allowed_defect_rate = seq(from = 0.2, by = 0.1, to = 0.9), cert = 0.95)
#> 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 
#>  13   8   5   4   3   2   1   1
```

### Varying over the certainty

Finally we can also vary over the certainty we apply.

``` r
margin_plot_varying_cert(posited_defect_rate = 0.001, allowed_defect_rate = 0.01)
```

<img src="man/figures/README-tiny-defects-vary-cert-plot-1.png" width="100%" />

As might be expected: if we can do with less certainty, then we need
less draws.

In raw numbers this looks like:

``` r
drawsneeded(posited_defect_rate = 0.001, allowed_defect_rate = 0.01, cert = seq(from = 0.50, by = 0.05, to = 0.95))
#>  0.5 0.55  0.6 0.65  0.7 0.75  0.8 0.85  0.9 0.95 
#>   76   88  102  118  137  159  187  223  275  365
```

Note that for cert 0.95, we get again 365 draws needed.

### All plots bundled in one picture

We can call combined_plots() to get all four plots in one picture.
However, it does not look very slick here due to lack of canvas space.

``` r
combined_plots(posited_defect_rate = 0.001, allowed_defect_rate = 0.01, cert = 0.95)
```

<img src="man/figures/README-tiny-defects-all-plots-1.png" width="100%" />

## Example: no defect expected

### posited_defect_rate = *0*, allowed_defect_rate = 0.01, cert = 0.95

You might expect to see no defect at all in the to be audited set of
items. Then you could calculate as follows:

``` r
drawsneeded(posited_defect_rate = 0, allowed_defect_rate = 0.01, cert = 0.95)
#> [1] 298
```

So then 298 samples is sufficient.

In a picture this looks like:

``` r
drawsneeded_plot(posited_defect_rate = 0, allowed_defect_rate = 0.01, cert = 0.95)
```

<img src="man/figures/README-zero-defects-plot-1.png" width="100%" />

Again, we can further analyse the situation by varying over
posited_defect_rate, or over allowed_defect_rate or over cert. As we
have above already studied varying the posited_defect_rate, we will not
repeat that here.

### Varying over the allowed defect rate

``` r
margin_plot_varying_allowed_defect_rate(posited_defect_rate = 0, cert = 0.95)
```

<img src="man/figures/README-zero-defects-vary-aer-plot-1.png" width="100%" />

We see that as the allowed_defect_rate moves away from the
posited_defect_rate, the number of draws falls sharply.

Non graphically this looks like:

``` r
drawsneeded(posited_defect_rate = 0, allowed_defect_rate = seq(from = 0.001, by = 0.001, to = 0.01), cert = 0.95)
#> 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009  0.01 
#>  2994  1496   997   747   597   497   426   372   331   298
```

and,

``` r
drawsneeded(posited_defect_rate = 0, allowed_defect_rate = seq(from = 0.02, by = 0.01, to = 0.1), cert = 0.95)
#> 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09  0.1 
#>  148   98   73   58   48   41   35   31   28
```

and,

``` r
drawsneeded(posited_defect_rate = 0, allowed_defect_rate = seq(from = 0.2, by = 0.1, to = 0.9), cert = 0.95)
#> 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 
#>  13   8   5   4   3   2   1   1
```

### Varying over the certainty

Finally we can also vary over the certainty we apply.

``` r
margin_plot_varying_cert(posited_defect_rate = 0, allowed_defect_rate = 0.01)
```

<img src="man/figures/README-zero-defects-vary-cert-plot-1.png" width="100%" />

As might be expected: more certainty needs more draws.

In raw numbers this looks like:

``` r
drawsneeded(posited_defect_rate = 0, allowed_defect_rate = 0.01, cert = seq(from = 0.50, by = 0.05, to = 0.95))
#>  0.5 0.55  0.6 0.65  0.7 0.75  0.8 0.85  0.9 0.95 
#>   68   79   91  104  119  137  160  188  229  298
```

We recognize the number 298 as the number of draws needed for cert 0.95.

### All plots bundled in one picture

We call combined_plots() to get all four plots in one picture. Again, it
is does not look very nice here, because of the cramped space.

``` r
combined_plots(posited_defect_rate = 0, allowed_defect_rate = 0.01, cert = 0.95)
```

<img src="man/figures/README-zero-defects-all-plots-1.png" width="100%" />

## Suggested extensions

- Handle case where only integer values of k are possible. For example
  when k \> 1 - cert, then round k up. Otherwise round k down.
- In extension to the above: Handle the case where per audited item only
  defect rates of fixed size are possible. E.g. an item has as only
  possible defect rates: 0.25, 0.5, 0.75 and 1.0.
- Add extra margin, so extra draws, by taking into account the standard
  deviation.
- Add a prior to drawsneeded().
- Add a cost function that makes additional draws more expensive. The
  function can help with planning of incremental drawing.
