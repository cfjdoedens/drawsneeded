# compute number of samples needed to establish maximum defect rate with enough certainty

In sampling we have a file of like items. We want to estimate the
overall defect rate of the items. This function guesses the number of
draws needed to establish with some certainty that the defect rate is
below a certain threshold.

## Usage

``` r
drawsneeded(
  posited_defect_rate = 0,
  allowed_defect_rate = 0.01,
  cert = 0.95,
  distribution = "binomial"
)
```

## Arguments

- posited_defect_rate:

  The defect rate of which we want to see how many samples it would take
  to establish that it is below the allowed defect rate. Could for
  example be our guess of the defect rate.

- allowed_defect_rate:

  The highest defect rate that is still acceptable. So the threshold.
  Should be higher than posited_defect_rate.

- cert:

  The certainty level you want, e.g. `0.95`.

- distribution:

  One of "binomial", "Poisson" or "Poisson_interpolated".

## Value

An estimate of the needed number of samples according to the
distribution chosen. If the number of needed samples comes to close to
the maximum R integer value, or cert ==1, return Inf.

## Details

Each of the four arguments can have length \> 1, but only one of these
four arguments.

Note that the prior used by drawsneeded() is flat: each possible defect
rate is beforehand given an equal probability.

## Examples

``` r
  x <- drawsneeded(0.001, 0.02, cert = 0.95)
```
