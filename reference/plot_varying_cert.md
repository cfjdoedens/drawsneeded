# Show in plot how varying allowed defect rate influences draws needed

The plot is based on varying the variable allowed_defect_rate in a call
to drawsneeded(posited_defect_rate, allowed_defect_rate, cert).

## Usage

``` r
plot_varying_cert(
  posited_defect_rate = 0.01,
  allowed_defect_rate = 0.02,
  max_n = 1000,
  S = 10000
)
```

## Arguments

- posited_defect_rate:

  The proposed defect rate. Should be lower than allowed_defect_rate.

- allowed_defect_rate:

  The highest defect rate that is still acceptable. Should be higher
  than posited_defect_rate.

- max_n:

  The number of to be drawn items maximally shown in the plot.#'

- S:

  The number of points on the X-axis of the plot.

## Value

A ggplot.

## Details

To keep the plot interesting, only the points representing up to max_n
draws are shown.

## Examples

``` r
  plot_varying_cert(posited_defect_rate = 0.01, allowed_defect_rate = 0.02)
```
