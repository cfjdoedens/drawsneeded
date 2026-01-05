# Show in plot how allowed defect rate influences draws needed

The plot is based on varying the variable allowed_defect_rate in a call
to drawsneeded(posited_defect_rate, allowed_defect_rate, cert)

## Usage

``` r
plot_varying_allowed_defect_rate(
  posited_defect_rate = 0.01,
  cert = 0.95,
  max_n = 1000,
  S = 10000
)
```

## Arguments

- posited_defect_rate:

  The estimated defect rate from earlier knowledge.

- cert:

  The certainty level you want, e.g. `0.95`.

- max_n:

  The number of to be drawn items maximally shown in the plot.

- S:

  The number of points on the X-axis of the plot.

## Value

A ggplot.

## Details

To keep the plot interesting, only the points representing up to max_n
draws are shown.

## Examples

``` r
  plot_varying_allowed_defect_rate(posited_defect_rate = 0.01, cert = 0.95)
```
