# Show in plot how defect defect rate influences draws needed

The plot is based on varying the variable posited_defect_rate in a call
to drawsneeded(posited_defect_rate, allowed_defect_rate, cert).

## Usage

``` r
plot_varying_posited_defect_rate(
  allowed_defect_rate,
  cert = 0.95,
  max_n = 1000,
  S = 10000
)
```

## Arguments

- allowed_defect_rate:

  The highest defect rate that is still acceptable.

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
  plot_varying_posited_defect_rate(allowed_defect_rate = 0.02, cert = 0.95)
```
