# Graphically analyse through multiple plots a call to `drawsneeded()`

Show in one figure the result of a call to
[`drawsneeded()`](https://cfjdoedens.github.io/drawsneeded/reference/drawsneeded.md)
by means of four plots. The first plot shows what chance graph will
result if the foreseen number of defects do indeed emerge by drawing and
checking the planned number of monetary units.

## Usage

``` r
combined_plots(
  posited_defect_rate,
  allowed_defect_rate,
  cert = 0.95,
  max_n = 1000,
  S = 10000,
  high_density_area = 0.999
)
```

## Arguments

- posited_defect_rate:

  The estimated defect rate from earlier knowledge.

- allowed_defect_rate:

  The highest defect rate that is still acceptable. Should be higher
  than posited_defect_rate.

- cert:

  The certainty level you want, e.g. `0.95`.

- max_n:

  The number of to be drawn items maximally shown in the plot.

- S:

  The number of points on the X-axis of the plot.

- high_density_area:

  Only points with a probability high enough to be part of the high
  density interval, which has size high_density_area, are shown in the
  plot.

## Value

A ggplot.

A ggplot.

## Details

The other three plots analyse the influence of respectively the
parameters `posited_defect_rate`, `allowed_defect_rate` and `cert` on
the number of draws needed. "Draws needed": meaning here the number of
draws needed to have enough random monetary units seen, to conclude with
certainty cert that the defect rate in the monetary unit lies below the
allowed_defect_rate.

## Examples

``` r
  combined_plots(0.001, 0.02, cert = 0.95)
```
