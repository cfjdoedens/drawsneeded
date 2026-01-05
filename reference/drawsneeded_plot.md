# Graphically show the results of a call to `drawsneeded()`

Let n be the result of the call n \<- drawsneeded(posited_defect_rate =
eer, allowed_defect_rate = aer, cert = c). chance density graph for k =
n\*eer, n = n. The vertical lines to denote posited_defect_rate and
allowed_defect_rate are also on the graph.

## Usage

``` r
drawsneeded_plot(
  posited_defect_rate,
  allowed_defect_rate,
  cert = 0.95,
  S = 1e+05,
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

  The certainty level you want, e.g. `0.95`.#'

- S:

  The number of points on the X-axis of the plot. but less points might
  be shown, as only points with in the high density interval are shown.

- high_density_area:

  Only points with a probability high enough to be part of the high
  density interval, which has size high_density_area, are shown in the
  plot.

## Value

A ggplot.

A ggplot.

## Details

If n \< 1, show a diagnostic message.

There is no support for vector args with length \> 1.

## Examples

``` r
  drawsneeded_plot(0.001, 0.02, cert = 0.95)
```
