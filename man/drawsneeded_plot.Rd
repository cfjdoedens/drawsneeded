% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawsneeded_plot.R
\name{drawsneeded_plot}
\alias{drawsneeded_plot}
\title{Graphically show the results of a call to \code{drawsneeded()}}
\usage{
drawsneeded_plot(
  expected_error_rate,
  allowed_error_rate,
  cert = 0.95,
  max_n = 1000,
  S = 10000,
  min_prob = 1.5
)
}
\arguments{
\item{expected_error_rate}{The estimated error rate from earlier knowledge.}

\item{allowed_error_rate}{The highest error rate that is still acceptable.
Should be higher than expected_error_rate.}

\item{cert}{The certainty level you want, e.g. \code{0.95}.#'}

\item{max_n}{The maximum number of samples you are willing to use.}

\item{S}{The number of points on the X-axis of the
plot. but less points might be shown, as only points
with a probability density >= min_prob are shown.}

\item{min_prob}{Only points with at least that probability are
shown in the plot.}
}
\value{
A ggplot.

A ggplot.
}
\description{
Let r be the result of the call
  n <- drawsneeded(expected_error_rate = eer,
                   allowed_error_rate = aer,
                   cert = c).
chance density graph for k = n*eer, n = n.
The vertical lines to denote expected_error_rate and allowed_error_rate are
also on the graph.
}
\details{
There is no support for vector args with length > 1.
}
\examples{
  drawsneeded_plot(0.001, 0.02, cert = 0.95, max_n = 500)
}
