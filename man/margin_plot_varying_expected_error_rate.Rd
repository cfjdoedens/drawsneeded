% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margin_plot_varying_expected_error_rate.R
\name{margin_plot_varying_expected_error_rate}
\alias{margin_plot_varying_expected_error_rate}
\title{Show in plot how expected error rate influences draws needed}
\usage{
margin_plot_varying_expected_error_rate(
  allowed_error_rate,
  cert = 0.95,
  max_n = 1000,
  S = 10000
)
}
\arguments{
\item{allowed_error_rate}{The highest error rate that is still acceptable.}

\item{cert}{The certainty level you want, e.g. \code{0.95}.}

\item{max_n}{The maximum number of samples you are willing to use.}

\item{S}{The number of points on the X-axis of the
plot.}
}
\value{
A ggplot.
}
\description{
The plot is based on varying the variable
expected_error_rate in a call to drawsneeded(expected_error_rate, allowed_error_rate, cert, max_n)
}
\examples{
  margin_plot_varying_expected_error_rate(allowed_error_rate = 0.02, cert = 0.95, max_n = 500)
}
