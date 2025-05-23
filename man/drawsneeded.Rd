% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawsneeded.R
\name{drawsneeded}
\alias{drawsneeded}
\title{compute number of samples needed to establish maximum error level with enough certainty}
\usage{
drawsneeded(expected_error_rate, allowed_error_rate, cert = 0.95, max_n = 1000)
}
\arguments{
\item{expected_error_rate}{The estimated error rate from earlier knowledge.}

\item{allowed_error_rate}{The highest error rate that is still acceptable.
So the treshold.
Should be higher than expected_error_rate.}

\item{cert}{The certainty level you want, e.g. \code{0.95}.}

\item{max_n}{The maximum number of samples you are willing to use.}
}
\value{
An estimate of the needed number of samples. Or \code{-1} if the maximum number
    of samples needed is higher than max_n.
}
\description{
In monetary unit sampling we have a file of monetary statements.
In general this concerns money that has been spent.
We want to estimate the percentage of money from the file that was wrongfully spent,
the error rate.
This function guesses the number of monetary unit
draws needed
to establish with some certainty that the error rate is below a certain threshold.
}
\details{
Each of the four arguments can have length > 1, but only one of these
four arguments.
}
\examples{
  x <- drawsneeded(0.001, 0.02, cert = 0.95, max_n = 500)
}
