#' compute number of samples needed to establish minimal error level with enough certainty
#'
#' In monetary unit sampling we have a file of monetary statements.
#' In general this concerns money that has been spent.
#' We want to estimate the percentage of money from the file that was wrongfully spent,
#' the error rate.
#' The function guesses the number of monetary unit
#' draws needed
#' to establish with some certainty that the error rate is below a certain threshold.
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param certainty The certainty level you want, e.g. \code{0.95}.
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     Should be higher than expected_error_rate.
#' @param max_n The maximum number of samples you are willing to use.
#'
#' @returns An estimate of the needed number of samples. Or \code{-1} if the maximum number
#'     of samples needed is higher than max_n.
#' @export
#' @importFrom stats qbeta
#' @importFrom stats pbeta
#' @importFrom dplyr near
#' @examples
#'   x <- drawsneeded(0.001, 0.95, 0.02, max_n = 500)
drawsneeded <- function(expected_error_rate, certainty, allowed_error_rate, max_n = 1000) {
  # Check parameters.
  #  stopifnot(all(is.numeric(expected_error_rate)))
  stopifnot(0 <= expected_error_rate)
  stopifnot(expected_error_rate < 1)
  stopifnot(0 < certainty)
  stopifnot(certainty < 1)
  stopifnot(0 < allowed_error_rate)
  stopifnot(allowed_error_rate < 1)
  stopifnot(expected_error_rate < allowed_error_rate)
  stopifnot(0 < max_n)

  # We iterate from 1 to max_n to search for the n that will
  # give a satisfactory level of certainty.
  # A bit more complex, but more efficient, method would be
  # to use binary search.
  # I have not used binary search here to keep the code simple.
  for (n in 1:max_n) {
    k <- n*expected_error_rate

    # Compute maximum error rate, q, given certainty level.
    # We do this using the beta quantile function.
    # We can interpret certainty here as the surface below the chance
    # density function left of the vertical line error rate == q.
    q <- qbeta(certainty, shape1 = k + 1, shape2 = n - k + 1)

    # pbeta() is the inverse function:
    # The beta cumulative density function pbeta(), with parameter
    # q, and the same shape parameters, returns the certainty.
    stopifnot(near(certainty, pbeta(q, shape1 = k + 1, shape2 = n - k + 1)))

    if (q < allowed_error_rate) {
      return(n)
    }
  }
  return(-1)

  # To summarize:
  # Increasing n, will increase to a lesser extend k, and will increase q.
  # When n is sufficiently large q will rise above the allowed_error_rate.
  # That is the value of n we are looking for.
  # When n rises above the number of samples we are willing to
  # draw, we return -1.
}
