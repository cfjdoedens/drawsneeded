#' compute number of samples needed to establish minimal error level with enough certainty
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param certainty The certainty level you want, e.g. \code{0.95}.
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     Should be lower than expected_error_rate.
#' @param max_n The maximum number of samples you are willing to use.
#'
#' @returns An estimate of the needed number of samples. Or \code{-1} if the maximum number
#'     of samples needed is higher than max_n.
#' @export
#' @importFrom stats qbeta
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

  for (n in 1:max_n) {
    k <- n*expected_error_rate
    r <- qbeta(certainty, k + 1, n - k + 1)
    if (r < allowed_error_rate) {
      return(n)
    }
  }
  return(-1)
}
