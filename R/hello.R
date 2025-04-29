#' compute number of samples needed to establish minimal error level with enough certainty
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param certaintity The certainty level you want, e.g. 0.95 (95%)
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     Should be lowwr than expected_error_rate.
#' @param max_n The maximum number of samples you are willing to use.
#'
#' @returns An estimate of the needed number of samples. Or -1 if the maximum number
#'     of samples needed is higher than max_n
#' @export
#'
#' @examples
drawsneeded <- function(expected_error_rate, certaintity, allowed_error_rate, max_n = 1000) {
  for (n in 1:max_n) {
    k <- n*expected_error_rate
    r <- qbeta(certaintity, k + 1, n - k + 1)
    if (r < allowed_error_rate) {
      return(r)
    }
  }
  return(-1)
}
