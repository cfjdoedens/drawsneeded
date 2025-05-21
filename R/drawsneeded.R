#' compute number of samples needed to establish maximum error level with enough certainty
#'
#' In monetary unit sampling we have a file of monetary statements.
#' In general this concerns money that has been spent.
#' We want to estimate the percentage of money from the file that was wrongfully spent,
#' the error rate.
#' This function guesses the number of monetary unit
#' draws needed
#' to establish with some certainty that the error rate is below a certain threshold.
#'
#' Each of the four arguments can have length > 1, but only one of these
#' four arguments.
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     So the treshold.
#'     Should be higher than expected_error_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The maximum number of samples you are willing to use.
#'
#' @returns An estimate of the needed number of samples. Or \code{-1} if the maximum number
#'     of samples needed is higher than max_n.
#' @export
#' @importFrom stats qbeta
#' @importFrom stats pbeta
#' @importFrom dplyr near
#' @examples
#'   x <- drawsneeded(0.001, 0.02, cert = 0.95, max_n = 500)
drawsneeded <- function(expected_error_rate,
                        allowed_error_rate,
                        cert = 0.95,
                        max_n = 1000) {
  # We first handle recursion :-) .
  {
    # Check that all 4 arguments are numeric vectors.
    stopifnot(is.numeric(expected_error_rate))
    stopifnot(is.numeric(allowed_error_rate))
    stopifnot(is.numeric(cert))
    stopifnot(is.numeric(max_n))

    # If expected_error_rate has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(expected_error_rate) > 1) {
      stopifnot(length(allowed_error_rate) == 1)
      stopifnot(length(cert) == 1)
      stopifnot(length(max_n) == 1)

      r <- numeric(length(expected_error_rate))
      for (i in seq_along(r)) {
        stopifnot(length(expected_error_rate[[i]]) == 1)
        r[[i]] <- drawsneeded(
          expected_error_rate = expected_error_rate[[i]],
          allowed_error_rate = allowed_error_rate,
          cert = cert,
          max_n = max_n
        )
      }

      # Add names to r.
      names(r) <- expected_error_rate

      return(r)
    }

    # If allowed_error_rate has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(allowed_error_rate) > 1) {
      stopifnot(length(expected_error_rate) == 1)
      stopifnot(length(cert) == 1)
      stopifnot(length(max_n) == 1)

      r <- numeric(length(allowed_error_rate))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          expected_error_rate = expected_error_rate,
          allowed_error_rate = allowed_error_rate[[i]],
          cert = cert,
          max_n = max_n
        )
      }

      # Add names to r.
      names(r) <- allowed_error_rate

      return(r)
    }

    # If cert has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(cert) > 1) {
      stopifnot(length(expected_error_rate) == 1)
      stopifnot(length(allowed_error_rate) == 1)
      stopifnot(length(max_n) == 1)

      r <- numeric(length(cert))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          expected_error_rate = expected_error_rate,
          allowed_error_rate = allowed_error_rate,
          cert = cert[[i]],
          max_n = max_n
        )
      }

      # Add names to r.
      names(r) <- cert

      return(r)
    }

    # If max_n has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(max_n) > 1) {
      stopifnot(length(expected_error_rate) == 1)
      stopifnot(length(allowed_error_rate) == 1)
      stopifnot(length(cert) == 1)

      r <- numeric(length(max_n))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          expected_error_rate = expected_error_rate,
          allowed_error_rate = allowed_error_rate,
          cert = cert,
          max_n = max_n[[i]]
        )
      }

      # Add names to r.
      names(r) <- max_n

      return(r)
    }
  }

  # Non recursive case.
  {
    stopifnot(length(expected_error_rate) == 1)
    stopifnot(length(allowed_error_rate) == 1)
    stopifnot(length(max_n) == 1)
    stopifnot(length(cert) == 1)

    # Check parameters.
    stopifnot(0 <= expected_error_rate)
    stopifnot(expected_error_rate < 1)
    stopifnot(0 < allowed_error_rate)
    stopifnot(allowed_error_rate < 1)
    stopifnot(expected_error_rate < allowed_error_rate)
    stopifnot(0 < cert)
    stopifnot(cert < 1)
    stopifnot(0 < max_n)

    # Binary search for the smallest n such that
    # max_error_rate(n, expected_error_rate, cert) <= allowed_error_rate
    {
      begin_range <- 1
      end_range <- max_n
      if (max_error_rate(end_range, expected_error_rate, cert) > allowed_error_rate) {
        # The to be found n can not lie in [begin_range, end_range].
        return(-1)
      }

      # Invariant: the to be found n lies in [begin_range, end_range]
      while (TRUE) {
        if (begin_range == end_range) {
          # Necessarily, the to be found n equals begin_range (and also end_range).
          return(begin_range)
        } else  if (begin_range + 1 == end_range ) {
          # No proper middle.
          if (max_error_rate(n = begin_range, expected_error_rate, cert) <= allowed_error_rate) {
            return(begin_range)
          } else {
            return(end_range)
          }
        } else {
          # There is a proper middle.
          middle <- floor((end_range - begin_range) / 2) + begin_range
          if (max_error_rate(n = middle, expected_error_rate, cert) <= allowed_error_rate) {
            # We do away with the top half of [begin_range, end_range]
            end_range <- middle
            # Invariant: still, the to be found n lies in [begin_range, end_range]
          } else {
            # We do away with the bottom half of [begin_range, end_range]
            begin_range <- middle
            # Invariant: still, the to be found n lies in [begin_range, end_range]
          }
        }
        # To summarize:
        # Search for the smallest n for which
        # max_error_rate(n, expected_error_rate, cert) <= allowed_error_rate,
        # That is the value of n we are looking for.
      }
    }
  }
}

max_error_rate <- function(n, expected_error_rate, cert) {
  k <- n * expected_error_rate

  # Compute maximum error rate, q, given certainty level.
  # We do this using the beta quantile function.
  # We can interpret cert here as the surface below the chance
  # density function left of the vertical line error rate == q.
  q <- qbeta(cert, shape1 = k + 1, shape2 = n - k + 1)

  # pbeta() is the inverse function:
  # The beta cumulative density function pbeta(), with parameter
  # q, and the same shape parameters, returns the cert.
  stopifnot(near(cert, pbeta(
    q, shape1 = k + 1, shape2 = n - k + 1
  )))

  return(q)
}
