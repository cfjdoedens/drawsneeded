#' compute number of samples needed to establish maximum defect rate with enough certainty
#'
#' In sampling we have a file of like items.
#' We want to estimate the overall defect rate of  the items.
#' This function guesses the number of draws needed
#' to establish with some certainty that the defect rate is below a certain
#' threshold.
#'
#' Each of the three arguments can have length > 1, but only one of these
#' three arguments.
#'
#' Note that the prior used by drawsneeded() is flat: each possible defect rate
#' is beforehand given an equal probability.
#'
#' @param posited_defect_rate The defect rate of which we want to see how many
#'     samples it would take to establish that it is below the allowed
#'     defect rate.
#'     Could for example be our guess of the defect rate.
#' @param allowed_defect_rate The highest defect rate that is still acceptable.
#'     So the threshold.
#'     Should be higher than posited_defect_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#'
#' @returns An estimate of the needed number of samples. If the number of
#'     needed samples comes to close to the maximum R integer value, or
#'     cert ==1, return Inf.
#' @export
#' @importFrom stats qbeta
#' @importFrom stats pbeta
#' @importFrom dplyr near
#' @examples
#'   x <- drawsneeded(0.001, 0.02, cert = 0.95)
drawsneeded <- function(posited_defect_rate,
                        allowed_defect_rate,
                        cert = 0.95) {
  # We first handle recursion :-) .
  {
    # Check that all 3 arguments are numeric vectors.
    stopifnot(is.numeric(posited_defect_rate))
    stopifnot(is.numeric(allowed_defect_rate))
    stopifnot(is.numeric(cert))

    # If posited_defect_rate has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(posited_defect_rate) > 1) {
      stopifnot(length(allowed_defect_rate) == 1)
      stopifnot(length(cert) == 1)

      r <- numeric(length(posited_defect_rate))
      for (i in seq_along(r)) {
        stopifnot(length(posited_defect_rate[[i]]) == 1)
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate[[i]],
          allowed_defect_rate = allowed_defect_rate,
          cert = cert
        )
      }

      # Add posited_defect_rate values as names to r.
      names(r) <- posited_defect_rate

      return(r)
    }

    # If allowed_defect_rate has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(allowed_defect_rate) > 1) {
      stopifnot(length(posited_defect_rate) == 1)
      stopifnot(length(cert) == 1)

      r <- numeric(length(allowed_defect_rate))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate,
          allowed_defect_rate = allowed_defect_rate[[i]],
          cert = cert
        )
      }

      # Add names to r.
      names(r) <- allowed_defect_rate

      return(r)
    }

    # If cert has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(cert) > 1) {
      stopifnot(length(posited_defect_rate) == 1)
      stopifnot(length(allowed_defect_rate) == 1)

      r <- numeric(length(cert))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate,
          allowed_defect_rate = allowed_defect_rate,
          cert = cert[[i]]
        )
      }

      # Add names to r.
      names(r) <- cert

      return(r)
    }
  }

  # Non recursive case.
  {
    stopifnot(length(posited_defect_rate) == 1)
    stopifnot(length(allowed_defect_rate) == 1)
    stopifnot(length(cert) == 1)

    # Check parameters.
    stopifnot(0 <= posited_defect_rate)
    stopifnot(posited_defect_rate < 1)
    stopifnot(0 < allowed_defect_rate)
    stopifnot(allowed_defect_rate < 1)
    stopifnot(posited_defect_rate < allowed_defect_rate)
    stopifnot(0 <= cert)
    stopifnot(cert <= 1)

    if (cert == 0) {
      return(0)
    }

    if (cert == 1) {
      return(Inf)
    }

    # Binary search for the smallest n such that
    # max_defect_rate(n, posited_defect_rate, cert) <= allowed_defect_rate
    {
      max_n <- floor(.Machine$integer.max/2) # Largest R integer/2.
      begin_range <- 1
      end_range <- max_n
      if (max_defect_rate(end_range, posited_defect_rate, cert) > allowed_defect_rate) {
        # The to be found n can not lie in [begin_range, end_range].
        return(Inf)
      }

      # Invariant: the to be found n lies in [begin_range, end_range]
      while (TRUE) {
        if (begin_range == end_range) {
          # Necessarily, the to be found n equals begin_range (and also end_range).
          return(begin_range)
        } else  if (begin_range + 1 == end_range) {
          # No proper middle.
          if (max_defect_rate(n = begin_range, posited_defect_rate, cert) <= allowed_defect_rate) {
            return(begin_range)
          } else {
            return(end_range)
          }
        } else {
          # There is a proper middle.
          middle <- floor((end_range - begin_range) / 2) + begin_range
          if (max_defect_rate(n = middle, posited_defect_rate, cert) <= allowed_defect_rate) {
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
        # max_defect_rate(n, posited_defect_rate, cert) <= allowed_defect_rate,
        # That is the value of n we are looking for.
      }
    }
  }
}

max_defect_rate <- function(n, posited_defect_rate, cert) {
  k <- n * posited_defect_rate

  # Compute maximum defect rate, q, given certainty level.
  # We do this using the beta quantile function.
  # We can interpret cert here as the surface below the chance
  # density function left of the vertical line defect rate == q.
  q <- qbeta(cert, shape1 = k + 1, shape2 = n - k + 1)

  # pbeta() is the inverse function:
  # The beta cumulative density function pbeta(), with parameter
  # q, and the same shape parameters, returns the cert.
  stopifnot(near(cert, pbeta(
    q, shape1 = k + 1, shape2 = n - k + 1
  )))
  # print(c(q,n))

  return(q)
}
