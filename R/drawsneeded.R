#' @title
#' compute number of samples needed to establish maximum defect rate with enough certainty
#'
#' @description
#' In sampling we have a file of like items.
#' We want to estimate the overall defect rate of  the items.
#' This function esitimates the number of draws needed
#' to establish with some certainty that the defect rate is below a certain
#' threshold.
#'
#' @details
#' Each of the four arguments can have length > 1, but only one of these four
#' arguments may have a length greater than 1 at a time.
#'
#' Note that the prior used by drawsneeded() is flat: each possible defect rate
#' is beforehand given an equal probability.
#'
#' This function can compute the number of samples needed based on the
#' binomial distribution, or based on the Poisson distribution.
#' Note that the Poisson distribution is an approximation of the binomial
#' distribution.
#' The distribution arguments "binomial" and "Poisson" invoke a computation
#' that is as precise as possible given the machine precision
#' of the underlying R implementation.
#' The distribution argument "Poisson_interpolated" is an approximative
#' computation for the Poisson distribution.
#' It is included to be able to compare its results with the other two
#' distribution arguments.
#'
#' @param posited_defect_rate The defect rate of which we want to see how many
#'     samples it would take to establish that it is below the allowed
#'     defect rate.
#'     Could for example be our guess of the defect rate.
#' @param allowed_defect_rate The highest defect rate that is still acceptable.
#'     So the threshold.
#'     Should be higher than posited_defect_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param distribution One of "binomial", "Poisson" or "Poisson_interpolated".
#'
#' @returns An estimate of the needed number of samples according
#'     to the distribution chosen. If the number of
#'     needed samples comes to close to the maximum R integer value, or
#'     cert ==1, return Inf.
#' @export
#' @importFrom stats qbeta
#' @importFrom stats pbeta
#' @importFrom dplyr near
#' @examples
#'   x <- drawsneeded(0.001, 0.02, cert = 0.95)
drawsneeded <- function(posited_defect_rate = 0.0,
                        allowed_defect_rate = 0.01,
                        cert = 0.95,
                        distribution = "binomial") {
  # We first handle recursion :-) .
  {
    # Check that the 3 first arguments are numeric vectors.
    stopifnot(is.numeric(posited_defect_rate))
    stopifnot(is.numeric(allowed_defect_rate))
    stopifnot(is.numeric(cert))

    # If posited_defect_rate has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(posited_defect_rate) > 1) {
      stopifnot(length(allowed_defect_rate) == 1)
      stopifnot(length(cert) == 1)
      stopifnot(length(distribution) == 1)

      r <- numeric(length(posited_defect_rate))
      for (i in seq_along(r)) {
        stopifnot(length(posited_defect_rate[[i]]) == 1)
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate[[i]],
          allowed_defect_rate = allowed_defect_rate,
          cert = cert,
          distribution = distribution
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
      stopifnot(length(distribution) == 1)

      r <- numeric(length(allowed_defect_rate))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate,
          allowed_defect_rate = allowed_defect_rate[[i]],
          cert = cert,
          distribution = distribution
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
      stopifnot(length(distribution) == 1)

      r <- numeric(length(cert))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate,
          allowed_defect_rate = allowed_defect_rate,
          cert = cert[[i]],
          distribution = distribution
        )
      }

      # Add names to r.
      names(r) <- cert

      return(r)
    }

    # If distribution has length > 1,
    # map it over drawsneeded().
    # And return results in 1 vector.
    if (length(distribution) > 1) {
      stopifnot(length(posited_defect_rate) == 1)
      stopifnot(length(allowed_defect_rate) == 1)
      stopifnot(length(cert) == 1)

      r <- numeric(length(distribution))
      for (i in seq_along(r)) {
        r[[i]] <- drawsneeded(
          posited_defect_rate = posited_defect_rate,
          allowed_defect_rate = allowed_defect_rate,
          cert = cert,
          distribution = distribution[[i]]
        )
      }

      # Add names to r.
      names(r) <- distribution

      return(r)

    }
  }

  # Non recursive case.
  {
    stopifnot(length(posited_defect_rate) == 1)
    stopifnot(length(allowed_defect_rate) == 1)
    stopifnot(length(cert) == 1)
    stopifnot(length(distribution) == 1)

    # Check parameters.
    stopifnot(0 <= posited_defect_rate)
    stopifnot(posited_defect_rate < 1)
    stopifnot(0 < allowed_defect_rate)
    stopifnot(allowed_defect_rate < 1)
    stopifnot(posited_defect_rate < allowed_defect_rate)
    stopifnot(0 <= cert)
    stopifnot(cert <= 1)
    stopifnot(distribution %in% c("binomial", "Poisson", "Poisson_interpolated"))

    if (cert == 0) {
      return(0)
    }

    if (cert == 1) {
      return(Inf)
    }

    if (distribution == "Poisson_interpolated") {
      return(calc_n_inverse(allowed_defect_rate, posited_defect_rate, cert))
    }

    # Binary search for the smallest n such that
    # max_defect_rate(n, posited_defect_rate, cert) <= allowed_defect_rate
    {
      max_n <- floor(.Machine$integer.max / 2) # Largest R integer/2.
      begin_range <- 1
      end_range <- max_n
      if (max_defect_rate(end_range, posited_defect_rate, cert, distribution) > allowed_defect_rate) {
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
          if (max_defect_rate(n = begin_range, posited_defect_rate, cert, distribution) <= allowed_defect_rate) {
            return(begin_range)
          } else {
            return(end_range)
          }
        } else {
          # There is a proper middle.
          middle <- floor((end_range - begin_range) / 2) + begin_range
          if (max_defect_rate(n = middle, posited_defect_rate, cert, distribution) <= allowed_defect_rate) {
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
        # max_defect_rate(n, posited_defect_rate, cert, distribution) <= allowed_defect_rate,
        # That is the value of n we are looking for.
      }
    }
  }
}

get_R <- function(k, cert = 0.95) {
  qgamma(cert, shape = k + 1, scale = 1)
}

calc_n_inverse <- function(allowed_defect_rate,
                           posited_defect_rate,
                           cert) {
  ratio_target <- posited_defect_rate / allowed_defect_rate

  found <- FALSE
  k_limit <- 1000 # To top k off.
  k1 <- NA
  k2 <- NA

  for (k in 0:k_limit) {
    # Get ratio for k.
    R_k <- get_R(k, cert)
    ratio_k <- k / R_k

    # Get ratio for k+1.
    R_k_next <- get_R(k + 1, cert)
    ratio_k_next <- (k + 1) / R_k_next

    # Check if target ratio is in between.
    if (ratio_k <= ratio_target && ratio_target <= ratio_k_next) {
      k1 <- k
      k2 <- k + 1
      found <- TRUE
      break
    }
  }

  if (!found) {
    stop("k not inside 0:1000; posited_defect_rate too close to allowed_defect_rate")
  }

  R_k1 <- get_R(k1, cert)
  R_k2 <- get_R(k2, cert)

  # We apply the formula from Paul van Batenburg: numerator
  # numerator = M * (k2 * R_k1 - k1 * R_k2)
  # As we use M == 1 here, we get:
  numerator <- (k2 * R_k1 - k1 * R_k2)

  #   We apply the formula from Paul van Batenburg: denominator
  #   denominator = ME * (k2 - k1) - PE * (R_k2 - R_k1)
  #   We have that k2 = k1 + 1, so k2 - k1 = 1.
  #   And we use M = 1 here, so ME = allowed_defect_rate,
  #   and PE = posited_defect_rate.
  #   So we get:
  denominator <- allowed_defect_rate - posited_defect_rate * (R_k2 - R_k1)

  n <- numerator / denominator
}

max_defect_rate <- function(n,
                            posited_defect_rate,
                            cert = 0.95,
                            distribution = "binomial") {
  # Check arguments.
  stopifnot(0 < n)
  stopifnot(0 <= posited_defect_rate)
  stopifnot(posited_defect_rate <= 1)
  stopifnot(0 <= cert)
  stopifnot(cert <= 1)
  stopifnot(distribution %in% c("binomial", "Poisson"))

  k <- n * posited_defect_rate

  if (distribution == "binomial") {
    # Compute maximum defect rate, q, given certainty level.
    # We do this using the beta quantile function.
    # We can interpret cert here as the surface below the chance
    # density function left of the vertical line defect rate == q.
    q <- qbeta(cert, shape1 = k + 1, shape2 = n - k + 1)

    # pbeta() is the inverse function:
    # The beta cumulative density function pbeta(), with parameter
    # q, and the same shape parameters, returns the cert.
    stopifnot(dplyr::near(cert, pbeta(
      q, shape1 = k + 1, shape2 = n - k + 1
    )))
  } else {
    # distribution == "Poisson"
    risk_factor <- qgamma(cert, shape = k + 1, scale = 1)
    q <- risk_factor / n
  }

  # print(c(q,n))
  return(q)
}
