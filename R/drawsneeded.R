#' compute number of samples needed to establish maximum error level with enough certainty
#'
#' In monetary unit sampling we have a file of monetary statements.
#' In general this concerns money that has been spent.
#' We want to estimate the percentage of money from the file that was wrongfully spent,
#' the error rate.
#' This function guesses the number of monetary unit
#' draws needed
#' to establish with some certaintity that the error rate is below a certain threshold.
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

    # We iterate from 1 to max_n to search for the n that will
    # give a satisfactory level of certainty.
    # A bit more complex, but more efficient, method would be
    # to use binary search.
    # I have not used binary search here to keep the code simple.
    for (n in 1:max_n) {
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
}

#' Graphically show the results of a call to \code{drawsneeded()}
#'
#' Let r be the result of the call
#'   n <- drawsneeded(expected_error_rate = eer,
#'                    allowed_error_rate = aer,
#'                    cert = c).
#  If n > 0, show the graph for the binomial distribution the
#' chance density graph for k = n*eer, n = n.
#' The vertical lines to denote expected_error_rate and allowed_error_rate are
#' also on the graph.
#'
#' For the moment there is no support for vector args with length > 1.
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     Should be higher than expected_error_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.#'
#' @param max_n The maximum number of samples you are willing to use.
#' @param S The number of points on the X-axis of the
#'   plot. but less points might be shown, as only points
#'   with a probability density >= min_prob are shown.
#' @param min_prob The minimum probability to be shown in the plot.
#'
#' @returns A ggplot.
#' @export
#' @examples
#'   drawsneeded_plot(0.001, 0.02, cert = 0.95, max_n = 500)
#' @returns
#'   A ggplot.
#' @importFrom binompoiscont dbinom_continuous
#' @importFrom dplyr '%>%'
#' @importFrom dplyr filter
#' @importFrom ewgraph ew_from_vec
#' @importFrom ewgraph ew_get_h
#' @importFrom ewgraph ew_get_p
#' @importFrom ewgraph ew_maxcumh_p
#' @importFrom ewgraph ew_maxh
#' @importFrom ewgraph ew_round_prob
#' @importFrom ewgraph partition_0_1
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom tidyr all_of
#' @importFrom tidyr pivot_longer
#' @export
drawsneeded_plot <- function(expected_error_rate,
                             allowed_error_rate,
                             cert = 0.95,
                             max_n = 1000,
                             S = 10000,
                             min_prob = 1.5) {
  # Argument check.
  {
    # For the moment no support for drawsneeded() args with length > 1.
    stopifnot(length(expected_error_rate) == 1)
    stopifnot(length(allowed_error_rate) == 1)
    stopifnot(length(max_n) == 1)
    stopifnot(length(cert) == 1)

    stopifnot(length(S) == 1)
    stopifnot(posint(S))

    stopifnot(length(min_prob) == 1)
    stopifnot(is.numeric(min_prob))
    stopifnot(min_prob >= 0)

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  n <- drawsneeded(expected_error_rate, allowed_error_rate, cert, max_n)

  # Create an ew vector g for a binomial graph
  # with k = expected_error_rate * n, n = n.
  g <- ew_from_vec(dbinom_continuous(k = expected_error_rate * n, n = n, partition_0_1(S)))

  # Get location, i.e. value of p for most likely error rate.
  most_prob_p <- ew_maxh(g)[["p"]]
  most_prob_h <- ew_maxh(g)[["h"]]

  # Get location, i.e. value of p, where the surface under the part
  # of the chance graph from 0 to p equals cert.
  max <- ew_maxcumh_p(g, cert)

  # Combine p and h into one tibble.
  p <- ew_get_p(g)
  h <- ew_get_h(g)
  t <- tibble(p, h)

  # Keep only higher chance parts.
  t <- t %>% filter(h >= min_prob)

  # Count data rows for the plot.
  data_rows_available <- nrow(t)

  # Make long version of t for plotting.
  cols <- "h"
  t <- pivot_longer(t, all_of(cols), names_to = "what", values_to = "prob")

  # Construct title.
  title <- sprintf("Estimate of number of needed draws")

  # Construct subtitle.
  {
    line <- "input:\n"
    lines <- line

    line <- sprintf(
      "     expected_error_rate = %s; allowed_error_rate = %s; cert = %s;\n",
      formatf_without_trailing_zeros(expected_error_rate),
      formatf_without_trailing_zeros(allowed_error_rate),
      formatf_without_trailing_zeros(cert)
    )
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf(
      "     max_n = %d; S = %d, min_prob = %s\n",
      max_n,
      S,
      formatf_without_trailing_zeros(min_prob)
    )
    lines <- sprintf("%s%s", lines, line)

    line <- "output:\n"
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf("     n = estimated number of draws needed = %d\n", n)
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf(
      "     k = estimate of errors that will be found = n * expected_error_rate = %s\n",
      formatf_without_trailing_zeros(expected_error_rate * n)
    )
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf("     black dots: postulated error fraction versus probability, given n and k\n")
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf("     blue vertical line: expected_error_rate\n")
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf("     red vertical line:  allowed_error_rate\n")
    lines <- sprintf("%s%s", lines, line)

    if (data_rows_available == 0) {
      line <- sprintf(
        "     NO DATA POINTS SHOWN AS min_prob IS TOO HIGH; PLEASE MAKE min_prob <= %d\n",
        floor(most_prob_h)
      )
      lines <- sprintf("%s%s", lines, line)
    }

    subtitle <- lines
  }

  # Prepare vertical lines for max values.
  {
    vline_expected_error_rate <-
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = expected_error_rate,
        colour = "blue"
      )


    vline_allowed_error_rate <-
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = allowed_error_rate,
        colour = "red"
      )
  }


  # Call ggplot() on prepared data, title, subtitle.
  result <- ggplot(data = t) +
    vline_expected_error_rate +
    vline_allowed_error_rate +
    # theme(plot.subtitle = element_textbox_simple()) +
    # We can not use here:
    #   geom_point(mapping = aes(x = p, y = prob, color = what)) +
    # because this will invoke an error message from devtools::check() like:
    #   no visible binding for global variable ‘prob’
    # So we use .data$prob instead of prob.
    # The same for ‘what’.
    geom_point(mapping = aes(
      x = p,
      y = .data$prob#,
      # color = .data$what
    )) +
    # scale_colour_manual(values = scale_color_manual_values,
    #                     breaks = scale_color_manual_breaks,
    #                     labels = scale_color_manual_labels) +
    labs(
      title = title,
      subtitle = subtitle,
      # caption = " ",
      x = "postulated error fraction",
      y = "probability"#,
      # color = "what"
    )

  return(result)
}

remove_trailing_zeros <- function(s) {
  while (nchar(s) > 1) {
    if (substring(s, nchar(s), nchar(s)) == "0") {
      s <- substring(s, 1, nchar(s) - 1)
    } else {
      return(s)
    }
  }
  return(s)
}

formatf_without_trailing_zeros <- function(nr) {
  stopifnot(is.numeric(nr))
  stopifnot(length(nr) == 1)
  remove_trailing_zeros(sprintf("%f", nr))
}
