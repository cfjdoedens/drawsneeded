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
#' There is no support for vector args with length > 1.
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     Should be higher than expected_error_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.#'
#' @param max_n The maximum number of samples you are willing to use.
#' @param S The number of points on the X-axis of the
#'     plot. but less points might be shown, as only points
#'     with a probability density >= min_prob are shown.
#' @param min_prob Only points with at least that probability are
#'    shown in the plot.
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
#' @importFrom ewgraph partition_0_1
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
drawsneeded_plot <- function(expected_error_rate,
                             allowed_error_rate,
                             cert = 0.95,
                             max_n = 1000,
                             S = 10000,
                             min_prob = 1.5) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(expected_error_rate) == 1)
    stopifnot(length(allowed_error_rate) == 1)
    stopifnot(length(max_n) == 1)
    stopifnot(length(cert) == 1)

    # Check on arguments specific to drawsneeded_plot().
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

  # Get p and h vectors from g.
  p <- ew_get_p(g)
  h <- ew_get_h(g)

  # Make a vector with one (arbitrary) categorical value for colouring.
  # To be used below for colouring the chance graph.
  colouring <- rep("a", times = length(p))

  # Place p, h, and colouring in one tibble.
  t <- tibble(p, h, colouring)

  # Keep only higher chance parts.
  t <- t %>% filter(h >= min_prob)

  # Count data rows for the plot.
  data_rows_available <- nrow(t)

  # Make long version of t for plotting.
  cols <- "h"

  # Construct title.
  title <- sprintf("%d draws needed; projected chance graph", n)

  # Construct subtitle.
  {
    line <- "input:\n"
    lines <- line

    line <- sprintf(
      "     expected_error_rate = %s (blue dotted line); allowed_error_rate = %s (red dotted line); cert = %s;\n",
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

    # line <- sprintf("     black dots: postulated error fraction versus probability, given n and k\n")
    # lines <- sprintf("%s%s", lines, line)

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
  vline_expected_error_rate <-
    geom_vline(
      mapping = NULL,
      data = NULL,
      xintercept = expected_error_rate,
      colour = "blue",
      linetype = "dotted"
    )
  vline_allowed_error_rate <-
    geom_vline(
      mapping = NULL,
      data = NULL,
      xintercept = allowed_error_rate,
      colour = "red",
      linetype = "dotted"
    )

  # Call ggplot() on prepared data, title, subtitle.
  result <- ggplot(data = t) +
    vline_expected_error_rate +
    vline_allowed_error_rate +
    theme(plot.subtitle = element_text(size = 9, color = "blue")) +
    geom_point(mapping = aes(x = p, y = h, color = colouring)) +
    scale_colour_manual(
      values = c("blue"),
      guide = "none" #,
      # breaks = c(possible) #,
      # labels = c(possible)
    ) +
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
