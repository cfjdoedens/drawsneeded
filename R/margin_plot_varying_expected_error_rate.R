#' Show in plot how expected error rate influences draws needed
#'
#' The plot is based on varying the variable
#' expected_error_rate in a call to drawsneeded(expected_error_rate, allowed_error_rate, cert, max_n)
#'
#' @param allowed_error_rate The highest error rate that is still acceptable.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The maximum number of samples you are willing to use.
#' @param S The number of points on the X-axis of the
#'     plot.
#'
#' @examples
#'   margin_plot_varying_expected_error_rate(allowed_error_rate = 0.02, cert = 0.95, max_n = 500)
#' @returns
#'   A ggplot.
#' @importFrom ewgraph partition
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
margin_plot_varying_expected_error_rate <- function(
                        allowed_error_rate,
                        cert = 0.95,
                        max_n = 1000,
                        S = 10000) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(allowed_error_rate) == 1)
    stopifnot(length(max_n) == 1)
    stopifnot(length(cert) == 1)

    # check of specific arguments to margin_plot_varying_expected_error_rate().
    stopifnot(length(S) == 1)
    stopifnot(posint(S))

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  # Vary over expected_error_rate, leave rest of the arguments fixed.
  # And draw a figure: expected_error_rate x draws_needed.
  {
    expected_error_rate <- partition(begin = 0, end = allowed_error_rate, S = S)
    draws_needed <- drawsneeded(expected_error_rate = expected_error_rate, allowed_error_rate, cert, max_n)

    # Change -1 values from draws_needed to 0.
    for (i in 1:S) {
      if (draws_needed[[i]] == -1) {
        draws_needed[[i]] = 0
      }
    }

    # Add vector that describes whether or not there is a value for draws_needed.
    possible <- logical(S)
    for (i in 1:S) {
      if (draws_needed[[i]] == 0) {
        possible[[i]] = FALSE
      } else {
        possible[[i]] <- TRUE
      }

    }

    # Combine expected_error_rate and draws_needed into one tibble.
    t <- tibble(expected_error_rate, draws_needed, possible)

    # Find highest number of draws, highest_n, and
    # accompanying error rate, er_highest_n.
    highest_n <- max(draws_needed)
    er_highest_n <- expected_error_rate[[which.max(draws_needed)]]

    # Construct title.
    title <- sprintf(
      "varying expected error rate between 0 and allowed error rate (%s)",
      formatf_without_trailing_zeros(allowed_error_rate)
    )

    # Construct subtitle.
    {
      lines <- ""
      line <- sprintf(
        "     in: allowed_error_rate = %s (red dotted line); cert = %s; max_n = %d; S = %d\n",
        formatf_without_trailing_zeros(allowed_error_rate),
        formatf_without_trailing_zeros(cert),
        max_n,
        S
      )
      lines <- sprintf("%s%s", lines, line)

      line <- sprintf(
        "     out: highest number of draws = %d; reached for expected error rate = %s (green line)\n",
        highest_n,
        formatf_without_trailing_zeros(er_highest_n)
      )
      lines <- sprintf("%s%s", lines, line)

      subtitle <- lines
    }

    # Prepare vertical lines for max values.
    {
      vline_er_highest_n <-
        geom_vline(
          mapping = NULL,
          data = NULL,
          xintercept = er_highest_n,
          colour = "green"
        )


      vline_allowed_error_rate <-
        geom_vline(
          mapping = NULL,
          data = NULL,
          xintercept = allowed_error_rate,
          colour = "red",
          linetype = "dotted"
        )
    }

    # Call ggplot() on prepared data, title, subtitle.
    result <-
      ggplot(data = t) +
      vline_er_highest_n +
      vline_allowed_error_rate +
      theme(plot.subtitle = element_text(size = 9, color = "blue")) +
      geom_point(mapping = aes(
        x = expected_error_rate,
        y = draws_needed,
        color = possible
      )) +
      scale_colour_manual(
        values = c("black", "blue")
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        # caption = "piep",
        x = "expected error rate",
        y = "draws needed"#,
      )

    return(result)
  }
}
