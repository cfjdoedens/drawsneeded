#' Show in plot how allowed error rate influences draws needed
#'
#' The plot is based on varying the variable
#' allowed_error_rate in a call to drawsneeded(expected_error_rate, allowed_error_rate, cert, max_n)
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The maximum number of samples you are willing to use.
#' @param S The number of points on the X-axis of the
#'     plot.
#'
#' @examples
#'   margin_plot_varying_allowed_error_rate(expected_error_rate = 0.01, cert = 0.95, max_n = 500)
#' @returns
#'   A ggplot.
#' @importFrom ewgraph partition
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
margin_plot_varying_allowed_error_rate <- function(expected_error_rate = 0.01,
                                                   cert = 0.95,
                                                   max_n = 1000,
                                                   S = 10000) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(expected_error_rate) == 1)
    stopifnot(length(cert) == 1)
    stopifnot(length(max_n) == 1)

    # check of specific arguments to margin_plot_varying_allowed_error_rate().
    stopifnot(length(S) == 1)
    stopifnot(posint(S))

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  # Vary over allowed_error_rate, leave rest of the arguments fixed.
  # And draw a figure: allowed_error_rate x draws_needed.
  {
    allowed_error_rate <- partition(begin = expected_error_rate, end = 1, S = S)
    draws_needed <- drawsneeded(expected_error_rate = expected_error_rate, allowed_error_rate = allowed_error_rate, cert, max_n)

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

    # Combine allowed_error_rate and draws_needed into one tibble.
    t <- tibble(allowed_error_rate, draws_needed, possible)

    # Find highest number of draws, highest_n, and
    # accompanying error rate, er_highest_n.
    highest_n <- max(draws_needed)
    er_highest_n <- allowed_error_rate[[which.max(draws_needed)]]

    # Construct title.
    title <- sprintf(
      "varying allowed error rate between expected error rate (%s) and 1",
      formatf_without_trailing_zeros(expected_error_rate)
    )

    # Construct subtitle.
    {
      lines <- ""
      line <- sprintf(
        "     in: expected_error_rate = %s (red dotted line); cert = %s; max_n = %d; S = %d\n",
        formatf_without_trailing_zeros(expected_error_rate),
        formatf_without_trailing_zeros(cert),
        max_n,
        S
      )
      lines <- sprintf("%s%s", lines, line)

      line <- sprintf(
        "     out: highest number of draws = %d; reached for allowed error rate = %s (green line)\n",
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
          xintercept = expected_error_rate,
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
        x = allowed_error_rate,
        y = draws_needed,
        #.data$prob#,
        #color = possible
        color = possible
      )) +
      scale_colour_manual(
        values = c("black", "blue") #,
        # breaks = c(possible) #,
        # labels = c(possible)
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        # caption = "piep",
        x = "allowed error rate",
        y = "draws needed"#,
        # color = "what"
      )

    return(result)
  }
}
