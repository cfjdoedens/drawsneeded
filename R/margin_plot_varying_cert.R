#' Show in plot how varying allowed defect rate influences draws needed
#'
#' The plot is based on varying the variable
#' allowed_defect_rate in a call to drawsneeded(posited_defect_rate, allowed_defect_rate, cert).
#'
#' To keep the plot interesting, only the points representing up to
#' max_n draws are shown.
#'
#' @param posited_defect_rate The proposed defect rate.
#'     Should be lower than allowed_defect_rate.
#' @param allowed_defect_rate The highest defect rate that is still acceptable.
#'     Should be higher than posited_defect_rate.
#' @param max_n The number of to be drawn items maximally shown in the plot.#'
#' @param S The number of points on the X-axis of the
#'     plot.
#'
#' @examples
#'   margin_plot_varying_cert(posited_defect_rate = 0.01, allowed_defect_rate = 0.02)
#' @returns
#'   A ggplot.
#' @importFrom ewgraph partition
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
margin_plot_varying_cert <- function(posited_defect_rate = 0.01,
                                     allowed_defect_rate = 0.02,
                                     max_n = 1000,
                                     S = 10000) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(posited_defect_rate) == 1)
    stopifnot(length(allowed_defect_rate) == 1)

    # check of specific arguments to margin_plot_varying_cert().
    stopifnot(length(S) == 1)
    stopifnot(posint(S))

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  # Vary over cert, leave rest of the arguments fixed.
  cert <- partition(begin = 0.5, end = 1, S = S)
  draws_needed <- drawsneeded(
    posited_defect_rate = posited_defect_rate,
    allowed_defect_rate = allowed_defect_rate,
    cert = cert
  )

  # Change values from draws_needed > max_n to 0.
  for (i in 1:S) {
    if (draws_needed[[i]] > max_n) {
      draws_needed[[i]] <- 0
    }
  }

  # Add vector that describes whether or not there is a value for draws_needed.
  possible <- logical(S)
  for (i in 1:S) {
    if (draws_needed[[i]] <= 0) {
      possible[[i]] = FALSE
    } else {
      possible[[i]] <- TRUE
    }
  }

  # Combine allowed_defect_rate and draws_needed into one tibble.
  t <- tibble(cert, draws_needed, possible)

  # Find highest number of draws, highest_n, and
  # accompanying defect rate, er_highest_n.
  highest_n <- max(draws_needed)
  cert_highest_n <- cert[[which.max(draws_needed)]]

  # Construct title.
  title <- sprintf("varying certainty between 0.5 and 1, shown up to %d draws",
                   max_n)

  # Construct subtitle.
  {
    lines <- ""
    line <- sprintf(
      "     in: posited_defect_rate = %s; allowed_defect_rate = %s; S = %d\n",
      formatf_without_trailing_zeros(posited_defect_rate),
      formatf_without_trailing_zeros(allowed_defect_rate),
      S
    )
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf(
      "     out: highest number of draws = %d; reached for certainty = %s",
      highest_n,
      formatf_without_trailing_zeros(cert_highest_n)
    )
    lines <- sprintf("%s%s", lines, line)

    subtitle <- lines
  }

  # Prepare vertical lines for max values.
  {
    hline_cert_highest_n <-
      geom_hline(
        mapping = NULL,
        data = NULL,
        yintercept = max_n,
        colour = "green",
        linetype = "dotted"
      )

    # Call ggplot() on prepared data, title, subtitle.
    result <-
      ggplot(data = t) +
      hline_cert_highest_n +
      theme(plot.subtitle = element_text(size = 9, color = "blue")) +
      geom_point(mapping = aes(x = cert, y = draws_needed, color = possible)) +
      scale_colour_manual(values = c(`TRUE` = "blue", `FALSE` = "transparent", "blue"), guide = "none") +
      annotate(
        "text",
        x = 0.2,
        y = max_n,
        label = "ceiling for # draws",
        angle = 0,
        vjust = 0,
        hjust = 0,
        size = 3.5,
        color = "green"
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "cert",
        y = "draws needed"#,
      )

    return(result)
  }
}
