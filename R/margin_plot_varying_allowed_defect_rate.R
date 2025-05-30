#' Show in plot how allowed defect rate influences draws needed
#'
#' The plot is based on varying the variable
#' allowed_defect_rate in a call to drawsneeded(posited_defect_rate, allowed_defect_rate, cert)
#'
#' To keep the plot interesting, only the points representing up to
#' max_n draws are shown.
#'
#' @param posited_defect_rate The estimated defect rate from earlier knowledge.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The number of to be drawn items maximally shown in the plot.
#' @param S The number of points on the X-axis of the
#'     plot.
#'
#' @examples
#'   margin_plot_varying_allowed_defect_rate(posited_defect_rate = 0.01, cert = 0.95)
#' @returns
#'   A ggplot.
#' @importFrom ewgraph partition
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
margin_plot_varying_allowed_defect_rate <- function(posited_defect_rate = 0.01,
                                                   cert = 0.95,
                                                   max_n = 1000,
                                                   S = 10000) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(posited_defect_rate) == 1)
    stopifnot(length(cert) == 1)

    # check of specific arguments to margin_plot_varying_allowed_defect_rate().
    stopifnot(length(max_n) == 1)
    stopifnot(posint(max_n))
    stopifnot(length(S) == 1)
    stopifnot(posint(S))

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  # Vary over allowed_defect_rate, leave rest of the arguments fixed.
  allowed_defect_rate <- partition(begin = posited_defect_rate, end = 1, S = S)
  draws_needed <- drawsneeded(posited_defect_rate = posited_defect_rate,
                              allowed_defect_rate = allowed_defect_rate,
                              cert)

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
  t <- tibble(allowed_defect_rate, draws_needed, possible)

  # Find highest number of draws, highest_n, and
  # accompanying defect rate, er_highest_n.
  highest_n <- max(draws_needed)
  er_highest_n <- allowed_defect_rate[[which.max(draws_needed)]]

  # Construct title.
  title <- sprintf(
    "varying allowed defect rate between posited defect rate (%s) and 1,\nshown up to %d draws",
    formatf_without_trailing_zeros(posited_defect_rate),
    max_n
  )

  # Construct subtitle.
  {
    lines <- ""
    line <- sprintf(
      "     in: posited_defect_rate = %s (red dotted line); cert = %s; S = %d\n",
      formatf_without_trailing_zeros(posited_defect_rate),
      formatf_without_trailing_zeros(cert),
      S
    )
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf(
      "     out: highest number of draws = %d; reached for allowed defect rate = %s (green line)\n",
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


    vline_allowed_defect_rate <-
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = posited_defect_rate,
        colour = "red",
        linetype = "dotted"
      )
  }

  # Call ggplot() on prepared data, title, subtitle.
  result <-
    ggplot(data = t) +
    vline_er_highest_n +
    vline_allowed_defect_rate +
    theme(plot.subtitle = element_text(size = 9, color = "blue")) +
    geom_point(mapping = aes(
      x = allowed_defect_rate,
      y = draws_needed,
      #.data$prob#,
      #color = possible
      color = possible
    )) +
    scale_colour_manual(values = c(`TRUE` = "blue", `FALSE` = "transparent"), guide = "none" #,
      # breaks = c(possible) #,
      # labels = c(possible)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      # caption = "piep",
      x = "allowed defect rate",
      y = "draws needed"#,
      # color = "what"
    )

  return(result)
}
