#' Show in plot how defect defect rate influences draws needed
#'
#' The plot is based on varying the variable
#' posited_defect_rate in a call to drawsneeded(posited_defect_rate, allowed_defect_rate, cert).
#'
#' To keep the plot interesting, only the points representing up to
#' max_n draws are shown.
#'
#' @param allowed_defect_rate The highest defect rate that is still acceptable.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The number of to be drawn items maximally shown in the plot.
#' @param S The number of points on the X-axis of the
#'     plot.
#'
#' @examples
#'   plot_varying_posited_defect_rate(allowed_defect_rate = 0.02, cert = 0.95)
#' @returns
#'   A ggplot.
#' @importFrom ewgraph partition
#' @importFrom ewgraph posint
#' @import ggplot2
#' @importFrom tibble tibble
#' @export
plot_varying_posited_defect_rate <- function(allowed_defect_rate,
                                                    cert = 0.95,
                                                    max_n = 1000,
                                                    S = 10000) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(allowed_defect_rate) == 1)
    stopifnot(length(cert) == 1)

    # Check of specific arguments to plot_varying_posited_defect_rate().
    stopifnot(length(max_n) == 1)
    stopifnot(posint(max_n))
    stopifnot(length(S) == 1)
    stopifnot(posint(S))

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  # Vary over posited_defect_rate, leave rest of the arguments fixed.
  posited_defect_rate <- partition(begin = 0, end = allowed_defect_rate, S = S)
  draws_needed <- drawsneeded(posited_defect_rate = posited_defect_rate, allowed_defect_rate, cert)

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

  # Combine posited_defect_rate, draws_needed and possible into one tibble.
  t <- tibble(posited_defect_rate, draws_needed, possible)

  # Find highest number of draws, highest_n, and
  # accompanying defect rate, er_highest_n.
  highest_n <- max(draws_needed)
  er_highest_n <- posited_defect_rate[[which.max(draws_needed)]]

  # Construct title.
  title <- sprintf(
    "posited defect rate from 0 to allowed defect rate (%s)",
    formatf_without_trailing_zeros(allowed_defect_rate))

  # Construct subtitle.
  {
    lines <- ""
    line <- sprintf(
      "in:   allowed_defect_rate = %s; cert = %s; S = %d\n",
      formatf_without_trailing_zeros(allowed_defect_rate),
      formatf_without_trailing_zeros(cert),
      S
    )
    lines <- sprintf("%s%s", lines, line)

    line <- sprintf(
      "out: highest number of draws = %d, at posited defect rate = %s; shown up to %d draws\n",
      highest_n,
      formatf_without_trailing_zeros(er_highest_n),
      max_n
    )
    lines <- sprintf("%s%s", lines, line)

    subtitle <- lines
  }

  # Prepare vertical lines for max values.
  {
    hline_er_highest_n <-
      geom_hline(
        mapping = NULL,
        data = NULL,
        yintercept = max_n,
        colour = "green",
        linetype = "dotted"
      )


    vline_allowed_defect_rate <-
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = allowed_defect_rate,
        colour = "red",
        linetype = "dotted"
      )
  }

  # Call ggplot() on prepared data, title, subtitle.
  result <-
    ggplot(data = t) +
    hline_er_highest_n +
    vline_allowed_defect_rate +
    theme(plot.subtitle = element_text(size = 9, color = "blue")) +
    geom_point(mapping = aes(x = posited_defect_rate, y = draws_needed, color = possible)) +
    scale_colour_manual(values = c("transparent", "brown"), guide = "none") +
    annotate(
      "text",
      x = 0.2*allowed_defect_rate,
      y = max_n,
      label = "ceiling for # draws",
      angle = 0,
      vjust = 0,
      hjust = 0,
      size = 3.5,
      color = "green"
    ) +
    annotate(
      "text",
      x = allowed_defect_rate,
      y = max_n * 0.85,
      label = "allowed defect rate",
      angle = 270,
      # Rotate text vertically
      vjust = 0,
      hjust = 0,
      size = 3.5,
      color = "red"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      # caption = "piep",
      x = "posited defect rate",
      y = "draws needed"#,
    ) +
    theme(axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "brown")) +
    theme(plot.title = element_text(size = 14, color = "brown")) #, face = "bold"))

  return(result)
}
