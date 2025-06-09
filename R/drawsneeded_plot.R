#' Graphically show the results of a call to \code{drawsneeded()}
#'
#' Let n be the result of the call
#'   n <- drawsneeded(posited_defect_rate = eer,
#'                    allowed_defect_rate = aer,
#'                    cert = c).
#  If n >= 1, show the binomial
#' chance density graph for k = n*eer, n = n.
#' The vertical lines to denote posited_defect_rate and allowed_defect_rate are
#' also on the graph.
#'
#' If n < 1, show a diagnostic message.
#'
#' There is no support for vector args with length > 1.
#'
#' @param posited_defect_rate The estimated defect rate from earlier knowledge.
#' @param allowed_defect_rate The highest defect rate that is still acceptable.
#'     Should be higher than posited_defect_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.#'
#' @param S The number of points on the X-axis of the
#'     plot. but less points might be shown, as only points
#'     with a probability density >= min_prob are shown.
#' @param min_prob Only points with at least that probability are
#'    shown in the plot.
#'
#' @returns A ggplot.
#' @export
#' @examples
#'   drawsneeded_plot(0.001, 0.02, cert = 0.95)
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
#' @importFrom grDevices rgb
#' @importFrom tibble tibble
#' @export
drawsneeded_plot <- function(posited_defect_rate,
                             allowed_defect_rate,
                             cert = 0.95,
                             S = 1e5,
                             min_prob = 1.5) {
  # Argument check.
  {
    # No support for drawsneeded() args with length > 1.
    stopifnot(length(posited_defect_rate) == 1)
    stopifnot(length(allowed_defect_rate) == 1)
    stopifnot(length(cert) == 1)

    # Check on arguments specific to drawsneeded_plot().
    stopifnot(length(S) == 1)
    stopifnot(posint(S))
    stopifnot(length(min_prob) == 1)
    stopifnot(is.numeric(min_prob))
    stopifnot(min_prob >= 0)

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  n <- drawsneeded(posited_defect_rate, allowed_defect_rate, cert)

  if (n >= 1) {
    # Create an ew vector g for a binomial graph
    # with k = posited_defect_rate * n, n = n.
    g <- ew_from_vec(dbinom_continuous(k = posited_defect_rate * n, n = n, partition_0_1(S)))

    # Get location, i.e. value of p for most likely defect rate.
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
    t <- tibble(colouring, p, h)

    # Keep only higher chance parts.
    t <- t %>% filter(h >= min_prob)

    # Count data rows for the plot.
    data_rows_available <- nrow(t)

    # Find left and right x-values of higher chance parts.
    x_high_left <- NA
    x_high_right <- NA
    if (data_rows_available > 0) {
      x_high_left <- min(t$p)
      x_high_right <- max(t$p)
    }

    # Construct title.
    title <- sprintf("chance graph of postulated defect rate for n = %d, and k = %s",
                       n, formatf_without_trailing_zeros(n*posited_defect_rate))

    # Construct subtitle.
    {
      line <- "in:   "
      lines <- line

      line <- sprintf(
        "posited_defect_rate = %s; allowed_defect_rate = %s; cert = %s;",
        formatf_without_trailing_zeros(posited_defect_rate),
        formatf_without_trailing_zeros(allowed_defect_rate),
        formatf_without_trailing_zeros(cert)
      )
      lines <- sprintf("%s%s", lines, line)

      line <- sprintf(" S = %d, min_prob = %s\n",
                      S,
                      formatf_without_trailing_zeros(min_prob))
      lines <- sprintf("%s%s", lines, line)

      line <- "out: "
      lines <- sprintf("%s%s", lines, line)

      line <- sprintf("n = estimated draws needed = %d;", n)
      lines <- sprintf("%s%s", lines, line)

      line <- sprintf(
        " k = estimated defects = n * posited_defect_rate = %s",
        formatf_without_trailing_zeros(posited_defect_rate * n)
      )
      lines <- sprintf("%s%s", lines, line)

      # line <- sprintf("     black dots: postulated defect fraction versus probability, given n and k\n")
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
    vline_posited_defect_rate <-
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = posited_defect_rate,
        colour = "blue",
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

    # Subset for area shading to the left of the allowed_defect_rate
    t_shade <- t %>% filter(p <= allowed_defect_rate)

    # Call ggplot() on prepared data, title, subtitle.
    result <- ggplot(data = t, aes(x = p, y = h)) +
      vline_posited_defect_rate +
      vline_allowed_defect_rate +
      theme(plot.subtitle = element_text(size = 9, color = "blue")) +
      geom_area(
        data = t_shade,
        aes(x = p, y = h),
        fill = rgb(0.78, 0.89, 1, alpha = 0.6),
        color = "blue"
      ) +
      annotate(
        "text",
        x = posited_defect_rate + (allowed_defect_rate - posited_defect_rate) * .2,
        y = most_prob_h * 0.2,
        label = "area == cert",
        size = 4.5,
        color = "blue",
        hjust = 0
      ) +
      annotate(
        "text",
        x = posited_defect_rate,
        y = most_prob_h * 0.15,
        label = "posited defect rate",
        angle = 90,
        # Rotate text vertically, and up.
        vjust = 0,
        hjust = 0,
        size = 3.5,
        color = "blue"
      ) +
      annotate(
        "text",
        x = allowed_defect_rate,
        y = most_prob_h * 0.85,
        label = "allowed defect rate",
        angle = 270,
        # Rotate text vertically, and down.
        vjust = 0,
        hjust = 0,
        size = 3.5,
        color = "red"
      ) +
      geom_point(color = "blue") +
      scale_colour_manual(
        values = c("blue", "orange"),
        guide = "none" #,
        # breaks = c(possible) #,
        # labels = c(possible)
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "postulated defect rate",
        y = "probability"#,
        # color = "what"
      ) +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "blue")) +
      theme(plot.title = element_text(size = 14, color = "blue")) #, face = "bold"))

    return(result)
  } else
    # No solution found. Explain what could be done.
  {
    g <- ew_from_vec(rep.int(1, 1000))

    # Get p and h vectors from g.
    p <- ew_get_p(g)
    h <- ew_get_h(g)

    # Place p and h in one tibble.
    t <- tibble(p, h)

    # Construct title.
    title <- sprintf("no solution")

    # Construct subtitle.
    {
      line <- "in:\n"
      lines <- line

      line <- sprintf(
        "     posited_defect_rate = %s; allowed_defect_rate = %s; cert = %s;\n",
        formatf_without_trailing_zeros(posited_defect_rate),
        formatf_without_trailing_zeros(allowed_defect_rate),
        formatf_without_trailing_zeros(cert)
      )
      lines <- sprintf("%s%s", lines, line)

      line <- sprintf("     S = %d, min_prob = %s\n",
                      S,
                      formatf_without_trailing_zeros(min_prob))
      lines <- sprintf("%s%s", lines, line)

      subtitle <- lines
    }

    # Call ggplot() on prepared data, title, subtitle.
    result <- ggplot(data = t, aes(x = p, y = h)) +
      theme(plot.subtitle = element_text(size = 9, color = "blue")) +
      annotate(
        "text",
        x = allowed_defect_rate,
        y = max(t$h) * 0.85,
        label = "remedies:",
        angle = 0,
        vjust = 0,
        hjust = 0,
        size = 5,
        color = "black"
      ) +
      annotate(
        "text",
        x = 0.1,
        y = max(t$h) * 0.65,
        label = "posited_defect_rate: make it smaller",
        angle = 0,
        vjust = 0,
        hjust = 0,
        size = 5.5,
        color = "green"
      ) +
      annotate(
        "text",
        x = 0.1,
        y = max(t$h) * 0.55,
        label = "allowed_defect_rate: make it larger",
        angle = 0,
        vjust = 0,
        hjust = 0,
        size = 5.5,
        color = "green"
      ) +
      annotate(
        "text",
        x = 0.1,
        y = max(t$h) * 0.45,
        label = "cert: make it smaller",
        angle = 0,
        vjust = 0,
        hjust = 0,
        size = 5.5,
        color = "green"
      ) +
      geom_point(color = "yellow") +
      labs(
        title = title,
        subtitle = subtitle,
        # caption = " ",
        x = "postulated defect rate",
        y = "probability density"#,
        # color = "what"
      )

    return(result)
  }
}
