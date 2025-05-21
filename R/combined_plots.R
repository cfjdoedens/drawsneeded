#' Graphically analyse through multiple plots a call to \code{drawsneeded()}
#'
#' Show in one figure the result of a call to \code{drawsneeded()} by means of four plots.
#' The first plot shows what chance graph will result if the foreseen number of
#' errors do indeed emerge by drawing and checking the planned number of
#' monetary units.
#'
#' The other three plots analyse the influence of respectively the parameters
#' \code{expected_error_rate}, \code{allowed_error_rate} and \code{cert} on the number of
#' draws needed. "Draws needed": meaning here the number of draws needed to have
#' enough random monetary units seen, to conclude with certainty cert
#' that the error rate in the monetary unit lies below the allowed_error_rate.
#'
#' @param expected_error_rate The estimated error rate from earlier knowledge.
#' @param allowed_error_rate The highest error rate that is still acceptable.
#'     Should be higher than expected_error_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The maximum number of samples you are willing to use.
#' @param S The number of points on the X-axis of the
#'     plot.
#' @param min_prob Only points with at least that probability are
#'    shown in the first plot.
#'
#' @returns A ggplot.
#' @export
#' @examples
#'   combined_plots(0.001, 0.02, cert = 0.95, max_n = 500)
#' @returns
#'   A ggplot.
#' @importFrom cowplot plot_grid
#' @importFrom ewgraph posint
#' @import ggplot2
#' @export
combined_plots <- function(expected_error_rate,
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

    # We leave the rest of the argument checking to drawsneeded(), called directly below.
  }

  # Generate the four plots.
  plot1 <- drawsneeded_plot(
    expected_error_rate = expected_error_rate,
    allowed_error_rate = allowed_error_rate,
    cert = cert,
    max_n = max_n,
    S = S,
    min_prob = min_prob
  )
  plot2 <- margin_plot_varying_expected_error_rate(
    allowed_error_rate = allowed_error_rate,
    cert = cert,
    max_n = max_n,
    S = S
  )
  plot3 <- margin_plot_varying_allowed_error_rate(
    expected_error_rate = expected_error_rate,
    cert = cert,
    max_n = max_n,
    S = S
  )
  plot4 <- margin_plot_varying_cert(
    expected_error_rate = expected_error_rate,
    allowed_error_rate = allowed_error_rate,
    max_n = max_n,
    S = S
  )

  cowplot::plot_grid(plot1, plot2, plot3, plot4, ncol = 2)
}
