#' Graphically analyse through multiple plots a call to \code{drawsneeded()}
#'
#' Show in one figure the result of a call to \code{drawsneeded()} by means of four plots.
#' The first plot shows what chance graph will result if the foreseen number of
#' defects do indeed emerge by drawing and checking the planned number of
#' monetary units.
#'
#' The other three plots analyse the influence of respectively the parameters
#' \code{posited_defect_rate}, \code{allowed_defect_rate} and \code{cert} on the number of
#' draws needed. "Draws needed": meaning here the number of draws needed to have
#' enough random monetary units seen, to conclude with certainty cert
#' that the defect rate in the monetary unit lies below the allowed_defect_rate.
#'
#' @param posited_defect_rate The estimated defect rate from earlier knowledge.
#' @param allowed_defect_rate The highest defect rate that is still acceptable.
#'     Should be higher than posited_defect_rate.
#' @param cert The certainty level you want, e.g. \code{0.95}.
#' @param max_n The number of to be drawn items maximally shown in the plot.
#' @param S The number of points on the X-axis of the
#'     plot.
#' @param high_density_area Only points with a probability high enough to
#'    be part of the high density interval, which has size high_density_area,
#'    are shown in the plot.
#' @returns A ggplot.
#' @export
#' @examples
#'   combined_plots(0.001, 0.02, cert = 0.95)
#' @returns
#'   A ggplot.
#' @importFrom cowplot plot_grid
#' @importFrom ewgraph posint
#' @import ggplot2
#' @export
combined_plots <- function(posited_defect_rate,
                           allowed_defect_rate,
                           cert = 0.95,
                           max_n = 1000,
                           S = 10000,
                           high_density_area = 0.999) {
  # Argument check.
  {
    # No support for args with length > 1.
    stopifnot(length(posited_defect_rate) == 1)
    stopifnot(length(allowed_defect_rate) == 1)
    stopifnot(length(cert) == 1)
    stopifnot(length(max_n) == 1)
    stopifnot(length(S) == 1)
    stopifnot(length(high_density_area) == 1)

  }

  # Generate the four plots.
  plot1 <- drawsneeded_plot(
    posited_defect_rate = posited_defect_rate,
    allowed_defect_rate = allowed_defect_rate,
    cert = cert,
    S = S,
    high_density_area = high_density_area
  )
  plot2 <- plot_varying_posited_defect_rate(
    allowed_defect_rate = allowed_defect_rate,
    cert = cert,
    max_n = max_n,
    S = S
  )
  plot3 <- plot_varying_allowed_defect_rate(
    posited_defect_rate = posited_defect_rate,
    cert = cert,
    max_n = max_n,
    S = S
  )
  plot4 <- plot_varying_cert(
    posited_defect_rate = posited_defect_rate,
    allowed_defect_rate = allowed_defect_rate,
    max_n = max_n,
    S = S
  )

  cowplot::plot_grid(plot1, plot2, plot3, plot4, ncol = 2)
}
