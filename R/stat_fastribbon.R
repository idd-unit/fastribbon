#' @export
StatFastribbon <- ggplot2::ggproto(
  "StatFastribbon",
  ggplot2::Stat,

  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    fill = ggplot2::after_stat(level),
    order = ggplot2::after_stat(level)
  ),
  extra_params = c("na.rm", "orientation", "point_interval"),

  setup_params = function(data, params) {
    if (is.null(params$.width)) {
      params$.width <- c(0.5, 0.8, 0.95)
    }
    params$.width <- .validate_widths(params$.width)
    .check_summary_options(params)
    params
  },

  finish_layer = function(data, params) {
    if (nrow(data) > 0L) {
      # Keep intervals separate even when fill is fixed or mapped to a group.
      data$group <- as.integer(interaction(data$group, data$.width, drop = TRUE))
    }
    data
  },

  compute_group = function(
    data,
    scales,
    .width = c(0.5, 0.8, 0.95),
    na.rm = FALSE
  ) {
    .width <- .validate_widths(.width)
    if (nrow(data) == 0) {
      return(data.frame())
    }

    if (!is.numeric(data$x) || !is.numeric(data$y)) {
      stop("`x` and `y` must be numeric after scale transformation.", call. = FALSE)
    }

    # Also handle missing values when this method is called directly.
    data <- ggplot2::remove_missing(
      data,
      na.rm = na.rm,
      vars = c("x", "y"),
      name = "stat_fastribbon",
      finite = TRUE
    )
    if (nrow(data) == 0) {
      return(data.frame())
    }
    dt <- data.table::as.data.table(data)

    if (!"group" %in% names(dt)) {
      dt[, group := 1L]
    }

    # The C++ implementation requires values sorted within each group and x.
    data.table::setkey(dt, group, x, y)

    res_list <- compute_ribbon_stats(
      dt$x,
      dt$y,
      dt$group,
      .width
    )

    out <- data.table::setDT(res_list)

    widths <- sort(unique(out$.width), decreasing = TRUE)

    out[, level := factor(
      .width,
      levels = widths,
      labels = paste0(100 * widths, "%")
    )]

    as.data.frame(out)
  }
)

#' Fast median lines with uncertainty ribbons
#'
#' Calculate medians and equal-tailed intervals at each x value, optimized for
#' large numbers of trajectories.
#'
#' @details
#' Requires \code{x} and \code{y} aesthetics. Summaries are calculated within
#' each group, after scale transformation. Only vertical ribbons of numeric sample values
#' are supported; analytical distributions, weights, and alternative point or
#' interval summaries are not supported. Use \code{\link[ggdist]{stat_lineribbon}} for
#' those features. Use \code{ggplot2::coord_flip()} to display the result horizontally.
#'
#' @inheritParams ggdist::stat_lineribbon
#' @param geom The geom used to display the summaries. Defaults to
#'   \code{ggdist::GeomLineribbon}.
#' @param .width A nonempty numeric vector of finite probabilities between 0 and
#'   1, inclusive. Duplicate widths are removed. A width of 0 gives the median;
#'   a width of 1 gives the full range.
#' @param ... Additional arguments passed to \code{ggplot2::layer()} through
#'   its \code{params} argument, including geom parameters such as \code{alpha}
#'   and \code{linewidth}, and fixed aesthetics such as \code{colour}.
#'   \code{orientation} may be \code{NA}, \code{"x"}, or \code{"vertical"};
#'   ribbons are always computed vertically. \code{point_interval} is not supported.
#' @return A ggplot2 layer that can be added to a plot.
#' @section Computed variables:
#' These variables can be mapped with \code{ggplot2::after_stat()}:
#' \describe{
#'   \item{\code{x}}{The x value at which the summary is calculated.}
#'   \item{\code{y}}{The median of the sample values at each x within a group.}
#'   \item{\code{ymin}, \code{ymax}}{The lower and upper equal-tailed interval
#'     bounds, calculated using \code{stats::quantile()}'s type 7 convention.}
#'   \item{\code{.width}}{The probability covered by the interval.}
#'   \item{\code{level}}{A factor of percentage labels, such as \code{"95\%"},
#'     with levels ordered from widest to narrowest interval. Used for
#'     \code{fill} and drawing \code{order} by default.}
#' }
#' @importFrom ggplot2 layer
#' @examples
#' set.seed(123)
#' draws <- expand.grid(x = seq(0, 10, length.out = 51), draw = seq_len(100))
#' draws$y <- stats::rnorm(nrow(draws), mean = sin(draws$x), sd = 0.5)
#'
#' ggplot2::ggplot(draws, ggplot2::aes(x, y)) +
#'   stat_fastribbon(.width = c(0.5, 0.8, 0.95), alpha = 0.6) +
#'   ggplot2::labs(fill = "Interval probability")
#' @export
stat_fastribbon <- function(
  mapping = NULL,
  data = NULL,
  geom = ggdist::GeomLineribbon,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  .width = c(0.5, 0.8, 0.95),
  ...
) {
  .width <- .validate_widths(.width)
  params <- list(...)
  .check_summary_options(params)
  params$orientation <- "x"
  params$na.rm <- na.rm
  params$.width <- .width

  ggplot2::layer(
    stat = StatFastribbon,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

.validate_widths <- function(widths) {
  if (
    !is.numeric(widths) || length(widths) == 0L || !is.null(dim(widths)) ||
      any(!is.finite(widths)) || any(widths < 0 | widths > 1)
  ) {
    stop(
      "`.width` must be a nonempty numeric vector of finite numbers between 0 and 1.",
      call. = FALSE
    )
  }

  unique(widths)
}

.check_summary_options <- function(params) {
  if ("point_interval" %in% names(params)) {
    stop(
      "`point_interval` is not supported. Use `ggdist::stat_lineribbon()` for alternative summaries.",
      call. = FALSE
    )
  }

  orientation <- params$orientation
  if (!is.null(orientation) && (
    length(orientation) != 1L ||
      !(is.na(orientation) || orientation %in% c("x", "vertical"))
  )) {
    stop(
      "`orientation` must be NA, \"x\", or \"vertical\". Use `coord_flip()` to display ribbons horizontally.",
      call. = FALSE
    )
  }
}
