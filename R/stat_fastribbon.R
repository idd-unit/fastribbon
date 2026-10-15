#' @export
StatFastribbon <- ggplot2::ggproto(
  "StatFastribbon",
  ggplot2::Stat,

  required_aes = c("x", "y|ydist"),
  optional_aes = "ydist",
  dropped_aes = "ydist",
  default_aes = ggplot2::aes(
    fill = ggplot2::after_stat(level),
    order = ggplot2::after_stat(-.width)
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

  compute_layer = function(self, data, params, layout) {
    # Distribution summaries need panel-specific scales. Let ggplot2 handle
    # those and invalid inputs through its usual path.
    parent <- ggplot2::ggproto_parent(ggplot2::Stat, self)
    if (!is.null(data$ydist) || !is.numeric(data$x) ||
        !is.numeric(data$y) || !nrow(data)) {
      return(parent$compute_layer(data, params, layout))
    }
    data <- ggplot2::remove_missing(
      data, na.rm = params$na.rm, vars = c("x", "y"),
      name = "stat_fastribbon", finite = TRUE
    )
    if (!nrow(data)) return(data.frame())

    # Group numbers can recur across panels: give each pair a distinct ID for
    # the C++ segment scan, then restore the original panel and group below.
    keys <- data.table::data.table(PANEL = data$PANEL, group = data$group)
    keys[, batch := .GRP, by = c("PANEL", "group")]
    batch <- keys$batch
    first <- which(!duplicated(batch))

    # The parent stat restores constant aesthetics and warns about varying
    # ones. Retain that behaviour for unusual mappings through the parent path.
    extra <- setdiff(names(data), c("x", "y", "PANEL", "group"))
    if (length(extra)) {
      metadata <- data.table::as.data.table(data[, extra, drop = FALSE])
      constant <- metadata[, lapply(.SD, function(x) {
        data.table::uniqueN(x) == 1L
      }), by = batch]
      if (!all(unlist(constant[, -1L, with = FALSE], use.names = FALSE))) {
        return(parent$compute_layer(data, params, layout))
      }
    }

    # compute_group owns its narrow input and performs one sort for the layer.
    # On errors, use the parent path to retain ggplot2's panel-wise warnings.
    out <- tryCatch(self$compute_group(
      data.frame(x = data$x, y = data$y, group = batch),
      scales = NULL, .width = params$.width, na.rm = params$na.rm
    ), error = function(e) NULL)
    if (is.null(out)) return(parent$compute_layer(data, params, layout))
    source <- data[first[out$group], , drop = FALSE]
    out$group <- source$group
    for (name in setdiff(names(source), names(out))) {
      out[[name]] <- source[[name]]
    }
    out
  },

  finish_layer = function(data, params) {
    if (nrow(data) > 0L) {
      # Keep intervals separate even when fill is fixed or mapped to a group.
      # GeomRibbon draws in group order, so wider intervals must come first.
      width <- factor(
        data$.width,
        levels = sort(unique(data$.width), decreasing = TRUE)
      )
      data$group <- as.integer(interaction(data$group, width, drop = TRUE))
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

    if (!is.null(data$ydist)) {
      return(.summarise_distributions(data, scales, .width, na.rm))
    }

    if (!is.numeric(data$x) || !is.numeric(data$y)) {
      stop("`x` and `y` must be numeric after scale transformation.", call. = FALSE)
    }

    # Only these columns are needed by the summary. In particular, do not copy
    # and reorder every aesthetic (or every column supplied by a direct caller)
    # for every draw. data.table() owns its columns, so sorting cannot mutate
    # the caller's data, including when it is already a data.table.
    data <- data.table::data.table(
      x = data$x,
      y = data$y,
      group = if (is.null(data$group)) 1L else data$group
    )

    # The usual ggplot path has already removed non-finite values. Check the
    # numeric vectors directly instead of constructing a row-wise missingness
    # matrix. Delegate removal, warnings and invalid na.rm values to ggplot2.
    if (!all(is.finite(data$x)) || !all(is.finite(data$y)) ||
        !is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
      data <- ggplot2::remove_missing(
        data,
        na.rm = na.rm,
        vars = c("x", "y"),
        name = "stat_fastribbon",
        finite = TRUE
      )
    }
    if (nrow(data) == 0) {
      return(data.frame())
    }
    dt <- data

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

.summarise_distributions <- function(data, scales, .width, na.rm) {
  if (!distributional::is_distribution(data$ydist)) {
    stop("`ydist` must be a distributional distribution vector.", call. = FALSE)
  }

  # Inspect each element: a distribution vector can contain different families.
  # Transformed, weighted, and multivariate samples use ggdist's implementation.
  sample <- vapply(unclass(data$ydist), function(d) {
    inherits(d, "dist_sample") && is.numeric(d$x) && is.null(dim(d$x))
  }, logical(1))
  results <- list()
  if (any(sample)) {
    rows <- which(sample)
    draws <- lapply(unclass(data$ydist[rows]), function(d) d$x)
    # Use a separate synthetic x for each distribution, so distributions at
    # the same x are never pooled. Preserve the original row's aesthetics.
    values <- unlist(draws, use.names = FALSE)
    if (!is.null(scales$y)) {
      values <- scales$y$trans$transform(values)
    }
    out <- StatFastribbon$compute_group(
      data.frame(x = rep(seq_along(rows), lengths(draws)), y = values),
      scales, .width = .width, na.rm = na.rm
    )
    if (nrow(out)) {
      source <- data[rows[out$x], , drop = FALSE]
      out$x <- source$x
      out$group <- NULL
      for (name in setdiff(names(source), names(out))) {
        out[[name]] <- source[[name]]
      }
      results[[length(results) + 1L]] <- out
    }
  }
  if (any(!sample)) {
    # Obtain the ggdist stat through its public layer constructor rather than
    # depending on unexported ggproto objects or distribution-specific methods.
    stat <- ggdist::stat_lineribbon()$stat
    other <- data[!sample, , drop = FALSE]
    params <- stat$setup_params(other, list(
      .width = .width, na.rm = na.rm, orientation = "x"
    ))
    other <- stat$setup_data(other, params)
    results[[length(results) + 1L]] <- do.call(
      stat$compute_panel, c(list(data = other, scales = scales), params)
    )
  }
  results <- lapply(results, function(out) {
    out$level <- NULL
    out$ydist <- NULL
    out
  })
  out <- as.data.frame(data.table::rbindlist(results, fill = TRUE))
  if (nrow(out)) {
    widths <- sort(unique(out$.width), decreasing = TRUE)
    out$level <- factor(out$.width, levels = widths,
                        labels = paste0(100 * widths, "%"))
  }
  out
}

#' Fast median lines with uncertainty ribbons
#'
#' Calculate medians and equal-tailed intervals at each x value, optimized for
#' large numbers of trajectories.
#'
#' @details
#' Requires \code{x} and either numeric \code{y} or a \code{ydist} aesthetic
#' containing a \pkg{distributional} distribution vector. Univariate numeric
#' \code{distributional::dist_sample()} distributions use the C++ implementation;
#' other distributions use the default \code{\link[ggdist]{stat_lineribbon}}
#' implementation. Weights and alternative point or interval summaries are
#' not supported. Only vertical ribbons are computed; use
#' \code{ggplot2::coord_flip()} to display the result horizontally.
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
#'     \code{fill} by default.}
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
#'
#' # Sample distributions use the fast implementation.
#' samples <- data.frame(x = 1:3)
#' samples$dist <- distributional::dist_sample(list(1:10, 2:11, 3:12))
#' ggplot2::ggplot(samples, ggplot2::aes(x = x, ydist = dist)) +
#'   stat_fastribbon()
#'
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
