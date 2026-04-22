
#' @export
StatFastribbon <- ggproto(
  "StatFastribbon",
  ggdist:::StatLineribbon,

  compute_panel = function(self, data, scales, .width = c(0.5, 0.8, 0.95), na.rm = FALSE, ...) {
    if (nrow(data) == 0) return(data.frame())

    dt <- data.table::as.data.table(data)

    if (na.rm) {
      dt <- dt[!is.na(y)]
    }

    if (!"group" %in% names(dt)) dt[, group := 1L]

    # Sort by group -> x -> y to prepare for c++ code
    data.table::setkey(dt, group, x, y)

    res_list <- compute_ribbon_stats(
      dt$x,
      dt$y,
      dt$group,
      .width
    )

    out <- data.table::setDT(res_list)

    labels <- paste0(.width * 100, "%") |>
      `names<-`(as.character(seq_len(length(.width)))) |>
      rev()

    out[, level := factor(
      level,
      levels = unique(level),
      labels = labels
    )]

    # Add dummy columns required by ggdist/ggplot2
    out[, `:=`(
      f = NA_real_,
      pdf = NA_real_,
      density = NA_real_
    )]

    return(out)
  }
)

#' Fast stat_lineribbon
#'
#' A re-implementation of ggdist::stat_lineribbon optimized for large numbers of trajectories
#'
#' @inheritParams ggdist::stat_lineribbon
#' @param .width a vector of probabilities to use that determine the widths of the resulting intervals
#' @param ... Other arguments passed to stat_lineribbon
#' @importFrom ggplot2 layer
#' @examplesIf interactive()
#' df <- expand_grid(x = seq(0, 100, by = 0.1), y = rep(0, 1000)) |>
#'     mutate(y = rnorm(n(), cos(x / 10) * 1, 1.5 + sin(x / 10)))
#'
#' ggplot(df) +
#'    stat_fastribbon(aes(x = x, y = y))
#' @export
stat_fastribbon <- function(mapping = NULL, data = NULL, geom = ggdist::GeomLineribbon,
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, .width = c(0.50, 0.80, 0.95), ...) {

  ggplot2::layer(
    stat = StatFastribbon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, .width = .width, ...)
  )
}
