legacy_fastribbon <- ggplot2::ggproto(
  "LegacyFastribbon", StatFastribbon,
  compute_layer = function(self, data, params, layout) {
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  }
)

canonical_ribbon <- function(out) {
  out <- out[do.call(order, out[c("PANEL", "group", "x", ".width")]),
             sort(names(out)), drop = FALSE]
  rownames(out) <- NULL
  out
}

test_that("layer batching matches panel/group summaries across facet scales", {
  data <- expand.grid(x = 1:4, draw = 1:9, category = c("a", "b"), panel = 1:3)
  data$y <- data$draw * data$panel + 100 * (data$category == "b")
  set.seed(12)
  data <- data[sample(nrow(data)), ]
  data$y[1:2] <- c(NA, Inf)
  for (scales in c("fixed", "free_x", "free_y", "free")) {
    for (transform in c("identity", "log10", "reverse")) {
      plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = category)) +
        ggplot2::facet_wrap(~panel, scales = scales) +
        ggplot2::scale_y_continuous(transform = transform)
      layer <- stat_fastribbon(.width = c(0, 0.5, 0.95, 1), na.rm = TRUE)
      expect_warning(fast <- ggplot2::ggplot_build(plot + layer), NA)
      layer$stat <- legacy_fastribbon
      old <- ggplot2::ggplot_build(plot + layer)
      expect_equal(canonical_ribbon(fast$data[[1]]),
                   canonical_ribbon(old$data[[1]]))
    }
  }
})

test_that("batching handles replicated layers, margins, and unused panels", {
  data <- expand.grid(x = 1:3, draw = 1:5, panel = factor(1:3))
  data$y <- data$draw + 10 * as.integer(data$panel)
  plots <- list(
    ggplot2::ggplot(data, ggplot2::aes(x, y)) +
      ggplot2::facet_grid(~panel, margins = TRUE),
    ggplot2::ggplot(data[data$panel != 2, ], ggplot2::aes(x, y)) +
      ggplot2::facet_wrap(~panel, drop = FALSE),
    ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::facet_wrap(~panel)
  )
  for (i in seq_along(plots)) {
    layer <- stat_fastribbon(fill = "steelblue")
    if (i == 3L) layer$data <- data.frame(x = rep(1:3, 5), y = 1:15)
    fast <- ggplot2::ggplot_build(plots[[i]] + layer)$data[[1]]
    layer$stat <- legacy_fastribbon
    old <- ggplot2::ggplot_build(plots[[i]] + layer)$data[[1]]
    expect_equal(canonical_ribbon(fast), canonical_ribbon(old))
  }
})

test_that("numeric layers make one summary call without pooling panels", {
  calls <- 0L
  counting <- ggplot2::ggproto("CountingFastribbon", StatFastribbon,
    compute_group = function(data, scales, .width = c(0.5, 0.8, 0.95), na.rm = FALSE) {
      calls <<- calls + 1L
      StatFastribbon$compute_group(data, scales, .width, na.rm)
    }
  )
  data <- expand.grid(x = 1:2, draw = 1:5, category = c("a", "b"), panel = 1:3)
  data$y <- data$draw + 100 * data$panel
  layer <- stat_fastribbon()
  layer$stat <- counting
  out <- ggplot2::ggplot_build(
    ggplot2::ggplot(data, ggplot2::aes(x, y, colour = category)) +
      layer + ggplot2::facet_wrap(~panel)
  )$data[[1]]
  expect_equal(calls, 1L)
  expect_equal(sort(unique(out$y)), c(103, 203, 303))
})

test_that("varying aesthetics retain the parent stat's warnings and output", {
  data <- expand.grid(x = 1:3, draw = 1:5, panel = 1:2)
  data$y <- data$draw
  data$alpha <- seq_len(nrow(data))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, alpha = alpha)) +
    ggplot2::facet_wrap(~panel)
  layer <- stat_fastribbon()
  warnings <- character()
  capture <- function(expr) withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  fast <- capture(ggplot2::ggplot_build(plot + layer))
  fast_warnings <- warnings
  warnings <- character()
  layer$stat <- legacy_fastribbon
  old <- capture(ggplot2::ggplot_build(plot + layer))
  expect_equal(fast_warnings, warnings)
  expect_true(all(grepl("aesthetics were dropped", warnings)))
  expect_length(warnings, 2L)
  expect_equal(canonical_ribbon(fast$data[[1]]), canonical_ribbon(old$data[[1]]))
})
