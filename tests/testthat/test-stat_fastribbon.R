test_that("widths are validated when constructing a layer", {
  invalid <- list(NULL, numeric(), "0.5", TRUE, NA_real_, NaN, Inf, -0.1, 1.1,
                  matrix(0.5))
  for (width in invalid) {
    expect_error(stat_fastribbon(.width = width), "`\\.width` must be")
  }
})

test_that("duplicate widths are removed and boundary widths work", {
  data <- data.frame(x = 1, y = 1:5)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    stat_fastribbon(.width = c(0, 1, 1))
  out <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(nrow(out), 2L)
  expect_equal(out$ymin[out$.width == 0], 3)
  expect_equal(out$ymax[out$.width == 0], 3)
  expect_equal(out$ymin[out$.width == 1], 1)
  expect_equal(out$ymax[out$.width == 1], 5)
})

test_that("unsupported summary and orientation options fail clearly", {
  expect_error(stat_fastribbon(point_interval = "mean_qi"), "alternative summaries")
  for (orientation in list("y", "horizontal", "invalid", character())) {
    expect_error(stat_fastribbon(orientation = orientation), "`orientation` must be")
  }
  expect_error(stat_fastribbon(orientation = "vertical"), NA)
  expect_error(stat_fastribbon(orientation = NA), NA)
})

test_that("empty and all-missing groups return empty data frames", {
  empty <- data.frame(x = numeric(), y = numeric())
  expect_equal(nrow(StatFastribbon$compute_group(empty, NULL)), 0L)
  missing <- data.frame(x = c(1, NA_real_), y = c(NA_real_, 2))
  expect_warning(
    out <- StatFastribbon$compute_group(missing, NULL),
    "Removed 2 rows"
  )
  expect_equal(nrow(out), 0L)
  expect_warning(
    out <- StatFastribbon$compute_group(missing, NULL, na.rm = TRUE),
    NA
  )
  expect_equal(nrow(out), 0L)
})

test_that("the public stat requires x and y", {
  plot <- ggplot2::ggplot(data.frame(y = 1:5), ggplot2::aes(y = y)) +
    stat_fastribbon()
  expect_error(ggplot2::ggplot_build(plot), "missing aesthetics: x")
})

test_that("the independent stat preserves grouping, aesthetics, and rendering", {
  data <- expand.grid(x = 1:3, draw = 1:5, category = c("a", "b"))
  data$y <- data$draw + ifelse(data$category == "b", 100, 0)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = category)) +
    stat_fastribbon(.width = c(0.5, 0.95))
  expect_warning(built <- ggplot2::ggplot_build(plot), NA)
  out <- built$data[[1]]
  expect_equal(nrow(out), 12L)
  expect_equal(length(unique(out$colour)), 2L)
  expect_equal(length(unique(out$group)), 4L)
  expect_equal(sort(unique(out$y)), c(3, 103))
  expect_equal(as.character(out$level), paste0(100 * out$.width, "%"))
  expect_warning(ggplot2::ggplotGrob(plot), NA)

  fixed_fill <- ggplot2::ggplot(data, ggplot2::aes(x, y, group = category)) +
    stat_fastribbon(.width = c(0.5, 0.95), fill = "steelblue")
  out <- ggplot2::ggplot_build(fixed_fill)$data[[1]]
  expect_equal(length(unique(out$group)), 4L)
  expect_warning(ggplot2::ggplotGrob(fixed_fill), NA)
})

test_that("missing observations follow ggplot2 removal conventions", {
  data <- data.frame(x = c(1, 1, NA, 2), y = c(1, NA, 3, Inf))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y))
  expect_warning(ggplot2::ggplot_build(plot + stat_fastribbon()), "Removed 3 rows")
  expect_warning(ggplot2::ggplot_build(plot + stat_fastribbon(na.rm = TRUE)), NA)
  data$y[] <- NA_real_
  expect_warning(built <- ggplot2::ggplot_build(
    ggplot2::ggplot(data, ggplot2::aes(x, y)) + stat_fastribbon(na.rm = TRUE)
  ), NA)
  expect_equal(nrow(built$data[[1]]), 0L)
})
