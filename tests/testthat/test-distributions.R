# Compare the statistical output independently of the two stats' fill labels
# and group identifiers (fastribbon separates widths for GeomRibbon).
build_distribution_summary <- function(data, stat, log_scale = FALSE) {
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, ydist = dist)) +
    stat(.width = c(0.5, 0.95))
  if (log_scale) plot <- plot + ggplot2::scale_y_log10()
  out <- ggplot2::ggplot_build(plot)$data[[1]]
  out <- out[order(out$x, out$.width), c("x", "y", "ymin", "ymax", ".width")]
  rownames(out) <- NULL
  out
}

test_that("sample distributions match ggdist on identity and transformed scales", {
  data <- data.frame(x = c(3, 1, 2))
  data$dist <- distributional::dist_sample(list(c(1, 3, 8, 20), 2:11, 10:30))
  for (log_scale in c(FALSE, TRUE)) {
    expect_warning(
      fast <- build_distribution_summary(data, stat_fastribbon, log_scale), NA
    )
    expect_equal(fast, build_distribution_summary(
      data, ggdist::stat_lineribbon, log_scale
    ))
  }
})

test_that("other distribution families use ggdist summaries", {
  for (dist in list(
    distributional::dist_normal(c(1, 2, 3), c(1, 2, 4)),
    distributional::dist_lognormal(c(1, 2, 3)),
    distributional::dist_poisson(c(1, 3, 8)),
    distributional::dist_sample(list(1:5, 2:8, 3:12)) + 1
  )) {
    data <- data.frame(x = 1:3)
    data$dist <- dist
    expect_warning(fast <- build_distribution_summary(data, stat_fastribbon), NA)
    expect_equal(fast, build_distribution_summary(data, ggdist::stat_lineribbon))
  }
})

test_that("mixed distribution vectors dispatch per element", {
  data <- data.frame(x = 1:3)
  data$dist <- c(
    distributional::dist_sample(list(1:10)),
    distributional::dist_normal(5, 2),
    distributional::dist_sample(list(20:30))
  )
  expect_warning(fast <- build_distribution_summary(data, stat_fastribbon), NA)
  expect_equal(fast, build_distribution_summary(data, ggdist::stat_lineribbon))
})

test_that("distribution ribbons preserve grouping, facets, and rendering", {
  data <- expand.grid(x = 1:3, category = c("a", "b"), panel = 1:2)
  data$dist <- distributional::dist_sample(lapply(seq_len(nrow(data)), function(i) {
    i + 1:10
  }))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, ydist = dist, colour = category)) +
    stat_fastribbon(fill = "steelblue") + ggplot2::facet_wrap(~panel)
  expect_warning(out <- ggplot2::ggplot_build(plot)$data[[1]], NA)
  expect_equal(nrow(out), nrow(data) * 3L)
  expect_equal(length(unique(out$colour)), 2L)
  expect_equal(length(unique(out$PANEL)), 2L)
  expect_equal(length(unique(out$group)), 6L)
  expect_equal(out$order, -out$.width)
  expect_warning(ggplot2::ggplotGrob(plot), NA)
})

test_that("sample distributions at the same x are summarized independently", {
  data <- data.frame(x = c(1, 1))
  data$dist <- distributional::dist_sample(list(1:5, 101:105))
  out <- build_distribution_summary(data, stat_fastribbon)
  expect_equal(sort(unique(out$y)), c(3, 103))
})

test_that("missing draws are removed and empty sample distributions are handled", {
  data <- data.frame(x = 1:3)
  data$dist <- distributional::dist_sample(list(c(1, NA, 3), numeric(), c(NA_real_)))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, ydist = dist))
  expect_warning(ggplot2::ggplot_build(plot + stat_fastribbon()), "Removed 2 rows")
  expect_warning(out <- ggplot2::ggplot_build(plot + stat_fastribbon(na.rm = TRUE)), NA)
  expect_equal(out$data[[1]]$y, rep(2, 3))
})
