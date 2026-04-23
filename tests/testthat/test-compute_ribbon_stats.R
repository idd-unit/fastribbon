library(testthat)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Prepare inputs the way StatFastribbon does: sort by group -> x -> y,
# then call compute_ribbon_stats.
run_stats <- function(x, y, group = rep(1L, length(x)),
                      widths = c(0.50, 0.80, 0.95)) {
  ord  <- order(group, x, y)
  res  <- compute_ribbon_stats(x[ord], y[ord], group[ord], widths)
  as.data.frame(res)
}

# Reference quantile using R's default type 7 (mathematically identical to
# the C++ get_quantile_from_index implementation).
q7 <- function(v, p) as.numeric(quantile(v, probs = p, type = 7))


# ===========================================================================
# 1. Empty input
# ===========================================================================
test_that("empty input returns an empty list", {
  res <- compute_ribbon_stats(numeric(0), numeric(0), integer(0), c(0.5, 0.8))
  expect_equal(length(res), 0)
})


# ===========================================================================
# 2. Single observation
# ===========================================================================
test_that("single observation: median equals the value itself", {
  res <- run_stats(x = 1, y = 42, widths = 0.9)
  expect_equal(res$y, 42)
})

test_that("single observation: ymin == ymax == y for any width", {
  res <- run_stats(x = 1, y = 7, widths = c(0.5, 0.8, 0.95))
  expect_true(all(res$ymin == res$y))
  expect_true(all(res$ymax == res$y))
})


# ===========================================================================
# 3. Two observations
# ===========================================================================
test_that("two observations: median matches type-7 quantile", {
  vals <- c(3, 9)
  res  <- run_stats(x = c(1, 1), y = vals, widths = 0.5)
  expect_equal(res$y, q7(vals, 0.5))
})

test_that("two observations: ymin / ymax match type-7 quantiles", {
  vals  <- c(2, 8)
  width <- 0.8
  res   <- run_stats(x = c(1, 1), y = vals, widths = width)
  lo    <- (1 - width) / 2
  hi    <- 1 - lo
  expect_equal(res$ymin, q7(vals, lo))
  expect_equal(res$ymax, q7(vals, hi))
})


# ===========================================================================
# 4. Correctness against R's quantile() for various sizes
# ===========================================================================

check_quantiles <- function(n_obs, widths = c(0.5, 0.8, 0.95)) {
  set.seed(42)
  vals <- rnorm(n_obs)
  res  <- run_stats(x = rep(0, n_obs), y = vals, widths = widths)

  for (w in widths) {
    row <- res[res$.width == w, ]
    lo  <- (1 - w) / 2
    hi  <- 1 - lo

    expect_equal(row$y,    q7(vals, 0.5), label = sprintf("median n=%d w=%.2f", n_obs, w))
    expect_equal(row$ymin, q7(vals, lo),  label = sprintf("ymin   n=%d w=%.2f", n_obs, w))
    expect_equal(row$ymax, q7(vals, hi),  label = sprintf("ymax   n=%d w=%.2f", n_obs, w))
  }
}

test_that("quantiles are exact for n = 10",   {check_quantiles(10)})
test_that("quantiles are exact for n = 100",  {check_quantiles(100)})
test_that("quantiles are exact for n = 1000", {check_quantiles(1000)})
test_that("quantiles are exact for n = 10001 (crosses checkUserInterrupt boundary)", {check_quantiles(10001) })


# ===========================================================================
# 5. Interval properties
# ===========================================================================
test_that("ymin <= y (median) <= ymax for any data", {
  set.seed(7)
  vals <- rnorm(200)
  res  <- run_stats(x = rep(1, 200), y = vals, widths = c(0.5, 0.8, 0.95))
  expect_true(all(res$ymin <= res$y))
  expect_true(all(res$y    <= res$ymax))
})

test_that("wider intervals always contain narrower ones", {
  set.seed(8)
  vals <- rnorm(300)
  res  <- run_stats(x = rep(1, 300), y = vals, widths = c(0.5, 0.8, 0.95))

  get_row <- function(w) res[res$.width == w, ]
  r50 <- get_row(0.50); r80 <- get_row(0.80); r95 <- get_row(0.95)

  expect_true(r95$ymin <= r80$ymin)
  expect_true(r80$ymin <= r50$ymin)
  expect_true(r50$ymax <= r80$ymax)
  expect_true(r80$ymax <= r95$ymax)
})

test_that("symmetric dataset -> intervals are symmetric around the median", {
  # Mirror the data so the sample itself is exactly symmetric around 0.
  base <- sort(rnorm(250, mean = 0))
  vals <- c(base, -base)           # exactly symmetric; median = 0

  res <- run_stats(x = rep(1, 500), y = vals, widths = c(0.5, 0.8, 0.95))

  for (i in seq_len(nrow(res))) {
    expect_equal(
      res$ymin[i] + res$ymax[i],
      2 * res$y[i],                # 2 * median = 0
      tolerance = 1e-10
    )
  }
})


# ===========================================================================
# 6. Multiple x values at a single group
# ===========================================================================
test_that("each unique x gets its own independent quantile calculation", {
  set.seed(10)
  x_vals <- c(1, 1, 1, 2, 2, 2)
  y_vals <- c(1, 2, 3, 10, 20, 30)
  res <- run_stats(x_vals, y_vals, widths = 0.5)

  row_x1 <- res[res$x == 1, ]
  row_x2 <- res[res$x == 2, ]

  expect_equal(row_x1$y, q7(c(1, 2, 3),    0.5))
  expect_equal(row_x2$y, q7(c(10, 20, 30), 0.5))
})

test_that("number of output rows == n_unique_x * n_widths", {
  set.seed(11)
  n_x      <- 5
  n_rep    <- 20
  widths   <- c(0.5, 0.8, 0.95)
  x_vals   <- rep(seq_len(n_x), each = n_rep)
  y_vals   <- rnorm(n_x * n_rep)
  res <- run_stats(x_vals, y_vals, widths = widths)
  expect_equal(nrow(res), n_x * length(widths))
})


# ===========================================================================
# 7. Multiple groups
# ===========================================================================
test_that("groups are processed independently", {
  set.seed(12)
  group_vals <- c(rep(1L, 50), rep(2L, 50))
  x_vals     <- rep(0, 100)
  y_vals     <- c(rnorm(50, mean = 0), rnorm(50, mean = 100))
  res <- run_stats(x_vals, y_vals, group = group_vals, widths = 0.5)

  g1 <- res[res$group == 1, ]
  g2 <- res[res$group == 2, ]

  expect_equal(g1$y, q7(y_vals[1:50],  0.5))
  expect_equal(g2$y, q7(y_vals[51:100], 0.5))
})

test_that("different groups at the same x do not bleed into each other", {
  # All observations share x = 1 but belong to two different groups.
  # The quantiles must reflect each group's y values, not the union.
  y_g1 <- c(0, 1, 2)
  y_g2 <- c(100, 200, 300)
  x    <- rep(1, 6)
  grp  <- c(1L, 1L, 1L, 2L, 2L, 2L)
  res  <- run_stats(x, c(y_g1, y_g2), grp, widths = 0.5)

  expect_equal(res[res$group == 1, ]$y, q7(y_g1, 0.5))
  expect_equal(res[res$group == 2, ]$y, q7(y_g2, 0.5))
})


# ===========================================================================
# 8. Width = 1  =>  full range
# ===========================================================================
test_that("width = 1 gives the minimum and maximum of the data", {
  vals <- c(3, 1, 4, 1, 5, 9, 2, 6)
  res  <- run_stats(x = rep(1, 8), y = vals, widths = 1.0)
  expect_equal(res$ymin, min(vals))
  expect_equal(res$ymax, max(vals))
})


# ===========================================================================
# 9. Constant y values
# ===========================================================================
test_that("constant y: median == ymin == ymax for all widths", {
  vals <- rep(7, 50)
  res  <- run_stats(x = rep(1, 50), y = vals, widths = c(0.5, 0.8, 0.95))
  expect_true(all(res$y    == 7))
  expect_true(all(res$ymin == 7))
  expect_true(all(res$ymax == 7))
})


# ===========================================================================
# 10. Output structure
# ===========================================================================
test_that("output contains the required columns", {
  res <- run_stats(x = 1:3, y = 1:3, widths = c(0.5, 0.8))
  expected_cols <- c("x", "group", "y", "ymin", "ymax", ".width", "level")
  expect_true(all(expected_cols %in% names(res)))
})

test_that(".width column matches the requested widths", {
  widths <- c(0.5, 0.8, 0.95)
  res    <- run_stats(x = rep(1, 10), y = rnorm(10), widths = widths)
  expect_setequal(unique(res$.width), widths)
})

test_that("level column is a character formatted to two decimal places", {
  res <- run_stats(x = rep(1, 5), y = 1:5, widths = c(0.5, 0.8, 0.95))
  expect_true(all(res$level %in% c("0.50", "0.80", "0.95")))
})

test_that("x and group columns are preserved correctly", {
  x   <- c(1, 1, 2, 2)
  y   <- c(10, 20, 30, 40)
  grp <- c(1L, 1L, 2L, 2L)
  res <- run_stats(x, y, grp, widths = 0.5)
  expect_setequal(res$x,     c(1, 2))
  expect_setequal(res$group, c(1L, 2L))
})


# ===========================================================================
# 11. Widths are sorted descending internally (order robustness)
# ===========================================================================
test_that("providing widths in any order gives the same results", {
  set.seed(13)
  y     <- rnorm(100)
  x     <- rep(1, 100)
  w_asc <- c(0.5, 0.8, 0.95)
  w_des <- rev(w_asc)
  w_mix <- c(0.8, 0.95, 0.5)

  res_asc <- run_stats(x, y, widths = w_asc)
  res_des <- run_stats(x, y, widths = w_des)
  res_mix <- run_stats(x, y, widths = w_mix)

  for (w in w_asc) {
    y_asc <- res_asc[res_asc$.width == w, ]$y
    y_des <- res_des[res_des$.width == w, ]$y
    y_mix <- res_mix[res_mix$.width == w, ]$y
    expect_equal(y_asc, y_des)
    expect_equal(y_asc, y_mix)
  }
})

test_that("unsorted y input to compute_ribbon_stats produces wrong quantiles", {
  # Without sorting, the C++ segment logic reads values in whatever order
  # they arrive, so quantile positions are meaningless.
  y_sorted   <- c(1, 2, 3, 4, 5)
  y_shuffled <- c(3, 5, 1, 4, 2)
  x <- rep(1, 5); g <- rep(1L, 5)

  res_sorted   <- compute_ribbon_stats(x, y_sorted,   g, 0.8)
  res_shuffled <- compute_ribbon_stats(x, y_shuffled, g, 0.8)

  # Correct answer comes from sorted input; shuffled gives something different
  expect_false(isTRUE(all.equal(res_sorted$ymin, res_shuffled$ymin)))
})

test_that("width = 0 collapses ymin and ymax to the median", {
  vals <- rnorm(100)
  res  <- run_stats(x = rep(1, 100), y = vals, widths = 0)
  expect_equal(res$ymin, res$y)
  expect_equal(res$ymax, res$y)
})

test_that("level string matches .width value in every row", {
  res <- run_stats(x = rep(1, 10), y = 1:10, widths = c(0.5, 0.8, 0.95))
  expect_true(all(res$level == sprintf("%.2f", res$.width)))
})

test_that("NA in y propagates to output without na.rm", {
  vals <- c(1, 2, NA, 4, 5)
  res  <- run_stats(x = rep(1, 5), y = vals, widths = 0.5)
  # NA poisons the sorted order; output quantiles should be NA
  expect_true(is.na(res$ymin) || is.na(res$ymax))
})
