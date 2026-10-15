#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <functional>
#include <vector>

using namespace Rcpp;

namespace {

// Type-7 quantile of a nonempty, sorted segment of finite values.
inline double get_quantile_from_index(const NumericVector& y, R_xlen_t start,
                                      R_xlen_t count, double p) {
  const double position = (count - 1) * p;
  const R_xlen_t index = static_cast<R_xlen_t>(std::floor(position));
  const double fraction = position - index;
  const double lower = y[start + index];

  if (fraction == 0.0 || index == count - 1) return lower;

  return (1.0 - fraction) * lower + fraction * y[start + index + 1];
}

inline bool ends_segment(const NumericVector& x, const IntegerVector& group,
                         R_xlen_t i, R_xlen_t n) {
  return i == n - 1 || group[i] != group[i + 1] || x[i] != x[i + 1];
}

} // namespace

// [[Rcpp::export]]
List compute_ribbon_stats(NumericVector x, NumericVector y,
                          IntegerVector group, NumericVector widths) {
  const R_xlen_t n = x.size();
  if (n == 0) return List::create();

  std::vector<double> sorted_widths(widths.begin(), widths.end());
  std::sort(sorted_widths.begin(), sorted_widths.end(), std::greater<double>());
  const R_xlen_t n_widths = widths.size();
  std::vector<double> lower_probs(n_widths);
  std::vector<double> upper_probs(n_widths);
  for (R_xlen_t w = 0; w < n_widths; ++w) {
    lower_probs[w] = (1.0 - sorted_widths[w]) / 2.0;
    upper_probs[w] = 1.0 - lower_probs[w];
  }

  std::vector<double> out_x, out_y, out_ymin, out_ymax, out_width;
  std::vector<int> out_group;

  R_xlen_t start = 0;
  R_xlen_t row = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (i % 10000 == 0) Rcpp::checkUserInterrupt();
    if (!ends_segment(x, group, i, n)) continue;

    const R_xlen_t count = i - start + 1;
    const double median = get_quantile_from_index(y, start, count, 0.5);
    if (n_widths > R_XLEN_T_MAX - row) {
      stop("Ribbon output exceeds the maximum R vector length.");
    }
    for (R_xlen_t w = 0; w < n_widths; ++w, ++row) {
      if (row % 10000 == 0) Rcpp::checkUserInterrupt();
      out_x.push_back(x[i]);
      out_group.push_back(group[i]);
      out_y.push_back(median);
      out_width.push_back(sorted_widths[w]);
      out_ymin.push_back(get_quantile_from_index(y, start, count, lower_probs[w]));
      out_ymax.push_back(get_quantile_from_index(y, start, count, upper_probs[w]));
    }
    start = i + 1;
  }

  return List::create(
    Named("x") = out_x,
    Named("group") = out_group,
    Named("y") = out_y,
    Named("ymin") = out_ymin,
    Named("ymax") = out_ymax,
    Named(".width") = out_width
  );
}
