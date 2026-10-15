

## fastribbon

This package includes a C++ implementation of [ggdist's line-ribbon plot](https://mjskay.github.io/ggdist/articles/lineribbon.html) optimized for large numbers (thousands) of high-resolution time-series trajectories.

### Installation

Install from GitHub using `remotes`:

```R
install.packages("remotes") # If needed
remotes::install_github("idd-unit/fastribbon")
```

### Usage

One function is included: `stat_fastribbon`

```R
library(tidyverse)
library(fastribbon)

df <- expand_grid(x = seq(0, 100, by = 0.1), y = rep(0, 1000)) |> 
  mutate(y = rnorm(n(), cos(x / 10) * 1, 1.5 + sin(x / 10)))

ggplot(df) +
  stat_fastribbon(aes(x = x, y = y)) +
  
  scale_fill_brewer() +
  
  theme_bw()

```
<img width="806" height="483" alt="image" src="https://github.com/user-attachments/assets/b6140ad7-ec24-417d-8f7f-becc04979a8e" />

You can also map a [distributional](https://pkg.mitchelloharawild.com/distributional/)
distribution vector to `ydist`:

```R
library(tidyverse)
library(distributional)

samples <- tibble(x = seq(0, 100, by = 0.1)) |>
  mutate(dist = dist_sample(lapply(x, function(t) {
    rnorm(5000, mean = cos(t / 10), sd = 1.5 + sin(t / 10))
  })))

ggplot(samples, aes(x = x, ydist = dist)) +
  stat_fastribbon() +
  scale_fill_brewer() +
  theme_bw()
```

Univariate numeric `dist_sample()` distributions use the C++ implementation,
while other distributions use ggdist's default lineribbon summaries.

### Simple benchmark results

Plotting `df` above is ~40x faster using `stat_fastribbon`:

```R
p_custom <- ggplot(df, aes(x, y)) + stat_fastribbon() + scale_fill_brewer()
p_base <- ggplot(df, aes(x, y)) + stat_lineribbon() + scale_fill_brewer()

bench::mark(
  custom = ggplot_build(p_custom),
  base   = ggplot_build(p_base),
  check  = FALSE, 
  iterations = 10
)

# A tibble: 2 × 13
  expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory                   time            gc               
  <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>                   <list>          <list>           
1 custom      271.1ms  301.3ms    2.01    478.87MB     5.22    10    26      4.98s <NULL> <Rprofmem [502 × 3]>     <bench_tm [10]> <tibble [10 × 3]>
2 base          10.5s    11.1s    0.0908    3.28GB     1.52    10   167      1.84m <NULL> <Rprofmem [472,356 × 3]> <bench_tm [10]> <tibble [10 × 3]>


```
