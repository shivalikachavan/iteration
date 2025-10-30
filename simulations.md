Simulations
================
Shivalika Chavan
2025-10-30

``` r
library(tidyverse)
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.1.0     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
source("source/sim_mean_sd.R")

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d


set.seed(1)
```

``` r
sim_mean_sd(n_subj = 40)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.18      1.77

Running a simulation. “Verifying” CLT using a for loop

``` r
output = vector("list", length = 100)

for (i in 1:100) {
  
  output[[i]] = sim_mean_sd(n_subj = 30)
  
}

sim_results = bind_rows(output)

sim_results |> 
  ggplot(aes(x = mu_hat)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="simulations_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Using map statement

``` r
sim_results_df = 
  expand_grid(
    sample_size = 30,
    iter = 1:100
  ) |> 
  mutate(
    results = map(sample_size, sim_mean_sd)
  ) |> 
  unnest(results)

sim_results_df |> 
  ggplot(aes(x = mu_hat)) + 
  geom_density()
```

<img src="simulations_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

``` r
sim_results_df = 
  expand_grid(
    sample_size = c(30, 60, 90, 120),
    iter = 1:1000
  ) |> 
  mutate(
    results = map(sample_size, sim_mean_sd)
  ) |> 
  unnest(results)
```

Looking at the results

``` r
sim_results_df |> 
  mutate(
    samp = str_c("n = ", sample_size),
    samp = fct_inorder(samp)) |>
  ggplot(aes(x = sample_size, y = mu_hat, fill = samp)) + 
  geom_violin()
```

<img src="simulations_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

Summarizing

``` r
sim_results_df |> 
  group_by(sample_size) |> 
  summarize(
    empirical_mean = mean(mu_hat),
    empirical_se = sd(mu_hat)
  )
```

    ## # A tibble: 4 × 3
    ##   sample_size empirical_mean empirical_se
    ##         <dbl>          <dbl>        <dbl>
    ## 1          30           3.00        0.358
    ## 2          60           2.99        0.255
    ## 3          90           3.00        0.207
    ## 4         120           3.00        0.188
