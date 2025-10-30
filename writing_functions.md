Writing Functions
================
Shivalika Chavan
2025-10-23

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
library(rvest)
## 
## Attaching package: 'rvest'
## 
## The following object is masked from 'package:readr':
## 
##     guess_encoding
library(readxl)

set.seed(1)
```

Z scores

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
##  [1] -0.894579096 -0.007534108 -1.123622566  1.538189109  0.152185414
##  [6] -1.107022329  0.325106999  0.599834216  0.421851526 -0.543016965
## [11]  1.446758183  0.218251901 -0.888870682 -2.633686248  1.023162590
## [16] -0.257822640 -0.226349080  0.824866429  0.690604708  0.441692638
```

Write a function to compute z score

``` r
z_scores = function(x){
  if(!is.numeric(x)){
    stop("The input of x should be numeric")
  }
  if(length(x)<5){
    stop("Only compute z scores when input has 5 or more numbers")
  }
  z = (x - mean(x)) / sd(x)
  z
}
```

Trying function

``` r
z_scores(x = x_vec)
##  [1] -0.894579096 -0.007534108 -1.123622566  1.538189109  0.152185414
##  [6] -1.107022329  0.325106999  0.599834216  0.421851526 -0.543016965
## [11]  1.446758183  0.218251901 -0.888870682 -2.633686248  1.023162590
## [16] -0.257822640 -0.226349080  0.824866429  0.690604708  0.441692638

num_vec = rnorm(123, mean = 14, sd = 0.4)
z_scores(x = num_vec)
##   [1]  1.01992219  0.86446496  0.06063410 -2.28406230  0.68007333 -0.08783948
##   [7] -0.20106513 -1.69491160 -0.56727349  0.45072438  1.51944241 -0.14084606
##  [13]  0.41633644 -0.08519966 -1.58847260 -0.49552619 -0.47200488 -0.09145738
##  [19]  1.22560036  0.84292498 -0.21098061 -0.31190442  0.76770493  0.60831801
##  [25] -0.80653038 -0.82781920  0.39010562  0.84901095 -0.15170490  0.97690068
##  [31]  0.42819021 -0.71936266  0.36345149 -1.30707897  1.60390050  2.22574295
##  [37] -0.44125395 -1.21025584  0.62315066 -0.17750259  2.70426421 -0.06865316
##  [43]  0.75949814  0.00773677 -0.86846458  0.19040115 -2.07458399  1.64085727
##  [49]  0.15002739  2.44410363  0.51612402 -0.83060395  0.66973603 -1.08524917
##  [55] -1.44825528  0.30702035 -0.52767313 -0.02281914  0.06038002 -0.69379552
##  [61] -0.67010653 -0.17764347  1.31428165 -1.75491101  0.65067306  0.35417080
##  [67]  1.18365139 -0.36964063  0.39628211  0.27936063 -0.64040050  1.34811391
##  [73]  1.29419145  0.77139738  1.77863487  0.61038927 -1.47433746 -0.67532855
##  [79] -1.41528645 -0.56187795 -0.72883757  0.02377054 -1.05892028  0.15545249
##  [85] -0.76771063  1.98363801  0.79013506  1.00992145  0.41237591  1.88694811
##  [91] -0.74629829 -0.54852275  1.60305817 -0.76329337 -0.25966814 -0.47032124
##  [97] -0.38760026 -0.34115935  0.53734392 -0.22552977 -0.59886388  1.50167389
## [103] -0.26784612 -0.22805865 -0.13789577  0.78554412 -0.10764713 -0.06682887
## [109] -0.79846992 -0.39245957  0.04426993 -0.69308384  0.57972723 -1.74903458
## [115]  0.32418780 -1.76954667 -0.36599644 -0.62422312 -0.76488204 -0.08871200
## [121] -2.19886791  1.31257340 -1.91555378
```

Breaking the function

``` r
z_scores(3)
## Error in z_scores(3): Only compute z scores when input has 5 or more numbers
z_scores("My name is Shivalika")
## Error in z_scores("My name is Shivalika"): The input of x should be numeric
```

Computing some stuff Return mean and sd of a numeric vector

``` r
mean_and_sd = function(x){
  
  if(!is.numeric(x)){
    stop("The input of x should be numeric")
  }
  
  if(length(x) < 5){
    stop("Only compute z scores when input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  # c(mean_x, sd_x)
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}

mean_and_sd(x_vec)
## # A tibble: 1 × 2
##    mean    sd
##   <dbl> <dbl>
## 1  10.7  3.20
```

Making up data (simulations)

``` r
sim_df = 
  tibble(
    x = rnorm(n = 30, mean = 3, sd = 2)
  )


sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.95      2.26

``` r
source("./source/sim_mean_sd.R")
sim_mean_sd(mu = 48, 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   48.2      2.07

Import LoTR data

``` r
fellowship_ring = 
  read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(
    movie = "Fellowship of the Ring"
  )

two_towers = 
  read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(
    movie = "Two Towers"
  )

return_of_the_king = 
  read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(
    movie = "Return of the King"
  )

lotr_df = bind_rows(fellowship_ring, two_towers, return_of_the_king)
```

Same thing but a function

``` r
import_lotr = function(filepath = "./data/LotR_Words.xlsx", xlsx_range, title){
  movie_df = 
  read_excel(filepath, range = xlsx_range) |> 
  mutate(
    movie = title
  )
}
fellowship_ring = import_lotr(xlsx_range = "B3:D6", title = "Fellowship of the Ring")
two_towers = import_lotr(xlsx_range = "F3:H6", title = "Two Towers")
return_of_the_king = import_lotr(xlsx_range = "J3:L6", title = "Return of the King")

lotr_df = bind_rows(fellowship_ring, two_towers, return_of_the_king)
```

Last example

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

``` r
nsduh_table <- function(html, table_num, table_name) {
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}

nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, table_num = 1, "marj_one_year"),
    nsduh_table(nsduh_html, table_num = 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, table_num = 5, "heroin_one_year")
  )
```
