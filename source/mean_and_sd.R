library(tidyverse)


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