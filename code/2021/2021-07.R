library(dplyr)
library(stringr)

dat <- readLines("data/2021-07.txt")
dat_n <- str_split(dat, ",")[[1]] %>%
  as.numeric()
dat_n

# Optimal solution for when cost function of absolute error is median
opt_pos <- median(dat_n)
abs(dat_n - opt_pos) %>% sum

# Part 2
opt_func <- function (pos) {
  steps <- abs(dat_n - pos)
  fuel <- steps * (steps + 1) / 2
  return(sum(fuel))
}

opt_pos <- optimize(opt_func, interval = range(dat_n))$minimum
opt_pos <- round(opt_pos)
opt_func(opt_pos)
