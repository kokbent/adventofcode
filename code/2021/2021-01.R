# Day 1 of 2021

library(dplyr)

df <- data.table::fread("data/2021-01.txt")

# Part 1
d <- diff(df$V1)
sum(d > 0)

# Part 2
r <- stats::filter(df$V1, rep(1, 3))
dr <- diff(r)
sum(dr > 0, na.rm=T)
