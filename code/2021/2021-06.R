library(dplyr)
library(stringr)
library(expm)

dat <- readLines("data/2021-06.txt")
dat_n <- str_split(dat, ",")[[1]] %>%
  as.numeric()

# Initial count by day
counts <- table(dat_n) %>% as.data.frame(stringAsFactor = F)
counts$dat_n <- as.numeric(counts$dat_n)

ini_vec <- rep(0, 9)
names(ini_vec) <- 0:8
ini_vec[counts$dat_n+1] <- counts$Freq

# Define transition matrix
trans_mat <- matrix(0, nrow = 9, ncol = 9)
diag(trans_mat) <- 0
trans_mat[row(trans_mat) - 1 == col(trans_mat)] <- 1
trans_mat[1,9] <- 1
trans_mat[1,7] <- 1

# Use expm %^% to calculate power of matrix
n_80 <- t(ini_vec) %*% (trans_mat %^% 80)
sum(n_80)

# Part 2
n_256 <- t(ini_vec) %*% (trans_mat %^% 256)
sum(n_256) %>%
  format(scientific = F)
