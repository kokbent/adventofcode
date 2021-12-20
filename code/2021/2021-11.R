library(dplyr)
library(stringr)

nr <- 10
nc <- 10
dat <- readLines("data/2021-11.txt")
dat1 <- dat %>%
  str_split("", simplify = T) %>%
  apply(2, as.numeric)

flash <- function(mat) {
  mat <- mat + 1
  while (sum(mat >= 10, na.rm = T)) {
    ind <- which(mat >= 10)[1]
    y <- row(mat)[ind]
    x <- col(mat)[ind]
    
    neighbours <- expand.grid(x = (x-1):(x+1), y = (y-1):(y+1))[-5,] %>%
      as.matrix
    cond1 <- neighbours[,1] > 0 & neighbours[,1] <= nc
    cond2 <- neighbours[,2] > 0 & neighbours[,2] <= nr
    neighbours <- neighbours[cond1 & cond2,,drop=F]
    nb_ind <- (neighbours[,1] - 1) * nr + neighbours[,2]
    mat[ind] <- NA
    mat[nb_ind] <- mat[nb_ind] + 1
  }
  
  nflash <- sum(is.na(mat))
  mat[is.na(mat)] <- 0
  return(list(nflash, mat))
}

mat <- dat1
nflash <- 0
while (i in 1:100) {
  out <- flash(mat)
  nflash <- nflash + out[[1]]
  mat <- out[[2]]
}
nflash

# Part 2
mat <- dat1
nflash <- 0
i <- 0
while (sum(mat) != 0) {
  i <- i + 1
  out <- flash(mat)
  nflash <- nflash + out[[1]]
  mat <- out[[2]]
}
i
