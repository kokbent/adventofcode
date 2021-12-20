library(dplyr)
library(stringr)

dat <- readLines("data/2021-09.txt")
dat <- str_split(dat, "", simplify = T)
dat <- apply(dat, 2, as.numeric)
dat[,100]

eval_minimum <- function (vec) {
  d1 <- c(-1, diff(vec))
  d2 <- diff(rev(vec)) %>% rev %>% c(-1)
  cond <- d1 < 0 & d2 < 0
  return(cond)
}

rows <- apply(dat, 1, eval_minimum) %>% t
cols <- apply(dat, 2, eval_minimum)
conds <- rows & cols
sum(dat[conds] + 1)

# Part 2
find_eligible_neighbour <- function (xyz) {
  x <- xyz[1]
  y <- xyz[2]
  z <- xyz[3]
  
  mat <- cbind(c(x, x+1, x, x-1), c(y+1, y, y-1, y))
  cond1 <- mat[,1] > 0 & mat[,2] > 0
  cond2 <- mat[,1] <= 100 & mat[,2] <= 100
  mat <- mat[cond1&cond2,,drop=F]
  
  # extract value
  mat <- cbind(mat, apply(mat, 1, function (xx) dat[xx[2], xx[1]]))
  cond3 <- mat[,3] != 9 & mat[,3] > z
  
  mat <- mat[cond3,,drop=F]
  return(mat)
}

find_match_point <- function (m1, m2) {
  apply(m1, 1, function (x) any(x[1] == m2[,1] & x[2] == m2[,2]))
}

rr <- row(conds)[conds]
cc <- col(conds)[conds]
val <- dat[conds]
sizes <- rep(NA, sum(conds))
for (i in 1:sum(conds)) {
  basin <- matrix(c(cc[i], rr[i], val[i]), nrow = 1)
  p <- 1
  
  while (p <= nrow(basin)) {
    basin_cand <- find_eligible_neighbour(basin[p,,drop=F])
    cond <- find_match_point(basin_cand, basin)
    basin_cand <- basin_cand[!cond,,drop=F]
    basin <- rbind(basin, basin_cand)
    p <- p + 1
  }
  
  sizes[i] <- nrow(basin)
}

s <- sizes %>% sort(decreasing = T)
prod(s[1:3])
