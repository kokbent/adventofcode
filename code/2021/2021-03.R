library(dplyr)
library(stringr)
report <- readLines("data/2021-03.txt")
mat <- str_split(report, "", simplify = T)
mat1 <- apply(mat, 2, as.numeric)
mat1

gamm1 <- as.numeric(colSums(mat1) > 500) %>% paste(collapse = "")
gamm <- strtoi(gamm1, base = 2)

eps <- 2^(ncol(mat1)) - 1 - gamm

gamm * eps

#### Part 2
prog_filter <- function (mat, majority = T) {
  pos <- 1
  while (nrow(mat) > 1) {
    if (majority) {
      bit <- sum(mat[,pos]) >= nrow(mat)/2
    } else {
      bit <- sum(mat[,pos]) < nrow(mat)/2
    }
    
    cond <- mat[,pos] == as.numeric(bit)
    mat <- mat[cond,,drop=F]
    pos = pos + 1
  }
  
  return(mat)
}

oxy <- prog_filter(mat1)
oxy <- oxy %>%
  as.vector() %>%
  paste(collapse = "")
oxy <- strtoi(oxy, 2)

co2 <- prog_filter(mat1, F)
co2 <- co2 %>%
  as.vector() %>%
  paste(collapse = "")
co2 <- strtoi(co2, 2)

oxy*co2
