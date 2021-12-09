library(dplyr)
library(stringr)

dat <- data.table::fread("data/2021-08.txt", sep = " ",
                         header = F)
dat <- dat[,-11]
output <- as.matrix(dat[,11:14])
(str_length(output) %in% c(2, 3, 4, 7)) %>% sum

# analyze 10 strings
sort_str <- function (string) {
  string %>%
    str_split("") %>%
    lapply(str_sort) %>%
    sapply(function (x) paste(x, collapse=""))
}

analyze_10str <- function (strings) {
  mapper <- rep("", 10)
  names(mapper) <- 0:9
  
  strings <- sort_str(strings)
  str_len <- str_length(strings)
  mapper["1"] <- strings[str_len == 2]
  mapper["7"] <- strings[str_len == 3]
  mapper["4"] <- strings[str_len == 4]
  mapper["8"] <- strings[str_len == 7]
  
  str6 <- strings[str_len == 6]
  str6_mat <- str_split(str6, "", simplify = T)
  is6 <- apply(str6_mat, 1, 
                function (x) !all(str_split(mapper["1"], "")[[1]] %in% x))
  is0 <- apply(str6_mat, 1, 
               function (x) !all(str_split(mapper["4"], "")[[1]] %in% x))
  is0 <- is0 & !is6
  
  mapper["0"] <- str6[is0]
  mapper["6"] <- str6[is6]
  mapper["9"] <- str6[!is0 & !is6]
  
  str5 <- strings[str_len == 5]
  str5_mat <- str_split(str5, "", simplify = T)
  is5 <- apply(str5_mat, 1, 
               function (x) all(x %in% str_split(mapper["6"], "")[[1]]))
  is3 <- apply(str5_mat, 1, 
               function (x) all(str_split(mapper["7"], "")[[1]] %in% x))
  mapper["5"] <- str5[is5]
  mapper["3"] <- str5[is3]
  mapper["2"] <- str5[!is5 & !is3]
  
  return(mapper)
}

strings_mat <- as.matrix(dat[,1:10])
mappers_mat <- apply(strings_mat, 1, analyze_10str) %>% t

output <- t(apply(output, 1, sort_str))

n <- rep(0, nrow(output))
for (i in 1:nrow(output)) {
  ind <- sapply(output[i,], function (x) which(x == mappers_mat[i,]))
  n[i] <- paste(colnames(mappers_mat)[ind], collapse = "") %>%
    as.numeric()
}

sum(n)
