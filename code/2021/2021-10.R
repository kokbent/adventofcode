library(dplyr)
library(stringr)

dat <- readLines("data/2021-10.txt")

rm_matched_char <- function (string) {
  str_new <- string
  str_new <- str_remove_all(string, "\\[\\]|\\<\\>|\\{\\}|\\(\\)")
  while (str_length(str_new) < str_length(string)) {
    string <- str_new
    str_new <- str_remove_all(string, "\\[\\]|\\<\\>|\\{\\}|\\(\\)")
  }
  return(string)
}

find_1st_close <- function (string) {
  l <- str_locate(string, "\\)|\\]|\\}|\\>")
  m <- str_sub(string, l[1]-1, l[1])
  return(c(m, str_sub(m, 1, 1), str_sub(m, 2, 2)))
}

clean_string <- sapply(dat, rm_matched_char, USE.NAMES = F)
res_mat <- sapply(clean_string, find_1st_close, USE.NAMES = F) %>% t
illegal_char <- res_mat[,3][!is.na(res_mat[,3])]
score <- rep(NA, length(illegal_char))
score[illegal_char == ")"] <- 3
score[illegal_char == "]"] <- 57
score[illegal_char == "}"] <- 1197
score[illegal_char == ">"] <- 25137
sum(score)

# Part 2
close_string <- function(string) {
  str_vec <- str_split(string, "", simplify = T) %>% rev
  close_vec <- rep(NA, length(str_vec))
  close_vec[str_vec == "("] <- ")"
  close_vec[str_vec == "["] <- "]"
  close_vec[str_vec == "{"] <- "}"
  close_vec[str_vec == "<"] <- ">"
  
  return(close_vec)
}

score <- function (vec) {
  score_vec <- rep(0, length(vec))
  score_vec[vec == ")"] <- 1
  score_vec[vec == "]"] <- 2
  score_vec[vec == "}"] <- 3
  score_vec[vec == ">"] <- 4
  multiplier <- 5^((length(vec)-1):0)
  sc <- (t(score_vec) %*% (multiplier)) %>% as.vector
  return(sc)
}

incomp_string <- clean_string[is.na(res_mat[,1])]
scores <- sapply(incomp_string, function (x) close_string(x) %>% score,
                 USE.NAMES = F)
median(scores)
