library(dplyr)
library(stringr)
library(purrr)

# Read boards
input <- readLines("data/2021-04.txt")
ln <- length(input)

board_start <- seq(3, ln-4, by = 6)
boards <- list()
for (i in 1:length(board_start)) {
  s <- str_split(trimws(input[board_start[i]:(board_start[i]+4)]), 
                 "[ ]+", simplify = T)
  mat <- as.numeric(s) %>% matrix(nrow = 5)
  boards[[i]] <- mat
}

# Read chosen nums
chosen <- str_split(input[1], ",", simplify = T) %>%
  as.numeric()

# Start matching
bingo <- function (mat) {
  cond1 <- any(colSums(mat) == 5)
  cond2 <- any(rowSums(mat) == 5)
  cond <- cond1 | cond2
  return(any(cond))
}

progs <- boards %>%
  map(~ .x == -1)

pos <- 1
bingo_stat <- progs %>%
  map_lgl(~ bingo(.x))
while (!any(bingo_stat)) {
  progs1 <- boards %>%
    map(~ .x == chosen[pos])
  progs <- map2(progs, progs1, ~ .x | .y)
  bingo_stat <- progs %>%
    map_lgl(~ bingo(.x))
  pos = pos + 1
}

bingo_board <- boards[[which(bingo_stat)]]
bingo_progs <- progs[[which(bingo_stat)]]
unmarked_sum <- sum(bingo_board[!bingo_progs])
last_called <- chosen[pos - 1]

unmarked_sum * last_called

# Part 2
progs <- boards %>%
  map(~ .x == -1)

pos <- 1
bingo_stat <- progs %>%
  map_lgl(~ bingo(.x))
while (sum(bingo_stat) < length(bingo_stat)) {
  if (sum(bingo_stat) == length(bingo_stat) - 1) {
    last_ind <- which(!bingo_stat)
  }
  progs1 <- boards %>%
    map(~ .x == chosen[pos])
  progs <- map2(progs, progs1, ~ .x | .y)
  bingo_stat <- progs %>%
    map_lgl(~ bingo(.x))
  pos = pos + 1
}

last_board <- boards[[last_ind]]
last_progs <- progs[[last_ind]]
unmarked_sum <- sum(last_board[!last_progs])
last_called <- chosen[pos - 1]

unmarked_sum * last_called
