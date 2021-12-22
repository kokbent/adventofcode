library(igraph)
library(stringr)
library(dplyr)
library(tidyr)

dat <- readLines("data/2021-15.txt")
mat <- str_split(dat, "", simplify = T) %>%
  apply(2, as.numeric)

is_neighbour <- function (bl1, bl2, mat) {
  cond1 <- row(mat)[bl1] == row(mat)[bl2] & abs(col(mat)[bl1] - col(mat)[bl2]) == 1
  cond2 <- col(mat)[bl1] == col(mat)[bl2] & abs(row(mat)[bl1] - row(mat)[bl2]) == 1
  return(cond1 | cond2)
}

G <- expand.grid(fr = 1:10000, to = 1:10000) %>%
  filter(fr != to) %>%
  filter(is_neighbour(fr, to, mat)) %>%
  mutate(z = mat[to]) %>%
  arrange(fr)
graph <- graph_from_data_frame(G[,1:2])
E(graph)$weight <- G$z
distances(graph, "1", "10000", mode = "out",
          algorithm = "dijkstra")
shortest_paths(graph, "1", "100", mode = "out")

# Part 2 (need better way to find neighbours)
mat1 <- cbind(mat, mat+1, mat+2, mat+3, mat+4)
bigmat <- rbind(mat1, mat1+1, mat1+2, mat1+3, mat1+4)
bigmat <- ifelse(bigmat > 9, bigmat - 9, bigmat)

nr <- nrow(bigmat)
neighbour_giv_block <- function (bl) {
  neighbours <- c(bl - 1, bl + 1, bl - nr, bl + nr)
  neighbours <- neighbours[neighbours > 0 & neighbours <= nr^2]
  if(bl %% nr == 0) neighbours <- neighbours[neighbours != bl + 1]
  if(bl %% nr == 1) neighbours <- neighbours[neighbours != bl - 1]
  return(list(neighbours))
}

G <- data.frame(fr = 1:nr^2)
G$to <- sapply(G$fr, neighbour_giv_block)
G <- G %>%
  unnest(to)
G <- G %>%
  mutate(z = bigmat[to])

graph <- graph_from_data_frame(G[,1:2])
E(graph)$weight <- G$z
distances(graph, "1", as.character(nr^2), mode = "out",
          algorithm = "automatic")
