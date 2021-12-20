library(dplyr)
library(stringr)

df <- data.table::fread("data/2021-12.txt", sep = "-", header = F)
colnames(df) <- c("left", "right")

mat <- as.matrix(df)
cond <- mat[,"right"] == "start"
mat[,"right"][cond] <- mat[,"left"][cond]
mat[,"left"][cond] <- "start"
cond <- mat[,"left"] == "end"
mat[,"left"][cond] <- mat[,"right"][cond]
mat[,"right"][cond] <- "end"

ini_mat <- mat[mat[,"left"] == "start",]
mat <- mat[!mat[,"left"] == "start",]
mat_flip <- mat[!mat[,"right"] == "end",]
tmp <- mat_flip[,"left"]
mat_flip[,"left"] <- mat_flip[,"right"]
mat_flip[,"right"] <- tmp
mat <- rbind(mat, mat_flip)

check_valid <- function (string) {
  cond <- str_split(string, "-") %>%
    map(~ .x[str_detect(.x, "^[a-z]{2}$")]) %>%
    map(~ table(.x)) %>%
    map_lgl(~ any(.x > 1))
  return(!cond)
}

net_df <- as.data.frame(mat)
df_i <- as.data.frame(ini_mat) %>%
  mutate(string = paste(left, right, sep = "-"),
         connect = right) %>%
  select(-left, -right)

while (!all(is.na(df_i$connect))) {
  df_i <- df_i %>%
    left_join(net_df, by = c("connect" = "left"))
  df_i <- df_i %>%
    mutate(string = ifelse(is.na(right), string, paste(string, right, sep="-")),
           connect = right) %>%
    filter(check_valid(string)) %>%
    select(-right)
}
nrow(df_i)

# Part 2
check_valid <- function (string) {
  cond <- str_split(string, "-") %>%
    map(~ .x[str_detect(.x, "^[a-z]{2}$")]) %>%
    map(~ table(.x)) %>%
    map_lgl(~ any(.x > 2) | (sum(.x > 1) > 1))
  return(!cond)
}

df_i <- as.data.frame(ini_mat) %>%
  mutate(string = paste(left, right, sep = "-"),
         connect = right) %>%
  select(-left, -right)

while (!all(is.na(df_i$connect))) {
  df_i <- df_i %>%
    left_join(net_df, by = c("connect" = "left"))
  df_i <- df_i %>%
    mutate(string = ifelse(is.na(right), string, paste(string, right, sep="-")),
           connect = right) %>%
    filter(check_valid(string)) %>%
    select(-right)
}
nrow(df_i)
