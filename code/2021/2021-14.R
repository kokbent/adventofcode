library(dplyr)
library(stringr)
library(bit64)

in_polymer <- readLines("data/2021-14.txt", n = 1)
instr <- data.table::fread("data/2021-14.txt", skip = 2, header = F)
instr <- instr %>%
  select(pair = V1, add = V3)

slidethrough <- function (polymer) {
  l <- str_length(polymer)
  purrr::map_chr(1:l, ~ str_sub(polymer, .x, .x+1))
}

out_string <- in_polymer
for (i in 1:10) {
  df <- data.frame(pair = slidethrough(out_string))
  df <- df %>%
    left_join(instr) %>%
    mutate(out = ifelse(is.na(add), pair, paste0(str_sub(pair, 1, 1), add)))
  out_string <- paste(df$out, collapse = "")
}
str_length(out_string)
out_vec <- str_split(out_string, "", simplify = T)
table(out_vec) %>%
  {max(.) - min(.)}

# Part 2
count_pairs <- function (pair, round = 5) {
  out_string <- pair
  for (i in 1:5) {
    df <- data.frame(pair = slidethrough(out_string))
    df <- df %>%
      left_join(instr, by = "pair") %>%
      mutate(out = ifelse(is.na(add), pair, paste0(str_sub(pair, 1, 1), add)))
    out_string <- paste(df$out, collapse = "")
  }
  
  df <- data.frame(pair = slidethrough(out_string))
  df <- df[-nrow(df),,drop=F] %>%
    count(pair)
  return(df)
}

pairss <- instr$pair
names(pairss) <- pairss
lookup <- purrr::map_dfr(pairss, ~ count_pairs(.x), .id = "root_pair")

ini_pairs <- slidethrough(in_polymer)
ini_pairs <- ini_pairs[-length(ini_pairs)]
df <- data.frame(root_pair = ini_pairs) %>%
  count(root_pair) %>%
  rename(root_n = n)

df1 <- df
for (i in 1:8) {
  df1 <- df1 %>% left_join(lookup) %>%
    group_by(pair) %>%
    summarise(n = as.integer64(sum(root_n * n))) %>%
    rename(root_pair = pair, root_n = n)
}

df1 <- data.table::as.data.table(df1)
char_count <- df1 %>% mutate(first = str_sub(root_pair, 1, 1)) %>%
  group_by(first) %>%
  summarise(n = sum(root_n)) %>%
  arrange(n)
char_count <- data.table::as.data.table(char_count)

format(char_count$n[10] - char_count$n[1] + 1, scientific = F)