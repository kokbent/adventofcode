library(dplyr)
library(stringr)

input <- readLines("data/2021-05.txt")
coords <- str_match(input, "(.*),(.*) -> (.*),(.*)")[,2:5]
coords <- apply(coords, 2, as.numeric)
colnames(coords) <- c("x1", "y1", "x2", "y2")
df <- as.data.frame(coords)

# vertical or horizontal lines
df1 <- df %>%
  filter(x1 == x2 | y1 == y2) %>%
  mutate(lty = ifelse(x1 == x2, 'vertical', 'horizontal'))
apply(df1, 2, range)

out_df <- data.frame()
for (i in 1:nrow(df1)) {
  if (df1$lty[i] == "vertical") {
    ddf <- data.frame(x = df1$x1[i], y = df1$y1[i]:df1$y2[i])
  } else {
    ddf <- data.frame(x = df1$x1[i]:df1$x2[i], y = df1$y1[i])
  }
  
  out_df <- bind_rows(out_df, ddf)
  rm(ddf)
}

out_df1 <- out_df %>% 
  count(x, y)
sum(out_df1$n >= 2)

# Part 2
df2 <- df %>%
  mutate(lty = case_when(
    x1 == x2 ~ "vertical",
    y1 == y2 ~ "horizontal",
    abs(x1 - x2) == abs(y1 - y2) ~ "diagonal",
    T ~ "other"
  ))
table(df2$lty)

out_df <- data.frame()
for (i in 1:nrow(df2)) {
  if (df2$lty[i] == "vertical") {
    ddf <- data.frame(x = df2$x1[i], y = df2$y1[i]:df2$y2[i])
  } else if (df2$lty[i] == "horizontal") {
    ddf <- data.frame(x = df2$x1[i]:df2$x2[i], y = df2$y1[i])
  } else {
    ddf <- data.frame(x = df2$x1[i]:df2$x2[i], y = df2$y1[i]:df2$y2[i])
  }
  
  out_df <- bind_rows(out_df, ddf)
  rm(ddf)
}

out_df1 <- out_df %>% 
  count(x, y)
sum(out_df1$n >= 2)
