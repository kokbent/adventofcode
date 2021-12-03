library(dplyr)

df <- data.table::fread("data/2021-02.txt")
colnames(df) <- c("dir", "step")

df1 <- df %>%
  group_by(dir) %>%
  summarise(step = sum(step))
df1
d <- df1$step[df1$dir == "down"] - df1$step[df1$dir == "up"] # depth
h <- df1$step[df1$dir == "forward"] # horizontal
d*h

### Part 2
df2 <- df %>%
  mutate(aim = case_when(
    dir == "forward" ~ 0.0,
    dir == "up" ~ -step * 1.0,
    dir == "down" ~ step * 1.0
  ), aim_pos = cumsum(aim))

df3 <- df2 %>%
  filter(dir == "forward")
new_depth <- df3$step * df3$aim_pos

sum(new_depth) * h
