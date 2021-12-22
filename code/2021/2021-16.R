library(dplyr)
library(stringr)

dat <- readLines("data/2021-16.txt")

#### Functions dump ----
strtoi2 <- function (string) {
  vec <- str_split(string, "", simplify = T) %>% as.integer
  sum(vec * 2^((length(vec):1)-1))
}

read_version <- function (string) {
  v_str <- str_sub(string, 1, 3)
  str_sub(string, 1, 3) <- ""
  vers <- strtoi(v_str, 2)
  return(list(vers, string))
}

read_type <- function (string) {
  t_str <- str_sub(string, 1, 3)
  str_sub(string, 1, 3) <- ""
  type <- strtoi(t_str, 2)
  return(list(type, string))
}

read_op_type <- function (string) {
  optype <- str_sub(string, 1, 1) %>% as.numeric
  str_sub(string, 1, 1) <- ""
  return(list(optype, string))
}

read_literal <- function (string) {
  end_signal <- FALSE
  val_str <- ""
  while (!end_signal) {
    b5 <- str_sub(string, 1, 5)
    str_sub(string, 1, 5) <- ""
    if (str_starts(b5, "0")) end_signal <- T
    b4 <- str_sub(b5, 2, 5)
    val_str <- paste0(val_str, b4)
  }
  print(strtoi2(val_str))
  return(list(lit = strtoi2(val_str), leftover = string))
}

read_length <- function (string, optype) {
  if (optype == 0) {
    len <- str_sub(string, 1, 15) %>%
      strtoi(2)
    str_sub(string, 1, 15) <- ""
    return(list(len, string))
  } else {
    len <- str_sub(string, 1, 11) %>%
      strtoi(2)
    str_sub(string, 1, 11) <- ""
    return(list(len, string))
  }
}

read_op0 <- function (string) {
  tmp <- read_length(string, 0)
  len <- tmp[[1]]
  out_str <- tmp[[2]]
  
  list_subpacket <- list()
  while (len > 0) {
    og_len <- str_length(out_str)
    out <- crawl_string(out_str)
    list_subpacket[[length(list_subpacket)+1]] <- out
    new_len <- str_length(out$leftover)
    len <- len - og_len + new_len
    out_str <- out$leftover
  }
  
  return(list(lsp = list_subpacket, leftover = out_str))
}

read_op1 <- function (string) {
  tmp <- read_length(string, 1)
  n <- n_subpack <- tmp[[1]]
  out_str <- tmp[[2]]
  
  list_subpacket <- list()
  while (n > 0) {
    out <- crawl_string(out_str)
    list_subpacket[[length(list_subpacket)+1]] <- out
    n <- n - 1
    out_str <- out$leftover
  }
  
  return(list(lsp = list_subpacket, leftover = out_str))
}

crawl_string <- function (string) {
  tmp <- read_version(string)
  vers <- tmp[[1]]
  tmp <- read_type(tmp[[2]])
  type <- tmp[[1]]
  
  if (type == 4) {
    lit <- read_literal(tmp[[2]])
    return(list(vers = vers, type = type, 
                lit = lit$lit, leftover = lit$leftover))
  } else {
    tmp <- read_op_type(tmp[[2]])
    optype <- tmp[[1]]
    if (optype == 0) {
      lsp <- read_op0(tmp[[2]])
      return(list(vers = vers, type = type, optype = optype, 
                  subpacket = lsp$lsp, leftover = lsp$leftover))
    } else {
      lsp <- read_op1(tmp[[2]])
      return(list(vers = vers, type = type, optype = optype, 
                  subpacket = lsp$lsp, leftover = lsp$leftover))
    }
  }
}

resolve_op <- function (L) {
  # Check members of subpacket, if contains non-lit
  # recursive call for resolving op
  for (i in 1:length(L$subpacket)) {
    if (!is.null(L$subpacket[[i]]$optype)) {
      L$subpacket[[i]] <- resolve_op(L$subpacket[[i]])
    }
  }
  
  newL <- list()
  lit_vec <- purrr::map_dbl(L$subpacket, "lit")
  if (L$type == 0) newL$lit <- sum(lit_vec)
  if (L$type == 1) newL$lit <- prod(lit_vec)
  if (L$type == 2) newL$lit <- min(lit_vec)
  if (L$type == 3) newL$lit <- max(lit_vec)
  if (L$type == 5) newL$lit <- as.numeric(lit_vec[1] > lit_vec[2])
  if (L$type == 6) newL$lit <- as.numeric(lit_vec[1] < lit_vec[2])
  if (L$type == 7) newL$lit <- as.numeric(lit_vec[1] == lit_vec[2])
  
  return(newL)
}

#### Solve ----
# HEX -> BIN
dat <- str_split(dat, "", simplify = T)
dat_int <- strtoi(dat, base = 16)
dat_bin <- R.utils::intToBin(dat_int) %>%
  str_pad(4, side = "left", pad = "0")
str_bin <- paste(dat_bin, collapse = "")

out <- crawl_string(str_bin)

# Cheat way of grabbing all vers
jsonlite::write_json(out, "tmp.json")
x <- readLines("tmp.json")
str_match_all(x, '\\"vers\\":\\[(\\d)]')[[1]][,2] %>%
  as.integer() %>%
  sum()

# Part 2
resolve_op(out)
