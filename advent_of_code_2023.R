# advent of code 2023


# Day 1: Trebuchet?! ------------------------------------------------------

word_numbers <- setNames(object = seq(1,9,1), 
                         nm = c('one', 'two', 'three', 'four',
                                'five', 'six', 'seven', 'eight', 'nine'))

#' Find calibration value
#'
#' @param data data read with readLines
#'
#' @return numeric vector
#' @export
#'
calibration_value <- function(data) {
  
  sum(
    sapply(
      X = data,
      FUN = function(x) {
        s <- unlist(strsplit(x[1], split = ""))
        numbers <-  which(!s %in% letters)
        first <- head(s[numbers], 1)
        last <- tail(s[numbers], 1)
        as.numeric(paste0(first, last))
        }
      ) 
    )
}

# test
readLines(con = "~/Projects/advent_of_code_2023/day1-sample.txt") %>%
  calibration_value()

# solution
readLines(con = "~/Projects/advent_of_code_2023/day1-task.txt") %>%
  calibration_value()


# Day 1: Trebuchet?! - part 2 ---------------------------------------------

#' Calculate sum of calibrated value
#'
#' @param data data read with readLines
#'
#' @return numeric 
#' @export
#'
calibration_value_all <- function(data) {
  
  pattern <- '(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))'
  
  data %>%
    stringr::str_match_all(pattern = pattern) %>%
    purrr::map(\(x) x[,2]) %>%
    lapply(., FUN = function(x) ifelse(nchar(x) >= 3, word_numbers[x], x)) %>%
    lapply(., FUN = function(x) as.numeric(paste0(head(x, n =1), tail(x, n =1)))) %>% 
    unlist() %>% 
    sum()
}

# test
readLines("~/Projects/advent_of_code_2023/day1-sample-2.txt") %>%
  calibration_value_all()

# solution
readLines(con = "~/Projects/advent_of_code_2023/day1-task.txt") %>%
  calibration_value_all()



# Day 2: Cube Conundrum ---------------------------------------------------

# The Elf would first like to know which games would have been possible 
# if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

# 3 red, 5 green, 4 blue

day2_sample <- readLines("~/Projects/advent_of_code_2023/data/day2-sample.txt")

#' Get number of sets in each game
#'
#' @param data 
#' @param return_n_sets to return split object or number of sets in each game
#'
#' @return either vector or list
#'
count_sets <- function(data, return_n_sets = TRUE) {
  
  tmp <-
    unname(
      sapply(data, 
             function(x) strsplit(gsub(pattern = ".*: ",  replacement = "", x = x), 
                                  split = ";")
                    
                  )
           )
  
  if(return_n_sets) sapply(tmp, length) else tmp
}

#' Compose matrix
#'
#' @param data input data loaded by readLines()
#' @param set set to choose
#'
#' @return matrix
#' 
compose_matrix <- function(data, set) {
  
  # extract games and sets
  t <- count_sets(data, return_n_sets = FALSE)
  
  # split elements
  t2 <- sapply(t, function(x) strsplit(x[set], split= ","))
  t2 <- sapply(t2, function(x) if(isTRUE(is.na(x))) { c("0 blue", "0 green", "0 red")} else x)
  
  # order elements
  colors <- c("blue", "red", "green")
  
  t2 <- lapply(t2, function(x) {
    
    if(length(x) < 3) {
      
      missing_value <-
        paste0("0 ", colors[! colors %in% gsub(pattern = '[^a-z]', replacement = "", x = x)])
    
      x <- append(x, missing_value)
      
    }
    
    tmp_order <- order(gsub(pattern = '[^a-z]', replacement = "", x = x))
    x <- x[tmp_order]
    x <- as.numeric(gsub(pattern = '[^0-9]', replacement = "", x = x))
    })
  
  matrix(unlist(t2), 
         ncol = 3, 
         byrow = TRUE, 
         dimnames = list(c(), c("blue", "green", "red"))
         )[1:length(data), ]
}


#' Sum number of valid games
#'
#' @param input_data input data loaded by readLines()
#' @param blue n blue observations
#' @param green n green observations 
#' @param red n red observations
#'
#' @returns list
#' 
sum_games <- function(input_data, blue, green, red) {
  
  n_sets_per_game <- count_sets(data = input_data, return_n_sets = T)
  #n_sets <- 1:max(n_sets_per_game)
  
  n_games <- 1:length(input_data)
  
  sum_matrix <- matrix(0,
                       nrow = 1, 
                       ncol = length(input_data), 
                       dimnames = list("sums", c() )
                       )
  
  for(game in seq_along(n_games)) {
    
    n_sets <- 1:max(n_sets_per_game[game])
    
    for(set in seq_along(n_sets)) {
      
      tmp <- t(compose_matrix(data = input_data, set = n_sets[set]))[,game]
      sum_matrix[,game] <- ifelse(all(tmp <= c(blue, green, red)), 
                                  sum_matrix[,game] + 1,
                                  sum_matrix[,game] +0)
      
    }
    
  }
  
  out <- list(mtx = sum_matrix, sum = sum(which(sum_matrix == n_sets_per_game)))
  return(out)
}


# 14 blue cubes, 13 green cubes, and 12 red cubes

# sample
day2_sample <- readLines("~/Projects/advent_of_code_2023/data/day2-sample.txt")
sum_games(input_data = day2_sample, 
          blue = 14, 
          green = 13, 
          red = 12)

# solution
day2_task <- readLines("~/Projects/advent_of_code_2023/data/day2-task.txt")
sum_games(input_data = day2_task, 
          blue = 14, 
          green = 13, 
          red = 12)



# part 2 ------------------------------------------------------------------


fewest_balls <- function(input_data, blue, green, red) {
  
  n_sets_per_game <- count_sets(data = input_data, return_n_sets = T)
  n_games <- 1:length(input_data)
  
  res <- list()
  for(game in seq_along(n_games)) {
    
    tmp <- list()
    n_sets <- 1:max(n_sets_per_game[game])
    
    for(set in seq_along(n_sets)) {
      
      tmp[[paste(game, set)]] <- t(compose_matrix(data = input_data, set = n_sets[set]))[,game]
      
    }
    
    res[[game]] <- tmp %>% do.call(rbind,.) %>% apply(MARGIN = 2, FUN = max)
  }
  
  out <- list(mtx = res, sum = sum(sapply(res, prod)))
  return(out)
}


# sample
fewest_balls(input_data = day2_sample, 
             blue = 14, 
             green = 13, 
             red = 12)

# solution

fewest_balls(input_data = day2_task, 
             blue = 14, 
             green = 13, 
             red = 12)

# Day 3: Gear Ratios ------------------------------------------------------
# that was tough one. I did most of the task by myself, but then stuck and search for hints
# still I would like to work on my initial idea with symbol

day3_sample <- readLines("~/Projects/advent_of_code_2023/data/day3-sample.txt")
day3_task <- readLines("~/Projects/advent_of_code_2023/data/day3-task.txt")

neighbor <- function(x, y, x_max = nrow(res_mtx), y_max = ncol(res_mtx)) {
  
  xs <- x + c(-1, 0, 1)
  ys <- y + c(-1, 0, 1)
  
  xs <- xs[xs > 0]
  ys <- ys[ys > 0]
  xs <- xs[xs <= x_max]
  ys <- ys[ys <= y_max]
  
  mtx[xs, ys]
}


#data <- day3_task
data <- day3_sample


n_elements <- table(strsplit(data, split = "") %>% sapply(., length))

mtx <- 
  strsplit(data, split = "") %>% 
  unlist() %>% 
  matrix(ncol = n_elements, byrow = T)

digits <- c(as.character(seq(0,9,1)), ".")
numbers <- digits[-length(digits)]

# for(row in 1:nrow(mtx)) {
#   for(col in 1:ncol(mtx)) {
#     
#     if(! mtx[row,col] %in% digits ) {
#       mtx[row, col] <- "Symbol"
#     } 
#   }
# }
# 
# symbol_locations <- which(mtx == "Symbol", arr.ind = TRUE)

res_mtx <- matrix(FALSE, nrow = ncol(mtx), ncol = ncol(mtx))

for(row in 1:nrow(mtx)) {
  for(col in 1:ncol(mtx)) {
    
    res_mtx[row, col] <- ifelse(
      mtx[row, col] %in% numbers,
      any(!neighbor(row, col) %in% digits),
      FALSE
    )
  }
}


num_len <- data %>% stringr::str_locate_all("\\d+")

res <- c()

for (i in seq_along(data)) {
  rows <- num_len[[i]]
  for (row in seq_len(nrow(rows))) {
    start <- rows[row, 1]
    end <- rows[row, 2]
    if (any(res_mtx[i, seq(start, end)])) {
      res <- c(res, stringr::str_sub(data[i], start, end))
    }
  }
}


as.numeric(res) %>% sum()




# part 2 ------------------------------------------------------------------



# Day 4: Scratchcards -----------------------------------------------------

day4_sample <- readLines("~/Projects/advent_of_code_2023/data/day4-sample.txt")
day4_task <- readLines("~/Projects/advent_of_code_2023/data/day4-task.txt")

count_points <- function(data) {
  
  #browser()
  split_list <- sapply(X = data, strsplit, split = "\\|")
  split_list <- lapply(X = split_list, gsub, pattern = "Card [0-9]+:", replacement = "")
  
  names(split_list) <- stringr::str_replace(string = names(split_list), 
                                            pattern = ":.*", 
                                            replacement = "")
  
  split_list <- sapply(X = split_list, strsplit, split = " ")
  split_list <- lapply(X = split_list, stringi::stri_remove_empty)
  #split_list <- sapply(split_list, as.numeric)
  
  n_cards <- 1:(length(split_list)/2)
  c1 <- seq(1,max(n_cards)*2,2)
  c2 <- seq(2,max(n_cards)*2,2)
  
  res <- c()
  for(i in n_cards) {
    if(i < max(n_cards)) {
      res <- append(res, length(intersect(split_list[[c1[i]]], split_list[[c2[i]]])))
    }
  }
  
  points_total <- c()
  n_matches <- c() 
  for(i in seq_along(res)) {
    if(res[i] == 0){
      points_total <- append(points_total, 0)
      n_matches <- append(n_matches, 0)
    } else {
      points_total <- append(points_total, (2^(res[i]-1)))
      n_matches <- append(n_matches, res[i])
    }
  }
  
  list("n_matches" = c(n_matches, 0),
       "points_total" = c(points_total, 0),
       "sum" = sum(points_total),
       "total_cards" = max(n_cards))
  
}

# sample
count_points(data = day4_sample)

count_points(data = day4_task)


# part 2 ------------------------------------------------------------------

res_sample <- count_points(data = day4_sample)
res_task <- count_points(data = day4_task)

m <- res_task$n_matches
# I missed this part and needed to get inspiration from others
cards <- rep(1, res_task$total_cards)

for(i in seq_along(cards)) {

    if(m[i] !=0) {
      cards[ (i+1):(i + m[i]) ] <- cards[ (i+1):(i + m[i]) ] + cards[i] # and this
    }
}

sum(cards)


# Day 6: Wait For It  -----------------------------------------------------

# sample
data <- readLines("~/Projects/advent_of_code_2023/data/day6-sample.txt")
# task
data <- readLines("~/Projects/advent_of_code_2023/data/day6-task.txt")

s <- strsplit(data, ":") %>%
  lapply(., function(x) {
  tmp <- as.numeric(unlist(strsplit(x[-1], " ")))
  tmp[!is.na(tmp)]
  })

raceTime <- s[[1]]
raceDistance <- s[[2]] 

combinations <- c()

for(i in seq_along(raceTime)) {
  t <- seq_len(raceTime[i])
  sums <- 0
  
  for(j in t) {
      remaining_time <- max(t) - j
      covered_distance <- j*remaining_time
      sums <- sum(append(sums, covered_distance > raceDistance[i]))
  }
  
  combinations <- append(combinations, sums)
}

prod(combinations)


# part2 -------------------------------------------------------------------

# sample
data <- readLines("~/Projects/advent_of_code_2023/data/day6-sample.txt")
# task
data <- readLines("~/Projects/advent_of_code_2023/data/day6-task.txt")

s <- strsplit(data, ":") %>%
  lapply(., function(x) {
    tmp <- as.numeric(unlist(strsplit(x[-1], " ")))
    tmp[!is.na(tmp)]
  })

raceTime_2 <- as.numeric(paste0(s[[1]], collapse = ""))
raceDistance_2 <- as.numeric(paste0(s[[2]], collapse = ""))


# slow solution
# sums <- 0
# 
# for(i in seq_len(raceTime_2)) {
#   
#   remaining_time <- raceTime_2 - i
#   covered_distance <- i * remaining_time
#   sums <- sum(append(sums, covered_distance > raceDistance_2))
# }

# much faster 
time_intervals <- seq_len(raceTime_2)
combinations <- (raceTime_2 - time_intervals) * time_intervals > raceDistance_2
sum(combinations)


# Day 7: Camel Cards ------------------------------------------------------


# Day 8: Haunted Wasteland  -----------------------------------------------

day8_sample <- readLines("~/Projects/advent_of_code_2023/data/day8-sample.txt")
day8_sample_2 <- readLines("~/Projects/advent_of_code_2023/data/day8-sample-2.txt")
day8_taks <- readLines("~/Projects/advent_of_code_2023/data/day8-task.txt")

count_steps <- function(input) {
  
  directions <- unlist(strsplit(input[1], ""))
  directions <- ifelse(directions == "L", 1, 2)
  
  nodes <- gsub("[()]", "", input[-c(1,2)])
  
  tmp <- 
    lapply(nodes, function(x) {
      z <- unlist(strsplit(x, "="))
      gsub(x = unlist(strsplit(z, ","))," ", "")
    })
  
  data <- lapply(tmp, function(x) x[-1])
  names(data) <- lapply(tmp, function(x) x[1])
  
  
  nm <- "AAA"
  i <- 1
  steps <- 0
  
  while(nm != "ZZZ") {
    
    dir <- directions[i]
    nm <- data[[nm]][[dir]]
    steps <- steps +1
    i <- i+1
    if(i > length(directions)) i <- 1
    
  }
  steps
  
}


count_steps(input = day8_sample)
count_steps(input = day8_sample_2)
count_steps(input = day8_taks)


# part 2 ------------------------------------------------------------------

day8_p2 <- readLines("~/Projects/advent_of_code_2023/data/day8-sample-part2.txt")
day8_taks <- readLines("~/Projects/advent_of_code_2023/data/day8-task.txt")

input <- day8_p2

directions <- unlist(strsplit(input[1], ""))
directions <- ifelse(directions == "L", 1, 2)

nodes <- gsub("[()]", "", input[-c(1,2)])

tmp <- 
  lapply(nodes, function(x) {
    z <- unlist(strsplit(x, "="))
    gsub(x = unlist(strsplit(z, ","))," ", "")
  })

data <- lapply(tmp, function(x) x[-1])
names(data) <- lapply(tmp, function(x) x[1])


# working on sample but not on task data
start_pos <- names(data)[grepl("^..A$", names(data))]
i <- 1
steps <- 0

while(! all(grepl("^..Z$", start_pos))) {
  
  dir <- directions[i]
  start_pos <- sapply(X = data[ start_pos ], function(x) x[dir])
  steps <- steps +1
  i <- i+1
  if(i > length(directions)) i <- 1
  
}
steps

#####
# TO BE CONTINUED
start_pos <- names(data)[grepl("^..A$", names(data))]
out <- list()

for(j in seq_along(start_pos)) {
  
  i <- 1
  steps <- 0
  res <- c()
  
  while(length(res) < 4) {
    
    dir <- directions[i]
    start_pos <- sapply(X = data[ start_pos[j] ], function(x) x[dir])
    print(start_pos)
    steps <- steps +1
    i <- i+1
    if(i > length(directions)) i <- 1
    if(grepl("^..Z$", start_pos[j])) res <- c(res, steps)
  }
  out[[j]] <- res
}

steps


# Day 9: Mirage Maintenance -----------------------------------------------

day9_sample <- 
  readLines("~/Projects/advent_of_code_2023/data/day9-sample.txt") %>%
  lapply(., function(x) {
    tmp <- unname(strsplit(x, " "))
    as.numeric(unlist(tmp))
  })

day9_task <- 
  readLines("~/Projects/advent_of_code_2023/data/day9-task.txt") %>%
  lapply(., function(x) {
    tmp <- unname(strsplit(x, " "))
    as.numeric(unlist(tmp))
  })

vec_diff <- function(vec) {
  res <- rep(NA, length(vec) -1)
  
  for(i in seq_along(vec)) {
    if(i < length(vec)) {
      res[i] <- vec[i+1] - vec[i] 
    }
  }

  if(all(res == 0)) res <- c(res,0)
  res
}

fill_mtx <- function(vec) {
  
  input_len <- length(vec)
  extra_row <- vec_diff(vec)
  
  mtx <- matrix(NA, byrow = T, ncol = input_len +1)
  mtx <- vec
  mtx <- rbind(mtx, c(rep(0, length(vec) - length(extra_row)), extra_row))
  i <- 2
  j <- 1
  
  while(!all(extra_row == 0)) {
    
    extra_row <- vec_diff(mtx[i,][-c(1:j)])
    mtx <- rbind(mtx, c(rep(0, length(vec) - length(extra_row)), extra_row))
    i <- i+1
    j <- j+1
  }
  mtx
}


fill_last_col <- function(mtx) {
  
  last_column <- mtx[,ncol(mtx)]
  pos <- (nrow(mtx):1)
  last_col <- rep(NA, nrow(mtx))
  
  for(i in seq_along(pos)) {
    last_col[pos[i]] <- sum(last_column[(length(last_column) -(i -1)): length(last_column)])
  }
  last_col
}


# finally solution
res <- rep(NA, length(day9_task))
for(i in 1:length(day9_task)) {
  
  tmp <- fill_mtx(vec = day9_task[[i]])
  res[i] <- fill_last_col(mtx = tmp)[1]

}

sum(res)


# solution inspired by python user - decided to translate to R

extrapolate <- function(l) {
  diffs <- diff(l)
  return(tail(l, 1) + if(length(l) > 1) extrapolate(diffs) else 0)
}

res2 <- rep(NA, 200)
for(i in 1:length(day9_task)) {
  res2[i] <- extrapolate(day9_task[[i]])
}



# part2 -------------------------------------------------------------------

get_reverse_vec <- function(data) {

  mtx <- fill_mtx(data)
  vec <- rep(NA, nrow(mtx))
  for(i in 1:nrow(mtx)) {
    vec[i] <- mtx[i,i]
  }
  vec
}

fill_rev_col <- function(col) {
  
  pos <- (length(col):1)
  rev_col <- rep(NA, length(col))
  tmp <- 0
  
  for(i in seq_along(pos)) {
    if(i <= length(pos)) {
      rev_col[pos[i]] <- col[pos[i]] - tmp
      tmp <- rev_col[!is.na(rev_col)][1]
    }
  }
  rev_col
}


# solution part 2
res <- rep(NA, length(day9_task))

for(i in 1:length(day9_task)) {
  
  tmp <- get_reverse_vec(data = day9_task[[i]])
  res[i] <- fill_rev_col(col = tmp)[1]
}

sum(res)
