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
             function(x) strsplit(gsub(pattern = ".*: ", replacement = "", x = x), 
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

