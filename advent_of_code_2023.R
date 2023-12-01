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
