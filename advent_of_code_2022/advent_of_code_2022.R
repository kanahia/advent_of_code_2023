
# Day 1: Calorie Counting -------------------------------------------------

elf <- read.table("task1.txt", blank.lines.skip = F) %>%
  dplyr::mutate(V1 = ifelse(is.na(V1), 0, V1))

index_i <- c(1, zeros[seq(1,length(zeros), 2)])
index_j <- c(7, zeros[seq(2,length(zeros), 2)], length(elf$V1))

l <- list()

for(i in 1:length(index_i)) {
  
  l[[i]] <- 
    sum(elf$V1[index_i[i]:index_j[i]])
  
}

l %>% unlist() %>% max()


# task2 -------------------------------------------------------------------
l %>% unlist() %>% sort() %>% tail(3) %>% sum()



# Day 2: Rock Paper Scissors ----------------------------------------------
# The first column is what your opponent is going to play: 
# A for Rock, 
# B for Paper, and 
# C for Scissors

# The second column, you reason, must be what you should play in response: 
# X for Rock, 
# Y for Paper, and 
# Z for Scissors

# 1 for Rock, 2 for Paper, and 3 for Scissors +
# plus the score for the outcome of the round (0 if lost, 3 if draw, and 6 if won)

game <-
  read.table("task3.txt") %>%
  dplyr::mutate(V1 = case_when(V1 == "A" ~ "Rock",
                               V1 == "B" ~ "Paper",
                               V1 == "C" ~ "Scissors"),
                V2 = case_when(V2 == "X" ~ "Rock",
                               V2 == "Y" ~ "Paper",
                               V2 == "Z" ~ "Scissors"),
                outcome = sapply(X = 1:nrow(.),
                                 FUN = function(x) {
                                   if(V1[x] == "Rock" & V2[x] == "Paper" | 
                                      V1[x] == "Paper" & V2[x] == "Scissors" |
                                      V1[x] == "Scissors" & V2[x] == "Rock") {
                                     "win"
                                   } else if(V1[x] == "Paper" & V2[x] == "Rock" | 
                                             V1[x] == "Scissors" & V2[x] == "Paper" |
                                             V1[x] == "Rock" & V2[x] == "Scissors"){
                                     "lost"
                                   } else if(V1[x] == V2[x]){
                                     "draw"
                                   }
                                 }),
                points = case_when(V2 == "Rock" ~ 1,
                                   V2 == "Paper" ~ 2,
                                   V2 == "Scissors" ~ 3),
                points_outcome = case_when(outcome == "win" ~ 6,
                                           outcome == "draw" ~ 3,
                                           outcome == "lost" ~ 0)
  )

sum(game[, 4:5])


# task2 -------------------------------------------------------------------
# X means you need to lose, 
# Y means you need to end the round in a draw 
# Z means you need to win

game2 <-
  read.table("task3.txt") %>%
  dplyr::mutate(V1 = case_when(V1 == "A" ~ "Rock",
                               V1 == "B" ~ "Paper",
                               V1 == "C" ~ "Scissors"),
                V2 = case_when(V2 == "Y" ~ "draw",
                               V2 == "X" ~ "loose",
                               V2 == "Z" ~ "win"),
                show = sapply(X = 1:nrow(.),
                              FUN = function(x) {
                                if(V2[x] == "draw") {
                                  V1[x]
                                } else if(V2[x] == "loose" & V1[x] == "Rock") {
                                  "Scissors"
                                } else if(V2[x] == "loose" & V1[x] == "Paper") {
                                  "Rock"
                                } else if(V2[x] == "loose" & V1[x] == "Scissors") {
                                  "Paper"
                                } else if(V2[x] == "win" & V1[x] == "Rock") {
                                  "Paper"
                                } else if(V2[x] == "win" & V1[x] == "Paper") {
                                  "Scissors"
                                } else if(V2[x] == "win" & V1[x] == "Scissors") {
                                  "Rock"
                                }
                              }),
                points = case_when(show == "Rock" ~ 1,
                                   show == "Paper" ~ 2,
                                   show == "Scissors" ~ 3),
                points_outcome = case_when(V2 == "win" ~ 6,
                                           V2 == "draw" ~ 3,
                                           V2 == "loose" ~ 0)
  )

sum(game2[, 4:5])


# Day 3: Rucksack Reorganization ------------------------------------------
items <- readLines("day3.txt") %>%
  as.list()


df_item <- tibble("string" = items %>% unlist() %>% as.data.frame() %>% .[[1]])

df_item <- 
  df_item %>%
  dplyr::mutate("length" = 
                  lapply(items, nchar) %>% unlist() %>% as.data.frame() %>% .[[1]],
                "partial_len" = 
                  lapply(items, function(x) nchar(x)/2) %>% unlist() %>% as.data.frame() %>% .[[1]]
  )

df_item <- 
  df_item %>%
  dplyr::mutate(
    "str1" = 
      sapply(X = 1:nrow(df_item), 
             FUN = function(x) substr(df_item$string[x], 1, df_item$partial_len[x]))
  )

df_item <- 
  df_item %>%
  dplyr::mutate(
    "str2" = 
      sapply(X = 1:nrow(df_item), 
             FUN = function(x) substr(df_item$string[[x]], df_item$partial_len[[x]]+1, df_item$length[[x]]))
  )

df_item <- 
  df_item %>%
  dplyr::mutate(
    "common" =
      sapply(X= 1:nrow(df_item),
             FUN = function(x){
               strsplit(df_item$str1[x], split = "")[[1]][ strsplit(df_item$str1[x],split = "")[[1]] %in% 
                                                             strsplit(df_item$str2[x], split = "")[[1]] ] %>%
                 unique()
             })
  )

df_item <- 
  df_item %>%
  dplyr::mutate(
    "type" = ifelse(stringr::str_detect(df_item$common,"[[:upper:]]"), "upper", "lower"),
    "score" = 
      sapply(
        X = 1:nrow(df_item),
        FUN = function(x) {
          if (any(letters %in% df_item$common[x])) {
            which(letters %in% df_item$common[x])
          } else if (any(LETTERS %in% df_item$common[x])) {
            26 + which(LETTERS %in% df_item$common[x])
          }
        }
      )
  )

df_item$score %>% sum()  

# very first approach -----------------------------------------------------


# df_item <- tibble("string" = items %>% unlist() %>% as.data.frame() %>% .[[1]],
#                   "length" = 
#                     lapply(items, nchar) %>% unlist() %>% as.data.frame() %>% .[[1]],
#                   "partial_len" = 
#                     lapply(items, function(x) nchar(x)/2) %>% unlist() %>% as.data.frame() %>% .[[1]],
#                   "str1" = 
#                     sapply(X = 1:nrow(df_item), 
#                            FUN = function(x) substr(df_item$string[x], 1, df_item$partial_len[x])),
#                   "str2" = 
#                     sapply(X = 1:nrow(df_item), 
#                            FUN = function(x) substr(df_item$string[[x]], df_item$partial_len[[x]]+1, df_item$length[[x]])),
#                   "common" =
#                     sapply(X= 1:nrow(df_item),
#                            FUN = function(x){
#                              strsplit(df_item$str1[x], split = "")[[1]][ strsplit(df_item$str1[x],split = "")[[1]] %in% 
#                                                                            strsplit(df_item$str2[x], split = "")[[1]] ] %>%
#                                unique()
#                            }),
#                   "type" = ifelse(stringr::str_detect(df_item$common,"[[:upper:]]"), "upper", "lower"),
#                   "score" = 
#                     sapply(
#                       X = 1:nrow(df_item),
#                       FUN = function(x) {
#                         if (any(letters %in% df_item$common[x])) {
#                           which(letters %in% df_item$common[x])
#                         } else if (any(LETTERS %in% df_item$common[x])) {
#                           26 + which(LETTERS %in% df_item$common[x])
#                         }
#                       }
#                     )
#                   )


# task2 -------------------------------------------------------------------
# macio showing how to
items <- readLines("day3.txt")
groups <- vector("list", length = length(items)/3)
counter <- 0
pointer <- 1
for (i in seq(1, 300)) {
  groups[[pointer]] <- c(groups[[pointer]], items[i])
  counter <- counter + 1
  if (counter == 3) {
    counter <- 0
    pointer <- pointer + 1
  }
}


res <- c()

for(i in 1:length(groups)){
  res[i] <-
    intersect(
      intersect((groups[[i]] %>% strsplit(., split = "") %>% .[[1]]),
                (groups[[i]] %>% strsplit(., split = "") %>% .[[2]])), 
      (groups[[i]] %>% strsplit(., split = "") %>% .[[3]]))
}

final_res <-
  sapply(
    X = 1:length(res),
    FUN = function(x) {
      if (any(letters %in% res[x])) {
        which(letters %in% res[x])
      } else if (any(LETTERS %in% res[x])) {
        26 + which(LETTERS %in% res[x])
      }
    }
  )
sum(final_res)


# Day 4: Camp Cleanup -----------------------------------------------------

groups <- readLines("day4.txt") %>%
  as.data.frame() %>%
  dplyr::rename("A" = 1) %>%
  tidyr::separate(., col = "A", c("A", "B"), sep = ",") %>%
  tidyr::separate(., col= "A", c("A_start", "A_end"), sep = "-") %>%
  tidyr::separate(., col= "B", c("B_start", "B_end"), sep = "-") %>%
  dplyr::mutate_if(is.character, as.numeric)

test <- c()
for (i in 1:nrow(groups)) {
  if (groups[i, 3] >= groups[i, 1] & groups[i, 4] <= groups[i, 2]) {
    test[i] <- TRUE
  } else if (groups[i, 1] >= groups[i, 3] &
             groups[i, 2] <= groups[i, 4]) {
    test[i] <- TRUE
  } else {
    test[i] <- FALSE
  }
}

groups$test <- test
sum(groups$test)

test2 <- c()
for(i in 1:nrow(groups)){
  a <- seq(groups[i,1], groups[i,2])
  b <- seq(groups[i,3], groups[i,4])
  
  test2[i] <- all(a %in% b) | all(b %in% a)
}


# task2 -------------------------------------------------------------------

test2 <- c()
for(i in 1:nrow(groups)){
  a <- seq(groups[i,1], groups[i,2])
  b <- seq(groups[i,3], groups[i,4])
  
  test2[i] <- sum(a %in% b)
  #sum(seq(groups[1,1], groups[1,2]) %in% seq(groups[1,3], groups[1,4]))
}

table(test2 > 0)


# Day 5: Supply Stacks ----------------------------------------------------

# [N]         [C]     [Z]            
# [Q] [G]     [V]     [S]         [V]
# [L] [C]     [M]     [T]     [W] [L]
# [S] [H]     [L]     [C] [D] [H] [S]
# [C] [V] [F] [D]     [D] [B] [Q] [F]
# [Z] [T] [Z] [T] [C] [J] [G] [S] [Q]
# [P] [P] [C] [W] [W] [F] [W] [J] [C]
# [T] [L] [D] [G] [P] [P] [V] [N] [R]
# 1   2   3   4   5   6   7   8   9 

boxes_header <- 
  readLines("day5.txt")


l <- vector("list", length = 9)
start <- seq(1,35, 4)
end <- seq(3,35,4)

for(line in 1:8){
  for(column in 1:9){
    
    #start <- -3 + 4*column
    l[[column]] <- c(l[[column]], substr(boxes_header[line], start[column], end[column]))
    
  }
}

l <- 
  lapply(l, function(x) x[! grepl(x, pattern = "   ")])

info <- 
  boxes_header[11:length(boxes_header)] %>%
  gsub(pattern = "move |from |to ", replacement = "") %>%
  as.data.frame() %>%
  tidyr::separate(col = 1, c("move", "from", "to"), " ") %>%
  dplyr::mutate_if(is.character, as.numeric)


l2 <- l

for(i in 1:nrow(info)){
  l2[[ info$to[i] ]]  <- 
    append(l2[[ info$to[i] ]], rev(l2[[ info$from[i] ]][ 1:info$move[i] ]), after = 0)
  
  l2[[ info$from[i] ]]  <- 
    l2[[ info$from[i] ]][- c(1:info$move[i])]
}

#l2 <- lapply(l2, function(x) x[!is.na(x)])

lapply(l2, function(x) x[[1]][1]) %>% unlist()


#####
t <- l2
t[[ info$to[1] ]]  <- 
  append(t[[ info$to[1] ]], rev(t[[ info$from[1] ]][ 1:info$move[1] ]), after = 0)


t[[ info$from[1] ]]  <- 
  t[[ info$from[1] ]][ -c(1:info$move[1])]


# task2 -------------------------------------------------------------------

l3 <- l

for(i in 1:nrow(info)){
  l3[[ info$to[i] ]]  <- 
    append(l3[[ info$to[i] ]], l3[[ info$from[i] ]][ 1:info$move[i] ], after = 0)
  
  l3[[ info$from[i] ]]  <- 
    l3[[ info$from[i] ]][- c(1:info$move[i])]
}

l3 <- lapply(l3, function(x) x[!is.na(x)])

lapply(l3, function(x) x[[1]][1]) %>% unlist()




# Day 6: Tuning Trouble ---------------------------------------------------

# Specifically, it needs to report the number of characters from the beginning 
# of the buffer to the end of the first such four-character marker.
code <- 
  readLines("day6.txt") %>%
  strsplit(code, split = "") %>% 
  unlist()

x <- c()
start <- 1
end <- 4
for (i in 1:length(code)) {
  x <- any(duplicated(code[start:end]))
  
  if (isTRUE(x)) {
    start = i + 1
    end <- end + 1
    
  } else {
    print(code[start:end])
    print(paste0("How many characters? -> ", i + 3))
    break
    
  }
}


# task2 -------------------------------------------------------------------
# 14 instead of 4 characters

x <- c()
start <- 1
end <- 14
for (i in 1:length(code)) {
  x <- any(duplicated(code[start:end]))
  
  if (isTRUE(x)) {
    start = i + 1
    end <- end + 1
    
  } else {
    print(code[start:end])
    print(paste0("How many characters? -> ", i + 13))
    break
    
  }
}


# Day 7: No Space Left On Device ------------------------------------------



# Day 8: Treetop Tree House -----------------------------------------------

trees <- 
  readLines("day8.txt") %>%
  lapply(., strsplit, split="")

for(i in 1:length(trees)) {
  trees[[i]] <- lapply(trees[[i]], function(x)
    as.numeric(x))
}

mtx <- matrix(unlist(trees), 
              nrow = length(trees[[1]][[1]]), 
              ncol = length(trees))

count_trees <- function(mtx,
                        from_right = FALSE,
                        out_mtx) {
  
  if (from_right == TRUE) {
    seq_row <- 1:nrow(mtx)
    seq_col <- rev(1:ncol(mtx))
  } else {
    seq_row <- 1:nrow(mtx)
    seq_col <- 1:ncol(mtx)
  }
  
  counter <- 0
  
  for (row in seq_row) {
    
    max <- -1
    
    for (col in seq_col) {
      print(paste0("processing row number: ", row, " and column: ", col))
      if (mtx[row, col] > max) {
        max <- mtx[row, col]
        counter <- counter + 1
        out_mtx[row, col] <- TRUE
      }
    }
  }
  return(out_mtx)
  
}

### bottom_top
count_trees_TB <- function(mtx,
                           from_bottom = FALSE,
                           out_mtx) {
  
  if (from_bottom == TRUE) {
    seq_row <- rev(1:nrow(mtx))
    seq_col <- 1:ncol(mtx)
  } else {
    seq_row <- 1:nrow(mtx)
    seq_col <- 1:ncol(mtx)
  }
  
  counter <- 0
  
  for (col in seq_col) {
    
    max <- -1
    
    for (row in seq_row) {
      print(paste0("processing row number: ", row, " and column: ", col))
      if (mtx[row, col] > max) {
        max <- mtx[row, col]
        counter <- counter + 1
        out_mtx[row, col] <- TRUE
      }
    }
  }
  return(out_mtx)
  
}

out <- 
  matrix(ncol = 99, nrow = 99) %>%
  apply(X = ., MARGIN = 1:2,FUN = function(x) isTRUE(x))

top <- count_trees_TB(mtx = mtx, from_bottom = FALSE, out_mtx = out)
bottom <- count_trees_TB(mtx = mtx, from_bottom = TRUE, out_mtx = out)
left <- count_trees(mtx = mtx, from_right = FALSE, out_mtx = out)
right <- count_trees(mtx = mtx, from_right = TRUE, out_mtx = out)

# test_data ---------------------------------------------------------------

test <- c(3,0,3,7,3,2,5,5,1,2,6,5,3,3,2,3,3,5,4,9,3,5,3,9,0) %>%
  matrix(., ncol =5, nrow = 5, byrow = T)

counter <- 0

for (row in 1:5) {
  max <-0
  for (col in 1:5) {
    print(paste0("processing row number: ", row, " and column: ", col))
    if (test[row, col] >= max) {
      max <- test[row, col]
      counter <- counter + 1
      out2[row, col] <- TRUE
    }
  }
}


out <- matrix(data = FALSE, nrow = 5, ncol = 5)

top <- count_trees_TB(mtx = test, from_bottom = FALSE, out_mtx = out)
bottom <- count_trees_TB(mtx = test, from_bottom = TRUE, out_mtx = out)
left <- count_trees(mtx = test, from_right = FALSE, out_mtx = out)
right <- count_trees(mtx = test, from_right = TRUE, out_mtx = out)

# part2 -------------------------------------------------------------------

test_data <-
  matrix(
    c(3, 0, 3, 7, 3,2, 5, 5, 1,2, 6, 5, 3, 3, 2, 3, 3, 5, 4, 9, 3, 5, 3, 9, 0),
    byrow = T,
    ncol = 5,
    nrow = 5
  )

score_mtx <- matrix(data = 0, 
                    nrow = length(test_data[1,]), 
                    ncol = length(test_data[,1]), 
                    byrow = T)

value <- -1
score <- 0

for(x in 1:length(test_data[1,])) {
  
  if(test_data[1, ][x] > value) {
    value <- test_data[1, ][x]
    score <- score + 1
    
  } else if(test_data[1, ][x] <= value) {
    
  }
}