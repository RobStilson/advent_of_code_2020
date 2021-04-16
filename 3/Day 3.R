# Advent of Code Day 2
# From: Alexander Rauhut
# https://www.youtube.com/watch?v=hI-8orPGGdo


#######################################################################################################
#######################################################################################################
##################################LOADING PACKAGES#####################################################
tryCatch(require(pacman),finally=utils:::install.packages(pkgs='pacman',repos='http://cran.r-project.org'));
require(pacman)
# if the above doesn't work, use this code#
# tryCatch
# detach("package:pacman", unload = TRUE)
# install.packages("pacman", dependencies = TRUE)
# install.packages("pacman")
pacman::p_load(tidyverse,
               conflicted
)

## Load packages

#Loading libraries
suppressPackageStartupMessages({
    library(plyr)
    library(tidyverse)
    library(conflicted)
    library(tidylog, warn.conflicts = FALSE)
})
for (f in getNamespaceExports("tidylog")) {
    conflicted::conflict_prefer(f, "tidylog", quiet = TRUE)
}



conflict_prefer("filter", "dplyr")

#######################################################################################################
#######################################################################################################
############################################ LOAD DATA ################################################

library(readr)
df <- read_csv("3/input.txt", col_names = FALSE)

#Transform it into a vector, otherwise the function can't read it correctly
df <- as.vector(df$X1)

# df <- scan("3/input.txt", "character", quite = TRUE)

#######################################################################################################
#######################################################################################################
############################################ ANALYSIS #################################################

# Part 1
slope <- c(3, 1)

check_tree <- function(x, y){
    substr(x, y, y) == "#"
}

# ---Demo:
test <- "..#..#"

check_tree(test, 3) # feed in `test` and check for a tree at position 3. It should return "TRUE" and it does

check_tree(test, 2) # Now it will check position 2 and it should return "FALSE" and it does

count_trees <- function(slope, x) {
    right <- slope[[1]]
    down <- slope[[2]]
    
    # make enough grid to get to the bottom
    width_factor <- ceiling(right * length(x) / nchar(x[1]))
    
    complete <- function(x) {
        paste(replicate(width_factor, x), collapse = "")
    }
    
    full <- sapply(x, complete)
    
    # set up coordinates
    coor_x <- seq(1, right * length(full), by = right)
    coor_y <- seq(1, length(full), by = down)
    
    sum(check_tree(full[coor_y], coor_x))
}

count_trees(slope, df)

#223

# Part 2
slope <- list(
    c(1, 1),
    c(3, 1),
    c(5, 1),
    c(7, 1),
    c(1, 2)
)

trees <- sapply(slope, count_trees, df)
prod(trees)