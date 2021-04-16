# Advent of Code Day 3
# From: Alexander Rauhut
# https://www.youtube.com/watch?v=zV1SBAO-KAI


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
############################################# PART 1 ##################################################

# --- Day 3: Toboggan Trajectory ---
#     With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.
# 
# Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:
# 
# ..##.......
# #...#...#..
# .#....#..#.
# ..#.#...#.#
# .#...##..#.
# ..#.##.....
# .#.#.#....#
# .#........#
# #.##...#...
# #...##....#
# .#..#...#.#
# These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:
#     
#     ..##.........##.........##.........##.........##.........##.......  --->
# #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........#.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...##....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
# 
# The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:
#     
#     From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
# 
# The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:
# 
# ..##.........##.........##.........##.........##.........##.......  --->
# #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........X.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...#X....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# In this example, traversing the map using this slope would cause you to encounter 7 trees.
# 
# Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
# 
# Your puzzle answer was 223.

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

#######################################################################################################
#######################################################################################################
############################################# PART 2 ##################################################

# --- Part Two ---
#     Time to check the rest of the slopes - you need to minimize the probability of a sudden arboreal stop, after all.
# 
# Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:
#     
#     Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
# In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.
# 
# What do you get if you multiply together the number of trees encountered on each of the listed slopes?
#     
#     Your puzzle answer was 3517401300.
# 
# Both parts of this puzzle are complete! They provide two gold stars: **
#     
#     At this point, you should return to your Advent calendar and try another puzzle.
# 
# If you still want to see it, you can get your puzzle input.
# 
# You can also [Share] this puzzle.

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