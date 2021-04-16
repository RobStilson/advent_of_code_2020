# Advent of Code Day 1
# From: Alexander Rauhut
# https://www.youtube.com/watch?v=YTK24iyCgfg


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

# --- Day 1: Report Repair ---
#     After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
# 
# The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
# 
# To save your vacation, you need to get all fifty stars by December 25th.
# 
# Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
#     
#     Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
# 
# Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
# 
# For example, suppose your expense report contained the following:
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
# 
# Your puzzle answer was 987339.

#######################################################################################################
#######################################################################################################
############################################ LOAD DATA ################################################

library(readr)
df <- read_csv("1/input.txt", col_names = FALSE)

x <- as.vector(df$X1)

# part one, a + b = 2020

# find numbers
x[match(x, 2020 - x, nomatch = 0)]

# 829 1191

prod(x[match(x, 2020 - x, nomatch = 0)])

# 987339

#######################################################################################################
#######################################################################################################
############################################# PART 2 ##################################################

# --- Part Two ---
#     The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.
# 
# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.
# 
# In your expense report, what is the product of the three entries that sum to 2020?
#     
#     Your puzzle answer was 259521570.
# 
# Both parts of this puzzle are complete! They provide two gold stars: **
#     
#     At this point, you should return to your Advent calendar and try another puzzle.
# 
# If you still want to see it, you can get your puzzle input.
# 
# You can also [Share] this puzzle.

# part two, a + b + c = 2020

matches <- list()
for (i in 2020 - x) {
    matches[[i]] <- x[match(x, i-x, nomatch = 0)]
}

prod(unique(unlist(matches)))

# 259521570