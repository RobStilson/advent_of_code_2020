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
df_raw <- read_table2("2/input.txt", col_names = FALSE)

# Create a range matrix that contains the 2 values from the X1 column

range <- stringr::str_split_fixed(df_raw$X1, "-", 2)

df <- data.frame(
    min = as.numeric(range[, 1]),
    max = as.numeric(range[, 2]),
    letter = gsub(":", "", df_raw$X2),
    pass = df_raw$X3
)

# Part one

letter_matcher <- function(x) {
    lengths(regmatches(x["pass"], gregexpr(x["letter"], x["pass"])))
}

gregexpr(df[1, "letter"], df[1, "pass"])

count <- apply(df, 1, letter_matcher)

#How would we do this with purrr?

# library(purrr)
# count2 <- pmap(df, ~letter_matcher)

poss_pass <- count >= df$min & count <= df$max
sum(poss_pass)

# Part two
min_letter <- substr(df[, "pass"], df[, "min"], df[, "min"])
max_letter <- substr(df[, "pass"], df[, "max"], df[, "max"])

check <- (min_letter == df[, "letter"]) - (max_letter == df[, "letter"])
sum(check != 0)
