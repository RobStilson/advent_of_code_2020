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

# part two, a + b + c = 2020

matches <- list()
for (i in 2020 - x) {
    matches[[i]] <- x[match(x, i-x, nomatch = 0)]
}

prod(unique(unlist(matches)))

# 259521570