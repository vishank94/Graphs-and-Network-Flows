###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(stringr)
library(MASS)
library(data.table)
registerDoMC(8) 

# get page rank features
imdb.page.rank <-wd_pagerank
page.rank.actor = cbind(names(imdb.page.rank$vector), imdb.page.rank$vector)
write.table(page.rank.actor, "page_rank_actors_2.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
