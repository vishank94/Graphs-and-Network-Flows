###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  

x = fread("movie_network_file.txt", header=FALSE, data.table = TRUE)
colnames(x) = c("Node 1", "Node 2", "weights")
moviesDf = graph.data.frame(x, directed=FALSE)
movieNet <- simplify(moviesDf)
saveRDS(movieNet, "newnet.rds")

