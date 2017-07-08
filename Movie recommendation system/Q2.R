###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

################# QUESTION 2 ###################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)

registerDoMC(8)  # parallel processing

setwd("/Users/VishankBhatia/Desktop/P2")

system("python /Users/VishankBhatia/Desktop/P2/Question_2.py")

cat("here")

data_list <-read.table(file="/Users/VishankBhatia/Desktop/P2/data/wd_graph.txt", header = FALSE, sep = "\t",quote="",
                       dec = ".")

cat("2")


colnames(data_list) = c("node_1", "node_2", "weight")
cat("3")
directed_graph <- graph.data.frame(data_list,directed = TRUE)


is_connected = is.connected(directed_graph)

cat("The connectivity status of graph is : ", is_connected)

################# QUESTION 2 ###################