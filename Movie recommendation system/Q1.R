###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

################# QUESTION 1 ###################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)

registerDoMC(8)  # parallel processing

setwd("C:/Users/VishankBhatia/Desktop/P2")
system("python C:/Users/VishankBhatia/Desktop/P2/Question_1.py")

################# QUESTION 1 ###################