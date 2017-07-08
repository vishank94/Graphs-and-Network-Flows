###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

################# QUESTION 3 ###################

library(Kmisc)
library(readr)
library(igraph)
library(data.table)

setwd("/Users/VishankBhatia/Desktop/P2/data")

#Reading weighted directed graph 
wdgraph <-read.table(file="wd_graph.txt", header = FALSE, sep = "\t",quote="", dec = ".")


#Generating dataframe from the graph data
wdgraph_df = graph.data.frame(wdgraph, directed=TRUE)

#Computing pagerank
wd_pagerank = page.rank(wdgraph_df, directed = T, damping = 0.85)

#Top actors having highest page ranks
top_celeb = sort(wd_pagerank$vector, decreasing = T, index.return = T)

#Names of top 10 celebs
top_celeb_name = matrix(names(top_celeb$x[1:10]), nrow=10, ncol=1)
top_celeb_pg = matrix(as.numeric(top_celeb$x[1:10]), nrow=10, ncol=1)
topten_celeb = cbind(top_celeb_name,top_celeb_pg)

#Top ten legendary actors (http://www.imdb.com/list/ls058546170/)

actor_vec_name <- c("Bogart, Humphrey", "Grant, Cary (I)", "Stewart, James (I)", "Brando, Marlon", "Astaire, Fred",
  "Poitier, Sidney", "Cooper, Gary (I)", "Tracy, Spencer (I)", "Cagney, James (I)", "Gable, Clark (I)")

actor_vec_pagerank <- c()

for(cname in actor_vec_name){
  actor_vec_pagerank<-c(actor_vec_pagerank,top_celeb$x[cname])
}  

famous_celeb_name = matrix(actor_vec_name, nrow=10, ncol=1)
famous_celeb_pg = matrix(actor_vec_pagerank, nrow=10, ncol=1)
topten_famous_celeb = cbind(famous_celeb_name,famous_celeb_pg)

################# QUESTION 3 ###################