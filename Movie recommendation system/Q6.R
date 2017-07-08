###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

# get vertex id of the movies in the graph
vertexIdSuperman <- which(V(movieNet)$name == "Batman v Superman: Dawn of Justice (2016)")
vertexIdMissImp <- which(V(movieNet)$name == "Mission: Impossible - Rogue Nation (2015)")
vertexIdMinion <- which(V(movieNet)$name == "Minions (2015)  (voice)")

#find the community 
supermanComm = moviesNetCommunity$membership[vertexIdSuperman]
misImpComm = moviesNetCommunity$membership[vertexIdMissImp]
minionsComm = moviesNetCommunity$membership[vertexIdMinion]

for(movie in c("Batman v Superman: Dawn of Justice (2016)", 
               "Mission: Impossible - Rogue Nation (2015)",
               "Minions (2015)  (voice)")){
  edgeNodes = x[which(x$"Node 1" == movie | x$"Node 2" == movie),]
  edgeNodes = edgeNodes[order(edgeNodes$"weights"),]
  top5Neighours = data.frame(edgeNodes[1:5,])
  print(top5Neighours$"weights")
  top5Neighours$"weights" = NULL
  
  nearest5 <- c()
  for (j in 1:5){
    nearest5 <- c(nearest5,top5Neighours[j,which((top5Neighours[j,])!=movie)])
  }
  cat("top 5 neighbours for Movie and the community", movie,"\n")
  
  for (neighbour in nearest5){
    vertexId <- which(V(movieNet)$name == neighbour)
    commVertex = moviesNetCommunity$membership[vertexId]
    cat("Movie:",neighbour," Community:",commVertex,"\n")
  }
}

cat("Community of 3 movies\n")
cat("Batman v Superman: Dawn of Justice (2016): ", supermanComm , "\n")
cat("Mission: Impossible - Rogue Nation (2015): ", misImpComm  , "\n")
cat("Minions (2015): ", minionsComm , "\n")
