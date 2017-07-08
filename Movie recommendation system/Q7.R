###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

################################################################################################
#Preprocessing

ratings = fread("movie_rating.txt", header = FALSE, sep = "\t")
ratings$V2 = NULL
Encoding(ratings$V1) = 'latin1'
ratings$V1 = iconv(ratings$V1, "latin1", "ASCII", sub="")
ratings$V1 = gsub("^\\(", "", ratings$V1)
ratings$V1 = gsub("\\([^[:digit:]]+\\)", "", ratings$V1)
ratings$V1 = gsub("^\\s+|\\s+$", "", ratings$V1)
write.table(ratings,"ratings_processed.txt",row.names = FALSE, sep='\t')

processedRatings <- fread("ratings_processed.txt",sep = "\t")
ratingDict <- list()
for (i in 1: length(processedRatings[[1]])){
  ratingDict[[ processedRatings[[1]][i] ]] <- processedRatings[[2]][i]
}

saveRDS(ratingDict,"ratingDict.rds")

#preprocessing ends
##########################################################################S

ratingDict <- readRDS("ratingDict.rds")
vertexIdSuperman <- which(V(movieNet)$name == "Batman v Superman: Dawn of Justice (2016)")
vertexIdMissImp <- which(V(movieNet)$name == "Mission: Impossible - Rogue Nation (2015)")
vertexIdMinion <- which(V(movieNet)$name == "Minions (2015)  (voice)")

#find the community 
supermanComm = moviesNetCommunity$membership[vertexIdSuperman]
misImpComm = moviesNetCommunity$membership[vertexIdMissImp]
minionsComm = moviesNetCommunity$membership[vertexIdMinion]
movies <- c("Batman v Superman: Dawn of Justice (2016)", 
            "Mission: Impossible - Rogue Nation (2015)",
            "Minions (2015)  (voice)")
community <- c(supermanComm, misImpComm, minionsComm)
k = 10
actualRatings = c(7.1,7.5,6.4)

for (i in 1:3) {
  commOfMovie <- community[i]
  vertices <- V(movieNet)[which(moviesNetCommunity$membership == commOfMovie)]
  commRatings = ratingDict[vertices]
  
  # non - zero ratings
  commRatings = commRatings[which(commRatings != 0)]
  
  # k-nearest neigbhors ratings
  edgeNodes = x[which(x$"Node 1" == movies[i] | x$"Node 2" == movies[i]),]
  edgeNodes =  edgeNodes[order(edgeNodes$"weights"),]
  
  topKNeighbours = data.frame(edgeNodes[1:k,])
  topKNeighbours$"weights" = NULL
  nearestNeighbours <- c()
  for (j in 1:k){
    nearestNeighbours <- c(nearestNeighbours,topKNeighbours[j,which((topKNeighbours[j,])!=movies[i])])
  }
  nearestNeighbourRatings = ratingDict[nearestNeighbours]
  nearestNeighbourRatings <- unlist(nearestNeighbourRatings)
  nearestNeighbourRatings = nearestNeighbourRatings[which(nearestNeighbourRatings != 0)]
  
  neighBour = unlist(neighbors(movieNet, movies[i]))
  neighbourRatings = ratingDict[neighBour]
  neighbourRatings = neighbourRatings[which(neighbourRatings != 0)]
  
  allRatings = c(as.numeric(unlist(commRatings)), as.numeric(unlist(nearestNeighbourRatings)), as.numeric(unlist(neighbourRatings)))
  cat(movies[i], actualRatings[i],"\n")
  cat("Community Rating ", mean(as.numeric(unlist(commRatings))), "\n")
  cat("Nearest Neighbor Rating", mean(as.numeric(unlist(nearestNeighbourRatings))), "\n")
  cat("Neighbors Rating", mean(as.numeric(unlist(neighbourRatings))), "\n")
  cat("All Combined ", mean(allRatings), "\n")
  cat("\n")
}


