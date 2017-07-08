###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(igraph)
library(corrplot)
library(ggplot2)
# Utility functions

getIdCircles <- function(egoNodeList){
  idsCircles <- numeric()
  for (id in egoNodeList) {
    # get the number of circles
    circlesFileName <- paste("gplus/" , id , ".circles" , sep="")
    circlesConnectFile <- file(circlesFileName , open="r")
    circleContent <- readLines(circlesConnectFile)
    close(circlesConnectFile)
    
    # check if greater than 2
    if(length(circleContent) > 2)
      idsCircles <- c(idsCircles, id)
  }
  print(paste("Answer 7: Total IDs with > 2 circles = ", length(idsCircles)))
  return(idsCircles)
}


compute <- function(idsCircles){
  for (id in c(idsCircles[1])) {
    edges <- paste("gplus/" , id  , ".edges" , sep="") # edge list
    circlesFileName <- paste("gplus/" , id , ".circles" , sep="") # circles list
    
    circlesConnectFile <- file(circlesFileName , open="r")
    circleContent <- readLines(circlesConnectFile)
    
    circles <- list()
    for (i in 1:length(circleContent)) {
      circleNodeList <- strsplit(circleContent[i],"\t")
      circles <- c(circles, list(circleNodeList[[1]][-1]))
  }
  
  close(circlesConnectFile)
  
  # create network using edge list
  graphDirected <- read.graph(edges , format = "ncol" , directed=TRUE)  
  graphDirected <- add.vertices(graphDirected, nv = 1, name = id)
  egoNodeID <- which(V(graphDirected)$name==id) 
  
  edgeList <- c()
  for (vertex in 1:(vcount(graphDirected) - 1)) {
    edgeList <- c(edgeList, c(egoNodeID, vertex))
  }
  
  graphDirected <- add_edges(graphDirected, edgeList)
  
  walktrapCommStr <- walktrap.community(graphDirected)
  infomapCommStr <- infomap.community(graphDirected)
  percentage <- vector()
  percentageCircle <- vector()
  # check percentage of match for walktrap community 
  for(m in 1:max(walktrapCommStr$membership)){
    communityNodes <- V(graphDirected)$name[which(walktrapCommStr$membership == m)]
    for (n in 1:length(circles)) {
      commonNodes <- intersect(communityNodes, circles[[n]])
      percentCircle <- length(commonNodes)/length(circles[[n]])
      percentageCircle <- c(percentageCircle, percentCircle)
    }
  }
  
  for(m in 1:max(infomapCommStr$membership)){
    
    communityNodes <- V(graphDirected)$name[which(infomapCommStr$membership == m)]
    
    for (n in 1:length(circles)) {
      commonNodes <- intersect(communityNodes, circles[[n]])
      percent <- length(commonNodes)/length(circles[[n]])
      percentage <- c(percentageCircle, percentCircle)
    }
  }
  
  
  par(oma=c(0,0,0,0))
  
  print(paste("Answer 7: Number of Circles = ", length(circles)))
  
  cPercent <- matrix(percentageCircle, nrow = max(walktrapCommStr$membership), ncol = length(circles))
  colnames(cPercent) <- paste("Circle ",    1:length(circles),     sep="")
  rownames(cPercent) <- paste("Community ",    1:max(walktrapCommStr$membership),     sep="")
  col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white",
                             "cyan", "#007FFF", "blue","#00007F"))
  png("Ans7WalktrapComOverlap.png")
  corrplot(cPercent, method="circle", col= col1(20), cl.lim=c(0,1), tl.cex = 2, tl.col = 'black')
  dev.off()
  
  iPercent <- matrix(percentage, nrow = max(infomapCommStr$membership), ncol = length(circles))
  colnames(iPercent) <- paste("Circle ",    1:length(circles),     sep="")
  rownames(iPercent) <- paste("Community ",    1:max(infomapCommStr$membership),     sep="")
  col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                             "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
  png("Ans7InfoMapComOverlap.png")
  corrplot(iPercent, method="circle", col= col1(200), cl.lim=c(0,1), tl.cex = 2, tl.col = 'black')
  dev.off()
  }
  
  # plot communities
  nodeSize <- rep(2, vcount(graphDirected))
  nodeSize[egoNodeID] <- 4
  png("Ans7WalktrapCommStr.png")
  plot(walktrapCommStr, graphDirected, vertex.size = nodeSize , asp = 9/16, vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
  dev.off()
  png("Ans7InfomapCommStr.png")
  plot(infomapCommStr, graphDirected, vertex.size = nodeSize , asp = 9/16, vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
  dev.off()
  
  # plot community structure
  png("Ans7CommStrFreq.png")
  ggplot(data.frame(sizes(infomapCommStr)), aes(x = Community.sizes, y = Freq)) + geom_bar(fill="darkgreen",stat="identity") + xlab("Community Number") +  ylab("Frequency") + labs(title = "Community Structure Frequency") + geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
  dev.off()
}


# Code 

fileName <- list.files("gplus/")
fileIDs <- sub("^([^.]*).*", "\\1", fileName)
egoNodeList <- unique(fileIDs) 

print(paste("Answer 7: Total Number of Ego Nodes = ", length(egoNodeList)))
idsCircles <- getIdCircles(egoNodeList)
compute(idsCircles)
