###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(igraph)

# Utility functions

# Return the vector with core nodes of the graph
getCoreNodes <- function(graphFB){
  coreNodes <- c()
  for (vert in V(graphFB)){
    if(length(neighbors(graphFB, vert)) > 200)
      coreNodes <- append(coreNodes, vert)
  }
  return(coreNodes)
}

# Return the personal network of the node being passed
getPersonalNetwork <- function(graphFB, nodeName){
  neighbors <- sort(unlist(neighborhood(graphFB, 1, nodeName)))
  personalNet <- induced_subgraph(graphFB, unlist(neighbors))
  personalNet$names <- sort(unlist(neighbors))
  return(personalNet)
}

# Mutual freinds of two passed nodes in the graph
mutualFriends <- function(graphFB, coreNode, personalNetNode){
  return(intersect(neighbors(graphFB, personalNetNode), neighbors(graphFB, coreNode)))
}

# Return embeddedness score between the two nodes from the graph
calcEmbeddedness <- function(graphFB, coreNode, personalNetNode){
  return(length(mutualFriends(graphFB, coreNode, personalNetNode)))
}

# Return dispersion score between the two nodes from the graph
calcDispersion <- function(graphFB, coreNode, personalNetNode, distances){
  dispersion <- 0
  mutualFriendsNet <- mutualFriends(graphFB, coreNode, personalNetNode)
  modifiedGraph <- delete_vertices(graphFB, c(coreNode, personalNetNode))
  if(length(mutualFriendsNet) >= 2){
    combs <- combn(mutualFriendsNet, 2)
    for(i in 1 : ncol(combs)){
      u = combs[1,i]
      v = combs[2,i]
      dispersion <- dispersion + distances[u,v]
    }
  }
  else{
    dispersion <- 0
  }
  return(dispersion)
}

# Plot histogram for the argument vector
plotHistogram <- function(plotVector, answerString, xlabel, ylabel, title, step, color){
  png(answerString)
  hist(plotVector, breaks = step, main = title, xlab = xlabel, 
       ylab = ylabel, col=color)
  dev.off()
}

# Return community structure for input graph 
getCommunityStructure <- function(graphObj){
  return(fastgreedy.community(graphObj))
}

# Plots the community structure with highlighted nodes with max dispersion/embeddedness or division value
plotCommunityStructure <- function(personalNetwork, answerString, coreNode,
                       hlNode, title,
                       vertexSize, hlVertexSize, hlColor, edgeSize, 
                       edgeColor, hlEdgeSize){
  # Community structure for personal network
  communityStructure <- getCommunityStructure(personalNetwork)
  # Vertex size and color
  vertexSizeVec <- rep(vertexSize, vcount(personalNetwork))
  vertexColorVec <- communityStructure$membership
  # Edge size and color
  edgeSizeVec <- rep(edgeSize, ecount(personalNetwork))
  edgeColorVec <- rep(edgeColor, ecount(personalNetwork))
  # Highlighted vertex
  vertexColorVec[hlNode] <- hlColor
  vertexSizeVec[hlNode] <- hlVertexSize
  #Highlighted edges
  elist = get.edge.ids(personalNetwork, c(t(ends(personalNetwork, E(personalNetwork)[from(hlNode)]))))
  edgeColorVec[elist] <- hlColor
  edgeSizeVec[elist] <- hlEdgeSize
  # Plotting
  png(answerString)
  plot.igraph(personalNetwork, vertex.size = vertexSizeVec, 
       vertex.label = NA, vertex.color = vertexColorVec, 
       edge.color = edgeColorVec, edge.width = edgeSizeVec, 
       layout = layout.fruchterman.reingold, main = title)
  dev.off()
}


getCommunityFeatures <- function(personalNetwork, coreNode){
  communityStructure <- getCommunityStructure(personalNetwork)
  communityFeatures <- NULL
  for(i in 1:length(communityStructure)){
    comNodes <- which(communityStructure$membership==i)
    if(length(comNodes) >= 10){
      community <- induced.subgraph(personalNetwork, comNodes)
      community$names <- unlist(comNodes)
      clusteringCoeff <- transitivity(community)
      commModularity <- modularity(fastgreedy.community(community))
      commDensity <- edge_density(community)
      commSize <- length(comNodes)
      commDiameter <- diameter(community)
      communityFeatures <- rbind(communityFeatures, c(commModularity, clusteringCoeff, commDensity, commSize, commDiameter))
    }
  }
  colnames(communityFeatures) <- c("Modularity Index", "Clustering Coefficient", "Density", "Community Size", "Diameter")
  return(communityFeatures)
}



################# Part 5  ##########################

# Obtain the graph
edgeList <- read.table("facebook_combined.txt", sep = " ")
colnames(edgeList) = c("Node 1", "Node 2")
graphFB <- graph.data.frame(edgeList, directed=FALSE)

# Obtain the core nodes
coreNodes <- getCoreNodes(graphFB)

# Embeddedness and Dispersion distribution
embeddedness <- c()
dispersion <- c()
for (coreNode in coreNodes){
  #print(paste("Core Node : ", coreNode))
  personalNetwork <- getPersonalNetwork(graphFB, coreNode)
  distances <- distances(graphFB)
  for (personalNetNode in personalNetwork$names){
    if(coreNode == personalNetNode)
      next
    embeddedness <- append(embeddedness, calcEmbeddedness(graphFB, coreNode, personalNetNode))
    dispersion <- append(dispersion, calcDispersion(graphFB, coreNode, personalNetNode, distances))
  }
}

# Histogram for above obtained distribution
plotHistogram(embeddedness,"Answer5Embed.png", "Embeddedness", "Frequency", "Embeddedness Distribution", 50,"blue")

plotHistogram(dispersion, "Answer5Disp.png", "Dispersion","Frequency","Dispersion Distributionn", 50,"green")


# Plotting community structure with first 3 core nodes 
coreNodeSubset = coreNodes[c(1,2,3)]
for (cNode in coreNodeSubset){
  personalNetwork <- getPersonalNetwork(graphFB, cNode)
  # Mapping core node in personal network
  coreNode <- which(personalNetwork$names == cNode)
  embeddedness <- c()
  dispersion <- c()
  distances <- distances(graphFB)
  for (personalNetNode in personalNetwork$names){
    if(coreNode == personalNetNode)
      next
    embeddedness <- append(embeddedness, calcEmbeddedness(graphFB, cNode, personalNetNode))
    dispersion <- append(dispersion, calcDispersion(graphFB, cNode, personalNetNode, distances))
  }
  # Maximum Embeddedness node
  maxEmbeddness <- personalNetwork$names[which(embeddedness == max(embeddedness))]
  maxEmbeddnessNode <- which(personalNetwork$names == maxEmbeddness)
  # Maximum Dispersion node  
  maxDispersion <- personalNetwork$names[which(dispersion == max(dispersion))]
  maxDispersionNode <- which(personalNetwork$names == maxDispersion)
  # Maximum Dispersion/Embeddedness node 
  dispEmbedVal <- dispersion/embeddedness
  dispEmbedVal[mapply(is.nan, dispEmbedVal)] <- 0
  maxDispEmbed <- personalNetwork$names[which(dispEmbedVal == max(dispEmbedVal))]
  maxDispEmbedNode <- which(personalNetwork$names == maxDispEmbed)
  # Plot graph (Max Embeddedness)
  ansString <- paste("Answer5", "MaxEmbed",cNode, ".png")
  plotCommunityStructure(personalNetwork, ansString, coreNode,
                         maxEmbeddnessNode, "Max Embeddedness highlighted Comm Str",
                         3, 8,"red", 0.3, "light grey", 0.5)
  # Plot graph (Max Dispersion)
  ansString <- paste("Answer5", "MaxDisp",cNode, ".png")
  plotCommunityStructure(personalNetwork, ansString, coreNode,
                         maxDispersionNode, "Max Dispersion highlighted Comm Str",
                         3, 8,"red", 0.3, "light grey", 0.5)
  # Plot graph (Max Dispersion/Embeddedness)
  ansString <- paste("Answer5", "MaxDispEmbed",cNode, ".png")
  plotCommunityStructure(personalNetwork, ansString, coreNode,
                         maxDispEmbedNode, "Max Dispersion/Embeddedness highlighted Comm Str",
                         3, 8,"red", 0.3, "light grey", 0.5)
}

################# Part 6  ##########################
featureVec <- NULL
print("Answer 6 Features:")
for(coreNode in coreNodes){
  personalNetwork <- getPersonalNetwork(graphFB, coreNode)
  commFeatures <- getCommunityFeatures(personalNetwork,coreNode)
  print(paste("Core node: ",coreNode))
  cat("Modularity Index: ", commFeatures[, "Modularity Index"],"\n")
  cat("Clustering Coefficient: ", commFeatures[, "Clustering Coefficient"],"\n")
  cat("Density: ", commFeatures[, "Density"],"\n")
  cat("Community Size: ", commFeatures[, "Community Size"],"\n")
  cat("Diameter: ", commFeatures[, "Diameter"],"\n")
  featureVec <- rbind(featureVec, commFeatures)
}
colnames(featureVec) <- c("Modularity Index", "Clustering Coefficient", "Density", "Community Size", "Diameter")

# Dividing into 2 clusters with above features gives the best
# between_SS / total_SS score after which it does not improve much
# So it tells which community is part of which cluster based on above
# features
clusters = kmeans(featureVec, 2)


library(cluster)
library(fpc)
plotcluster(featureVec, clusters$cluster)

