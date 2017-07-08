###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

#####################################################################################################
##                                        Part 3 and 4                                             ##
#####################################################################################################

#load the libraries required
library(igraph)
library(MASS)
#clean up the environment variables 
rm(list=ls())


#####################################################################################################
##                                  Utility functions                                              ##
#####################################################################################################

# Function to plot the community for question 3 and 4
pltCommunity = function(fbNetwork, core, fileName){
  ndColor = rep("lightblue",vcount(fbNetwork))
  ndSize = rep(2, vcount(fbNetwork))
  
  if(core){
    ndColor[fbNetwork$names == coreNode] = "red"
    ndSize[fbNetwork$names == coreNode] = 4
  }
  name = paste(fileName,"PersonalNetwork.png",sep="")
  png(name)
  plot(fbNetwork , vertex.size = ndSize, vertex.color=ndColor , vertex.label=NA , asp=9/16)
  dev.off()
  # fast greedy community alforithm 
  fastGreedyCom = fastgreedy.community(fbNetwork)
  pltGraph(fbNetwork, fastGreedyCom, "fastGreedy",fileName,ndSize)
  # edge betweenness community Algorithm 
  edgeBetweenCom = edge.betweenness.community(fbNetwork)
  pltGraph(fbNetwork, edgeBetweenCom, "edgeBetween",fileName,ndSize)
  # infomap community Algorithm 
  infoComm = infomap.community(fbNetwork)
  pltGraph(fbNetwork, infoComm, "infoComm",fileName,ndSize)
}

pltGraph = function(fbNetwork, commAlgo, commName,fileName,ndSize){
  print(paste("modularity",commName,": ",modularity(commAlgo)))
  print(paste("sizes",commName,":",sizes(commAlgo)))
  name = paste(fileName,commName,".png",sep="")
  png(name)
  plot(commAlgo, fbNetwork, vertex.size=ndSize , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
  dev.off()
  name = paste(fileName,commName,"2.png",sep="")
  png(name)
  plot(fbNetwork , vertex.size=ndSize , vertex.label=NA , vertex.color=commAlgo$membership, asp=9/16, layout=layout.fruchterman.reingold)
  dev.off()
}


#####################################################################################################
##                                        Part 3                                                 ##
#####################################################################################################

network = read.graph("facebook_combined.txt", directed=FALSE)
ntwDeg = degree(network)
coreNode = 0

#Find the core nodes in the network (nodes with the number of neighbours as: 200)
coreNds = numeric()
for (i in V(network)) {
  if (length(neighbors(network, i)) > 200)
    coreNds = c(coreNds, i)
} 

avgDeg = ntwDeg[coreNds]
cat("3a:Number of Core Nodes in the netwrok: ", length(coreNds))
cat("3a:Average Degree of Core Nodes in the network: ", mean(avgDeg))
print("3a:Core Nodes:")
print(coreNds)
coreNode = 1  

ntwkCoreNd = neighborhood(network, order = 1, nodes = coreNode)
ntwkCorePersonal = induced.subgraph(network, unlist(ntwkCoreNd))
ntwkCorePersonal$names = sort(unlist(ntwkCoreNd))
print(paste("3b: Size of Neighbors of Core Node :" , vcount(ntwkCorePersonal)))
print(paste("Number of Edges in Personal Network  :", ecount(ntwkCorePersonal)))
print("Output for 3b:")
pltCommunity(ntwkCorePersonal, TRUE, "Part3")


#####################################################################################################
##                                        Part 4                                                   ##
#####################################################################################################
ntwkNonCore = unlist(ntwkCoreNd)[(unlist(ntwkCoreNd) != coreNode)]
ntwkNonCorePersonal = induced.subgraph(network, vids = ntwkNonCore)

cat("Part 4: Size of Neighbors without core:" , vcount(ntwkNonCorePersonal))
cat("Part 4: Number of Edges in Personal Network without core :", ecount(ntwkNonCorePersonal))
print("Output for 4:")
pltCommunity(ntwkNonCorePersonal, FALSE, "Part4")
