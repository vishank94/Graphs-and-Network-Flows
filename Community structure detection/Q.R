###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(igraph)
library(netrw)

# Read edgeList from file and load in memory
edgeList <- read.table("sorted_directed_net.txt", sep = "\t")
# Graph from edgeList
colnames(edgeList) = c("Node 1", "Node 2", "weights")
graphDirected <- graph.data.frame(edgeList, directed=TRUE)

######## Part 1 ###############

isConnected <- is.connected(graphDirected)
print(paste("Answer 1: Graph is connected:",isConnected))

gccNodeList <- c()

# To find the GCC if graph is not connected 
if(!isConnected){
  cl <- clusters(graphDirected)  #, mode="strong"
  print(paste("Answer 1: No of clusters:",cl$no))
  print(paste("Answer 1: Size of GCC:",max(cl$csize)))
  gccIndex = which.max(cl$csize)
  
  count = 0;
  for(i in 1:length(V(graphDirected))){
    if(cl$membership[i] == gccIndex){
      gccNodeList <- append(gccNodeList, i)
    }
  }
  gcc <- induced.subgraph(graphDirected, gccNodeList)
}


######## Part 2 ###############

# Utility function
plotHistogram <- function(vec,answerString,xlabel,ylabel,title,color,step){
  png(answerString)
  hist(vec, breaks = seq(from = min(vec), to = max(vec), by=step),  main = title, xlab = xlabel, ylab = ylabel, border=color,col=color)
  dev.off()
}

# Solution
inDegree = degree(graphDirected,v=gccNodeList, mode="in")
outDegree = degree(graphDirected,v=gccNodeList, mode="out")

plotHistogram(inDegree, "Answer2_inDegree.png","In Degree", "Density","In Degree Distirbution of GCC","blue",1)
plotHistogram(outDegree, "Answer2_outDegree.png","Out Degree", "Density","Out Degree Distirbution of GCC","green",1)


######## Part 3 ###############

# Utility function
weightSqrtFun <- function(weight) sqrt(prod(weight))

# Solution

#Option 1
graphUndirectedEach = as.undirected(gcc, mode = "each")
#Label Propagation
communityStructureLabel1 = label.propagation.community (graphUndirectedEach, weights =E(graphUndirectedEach)$weights)
print(paste("Answer 3 Option 1(Unchanged Edges):Modularity",modularity(communityStructureLabel1)))
print(paste("Answer 3 Option 1(Unchanged Edges):No of communitiues",length(sizes(communityStructureLabel1))))
print(paste("Answer 3 Option 1(Unchanged Edges):Community sizes"))
print(sizes(communityStructureLabel1))


#Option 2
graphUndirectedCollapse = as.undirected(gcc, mode = "collapse", edge.attr.comb=weightSqrtFun)
#Label Propagation
communityStructureLabel2 = label.propagation.community (graphUndirectedCollapse, weights =E(graphUndirectedCollapse)$weights)
print(paste("Answer 3 Option 2(Collapsed Edges) with Label Propagation:Modularity", modularity(communityStructureLabel2)))
print(paste("Answer 3 Option 2(Collapsed Edges) with Label Propagation:No of communitiues", length(sizes(communityStructureLabel2))))
print(paste("Answer 3 Option 2(Collapsed Edges) with Label Propagation:Community sizes"))
print(sizes(communityStructureLabel2))

#Fast Greedy
communityStructureGreedy = fastgreedy.community(graphUndirectedCollapse, weights=E(graphUndirectedCollapse)$weights)
print(paste("Answer 3 Option 2(Collapsed Edges) with FastGreedy:Modularity",modularity(communityStructureGreedy)))
print(paste("Answer 3 Option 2(Collapsed Edges) with FastGreedy:No of communitiues",length(sizes(communityStructureGreedy))))
print(paste("Answer 3 Option 2(Collapsed Edges) with FastGreedy:Community sizes"))
print(sizes(communityStructureGreedy))

#weight function
sqrtWt = function(w) sqrt(prod(w))

#load the data and created the directed graph 
rawData = read.table("sorted_directed_net.txt", 
                     sep = "\t", 
                     header = FALSE) 
colnames(rawData) = c("Node 1", "Node 2", "weights")
directedGraph = graph.data.frame(rawData, directed=TRUE)

#find the giant connected component
graphClusters = clusters(directedGraph)  
gccIndex = which.max(graphClusters$csize) 
nonGccNodes = (1:vcount(directedGraph))[graphClusters$membership != gccIndex] 
gcc = delete.vertices(directedGraph, nonGccNodes)

#create the undirected graph and find community structure using fast greedy
undirectedGraph = as.undirected(gcc, 
                                mode = "collapse", 
                                edge.attr.comb = sqrtWt)
community = fastgreedy.community(undirectedGraph, 
                                 weights = E(undirectedGraph)$weights)

# find the sub Giant Connected Component
indexSubGcc = which.max(sizes(community))
nonSubGccNodes = (1:vcount(undirectedGraph))[community$membership != indexSubGcc]
subGcc = delete.vertices(undirectedGraph, nonSubGccNodes)

#find community for the sub GCC using the fast greedy algorithm
communitySubGcc = fastgreedy.community(subGcc, 
                                       weights=E(subGcc)$weights)
print("Part 4:")
print(modularity(communitySubGcc))
print(sizes(communitySubGcc))


#####################################################################################################
##                                        Part 5                                                   ##
#####################################################################################################
communitySize100 = which(sizes(community)>100)
for (i in 1:length(communitySize100)) {
  print(cat("Community no:",i))
  delNodes = (1:vcount(undirectedGraph))[community$membership != communitySize100[i]]
  comNodes = delete.vertices(undirectedGraph, delNodes)
  subGcc = fastgreedy.community(comNodes,
                                weights = E(comNodes)$weights)
  print("Modularity is:")
  print(modularity(subGcc))
  print("Structure is:")
  print(sizes(subGcc))
}




######## Part 6 ##########################################################################################

# Random walk for overlapping communities using personalised page rank

#Step1: Find visit probability of each node
network_data = read.table("sorted_directed_net.txt", sep = "\t") # read sorted directed network
colnames(network_data) = c("Node 1", "Node 2", "Edge Weight")
directed_network = graph.data.frame(network_data, directed=TRUE)
overlap_comm1 <- numeric(0)
overlap_comm2 <- numeric(0)


for (i in 1:vcount(directed_network)) {
  tele_prob = rep(0, vcount(directed_network))
  tele_prob[i] = 1
  page_rank = netrw(directed_network , walker.num = 1, start.node = i, damping = 0.85, teleport.prob = tele_prob, output.visit.prob = TRUE) # random walk on node_i
  visit_prob = page_rank$ave.visit.prob
  priority_visit_prob = sort(visit_prob, decreasing = TRUE, index.return = TRUE)


  top1_i = rep(0, length(communityStructureLabel1))
  top2_i = rep(0, length(communityStructureLabel2))


  #top 30
  for (j in 1:30)
  {
    top1_j = rep(0, length(communityStructureLabel1))
    top1_j[communityStructureLabel1$membership[which(V(graphUndirectedEach) == V(directed_network)[priority_visit_prob$ix[j]])]] = 1
    top1_i = top1_i + priority_visit_prob$x[j] * top1_j

    top2_j = rep(0, length(communityStructureLabel2))
    top2_j[communityStructureLabel2$membership[which(V(graphUndirectedCollapse) == V(directed_network)[priority_visit_prob$ix[j]])]] = 1
    top2_i = top2_i + priority_visit_prob$x[j] * top2_j

  }
  if (length(which(top1_i > 0.1)) >= 2) { # if there are more than 2 communities with this value
    overlap_comm1 = rbind(overlap_comm1, c(i, top1_i)) # store results and community score
  }
  if (length(which(top2_i > 0.1)) >= 2) { # if there are more than 2 communities with this value
    overlap_comm2 = rbind(overlap_comm2, c(i, top2_i)) # store results and community score
  }

}