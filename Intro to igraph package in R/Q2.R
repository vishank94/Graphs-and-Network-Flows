###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################


###########################################################################################
#Part 2 : Fat- tailed degree distribution 
###########################################################################################


##### using Barabasi ######
rm(list = ls())
library(igraph)

g = barabasi.game(n = 1000, power = -3, directed = FALSE)
png("Answer 2a.png")
hist(degree(g), xlab="node degree", ylab = "degree distribution",
    main= "Fat Tailed Degree Distribution", col ="blue")
print(paste("Diameter using barabasi method is:",diameter(g)))
dev.off()



###### Using sample_degseq ######
s = 999 ** 2
sprime = (s - 1) / s

getDist = function(nv) {
  ds = c()
  for(u in runif(nv)) {
    ds = c(ds, floor(sqrt(1 / (1 - sprime * u))))
  }
  
  ds
}

ds = getDist(1000)
while(sum(ds) %% 2 != 0){
  ds = getDist(1000)
}

g1 = sample_degseq(ds, method="simple.no.multiple")
png("Answer 2a_degseq.png")
hist(degree(g1), xlab="node degree", ylab = "degree distribution",
     main= "Fat Tailed Degree Distribution", col ="green")
print(paste("Diameter using degseq method is:",diameter(g1)))
dev.off()


############################################################################################
#2b : giant connected compo- nent (GCC) and use fast greedy method to find 
#the community structure.
############################################################################################


##### using Barabasi ######

print("Using Barabasi method\n")
isConnected <- is_connected(g)
print(paste("2b: Graph is connected:",isConnected))

##to find the GCC 
cl <- clusters(g)
print(paste("No of clusters",cl$no))
#only 1 cluster- GCC is the whole graph

commCluster <- fastgreedy.community(g, merges = TRUE, modularity = TRUE)
print(paste(" 2b:Modularity",modularity(commCluster)))
print(paste(" 2b:No of communitiues",length(sizes(commCluster))))

png("Answer 2b.png")
plot(commCluster, g,vertex.label = NA)
dev.off()



###### Using sample_degseq ######

print("Using degSeq method\n")
isConnected <- is_connected(g1)
print(paste("2b: Graph is connected:",isConnected))

##to find the GCC 
cl <- clusters(g1)
print(paste("No of clusters",cl$no))
#222 clusters - Not connected

commCluster <- fastgreedy.community(g1, merges = TRUE, modularity = TRUE)
print(paste(" 2b:Modularity",modularity(commCluster)))
print(paste(" 2b:No of communitiues",length(sizes(commCluster))))

png("Answer 2b_degseq.png")
plot(commCluster, g1,vertex.label = NA)
dev.off()



#############################################################################################
#2c : generate a larger network with 10000 nodes whose degree distribution
#is proportional to x−3.
##############################################################################################

##### using Barabasi ######

print("Using Barabasi method")
g2 = barabasi.game(n = 10000, power = -3, directed = FALSE)

commCluster2 <- fastgreedy.community(g2, merges = TRUE, modularity = TRUE)
print(paste("2c:Modularity",modularity(commCluster2)))
print(paste(" 2c:No of communitiues",length(sizes(commCluster2))))

png("Answer 2c.png")
plot(commCluster2, g2,vertex.label = NA)
dev.off()

###### Using sample_degseq ######

print("Using degSeq method")
s = 9999 ** 2
sprime = (s - 1) / s

getDist = function(nv) {
  ds = c()
  for(u in runif(nv)) {
    ds = c(ds, floor(sqrt(1 / (1 - sprime * u))))
  }
  
  ds
}

ds = getDist(10000)
while(sum(ds) %% 2 != 0){
  ds = getDist(10000)
}

g3 = sample_degseq(ds, method="simple.no.multiple")

commCluster2 <- fastgreedy.community(g3, merges = TRUE, modularity = TRUE)
print(paste("2c:Modularity",modularity(commCluster2)))
print(paste(" 2c:No of communitiues",length(sizes(commCluster2))))

png("Answer 2c_degSeq.png")
plot(commCluster2, g3,vertex.label = NA)
dev.off()

##########################################################################################
#2d : You can randomly pick a node i, and then randomly pick a neighbor j of that node.
#Measure and plot the degree distri- bution of nodes j that are picked with this process.
##########################################################################################

# Generating Randome Graphs using BA model
n <- 10000
g3 <- barabasi.game(n = 10000, power = -3, directed = FALSE) 
ddn <- numeric(0)

for (i in 1:10000) {
  i <- sample(n, 1, replace = FALSE, prob = NULL)
  ni <- neighbors(g3, i)
  if (length(ni) == 1) {j <- neighbors(g3, i)}
  else {j <- sample(ni, 1, replace = FALSE, prob = NULL)}
  ddn <- c(ddn, degree(g3, j))
}

hist(ddn, main ='Degree Distribution of jth node using barabasi.game()',  breaks = seq(from = min(ddn), to = max(ddn), by=1), xlab='Degree', ylab='Frequency')

# Generating Random Graphs using given degree sequence
pp = 999 ** 2
pnew = (pp - 1) / pp

distribution = function(var) {
  dis = c()
  for(ind in runif(var)) {
    dis = c(dis, floor(sqrt(1 / (1 - pnew * ind))))
  }
  
  dis
}

dis = distribution(1000)
while(sum(dis) %% 2 != 0){
  dis = distribution(1000)
}

graph = sample_degseq(dis, method="simple.no.multiple")
hist(
  degree(graph, mode="all"),
  col=colors,
  main="Degree Distribution of jth node using sample_degseq()",
  xlab="Degree",
  ylab="Frequency"
)