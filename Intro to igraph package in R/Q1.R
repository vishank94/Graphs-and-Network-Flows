###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################


###########################################################################
#Part 1 - a and b
#Create three undirected random networks with 1000 nodes, 
#and the probability p for drawing an edge between two arbi- trary vertices 0.01, 0.05 and 0.1 respectively. 
#Plot the degree distributions.
##########################################################################

rm(list = ls())
library(igraph)

prob <- c(0.01,0.05,0.1)
graphLabel <- "Degree Distribution Probability for drawing edge = "
  

for (p in prob){
  g <- erdos.renyi.game(1000, p, directed = FALSE)
  label <- paste(graphLabel, p, sep = " ")
  name<- paste(label,".png", sep ="")
  #png(name)
  plot(degree.distribution(g), xlab="Node Degree", ylab ="Degree Distribution", main = label, col ="red", cex = .4)
  deg1 <- degree(g)
  #png(name)
  hist(deg1, breaks = seq(from = min(deg1), to = max(deg1), by=1), main = "Degree Distribution for Random Undirected Graph (with p=0.01)", xlab = "Number of Degrees")
  lines(degree.distribution(g) ,col = "red")
  #dev.off()
  
  print(paste("Probabilty is:",p))
  print(paste("Diameter is:",diameter(g)))
  isConnected <- is.connected(g)
  print(paste("Graph is connected:",isConnected))
}

##########################################################################
#Part c - find Pc value 
##########################################################################
p <- 0.010
pc_value <- 0
while(TRUE){
  g <- erdos.renyi.game(1000, p, directed = FALSE)
  if(!is_connected(g)){
    pc_value <- p
    break
  }
  p <- p-0.001
}

print(paste("Pc value is",pc_value))