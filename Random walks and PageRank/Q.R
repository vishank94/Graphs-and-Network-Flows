###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

# Packages
library(igraph)
library(netrw)

sumdist <- 0
sumsd <- 0
avgdist <- 0
avgsd <- 0

# utility functions
plotUtilty <- function(graph,nodes,damping_factor,answerString,messageString)
{
  avg <- rep(NA,nodes)
  stdev <- rep(NA,nodes)
  for(i in (1:nodes))
  {
    r1<- netrw(graph, walker.num=nodes,start.node=1:vcount(graph), damping=damping_factor, T=i, output.walk.path=TRUE)
    sp <- rep(NA,nodes)
    for(j in (1:nodes))
    {
      a <-get.shortest.paths(graph, from=r1$walk.path[1,j], to=r1$walk.path[i,j])
      sp[j]<-length(a$vpath[[1]])-1
    }
    avg[i] = mean(sp)
    stdev[i] = sd(sp) 
    sumdist <- sum(sumdist,avg[i])
    sumsd <- sum(sumsd,stdev[i])
    
  }
  
  #plots
  name <- paste('Average distance',answerString, '.png')
  png(name)
  plot(1:nodes,avg,type="line",xlab="Number of Steps",ylab="Average distance", col="blue")
  name <- paste("Average distance",messageString)
  title(name)
  dev.off()
  
  name <- paste('Standard Deviation', answerString, '.png')
  png(name)
  plot(1:nodes,stdev,type="line",xlab="Number of Steps",ylab="Standard Deviation of distance", col="red")
  name <- paste("Standard Deviation of distance", messageString)
  title(name)
  dev.off()

  #part e
  name <- paste('Histogram', answerString, '.png')
  png(name)
  hist(degree(graph),xlab="Degree",ylab="Frequency",main="Degree Distribution of the graph")
  degdist <- rep(NA,nodes)
  for(i in (1:nodes))
    degdist[i] = degree(graph,r1$walk.path[nodes,i])
  hist(degdist,xlab="Degree",ylab="Frequency",main="Node degree distribution at the end of the random walk")
  dev.off()
}

plotMeasures <- function(graph,nodes,damping_factor,answerString, messageString)
{
  r1<- netrw(graph, walker.num=nodes,start.node=1:vcount(graph), damping=damping_factor, T=1000, output.walk.path=TRUE)
  
  name <- paste("Visit probability", answerString, ".png")
  png(name)
  plot(r1$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",pch=1, col="green")
  name <- paste("Visit probability for graph", messageString, "v/s degree")
  title(name)
  dev.off()
  
  degprob<- c()
  visitprob <- c()
  for(i in r1$walk.path[,1])
  {
    degprob<-append(degprob,degree(graph,i))
    visitprob<-append(visitprob,r1$ave.visit.prob[i])
  }
  
  final_measure<- cbind(degprob,visitprob)
  final_measure<- final_measure[order(final_measure[,1]),]
  
  name <- paste("Avg. visit probability", answerString, ".png")
  png(name)
  plot(final_measure[,1],final_measure[,2],xlab="Degree",ylab="Average Visit Probablity", col="blue")
  name <- paste("Avg. visit probability for graph", messageString, "v/s degree")
  title(name)
  dev.off()
  return(visitprob)
}

simulatePageRank <- function(graph,nodes,damping_factor,vector,answerString,messageString)
{
  r1<-netrw(graph, walker.num=nodes,start.node=1:vcount(graph),damping=damping_factor,T=1000, output.walk.path=TRUE,teleport.prob=vector)
  
  degprob<- c()
  visitprob <- c()
  for(i in r1$walk.path[,1])
  {
    degprob<-append(degprob,degree(graph,i))
    visitprob<-append(visitprob,r1$ave.visit.prob[i])
  }
  final_measure<- cbind(degprob,visitprob)
  
  name <- paste("Avg. visit probability", answerString, '.png')
  png(name)
  plot(1:1000,final_measure[,2],xlab="Number of nodes",ylab="Average Visit Probablity", col="red")
  name <- paste("Avg. visit probability for graph", messageString, "v/s nodes")
  title(name)
  dev.off()
  return(visitprob)
}  


combineGraphsPageRank<- function(graph1,graph2,nodes,damping_factor,answerString, messageString )
{
  r1<-netrw(graph1, walker.num=nodes,start.node=1:vcount(graph1),damping=damping_factor,T=1000, output.walk.path=TRUE)
  pagerank1<-page.rank(graph1,directed=TRUE,damping=0.85)
  r2<-netrw(graph2, walker.num=nodes,start.node=1:vcount(graph2),damping=damping_factor,T=1000, output.walk.path=TRUE,teleport.prob=pagerank1$vector)
  
  degprob<- c()
  visitprob <- c()
  final_measure<-c()
  for(i in r1$walk.path[,1])
  {
    degprob<-append(degprob,degree(graph1,i))
    visitprob<-append(visitprob,r1$ave.visit.prob[i])
  }
  final_measure<- cbind(degprob,visitprob)

  plot(1:1000,final_measure[,2],xlab="Number of nodes",ylab="Average Visit Probablity")
  
  degprob1<- c()
  visitprob1 <- c()
  final_measure1<-c()
  for(i in r2$walk.path[,1])
  {
    degprob1<-append(degprob1,degree(graph2,i))
    visitprob1<-append(visitprob1,r2$ave.visit.prob[i])
  }
  final_measure1<- cbind(degprob1,visitprob1)
  
  points(1:1000,final_measure1[,2],col="red")
}

################ Answer 1 ################

#part 1.a)
graph1000 <- random.graph.game(1000, 0.01, directed=FALSE)


#part 1.b)
plotUtilty(graph1000,1000,1,"Answer 1b","with 1000 nodes") #1000 nodes


#part 1.c)
plotUtilty(graph1000,1000,1,"Answer 1c","with 1000 nodes") #1000 nodes


#part 1.d)
graph100 <- random.graph.game(100, 0.01, directed=FALSE)
plotUtilty(graph100,100,1,"Answer 1d", "with 100 nodes") #100 nodes

graph10000<- random.graph.game(10000, 0.01, directed=FALSE)
plotUtilty(graph10000,10000,1,"Answer 1d", "with 10000 nodes") #10000 nodes


print(paste("Diameter for graph with 100 nodes is:",diameter(graph100)))
print(paste("Diameter for graph with 10000 nodes is:",diameter(graph10000)))




################ Answer 2 ################

#part 2.a)
graph1000Barabasi = barabasi.game(n = 1000, power = -3, directed = FALSE)

#part 2.b)
plotUtilty(graph1000Barabasi,1000,1,"Answer 2b","using barabasi (1000 nodes)")

#part 2.d)
#100 NODES
graph100Barabasi = barabasi.game(100,directed=FALSE)

print(paste("Diameter for graph with 100 nodes with barabasi is:",diameter(graph100Barabasi)))

#10000 NODES
graph10000Barabasi = barabasi.game(10000,directed=FALSE)

#Random walk
plotUtilty(graph100Barabasi,100,1,"Answer 2d","Fat tailed network with 100 nodes")

#part 2.e) is being calculated in the function
plotUtilty(graph1000Barabasi,1000,1, "Answer 2e", "network with 1000 nodes")


################ Answer 3 ################

#part 3.a)
graph1000 <- random.graph.game(1000, 0.01, directed=FALSE);
visit_probability <- plotMeasures(graph1000,1000,1,"Answer 3a", "network(1000 nodes)")
network_degree = degree(graph1000)


#part 3.b)
graph1000Directed <- random.graph.game(1000, 0.01, directed=TRUE);
visit_probability <- plotMeasures(graph1000Directed,1000,1,"Answer 3b", ":1000 nodes (directed)")
network_degree = degree(graph1000Directed)


#part 3.c)
visit_probability <- plotMeasures(graph1000,1000,0.85,"Answer 3c", "network(1000 nodes)")
network_degree = degree(graph1000)


################ Answer 4 ################
#part 4.a)
graph1000Directed <- random.graph.game(1000, 0.01, directed=TRUE)
simulatePageRank(graph1000Directed,1000,0.85,NULL,"Answer 4a", "network(1000 nodes)")
pagerank1 <- page.rank(graph1000Directed,directed=TRUE,damping=0.85)


#part 4.b)
graph1000Tele <- random.graph.game(1000, 0.01, directed=TRUE)
simulatePageRank(graph1000Tele,1000,0.85,pagerank1$vector, "Answer 4b_PR", "network(1000 nodes)" )
combineGraphsPageRank(graph1000Directed,graph1000Tele,1000,0.85, "Answer 4b_PersonalizedPR", "network(1000 nodes)")




