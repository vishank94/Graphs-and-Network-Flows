###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################


###########################################################################################
#Part 4: Use the forest fire model to create a directed network 
###########################################################################################
library(igraph)
g <- forest.fire.game(1000, fw.prob=0.37, bw.factor=0.32/0.37)
png("Answer 4a-1.png")
dd1 <- degree_distribution(g, mode="in")
dd2 <- degree_distribution(g, mode="out")
plot(seq(along=dd1)-1, dd1, log="xy")
points(seq(along=dd2)-1, dd2, col=2, pch=2)
dev.off()

png("Answer 4a InDegree.png")
hist(degree(g,m="in"),main="Forest Fire In Degree Distribution",
     xlab="In-Degree", ylab="No of Nodes",col="blue")
dev.off()

png("Answer 4a OutDegree.png")
hist(degree(g,m="out"),main="Forest Fire Out Degree Distribution",
     xlab="Out-Degree", ylab="No of Nodes",col="blue")
dev.off()


print(paste("4b:Diameter is:",diameter(g)))

commCluster <- spinglass.community(g)
print(paste(" 4c:Modularity",modularity(commCluster)))
print(paste(" 4c:No of communitiues",length(sizes(commCluster))))

png("Answer 4c.png")
plot(commCluster, g,vertex.label = NA)
dev.off()