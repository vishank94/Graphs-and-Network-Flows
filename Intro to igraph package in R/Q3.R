###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################


###########################################################################################
#Part 3 : Creates a random graph by simulating its evolution
###########################################################################################

g <- aging.barabasi.game(n = 1000,pa.exp = 1,  directed = FALSE, aging.exp = 1, aging.bin = 10)

png("Answer-3a.png")
hist(degree(g), xlab="node degree", ylab = "degree distribution",
     main= "Degree-distribution Preferential Attachment", col ="blue", breaks=c(0:20)*5)
dev.off()

commCluster <- fastgreedy.community(g, merges = TRUE, modularity = TRUE)
print(paste(" 3b:Modularity",modularity(commCluster)))
print(paste(" 3b:No of communitiues",length(sizes(commCluster))))

png("Answer 3b.png")
plot(commCluster, g,vertex.label = NA)
dev.off()