###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(igraph)
library(MASS)
library(ggplot2)

# Set working directory to source file location

# Read data from file and load in memory
fdata <- read.table("facebook_combined.txt", sep = " ")

# Generate graph from fdata - facebook network is undirected (bi)
fgraph <- graph.data.frame(fdata, directed=FALSE)

# Get the input undirected graph (Alternative method to generate graph)
fgraph_d  <- read.graph("facebook_combined.txt", directed=FALSE)

print("Question No. 1 -----------------------------------")

#################Question 1#################

isConnected <- is.connected(fgraph)
print(paste("Answer 1a: The graph is connected:",isConnected))

#-------------------------------------------

# diameter of network
fdiameter <- diameter(fgraph)
print(paste("Answer 1b: The diameter of the graph is:",fdiameter))

#-------------------------------------------

# degree distribution of network
png("Degree Distribution of facebook_combined graph (Line Graph).png")
p = plot(degree.distribution(fgraph), xlab="Node/Vertex Degree", ylab ="Degree Distribution", main = "Degree Distribution of facebook_combined graph", col ="red", cex = .4)
dev.off()

png("Degree Distribution of facebook_combined graph (Histogram).png")
fdegree <- degree(fgraph)
h = hist(fdegree, breaks = seq(from = min(fdegree), to = max(fdegree), by=1), border ="red", cex = .4, main = "Degree Distribution of facebook_combined graph", xlab = "Node/Vertex Degree")
dev.off()

#-------------------------------------------

# average degree of network
print(paste("Answer 1c: The average degree of the network:",mean(fdegree)))

# fitting multiple curves and checking for least MSE
fdf = data.frame(x=h$mids, y=h$density)

models = list(
  nls(y ~ (1/x*a) + b*x, data = fdf, start = list(a = 0, b = 0), trace=T), 
  nls(y ~ (a + b*log(x)), data = fdf, start = list(a = 0, b = 0), trace=T),
  nls(y ~ (exp(a + b * x)), data = fdf, start = list(a = 0, b = 0), trace=T),
  nls(y ~ (1/x*a)+b, data = fdf, start = list(a = 1, b = 1), trace=T),
  nls(y ~ (exp(1)^(a + b * x)), data = fdf, start = list(a =0, b = 0), trace=T))

# fitting power law distribution
fplot <- ggplot(fdf, aes(x, y)) + geom_point(size = 1.2) +
  geom_line(aes(x,fitted(models[[1]])),size = 1,colour = "yellow4") + 
  geom_line(aes(x,fitted(models[[2]])),size = 1, colour = "seagreen2") +
  geom_line(aes(x,fitted(models[[3]])),size = 1,  colour = "red") +
  geom_line(aes(x,fitted(models[[4]])),size = 1,  colour = "coral2")+
  geom_line(aes(x,fitted(models[[5]])),size = 1,  colour = "deepskyblue1")+
  ggtitle("Fitted curves for degree distribution")+ xlab("Nodes") +ylab("Degree Distribution")

summary(models[[5]])

fplot <- fplot + theme_bw() + theme(legend.background = element_rect(fill="grey95", colour=NA)) + theme(legend.position="top") + scale_colour_manual("", breaks = c("TempMax", "TempMedia", "TempMin"), values = c("red", "green", "blue"))
fplot <- fplot + scale_fill_continuous(guide = "legend")
png("fplot.png")
print(fplot)
dev.off()


print("Question No. 2 -----------------------------------")
#################Question 2#################SEEEEEEEEEEEEEEEEEE1E

#-------------------------------------------

node1 = neighborhood(fgraph, order = 1, nodes = 1)
personal_node1 = induced.subgraph(fgraph, vids = unlist(node1), impl = "auto")

# Giving attribute "names" to the object personal_node1; use attributes(obj) to return
personal_node1$names = sort(unlist(node1))
# Number of vertices in personal network of Node 1
print(paste("Answer 2a: Size of Neighbors of Node 1 :",vcount(personal_node1)))
# Number of edges in personal network of Node 1
print(paste("Answer 2b: Number of Edges in Personal Network :",ecount(personal_node1)))

# Plot the personal network of node 1
node_color = rep("red",vcount(personal_node1))
node_size = rep(2, vcount(personal_node1))
node_color[personal_node1$names == 1] = "green"
node_size[personal_node1$names == 1] = 5
plot(personal_node1 , vertex.size = node_size, vertex.color=node_color , vertex.label=NA , asp=9/21, layout = layout.fruchterman.reingold)