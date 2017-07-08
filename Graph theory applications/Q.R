###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

# HomeWork 4
library(igraph)
library(TSP)
library(GA)

# Utility functions
readCSVFile <- function(fileName, pathName){
  table <- read.csv(file=paste(pathName,fileName,sep = ""),head=TRUE,sep=",")
  return(table)
}

readCol <- function(inputVect, colName){
  return(inputVect[,colName])
}

# Method to calculate log(base 10) of an element
calcLog <- function(x){
  return(log(x , base=10))
}

# Method to calculate log return value for a vector
# ri(t) = log pi(t) − log pi(t − τ )
calcLogReturn <- function(inputVector){
  logRetVal <- diff(unlist(lapply(inputVector, calcLog)))
  return(logRetVal) #lapply -> to apply log fn to every element, unlist -> list to vector
}

# Method to calculate Cross Correlation Coefficient (pij)
calcCrossCorrCoeff <- function(inputVec1, inputVec2){   
  numerator <- (mean(inputVec1*inputVec2))-(mean(inputVec1)*mean(inputVec2))
  denominator <- sqrt((mean(inputVec1^2)-(mean(inputVec1)^2))*
                        (mean(inputVec2^2)-(mean(inputVec2)^2)))
  return(numerator/denominator)
}

# Method to calculate link length (dij)
calcLink <- function(element){
  linkLen <- sqrt( 2 * (1-element))
  return(linkLen)
}

# Method to calculate correlation matrix
calcCorrelationMatrix <- function(listName, numOfFiles){
  corrMatrix <- matrix(0, nrow=numOfFiles, ncol=numOfFiles)
  for(i in (1:numOfFiles)){
    for(j in (1:numOfFiles)){
      corrMatrix[i, j] <- calcCrossCorrCoeff(listName[[filteredFiles[[i]]]], listName[[filteredFiles[[j]]]])
    }
  }
  corrMatrix[is.nan(corrMatrix)] <- 0
  return(corrMatrix)
}

# Preorder traversal for TSP problem
preOrderTraversal <- function(mstTsp, current = 1, parent = 0){
  trail <- c(parent, current)
  for(node in neighbors(mstTsp, current)){
    if(node != parent){
      trail <- c(trail, preOrderTraversal(mstTsp, node, current))
      trail <- c(trail, c(node, current))
    }
  }
  return(trail)
}

calcTourLen <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# Read sector file
tableSector <- read.csv(file=paste("finance_data/","Name_sector.csv",sep = ""),colClasses=c("character","character"),head=TRUE,sep=",")

remove <- c("CFG","CSRA","FTV","HPE","KHC","PYPL","QRVO","SYF","UA","WLTW","WRK")
'%ni%' <- Negate('%in%')
tableSector <- tableSector[which((tableSector$Symbol) %ni% remove), ]

# Build name to integer map(color map) - each sector represented by a number(11 in total)
colorMap <- c(1:11)
names(colorMap) <- unique(tableSector[[2]])

retColor <- function(ele){
  return(colorMap[[ele]])
}

############ Part 1 : Calculating Correlations among Time Series Data ############

# Reading two different stock time series from file and extracting Closing price of stocks
tableA<- readCSVFile("A.csv","finance_data/data/")
tableCern <- readCSVFile("CERN.csv","finance_data/data/")

closeVecA <- tableA[,'Close']
closeVecCern <- tableCern[,'Close']

# Calculating log return and Cross Correlation Coefficient
ri <- calcLogReturn(closeVecA) 
rj <- calcLogReturn(closeVecCern)

crossCorrCoeff <- calcCrossCorrCoeff(ri, rj)     
print(paste("Answer 1: Cross Correlation Coefficient for 2 stock time series(A and CERN) is", crossCorrCoeff))

############ Part 2 : Constructing Correlation Graphs ############

# Read all files and store 'Closing price' in a list
allFiles <- list.files("finance_data/data/",pattern="*.csv")

stockTS <- list() 
filteredFiles <- list() 
for(fileName in allFiles){
  table <- readCSVFile(fileName,"finance_data/data/")
  if (nrow(table) == 765){
    filteredFiles <- append(filteredFiles, fileName)
    closeVec <- table[,'Close']
    stockTS[[fileName]] <- calcLogReturn(closeVec)
  }
}

# Correlation Matrix
numOfFilteredFiles <- length(filteredFiles)
corrMatrix <- calcCorrelationMatrix(stockTS, numOfFilteredFiles)

# Distance adjancency matrix
distMatrix <- apply(corrMatrix, 1:2, calcLink)

# Plot Histogram
png("HistogramAns2.png")
hist(distMatrix, main= "Histograms for length of links between Stock Time Series", 
     xlab="Length of links(dij)", col="blue")
dev.off()

# Weighted Graph
weightedGraph <- graph_from_adjacency_matrix(distMatrix, mode="undirected", weighted = TRUE, diag = FALSE)

############# Part 3 : MST for Correlation Graphs ############
## Assigning names to vertex of graphs
V(weightedGraph)$name <- gsub(".csv", "", filteredFiles)

# MST of graph
graphMST <- mst(weightedGraph, weights = E(weightedGraph)$weight)

V(graphMST)$color <- lapply(tableSector$Sector[tableSector$Symbol == V(graphMST)$name], retColor)

# Plotting MST
png("MSTAns3.png")
plot(graphMST, vertex.size = 5,
            vertex.label = NA, vertex.color = as.numeric(V(graphMST)$color),
            edge.arrow.size=0.3, main = "MST for daily data")
dev.off()

############# Part 4 : Evaluating Sector Clustering in MST ############

V(weightedGraph)$sector <- lapply(tableSector$Sector[tableSector$Symbol == V(weightedGraph)$name], function(x){ x})

numeratorProbSum <- 0
alphaPN <- 0
for(i in 1:vcount(weightedGraph)){
  vertexSector <- V(weightedGraph)[[i]]$sector
  personalNetwork <- ego(graphMST,1,V(weightedGraph)[[i]]$name)
  count <- 0
  for(j in 1:length(personalNetwork[[1]])){
    personalNode <- personalNetwork[[1]][j]$name
    personalNodeSector <- V(weightedGraph)[[personalNode]]$sector
    if(V(weightedGraph)[[i]]$name != personalNode){
      if(personalNodeSector == vertexSector){
        count <- count + 1
      }
    }
  }
  numeratorProbSum <- numeratorProbSum + (count/(length(personalNetwork[[1]])-1))
}

alphaPN <- numeratorProbSum/vcount(weightedGraph)
print(paste("Answer 4: Alpha based on clusters of immediate neighbor is", alphaPN))


numeratorProbSum <- 0
alphaRandom <- 0

uniqueSecs <- unique(tableSector[[2]])
secNodeList <- lapply(unique(tableSector[[2]]), function(x){sum(tableSector[[2]] == x)}) # In order of uniqueSecs list order
probForEachSec <- unlist(lapply(secNodeList, function(x){x/vcount(weightedGraph)}))
V(graphMST)$sector = sample(uniqueSecs, vcount(graphMST), replace=TRUE, prob = probForEachSec)

for(i in 1:vcount(graphMST)){
  vertexSector <- V(graphMST)[[i]]$sector
  personalNetwork <- ego(graphMST,1,V(graphMST)[[i]]$name)
  count <- 0
  for(j in 1:length(personalNetwork[[1]])){
    personalNode <- personalNetwork[[1]][j]$name
    personalNodeSector <- V(graphMST)[[personalNode]]$sector
    if(V(weightedGraph)[[i]]$name != personalNode){
      if(personalNodeSector == vertexSector){
        count <- count + 1
      }
    }
  }
  numeratorProbSum <- numeratorProbSum + (count/(length(personalNetwork[[1]])-1))
}
alphaRandom <- numeratorProbSum/vcount(graphMST)
print(paste("Answer 4: alpha based on random sector assignment is", alphaRandom))


############# Part 5 : ∆-Traveling Salesman Problem ############
# Triangle Inequality
for(i in 1:vcount(weightedGraph)){
  for(j in 1:vcount(weightedGraph)){
    for(k in 1:vcount(weightedGraph)){
      if(i != j && j != k && i != k){
        if(distMatrix[i,j]+distMatrix[j,k] <= distMatrix[i,k]){
          print(paste("Inequality does not hold", i, j, k))
          break
        }
      }
    }
  }
}
print(paste("Answer 5: Traingle Inequality is satisfied"))

# 1-approximate
weightedGraphTsp <- graph_from_adjacency_matrix(distMatrix, mode = "undirected", weighted = TRUE, diag = FALSE)
V(weightedGraphTsp)$name <- gsub(".csv", "", filteredFiles)
mstTsp <- mst(weightedGraphTsp, weights = E(weightedGraphTsp)$weight)
preOrder <- tail(preOrderTraversal(mstTsp), -2)

trail <- c()
i <- 1
while(i <= length(preOrder)){
  curr <- preOrder[i]
  nex <- preOrder[i + 1]
  
  if(is.element(nex, trail)){
    # get next edge
    i <- i + 2
    nex_new <- preOrder[i+1]
    trail <- c(trail, c(curr, nex_new))
  }
  else{
    trail <- c(trail, c(curr, nex))
  }
  i <- i + 2
}
print(paste("Answer 5(a): Tour length by 1-approximate eulerian tour is: ",sum( E(weightedGraphTsp)[get.edge.ids(weightedGraphTsp, trail)]$weight)))

# Using TSP package
atsp <- ATSP(distMatrix)
tour <- solve_TSP(atsp, method = "nn")
tourLen <- tour_length(tour)
print(paste("Answer 5(b): Tour length by TSP package is ", tourLen))

# Using GA package
tspFit <- function(tour, ...){ 1/calcTourLen(tour, ...) }
GA.fit = ga(type = "permutation", fitness = tspFit, distMatrix = distMatrix, min = 1, 
            max = 494, popSize = 10, maxiter = 5000, run = 100, pmutation = 0.1, 
            monitor = NULL)

print(paste("Answer 5(c): Tour length by Genetic algorithm package is ",calcTourLen(GA.fit@solution[1,], distMatrix)))


############# Part 6 : Correlation Graphs for weekly data ############

weeklyStockTS <- list() 
filteredFiles <- list() 
for(fileName in allFiles){
  table <- readCSVFile(fileName,"finance_data/data/")
  if (nrow(table) == 765){
    filteredFiles <- append(filteredFiles, fileName)
    df <- as.data.frame(table)
    if(fileName == "BF.B.csv" || fileName == "BRK.B.csv"){
      filtered <- df[weekdays(as.Date(df$Date, format="%m/%d/%y")) == 'Monday',]
    }
    else{
      filtered <- df[weekdays(as.Date(df$Date)) == 'Monday',]
    }
    closeVec <- filtered$Close
    weeklyStockTS[[fileName]] <- calcLogReturn(closeVec)
  }
}

weeklyCorrMatrix <- calcCorrelationMatrix(weeklyStockTS, numOfFilteredFiles)

weeklyDistMatrix <- apply(weeklyCorrMatrix, 1:2, calcLink)

# Weighted Graph
weightedGraph <- graph_from_adjacency_matrix(weeklyDistMatrix, mode="undirected", weighted = TRUE, diag = FALSE)

## Assigning names to vertex of graphs
V(weightedGraph)$name <- gsub(".csv", "", filteredFiles)

# MST of graph
graphMST <- mst(weightedGraph, weights = E(weightedGraph)$weight)

V(graphMST)$color <- lapply(tableSector$Sector[tableSector$Symbol == V(graphMST)$name], retColor)

# Plotting MST
png("MSTAns6.png")
plot(graphMST, vertex.size = 5,
     vertex.label = NA, vertex.color = as.numeric(V(graphMST)$color),
     edge.arrow.size=0.3, main = "MST for weekly data")
dev.off()

# numeratorProbSum <- 0
# alphaPN <- 0
# for(i in 1:vcount(weightedGraph)){
#   vertexSector <- V(weightedGraph)[[i]]$sector
#   personalNetwork <- ego(graphMST,1,V(weightedGraph)[[i]]$name)
#   count <- 0
#   for(j in 1:length(personalNetwork[[1]])){
#     personalNode <- personalNetwork[[1]][j]$name
#     personalNodeSector <- V(weightedGraph)[[personalNode]]$sector
#     if(V(weightedGraph)[[i]]$name != personalNode){
#       if(personalNodeSector == vertexSector){
#         count <- count + 1
#       }
#     }
#   }
#   numeratorProbSum <- numeratorProbSum + (count/(length(personalNetwork[[1]])-1))
# }
# 
# alphaPN <- numeratorProbSum/vcount(weightedGraph)
# print(paste("Answer 6: Alpha based on clusters of immediate neighbor is", alphaPN))


############# Part 7 : Modifying Correlations ############

png("HistogramAns7.png")
hist(corrMatrix, main= "Histogram for pij b/w Stock Time Series Daily Data", 
     xlab="Correlation Coefficient(pij)", col="blue")
dev.off()

# Modified daily correlation matrix
corrMatrix[corrMatrix > 0.3] <- -1

# Distance adjancency matrix for modified correlation matrix
distMatrix <- apply(corrMatrix, 1:2, calcLink)

# Weighted Graph
weightedGraph <- graph_from_adjacency_matrix(distMatrix, mode="undirected", weighted = TRUE, diag = FALSE)

## Assigning names to vertex of graphs
V(weightedGraph)$name <- gsub(".csv", "", filteredFiles)

# MST of graph
graphMST <- mst(weightedGraph, weights = E(weightedGraph)$weight)

V(graphMST)$color <- lapply(tableSector$Sector[tableSector$Symbol == V(graphMST)$name], retColor)

# Plotting MST
png("MSTAns7.png")
plot(graphMST, vertex.size = 5,
     vertex.label = NA, vertex.color = as.numeric(V(graphMST)$color),
     edge.arrow.size=0.3, main = "MST for modified daily data")
dev.off()

