###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing


#################################################################################################
#Preprocessing : run community algorithm AND generate dictonary of movie and generes. 
#store as rds objects which will be used in the second half
#Prerequisite run : Question_4.R 

moviesNetCommunity <- readRDS("newnetcommm.rds")
moviesNetCommunity <- fastgreedy.community(movieNet, weights =  E(movieNet)$weights)
saveRDS(moviesNetCommunity,"commObj2.rds")

genreList = fread("movie_genre.txt", sep = "\t", header = FALSE)
genreList$V2 = NULL
Encoding(genreList$V1) = 'latin1'
genreList$V1 = iconv(genreList$V1, "latin1", "ASCII", sub="")
genreList$V1 = gsub("^\\(", "", genreList$V1)
genreList$V1 = gsub("\\([^[:digit:]]+\\)", "", genreList$V1)
genreList$V1 = gsub("^\\s+|\\s+$", "", genreList$V1)
write.table(genreList, file = "processed_genre.txt",sep = "\t", row.names = FALSE)

#Create a dictonary 
processedGenreList <- fread("processed_genre2.txt",sep = "\t")
genreDict <- list()
for (i in 1: length(processedGenreList[[1]])){
  genreDict[processedGenreList[[1]][i]] = processedGenreList[[2]][i]
}
saveRDS(genreDict,"genre_dict.rds")

#Preprocessing ends
#################################################################################################
#Run the following code for community tags

moviesNetCommunity <- readRDS("newnetcommm.rds")
commNumber <- sort(unique(moviesNetCommunity$membership))
print(sizes(moviesNetCommunity))
movieGenreDict <- readRDS("genre_dict.rds")
print(table(unlist(movieGenreDict)))
print(modularity(moviesNetCommunity))
for (i in commNumber) {
  currCommMovies <- V(movieNet)[which(moviesNetCommunity$membership == i)]
  commGenres <- movieGenreDict[currCommMovies]
  genreSummary <- table(unlist(commGenres))
  print(genreSummary)
  threshold <- length(commGenres) * 0.1
  print(threshold)
  print(genreSummary[which(genreSummary > threshold)])
}



