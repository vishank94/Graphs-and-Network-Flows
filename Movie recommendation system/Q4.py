###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

#Importing Regular Expression and Decimal for Arithmetic
import re
import decimal

movies_filtered_file = open("C:/Users/VishankBhatia/Desktop/P2/data/actor_actress_filtered_new.txt", 'r')
movie_network_file = open("C:/Users/VishankBhatia/Desktop/P2/data/movie_network_file.txt", 'w')

actor_actress_movies_map = {}

# Iterate for actor/actress movies filtered file
for row in movies_filtered_file.readlines():
    row = row[:-1]
    row = re.sub("^\\(", "",row)
    row = re.sub("\\([^[:digit:]]+\\)", "",row)
    row = re.sub("^\\s+|\\s+$", "",row)
    namov = row.split("\t\t")
    namov[0].strip(" ")
    namov[0].strip("\t")

    # Iterate for all the movies in the list that they have acted in
    for movie_num in range(1, len(namov)):
        if (namov[movie_num] in actor_actress_movies_map):
            current_actor_list = actor_actress_movies_map[namov[movie_num]]
            if namov[0] not in current_actor_list:
                current_actor_list.append(namov[0])         
                actor_actress_movies_map[namov[movie_num]] = current_actor_list
        else:
            actor_actress_movies_map[namov[movie_num]] = [namov[0]]
           
movie_list_1 = []
movie_list_1 = list(actor_actress_movies_map.keys())
movie_list_2 = []
movie_list_2 = list(actor_actress_movies_map.keys())

p = 0
counter = 0

print("Number of nodes in the graph: ",len(movie_list_1))

# Iterate over list 1
while p < len(movie_list_1):
    movie_1_name = movie_list_1[p]
    movie_1_actors = actor_actress_movies_map[movie_1_name]
    q = p + 1
    while q < len(movie_list_2):
        movie_2_name = movie_list_2[q]
        movie_2_actors = actor_actress_movies_map[movie_2_name]
        overlap_actors = set(movie_1_actors) & set(movie_2_actors)
        if(len(overlap_actors) > 0):
            # Calculate jaccard index for movies
            jaccard_index = decimal.Decimal(len(overlap_actors) * 100)/(decimal.Decimal(len(movie_1_actors) + len(movie_2_actors) - len(overlap_actors)))
            movie_network_file.write(movie_1_name + "\t" + movie_2_name + "\t" + str(jaccard_index) + "\n")
            counter = counter + 1
        q = q + 1
    p = p + 1