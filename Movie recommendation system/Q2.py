###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

#Importing Regular Expression and Decimal for Arithmetic
import re
import decimal

movies_filtered_file = open("C:/Users/VishankBhatia/Desktop/P2/data/actor_actress_filtered_new.txt", 'r')
weighted_directed_graph_file = open("C:/Users/VishankBhatia/Desktop/P2/data/wd_graph.txt", 'w')

actor_actress_movies_map = {}

#Iterating for actor/actress movies filtered file
for row in movies_filtered_file.readlines():
    row = row[:-1]
    row = re.sub("^\\(", "",row)
    row = re.sub("\\([^[:digit:]]+\\)", "",row)
    row = re.sub("^\\s+|\\s+$", "",row)
    namov = row.split("\t\t")
    namov[0].strip(" ")
    namov[0].strip("\t")
    actor_actress_movies_map[namov[0]] = namov[1:len(namov)]

celeb_list_1 = []
celeb_list_1 = list(actor_actress_movies_map.keys())
celeb_list_2 = []
celeb_list_2 = list(actor_actress_movies_map.keys())

p = 0
counter = 0

print("The total number of nodes in the graph: ",len(celeb_list_1))

#Calculating the edge weights
while p < len(celeb_list_1):##############
    actor_1_name = celeb_list_1[p]
    actor_1_movies = actor_actress_movies_map[actor_1_name]
    q = p + 1
    while q < len(celeb_list_2):
        actor_2_name = celeb_list_2[q]
        actor_2_movies = actor_actress_movies_map[actor_2_name]
        overlap_movies = set(actor_1_movies) & set(actor_2_movies)
        if len(overlap_movies) > 0:
            actor_1_ratio = decimal.Decimal(len(overlap_movies)) \
                / decimal.Decimal(len(actor_1_movies))
            actor_1_ratio = format(actor_1_ratio, '.3f')
            actor_2_ratio = decimal.Decimal(len(overlap_movies)) \
                / decimal.Decimal(len(actor_2_movies))
            actor_2_ratio = format(actor_2_ratio, '.3f')
            if counter % 100 == 0:
                print('Count: ' + str(counter))
            weighted_directed_graph_file.write(actor_1_name + '\t' + actor_2_name + '\t' + str(actor_1_ratio) + '\n')
            weighted_directed_graph_file.write(actor_2_name + '\t' + actor_1_name + '\t' + str(actor_2_ratio) + '\n')
            counter = counter + 1
        q = q + 1
    p = p + 1     

weighted_directed_graph_file = open("C:/Users/VishankBhatia/Desktop/P2/data/wd_graph.txt", 'r')

r = 0
    
#Searching for all the actor/actress pairings that have weight=1
for line in weighted_directed_graph_file.readlines():
    edgeweight = line.split("\t")
    edgeweight[2] = edgeweight[2].strip("\n")
    if float(edgeweight[2].strip("\n")) == 1.000:
        print("Actor/Actress Pairings: ",line)