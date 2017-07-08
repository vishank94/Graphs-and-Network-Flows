###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

#Importing Regular Expression
import re

actor_movies_file = open("C:/Users/VishankBhatia/Desktop/P2/data/actor_movies.txt", 'r')
actress_movies_file = open("C:/Users/VishankBhatia/Desktop/P2/data/actress_movies.txt", 'r')
actor_actress_filtered_file = open("C:/Users/VishankBhatia/Desktop/P2/data/actor_actress_filtered_new.txt", 'w')
movies_filtered_file = open("C:/Users/VishankBhatia/Desktop/P2/data/movies_filtered.txt", 'w')

movies_count_map = {}
actor_actress_movies_map = {}

#Minimum number of movies
cutoff = 10
print("Merging actor_movies and actress_movies into one file")
files_list = [actor_movies_file,actress_movies_file]

#Iterating for actor_movies_file and actress_movies_file
for current_file in files_list:
	for row in current_file.readlines():
		#result = re.sub(pattern, repl, string, count=0, flags=0);
		#look for pattern in string and replace
		row = row[:-1]
		row = re.sub("^\\(", "",row)
		row = re.sub("\\([^[:digit:]]+\\)", "",row)
		row = re.sub("^\\s+|\\s+$", "",row)
		#remove on 2 tab spaces and returns a list
		namov = row.split("\t\t")
		if len(namov)<= cutoff+1:
			continue
			namov[0].strip(" ")
			namov[0].strip("\t")
		#Putting all the movies against the actor/actress name
		actor_actress_movies_map[namov[0]] = namov[1:len(namov)]
		for x in range(1, len(namov)):
			if (namov[x] in movies_count_map):
				count = 0
				count = movies_count_map[namov[x]]
				count = count+1
				movies_count_map[namov[x]] = count
			else:
				movies_count_map[namov[x]] = 1
movies_filtered=[]
for movie, count in movies_count_map.items():
	#Check for number of actors/actress in a movie
	if(count > 10):
		movies_filtered.append(movie)
print("Total filtered movies: ",len(movies_filtered))

counter = 0
current_movie_list = ""
actor_movies_file.close()
actress_movies_file.close()

#Iterating for actor/actress_name, movie_list in actor_actress_movies_map
for actor_actress, movie_list in actor_actress_movies_map.items():
	new_movies_list=[]
	movies_filtered_count=0
	current_movie_list=""
	for current_movie in movie_list:
		if current_movie in movies_filtered:
			movies_filtered_count=movies_filtered_count+1
			new_movies_list.append(current_movie)
			current_movie_list=current_movie_list+current_movie+"\t\t"
	actor_actress_movies_map[actor_actress] = new_movies_list
	if(movies_filtered_count > 10):
		actor_actress_filtered_file.write(actor_actress + "\t\t" + current_movie_list + "\n")
		counter = counter + 1
		if counter%100 == 0:
			print("Count: "+str(counter)+" "+actor_actress)
#print("Total actors_actress ",len(d['actor_actress_movies_map'])