###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

from operator import itemgetter
import numpy as np
import heapq
import ast
import re
import operator
from collections import OrderedDict
import pandas as pd
import sklearn
import numpy as np
from sklearn import datasets, linear_model


# run preprocess.py to get top pageranks for movies
ratings = file.readlines(file("ratings_processed.txt"))
rating_list = []
movies = []

for line in ratings[1:]:
    str1 = line.strip().split('\t')
    movies.append(str1[0])
    rating_list.append(float(str1[1]))

movies_ratings = dict(zip(movies, rating_list))
d_sorted_by_value = OrderedDict(sorted(movies_ratings.items(), key=lambda x: x[1], reverse=True))

top100 = dict(d_sorted_by_value.items()[:149])
top100_movies = top100.keys()


movies_director = {}
md_reader = file.readlines(file("director_movies_processed.txt", "rb"))

movies = []
directors = []
i = 1
for md in md_reader:
    content = md.strip().split("\t")
    if len(content) > 1:
        directors.extend([content[0]] * (len(content) - 1))
        for x in content[1:]:
            movie_name = ''.join(('"',x.strip(),'"'))
            movies.append(movie_name)
    i += 1

movies_director = dict(zip(movies, directors))


# fetch directors for top 100 movies
top100_directors = []
for movie in top100_movies[1:]:
    if movie in movies_director.keys():
        top100_directors.append(movies_director[movie])

df = pd.read_csv('moviePageRanks.txt', sep="\t", header = None, names = ['Movie','pg1','pg2','pg3','pg4','pg5','Director','Ratings'])
df["Director"] = 0
df['Ratings'] = 0


movies = []
director_bool = []
nrows = df.shape[0]
for i in range(0,nrows):
    movie = df.ix[i,0]

    movie_name = ''.join(('"',str(movie).strip(),'"'))

    val = 0
    if movie_name in movies_director:
        if movies_director[movie_name] in top100_directors:
             val = 1

        else:
            val = 0


    else:
        val = 0
    df.ix[i,6] = val

    if movie_name in movies_ratings:
        df.ix[i,7] = movies_ratings[movie_name]


############################## Linear Regression #################################

x_train = df.ix[0:nrows,1:6]
y_train = df.ix[0:nrows,7:]

batman_feature = df[df['Movie'] == 'Batman v Superman: Dawn of Justice (2016)']

regr = linear_model.LinearRegression()
regr.fit(x_train, y_train)

batman_feature = df[df['Movie'] == 'Batman v Superman: Dawn of Justice (2016)']

for i in range(1, nrows):
    movie = df.ix[i, 0]
    movie_name = ''.join(('"', str(movie).strip(), '"'))
    if(movie_name == '"Batman v Superman: Dawn of Justice (2016)"'):
        batman_feature = df.ix[i,1:6]
    if(movie_name == '"Mission: Impossible - Rogue Nation (2015)"'):
        MI_feature = df.ix[i,1:6]
    if (movie_name == '"Minions (2015)  (voice)"'):
        Minions_feature = df.ix[i, 1:6]

print('Coefficients: \n', regr.coef_)

print("prediction for batman v superman")
print(regr.predict(batman_feature))

print("prediction for batman v superman")
print(regr.predict(MI_feature))

print("prediction for batman v superman")
print(regr.predict(Minions_feature))
