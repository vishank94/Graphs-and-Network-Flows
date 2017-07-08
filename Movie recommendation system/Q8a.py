###########################################################################
#Copyright (C) 2017 Anshita, Swati and Vishank - All Rights Reserved.
###########################################################################

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from operator import itemgetter
import numpy as np
import heapq
import ast

page_rank = {} 
pg_reader = open("page_rank_actors_2.txt", "rb")

for pg in pg_reader.readlines():
    content = pg.strip().split('\t')
    page_rank[content[0]] = float(ast.literal_eval(content[1]))

ma_reader = open("actor_actress_filtered_new.txt", "rb")

actor_id = []
for ma in ma_reader.readlines():
    a = ma.strip().split("\t\t")
    actor_id.append(a[0])

actor_id = np.ravel(np.ravel(np.array(actor_id)))
actor_id = set(actor_id)

for ac in actor_id:
    if ac in page_rank:
        continue
    else:
        page_rank[ac] = 0


ma_reader = open("actor_actress_filtered_new.txt", "rb")

output = {}
for mb in ma_reader.readlines():
    values = mb.split('\t\t')
    movies = values[1:]
    actor = values[0]
    pageRankActor = page_rank[actor]
    for movie in movies:
        if movie not in output:
            output[movie]= []
        output[movie].append(pageRankActor)

ouputFile = open('moviePageRanks.txt',"w")
for movie in output.keys():
    pagerankVal = output[movie]
    pagerankVal.sort(reverse=True)
    strPgRnk =""
    for val in range(0,5):
        if(val< len(pagerankVal)):
            y = pagerankVal[val]
            strPgRnk = strPgRnk+str(y)+"\t"
        else:
            strPgRnk = strPgRnk+"0"+"\t"

    final = movie+"\t"+strPgRnk+"\n"
    ouputFile.write(final)


