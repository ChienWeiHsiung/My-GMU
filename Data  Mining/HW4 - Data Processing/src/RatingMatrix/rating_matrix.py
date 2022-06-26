import pandas as pd

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#

#store rating matrix
data = pd.DataFrame()  

#get all the movie id first
with open("../Data/movie_directors.dat", encoding="utf-8", errors='ignore') as f:
    movie_id = []
    i = 0 
    for line in f:
        if i != 0 : 
            row =  line.split(' ')
            row = row[0].split('\t')
            movie_id.append(int(row[0]))
        i += 1
    data["movie_id"] = movie_id   
f.close() 

#use movie_id as index
data.index = movie_id
#delete the column : movie_id
del data["movie_id"]

#rating matrix
with open("../Data/train.dat", encoding="utf-8", errors='ignore') as f:
    i = 0 
    for line in f:
        if i != 0 : 
            row = line.strip().split(' ')
            data.at[int(row[1]), row[0]] = float(row[2])
        i += 1  
f.close() 
#order the index
data = data.sort_index()
data.to_csv('rating.csv')

#---------------------------------------------------------#
print(datetime.now() - startTime)
#---------------------------------------------------------#
