import pandas as pd

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#

#use the same index as the rating martix
data = pd.read_csv('../RatingMatrix/rating.csv', index_col=0) #把第一行當作index
data = data.drop(data.columns, axis=1)

#get the genres matrix
with open("../Data/movie_tags.dat", encoding="utf-8", errors='ignore') as f:
    lines = f.readlines()[1:]
    for line in lines:
        row =  line.strip().split('\t')
        data.at[int(row[0]), row[1]] = float(row[2])
f.close() 
data = data.drop([787])

data.to_csv('tags.csv')

#---------------------------------------------------------#
print(datetime.now() - startTime)
#---------------------------------------------------------#
