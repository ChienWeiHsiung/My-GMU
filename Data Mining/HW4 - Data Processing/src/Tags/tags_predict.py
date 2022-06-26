import pandas as pd
import math
from sklearn.neighbors import NearestNeighbors

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#

k = 13
knn = NearestNeighbors(n_neighbors=k)

#rating matrix
data = pd.read_csv('../RatingMatrix/rating.csv', index_col=0) 
#tag matrix
tags = pd.read_csv('tags.csv', index_col=0)

#training data
train = tags.fillna(0.0)
#use NearestNeighbors
knn.fit(train)

with open("../Data/test.dat", encoding="utf-8", errors='ignore') as f, open("prediction.dat", 'w') as output :
    lines = f.readlines()[1:]
    for line in lines:
        row =  line.split(' ')
        #test data
        test = tags.loc[int(row[1])].fillna(0.0)
        #get the index of nearest neighbors
        neighbors = knn.kneighbors([test])[1][0]
        #compute average rating
        average = 0
        count = 0
        label = data[row[0]].values.tolist()

        for index in neighbors:
            if not math.isnan(label[index]): 
                average += label[index]
                count += 1
        #avoid dividing 0
        if count == 0:
            count += 1
        output.writelines(str(round(average/count, 1))+'\n')

f.close() 
output.close()

#---------------------------------------------------------#
print(datetime.now() - startTime)
#---------------------------------------------------------#