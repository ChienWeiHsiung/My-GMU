import pandas as pd
import math
from sklearn.neighbors import NearestNeighbors

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#

k = 13
knn = NearestNeighbors(n_neighbors=k)

data = pd.read_csv('../RatingMatrix/rating.csv', index_col=0) 
#training data
train = data.T.fillna(0.0)
#use NearestNeighbors
knn.fit(train)

with open("../Data/test.dat", encoding="utf-8", errors='ignore') as f, open("prediction.dat", 'w') as output :
    lines = f.readlines()[1:]
    for line in lines:
        #split the data
        row =  line.split(' ')
        #test data
        test = data[row[0]].fillna(0.0)
        #get the index of nearest neighbors
        neighbors = knn.kneighbors([test])[1]
        #compute average rating
        average = 0
        count = 0
        label = data.loc[int(row[1])].values.tolist()
        for index in neighbors[0]:
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