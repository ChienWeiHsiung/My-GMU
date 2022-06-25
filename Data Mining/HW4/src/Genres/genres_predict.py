import pandas as pd
from sklearn import svm

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#

#rating matrix
data = pd.read_csv('../RatingMatrix/rating.csv', index_col=0) 
#genres matrix
genres = pd.read_csv('genres.csv', index_col=0).fillna(0.0)

with open("../Datatest.dat", encoding="utf-8", errors='ignore') as f, open("prediction.dat", 'w') as output :

    lines = f.readlines()[1:]
    user_id = 0
    regr = svm.SVR()
    for line in lines:
        row =  line.strip().split(' ')
        # if different user_id, reconstruct the svm
        if int(row[0]) != user_id:
            user_id = int(row[0])
            #label data
            label = data[row[0]].dropna()
            #training data
            train = genres.loc[label.index]
            regr.fit(train, label)
        #test data
        test = genres.loc[int(row[1])]
        predict = regr.predict([test])
        output.writelines(str(round(predict[0], 1))+'\n')

output.close()
f.close() 

#---------------------------------------------------------#
print(datetime.now() - startTime)
#---------------------------------------------------------#