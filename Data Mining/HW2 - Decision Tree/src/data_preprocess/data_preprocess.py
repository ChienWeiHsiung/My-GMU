import pandas as pd
from sklearn.preprocessing import LabelEncoder

train = pd.read_csv("../train.csv")

#train data
#use fit_transform(). Transform categories of F10 & F11 from string to int 
le = LabelEncoder()
train.F10 = le.fit_transform(train.F10)
train.F11 = le.fit_transform(train.F11)
#Get rid of column 'id'
train = train.drop(['id'], axis='columns')

#Transform continuous value to categorical value 
#F1 F2 use quantile to split data into four catagory
train_F1_quantile = [-1, 9, 10 ,12, float("inf")]
train.F1 = pd.cut(train.F1, bins=train_F1_quantile , labels=['0','1','2','3'])

train_F2_quantile = [-1, 35, 40 ,45, float("inf")]
train.F2 = pd.cut(train.F2, bins=train_F2_quantile , labels=['0','1','2','3'])

#F5 F6 split the range of numerical data into four and then classify them
train_F5_quantile = [-1,0,15000,30000,float("inf")]
train.F5 = pd.cut(train.F5, train_F5_quantile , labels=['0','1','2','3'])

train_F6_quantile = [-1,0,1500,3000,float("inf")]
train.F6 = pd.cut(train.F6, bins=train_F6_quantile , labels=['0','1','2','3'])

train.to_csv('processed_train.csv', index=0)

#test
test = pd.read_csv("../test.csv")
#use fit_transform(). Transform categories of F10 & F11 from string to int 
test.F10 = le.fit_transform(test.F10)
test.F11 = le.fit_transform(test.F11)
#Get rid of column 'id'
test = test.drop(['id'], axis='columns')

#use the quantiles of train data to classify F1 F2 F5 F6 of test data
test.F1 = pd.cut(test.F1, bins=train_F1_quantile , labels=['0','1','2','3'])

test.F2 = pd.cut(test.F2, bins=train_F2_quantile , labels=['0','1','2','3'])

test.F5 = pd.cut(test.F5, train_F5_quantile , labels=['0','1','2','3'])

test.F6 = pd.cut(test.F6, bins=train_F6_quantile , labels=['0','1','2','3'])

test.to_csv('processed_test.csv', index=0)

