import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import Perceptron
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC

def save_file(prediction):
    f = open('Prediction.txt', 'w')
    for i in prediction:
        f.write(str(i)+'\n')
    f.close()


train = pd.read_csv("../data_preprocess/processed_train.csv")

x = train.drop('credit', axis='columns')
y = train.credit

test = pd.read_csv("../data_preprocess/processed_test.csv")

drop = int(input('Drop F5 & F6: 1. yes 2. no : '))

if drop == 1 :
    train = train.drop(['F5'], axis='columns')
    train = train.drop(['F6'], axis='columns')
    test = test.drop(['F5'], axis='columns')
    test = test.drop(['F6'], axis='columns')

print('---Method---\n1. Logistic Regression\n2. Perception\n3. Naive Bayes\n4. Support Vector Machine\n5. Decision Tree', end='')

i = int(input('Choose method: '))

if i == 1:
    LR = LogisticRegression()
    LR.fit(x,y)
    save_file(LR.predict(test))

elif i == 2:
    perc = Perceptron()
    perc.fit(x,y)
    save_file(perc.predict(test))
elif i == 3:
    NB = GaussianNB()
    NB.fit(x,y)
    save_file(NB.predict(test))
elif i == 4:
    svm = SVC()
    svm.fit(x,y)
    save_file(svm.predict(test))
elif i == 5:
    crit = int(input('1. gini 2. entropy: '))
    if crit == 1:
        crit = 'gini'
    else:
        crit = 'entropy'
    depth = int(input('max_depth: '))
    DecT = DecisionTreeClassifier(criterion=crit, max_depth=depth)
    DecT.fit(x,y)
    save_file(DecT.predict(test))


      
