

import numpy as np

#predict the rating
def predict(maximum, train_review, k):
    count = 0
    for i in range(k):
        x = 40 - 1 - i
        count += train_review[(maximum[x]['id']+3702)]
    if count > 0:
        return 1
    else:
        return -1
    
#compute accuracy
def accuracy(prediction, train_review):
    count = 0
    for i in range(len(train_review)):
        if prediction[i] == train_review[i]:
            count += 1
    return (count/len(train_review))

train_review = np.load('../train_data/train_review.npy', allow_pickle=True)
test_cos = np.load('k_select_cosine_data.npy', allow_pickle=True)
split_review = np.array_split(train_review,5)

#from k = 1 to k = 40 , compute their accuracy
for k in range(40):
    prediction = [0 for i in range(len(test_cos))]
    
    for i in range(len(test_cos)):
        prediction[i] = predict(test_cos[i],train_review, k+1)
    
    acc = accuracy(prediction, split_review[0])
    print('%2d'%(k+1) +' : '+'%.4f.  '%acc, end='')
    if (k+1)%5 == 0:
        print('')