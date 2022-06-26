

import numpy as np
from numpy.linalg import norm

#get the top k value of all the outcome
def getmax(outcome, k):
    maximum = [{'id':-1, 'value':-1} for i in range(k)]
    for i in range(len(outcome)):
        if outcome[i] > maximum[0]['value']:
            maximum[0]['id'] = i
            maximum[0]['value'] = outcome[i]
            maximum.sort(key = lambda x : x['value'])
    return maximum

train_word = np.load('../train_data/train_word.npy', allow_pickle=True)
train_freq = np.load('../train_data/train_freq.npy', allow_pickle=True)
test_word = np.load('../test_data/test_word.npy', allow_pickle=True)
test_freq = np.load('../test_data/test_freq.npy', allow_pickle=True)

#cos_all save all top 30 of each sentiment
cos_all = []
count = 0
for i in range(len(test_word)):
    print(count)
    count += 1
    cos_data =[]
    for j in range(len(train_word)):
        #create two lists of frequency for test and train
        f_train = np.copy(train_freq[j])
        f_test = np.array([0 for z in range(len(f_train))])
        #combine two lists of words and get the frequency
        for k in range(len(test_word[i])):
            if test_word[i][k] not in train_word[j]:
                f_train = np.append(f_train, 0)
                f_test = np.append(f_test, test_freq[i][k])
            else:
                index = train_word[j].index(test_word[i][k])
                f_test[index] = test_freq[i][k]
        #compute consine value
        a = norm(f_train)
        b = norm(f_test)
        if a == 0 or b == 0:
            cos_data.append(0)
        else:
            cos_data.append(np.dot(f_train,f_test)/(a*b))
    cos_all.append(getmax(cos_data, 30))
       
#save data as the file
np.save("cosine_data", np.array(cos_all, list))
