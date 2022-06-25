

import numpy as np
from numpy.linalg import norm

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#

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

#split train data into 5
temp_train = np.array_split(train_word,5)
temp_freq = np.array_split(train_freq, 5)
#20% as test data
test_word = temp_train[0]
test_freq = temp_freq[0]
#80% as train data
train_word = np.concatenate((temp_train[1], temp_train[2], temp_train[3], temp_train[4]))
train_freq = np.concatenate((temp_freq[1], temp_freq[2], temp_freq[3], temp_freq[4]))

#cos_first40 save all top 40 of each sentiment
cos_first40 = []
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
    cos_first40.append(getmax(cos_data, 40))
    
#save data as the file
np.save("k_select_cosine_data", np.array(cos_first40, list))

#---------------------------------------------------------#
print(datetime.now() - startTime)
#---------------------------------------------------------#