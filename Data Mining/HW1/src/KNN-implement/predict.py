
import numpy as np

#predict the rating
def predict(maximum, train_review, k):
    count = 0
    for i in range(k):
        x = 30 - 1 - i
        count += train_review[(maximum[x]['id'])]
    if count > 0:
        return 1
    else:
        return -1
    

train_review = np.load('../train_data/train_review.npy', allow_pickle=True)
test_cos = np.load('cosine_data.npy', allow_pickle=True)

prediction = [0 for i in range(len(test_cos))]
# input k value
k = int(input("Please input(1~30) : "))
    
#predict the rating of each sentiment
for i in range(len(test_cos)):
    prediction[i] = predict(test_cos[i],train_review, k)

#save as "test.dat"    
f = open('test.dat', 'w')
for i in prediction:
    if i == 1:
        f.write('+1\n')
    elif i == -1:
        f.write('-1\n')
   
f.close()    