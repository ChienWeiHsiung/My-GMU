import numpy as np
from sklearn.decomposition import PCA
import time

#------------------------------
start_time = time.time()
#------------------------------

data = []

file = open("image.txt", "r")
for  i in file:
    data.append([float(j) for j in i.split(',')])

x = np.array(data)
pca = PCA(n_components='mle')
x = pca.fit_transform(x)


np.save('data', x)

#------------------------------
print("--- %s seconds ---" % (time.time() - start_time))
#------------------------------

