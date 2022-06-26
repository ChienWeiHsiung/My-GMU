import random
import numpy as np

#classify each data to one of the centroids
def cluster_data(centroids, data, k):
    #clusters stores each data. clusters_index stores index in original data
    clusters = [ [] for i in range(k)]
    clusters_index = [ [] for i in range(k)]
    
    #compare each data to each centroid, find the min distance
    for i in range(0, len(data)):
        #min distance 和 index of that cluster
        min_distance = float("inf")   
        min_index = -1
        for j in range(0, len(centroids)): 
            #compute distance
            temp_distance = np.linalg.norm(np.array(data[i]) - np.array(centroids[j]))
            if  temp_distance < min_distance:
                min_distance = temp_distance
                min_index = j
        #put that data into the cluster with min distance
        clusters[min_index].append(data[i]) 
        clusters_index[min_index].append(i) 
    #if there is a empty cluster, add a random node into that cluster
    for i in range(0, len(clusters)):
        if len(clusters[i]) == 0 :
            clusters[i].append(data[random.sample(range(0, len(data)), 1)])
    return clusters, clusters_index
      
#compute new centroids  
def compute_mean(clusters): 
    new_centroids = []
    for i in clusters:
        mean = [sum(x)/len(i) for x in zip(*i)]
        new_centroids.append(mean)
    return  new_centroids

#compare whether new and old centroids are the same    
def centroid_compare (old, new, k):
    same = 0
    for i in range(0, k):
        z = [x for x, y in zip(new[i], old[i]) if x == y]
        if len(z) == len(old[i]):
            same += 1 
    return same   

#compute SSE
def SSE(clusters):
    summ = 0
    mean = compute_mean(clusters)
    for i in range(len(mean)):
        for j in clusters[i]:
            summ += np.linalg.norm(np.array(mean[i]) - np.array(j)) ** 2
    return summ
        
#------------------------------

#data store all the input data
data = np.load('data.npy').tolist()

#k represents k in k-means
k = 10
#select k nodes
index_centroid = random.sample(range(0, len(data)-1), k)

#put k nodes into centroids 
centroids = []
for i in index_centroid:
    centroids.append(data[i].copy())

#k-mean
clusters = []
clusters_index = []
while True:
    #classify data to each centroid
    clusters, clusters_index = cluster_data(centroids,data, k)
    #get new centroids
    new_centroids = compute_mean(clusters) 
    #If new and old centroids are the same, end the loop, else run again with new centroids
    if centroid_compare(centroids,new_centroids,k) == k:    
        print('hello')
        break
    else:
        centroids = new_centroids

print(SSE(clusters))

#write the result into the file
file = open("cluster.txt", "w")
result = [0 for i in range(len(data))]
for i in range(len(clusters_index)):
    for j in clusters_index[i]:
        result[j] = i + 1
for i in range(0,len(result)):
    if i == len(result)-1:
        file.write(str(result[i]))
    else:
        file.write(str(result[i])+'\n')
file.close()


