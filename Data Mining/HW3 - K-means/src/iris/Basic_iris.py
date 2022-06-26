import random
import numpy as np

#classify each data to one of the centroids
def cluster_data(centroids, data):
    #clusters stores each data. clusters_index stores index in original data
    clusters = [ [] for i in range(3)]
    clusters_index = [ [] for i in range(3)]
    
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
            print('hi')
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
        
#------------------------------

#data store all the input data
data = []

#open file
file = open("test_data.txt", "r")
for  i in file:
    i = i[:-1]
    data.append([float(j) for j in i.split(' ')])
#k-means的k, 代表要找k的點

#k represents k in k-means
k = 3
#select k nodes
index_centroid = random.sample(range(0, len(data)), k)

#put k nodes into centroids 
centroids = []
for i in index_centroid:
    centroids.append(data[i].copy())

#clusters stores each cluster. clusters_index stores index in original data
clusters = []
clusters_index = []
while True:
    #classify data to each centroid
    clusters, clusters_index = cluster_data(centroids, data) 
    #get new centroids
    new_centroids = compute_mean(clusters)      
    #If new and old centroids are the same, end the loop, else run again with new centroids
    if centroid_compare(centroids,new_centroids,k) == k:   
        break
    else:
        centroids = new_centroids
file.close()

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


