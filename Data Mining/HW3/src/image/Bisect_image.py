import random
import numpy as np
import time

#------------------------------
start_time = time.time()
#------------------------------

#classify each data to one of the centroids
def cluster_data(centroids, data, index):
    #clusters stores each data. clusters_index stores index in original data
    clusters = [ [] for i in range(2)]
    clusters_index = [ [] for i in range(2)]
    
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
        clusters[min_index].append(data[i]) #將data中的點加入到cluster
        clusters_index[min_index].append(index[i]) #將index放入其中
    return clusters, clusters_index

#compute mean of the cluster      
def compute_mean(clusters): 
    mean = []
    for i in clusters:
        temp = [sum(x)/len(i) for x in zip(*i)]
        mean.append(temp)
    return  mean

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

#basic k-means 
def kmeans(data, index):
    
    #store final bisection result
    Final_clusters = None
    Final_clusters_index = None
    Min_SSE = float("inf")
    #run max_iteration times to get better bisection
    for z in range(10):
        print(z)
        #select 2 nodes
        index_centroid = random.sample(range(0, len(data)-1), 2)
        #put 2 nodes into centroids 
        centroids = []
        for i in index_centroid:
                centroids.append(data[i].copy())
        #clusters stores each cluster. clusters_index stores index in original data
        clusters = []
        clusters_index = []
        while True:
            #classify data to each centroid
            clusters, clusters_index = cluster_data(centroids,data, index) 
            #get new centroids
            new_centroids = compute_mean(clusters)
            #If new and old centroids are the same, end the loop, else run again with new centroids
            if centroid_compare(centroids,new_centroids,2) == 2:    #若都相同了, 離開迴圈
                break
            else:
                centroids = new_centroids
        #compute SSE
        sse = SSE(clusters)
        if sse < Min_SSE:
            Final_clusters = clusters
            Final_clusters_index = clusters_index
            Min_SSE = sse
    
    return Final_clusters, Final_clusters_index 

        
#------------------------------

#data store all the input data
data = np.load('data.npy').tolist()

#k represents k in k-means
k = 10

#clusters stores each cluster. clusters_index stores index in original data
clusters = []
clusters.append(data.copy())
clusters_index = []
clusters_index.append([i for i in range(len(data))])
for i in range(k-1):
    print(i)
    #find the largest cluster
    size_clusters = [len(j) for j in clusters]               
    max_index = size_clusters.index(max(size_clusters))
    #pop the largest cluster out
    selected_clusters = clusters.pop(max_index)  
    selected_clusters_index = clusters_index.pop(max_index)
    #run kmeans, return the bisected clusters and their inedex
    x, y = kmeans(selected_clusters, selected_clusters_index)
    #put the bisected clusters into "clusters" list
    for j in x:
        clusters.append(j)
    #put the index into "clusters_index" list
    for j in y:
        clusters_index.append(j)

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

#------------------------------
print("--- %s seconds ---" % (time.time() - start_time))
#------------------------------