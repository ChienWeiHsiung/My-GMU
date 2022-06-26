# Readme

There are five ".py" files and they were done in Python 3.8.8 :
- "3queues-original.py" : original algorithm with 3 queues
- "3queues-modified.py" : modified algorithm with 3 queues
- "4queues-original.py" : original algorithm with 4 queues
- "4queues-modified.py" : modified algorithm with 4 queues
- "5queues-original.py" : original algorithm with 5 queues
For more detail description of code, please read my report.

## Libraries Used
I use "matplotlib" to show the result in the form of graph.

#### Install
```
pip install matplotlib
```

## Execution
Steps for "3queues-original.py", "3queues-modified.py", "4queues-original.py" and "4queues-modified.py" : 
 

- Start executing.
- Input a integer to choose a method to pick action : **1.** batch updating search.  **2.** greedy search.
- Wait for a while and it'll show a graph.
- Execution end.

Steps for "5queues-original.py" : 
- Start executing.
- Input a integer to choose a inital set of conditions and requirement for queues : **1.** set1 **2.** set2
- Wait for a while and it'll show a graph.
- Execution end.

### Set used in "5queues-original.py"

Using set 1 can get the result as Figure 6(a) in the report.  
Using set 2 can get the result as Figure 6(b) in the report.

- Set 1 :

Queue | Arrival Rate | Mean Delay Requirement |
--- | --- | --- | 
1 | 0.30 | 6 | 
2 | 0.10 | 3 | 
2 | 0.15 | 4 | 
2 | 0.20 | 5 | 
3 | 0.20 | best-effort | 

- Set 2 :

Queue | Arrival Rate | Mean Delay Requirement |
--- | --- | --- | 
1 | 0.30 | 6 | 
2 | 0.20 | 3 | 
2 | 0.20 | 4 | 
2 | 0.20 | 5 | 
3 | 0.20 | best-effort | 

