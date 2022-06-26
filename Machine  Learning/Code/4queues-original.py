import random
import queue
import matplotlib.pyplot as plt

class Queue:
    def __init__(self, arrival_rate=0.30, delay_requirement=5, P=100000):
        #one queue per datastream
        self.queue = queue.Queue()
        #parameter : arrival rate & delay requirement
        self.arrival_rate = arrival_rate
        self.delay_requirement = delay_requirement
        #mean delay time over P packets
        self.P = P  
        self.delay_of_packet = queue.Queue() #delay time of the latest P packets
        self.sum = 0
        self.mean_delay = 0
        
    def change(self, arrival_rate, delay_requirement):  #for changing parameter
        self.arrival_rate = arrival_rate
        self.delay_requirement = delay_requirement
        
    def isEmpty(self):  #check whether it's an empty queue
        if self.queue.empty():
            return True
        else:
            return False
    
    def enqueue(self, arrival_time): # enqueue with a probability
        chance = random.random()
        if chance < self.arrival_rate:
            self.queue.put(arrival_time)
        
    def dequeue(self, current_time): #dequeue. Then, update delay time of the latest P packets, and mean delay time
        #dequeue
        arrival_time = self.queue.get()
        #delay time = arrival time - current time
        delay = current_time - arrival_time
        #compute mean delay time
        self.sum += delay    
        self.delay_of_packet.put(delay)  
        if self.delay_of_packet.qsize() > self.P : 
            self.sum -= self.delay_of_packet.get()
            
        self.mean_delay = self.sum / len(self.delay_of_packet.queue)  

        
class System:
    def __init__(self, P=100000, change_time=100000):
        #Three queues, q1 q2 和 qb (best effort) in the router
        self.q1 = Queue(0.25, 6, P)
        self.q2 = Queue(0.20, 3, P)
        self.q3 = Queue(0.15, 4, P)
        self.qb = Queue(0.30, 0, P)
        
        #parameters for reward
        self.C1 = 50
        self.C2 = 20
        self.C3 = 100
        self.gamma = 0.5
        #parameters for e-greedy
        self.e = 0.2
        #parameters for batch
        self.T = 1000
        self.k = 3
        self.prob = [[0 for x in range(5)] for y in range(16)]
        #The time slot to change requirement
        self.change_time = change_time
        
        #Statistics : mean delay at each time slot for each queue
        self.statistic1 = []
        self.statistic2 = []
        self.statistic3 = []
        self.statisticb = []
        
        #Qtable. four states, three actions
        #state : (0,0)=0 (0,1)=1, (1,0)=2, (1,1)=3 | (q2, q1)
        #action: 0 = take packet from queue1, 1 = queue2, 2 = best effort queue
        self.Qtable = [[0 for x in range(4)] for y in range(8)]
        
    def Qlearning_greedy(self, iteration):

        #initial state
        state = 0
        #Each iteration is one timeslot
        for i in range(iteration):
            #requirement change
            if i == self.change_time :
                self.q1.change(0.15, 5)
                self.q2.change(0.25, 5)
                self.q3.change(0.20, 3)
                self.qb.change(0.3, 0)
            
            #New packet into each queue
            self.q1.enqueue(i)
            self.q2.enqueue(i)
            self.q3.enqueue(i)
            self.qb.enqueue(i)
            
            #Mean delay at current time slot
            self.statistic1.append(self.q1.mean_delay)
            self.statistic2.append(self.q2.mean_delay)
            self.statistic3.append(self.q3.mean_delay)
            self.statisticb.append(self.qb.mean_delay)
            
            #Choose an action
            #If all queues are empty, skip this time slot
            if self.all_queue_check() :
                continue
            action = self.action_greedy(state)
            action = self.check_action_greedy(action, state) #after choosing an action, checking whether this queue is empty
            #take action
            self.do_action(action, i)
            #Compute next state s'
            next_state = self.next_state()
            #Compute reward
            reward = self.reward(action, state, next_state)
            #Update Q table
            self.Qtable[state][action] = reward + self.gamma * max(self.Qtable[next_state])
            #Move to next state s'
            state = next_state
            
    def Qlearning_batch(self, iteration):

        #initial state
        state = 0
        #Each iteration is one timeslot
        for i in range(iteration):
            #requirement change
            if i == self.change_time :
                self.q1.change(0.15, 5)
                self.q2.change(0.25, 5)
                self.q3.change(0.20, 3)
                self.qb.change(0.3, 0)
            
            #New packet into each queue
            self.q1.enqueue(i)
            self.q2.enqueue(i)
            self.q3.enqueue(i)
            self.qb.enqueue(i)
            
            #Mean delay at current time slot
            self.statistic1.append(self.q1.mean_delay)
            self.statistic2.append(self.q2.mean_delay)
            self.statistic3.append(self.q3.mean_delay)
            self.statisticb.append(self.qb.mean_delay)
            
            #Choose an action
            #If all queues are empty, skip this time slot
            if self.all_queue_check() :
                continue
            action = self.action_batch(state)
            action = self.check_action_batch(action, state) #選完action, 再做check(以避免選到empty queue)
            #take action
            self.do_action(action, i)
            #Compute next state s'
            next_state = self.next_state()
            #Compute reward
            reward = self.reward(action, state, next_state)
            #Update Q table
            self.Qtable[state][action] = reward + self.gamma * max(self.Qtable[next_state])
            #Move to next state s'
            state = next_state
                                   
           
    
    def all_queue_check(self):
        if self.q1.isEmpty() and self.q2.isEmpty() and self.q3.isEmpty() and self.qb.isEmpty():
            return True
        else:
            return False
    
    def action_greedy(self, state):
        #Return action that has the highest Q value given a certain state
        action = self.Qtable[state].index(max(self.Qtable[state]))
        #With a probability to choose other action (exploration)
        if random.random() < self.e:
            temp = random.randint(0,3)
            while temp == action:
                temp = random.randint(0,3)
            action = temp
        return action
    
    def check_action_greedy(self, action, state):
        #If the queue of the chose action is empty, choose the action with second large Q value and not empty queue
        #True : empty, False : not empty
        temp = [self.q1.isEmpty(), self.q2.isEmpty(), self.q3.isEmpty(), self.qb.isEmpty()]
        if temp[action] == True:
            alter_action = 0
            alter_action_value = -100000
            for i in range(len(temp)):
                if temp[i] == False and self.Qtable[state][i] >= alter_action_value:
                    alter_action = i
                    alter_action_value = self.Qtable[state][i]
            return alter_action
        else:
            return action

    def action_batch(self, state):
        #Update Distribution per T iterations
        #Compute probability distribution (in the form of CMF) given a state. Ex : {(s,a1), (s,a2), (s,a3)}
        if (state % self.T) == 0:
            for i in range(8):
                k = self.k
                k_pow = [pow(k, self.Qtable[i][0]), pow(k, self.Qtable[i][1]), pow(k, self.Qtable[i][2]), pow(k, self.Qtable[i][3])]
                self.prob[i] = [k_pow[0]/sum(k_pow), (k_pow[0]+k_pow[1])/sum(k_pow), (k_pow[0]+k_pow[1]+k_pow[2])/sum(k_pow),  1]
        #Choose an action based on distribution
        action = 0
        number = random.random()
        if number < self.prob[state][0] :
            action = 0
        elif number < self.prob[state][1]:
            action = 1
        elif number < self.prob[state][2]:
            action = 2
        else:
            action = 3
        return action
    
    def check_action_batch(self, action, state):
        #If the queue of the chose action is empty, randomly choose anothor action with not empty queue
        #True : empty, False : not empty
        temp = [self.q1.isEmpty(), self.q2.isEmpty(), self.q3.isEmpty(), self.qb.isEmpty()]
        if temp[action] == True:
            alter_action = action
            while temp[alter_action] == True:
                alter_action = random.randint(0, 3)
            return alter_action
        else:
            return action

    def do_action(self, action, current_time):
        #take action
        if action == 0:
            self.q1.dequeue(current_time)
        elif action == 1:
            self.q2.dequeue(current_time)
        elif action == 2:
            self.q3.dequeue(current_time)
        else:
            self.qb.dequeue(current_time)

    def next_state(self):
        #Compute next state
        new_state = 0
        if self.q1.mean_delay > self.q1.delay_requirement:
            new_state += 1
        if self.q2.mean_delay > self.q2.delay_requirement:
            new_state += 2
        if self.q3.mean_delay > self.q3.delay_requirement:
            new_state += 4
        return new_state
    
    def reward(self, action, state, next_state):
        #Compute reward
        #rtime,1
        r1 = 0
        if self.q1.mean_delay < self.q1.delay_requirement:
            r1 = (self.C1 * self.q1.mean_delay) / self.q1.delay_requirement
        elif self.q1.mean_delay == self.q1.delay_requirement :
            r1 = self.C1
        else:
            r1 = -1*self.C2
        #rtime,2
        r2 = 0
        if self.q2.mean_delay < self.q2.delay_requirement:
            r2 = (self.C1 * self.q2.mean_delay) / self.q2.delay_requirement
        elif self.q2.mean_delay == self.q2.delay_requirement :
            r2 = self.C1
        else:
            r2 = -1*self.C2
        #rtime,3
        r3 = 0
        if self.q3.mean_delay < self.q3.delay_requirement:
            r3 = (self.C1 * self.q3.mean_delay) / self.q3.delay_requirement
        elif self.q3.mean_delay == self.q3.delay_requirement :
            r3 = self.C1
        else:
            r3 = -1*self.C2
        #rtime
        rtime = 0
        if action == 0 :
            rtime = r1 * 0.3 + r2 + r3
        elif action == 1 :
            rtime = r1 + r2 * 0.3 + r3
        elif action == 2 :
            rtime = r1 + r2 + r3 * 0.3
        else:
            rtime = r1 + r2 + r3
        #rstate
        rstate = 0
        #If s' < s, it means that next state is better than current state
        if "{0:b}".format(next_state).count("1") < "{0:b}".format(state).count("1"):
            rstate = self.C3
        reward = rtime + rstate
        return reward
  

#The total number of time slot
iteration = 600000    
    

#P : mean delay over P packets       
P = 100000


#The time slot to change requirement
time_slot_change = 100000

#start execution
system = System(P, time_slot_change)

#choose mode
print('Enter the mode: 1. greedy. 2. batch :')
flag = int(input())

if flag == 1 :
    system.Qlearning_greedy(iteration)
elif flag == 2 :
    system.Qlearning_batch(iteration)
else:
    print('error')


        
#Graph
plt.xlabel("time slot")
plt.ylabel("Mean delay")
#plt.ylim(0, 7)
plt.plot(range(len(system.statistic1)), system.statistic1, label='queue 1')
plt.plot(range(len(system.statistic2)), system.statistic2, label='queue 2')
plt.plot(range(len(system.statistic3)), system.statistic3, label='queue 3')
plt.plot(range(len(system.statisticb)), system.statisticb, label='best-effort queue')
plt.legend()
plt.show()       
















        