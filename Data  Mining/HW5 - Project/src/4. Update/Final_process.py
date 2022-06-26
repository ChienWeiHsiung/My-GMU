import pandas as pd

data = pd.read_csv('First_process_data.csv').to_dict('records')

for i in data:
    i['date'] = i['date'].split(' ')[0][6:].replace('-','/')

#read the already existing Final_data and add new data into it    
data_before = pd.read_csv('../3.Process_data/Final_data.csv').to_dict('records')
data = data + data_before
#Sort and Save           
final_data = pd.DataFrame(data)
final_data = final_data.sort_values(by=['date'])
final_data.to_csv('../3.Process_data/Final_data.csv', index = False)

#Use groupby
grouped = final_data.groupby('model')

#Count the number of cases of each processor type
model_list = []
for model,group in grouped:
    dic = {
            'number' : len(group),
            'type' : model
        }
    model_list.append(dic)

#Sort and Save
model_list = pd.DataFrame(model_list)
model_list = model_list.sort_values(by=['number'], ascending=False)
model_list.to_csv('../3.Process_data/Model_list.csv', index = False)  

