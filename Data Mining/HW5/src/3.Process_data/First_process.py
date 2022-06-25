import pandas as pd

data = pd.read_csv('../2.Get_type/product2.csv')

#get rid of useless case
drop_list = []
for index, row in data.iterrows():
    if 'amd' in str(row['model']).lower():
        continue
    elif 'intel' in str(row['model']).lower():
        continue
    else:
        drop_list.append(index)

data = data.drop(drop_list)

#transform to lower case
for i in data.index:
    data.at[i, 'model'] = data.at[i, 'model'].lower()
    
#Sort and Save
data = data.sort_values(by=['model'], ascending=False)
data.to_csv('First_process_data.csv', index = False)  

        
   

