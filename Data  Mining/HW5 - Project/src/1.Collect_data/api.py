import pandas as pd
from ebaysdk.finding import Connection as finding

#ebay api
api = finding(siteid='EBAY-US', appid='ChienWei-hsiung-PRD-0fa9190e6-77c32c8d')

request = {
    'categoryId' : ['164'],
    'itemFilter': [
        {'name': 'Condition', 'value': 'Used'}
    ],
    'aspectFilter': [
        {'aspectName': 'Brand', 'aspectValueName': 'AMD'}
    ],
    'paginationInput': {
        'entriesPerPage': '100',
        'pageNumber': '1' 	 
    },
    'sortOrder': 'StartTimeNewest'
}

#store all the data
data = []

#look for AMD
for i in range(1, 46):
    print(i)
    request['paginationInput']['pageNumber'] = str(i)
    dictstr = api.execute('findItemsByCategory', request)
    for j in dictstr.reply.searchResult.item:
        product = {
            'soldprice' : j.sellingStatus.convertedCurrentPrice.value,
            'date' : j.listingInfo.startTime,
            'link' : j.viewItemURL
        }   
        data.append(product)

#look for Intel
request['aspectFilter'][0]['aspectValueName'] = 'Intel'        
for i in range(1, 101):
    print(i)
    request['paginationInput']['pageNumber'] = str(i)
    dictstr = api.execute('findItemsByCategory', request)
    for j in dictstr.reply.searchResult.item:
        product = {
            'soldprice' : j.sellingStatus.convertedCurrentPrice.value,
            'date' : j.listingInfo.startTime,
            'link' : j.viewItemURL
        }   
        data.append(product)        
        
#Save      
Final_data = pd.DataFrame(data)
Final_data.to_csv('product.csv',index = False)