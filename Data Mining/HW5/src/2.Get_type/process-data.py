import requests
import pandas as pd
from bs4 import BeautifulSoup


def get_data(url):
    r = requests.get(url)
    soup = BeautifulSoup(r.text, 'html.parser')
    return soup

def parse(soup):
    if soup.find('section', {'class': 'product-spectification'}):
        try:
            product = soup.find('section', {'class': 'product-spectification'}).get_text(separator='.').split('Processor Model')
            product = product[1].split('.')[1]
        except:
            product = 'NO'
    elif soup.find('div', {'class': 'itemAttr'}):
        try:
            product = soup.find('div', {'class': 'itemAttr'}).text.split('Processor Model:')[1].strip().split('\n')[0]
        except:
            product = 'NO'
    else:
        product = 'NO'
    return product

def output(data):
    products = pd.DataFrame(data)
    products.to_csv('product2.csv', index = False)
    

data = pd.read_csv('../1.Collect_data/product.csv').to_dict('records')

#parse the link, and then get the processor type    
k = 0
for i in data:
    print(k)
    k += 1
    url = i['link']
    del i['link']
    soup = get_data(url)
    model = parse(soup)
    i['model'] = model
    
output(data)