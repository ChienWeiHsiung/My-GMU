
import numpy as np
import nltk
import re 
from nltk.corpus import wordnet

#---------------------------------------------------------#
from datetime import datetime
startTime = datetime.now()
#---------------------------------------------------------#


#determine the part of speech of a word
def get_subject(word):
    if word.startswith('J'):
        return wordnet.ADJ
    elif word.startswith('V'):
        return wordnet.VERB
    elif word.startswith('N'):
        return wordnet.NOUN
    elif word.startswith('R'):
        return wordnet.ADV
    else:
        return None

# stopwords = useless words, lemmatize can convert a word to its basic form
stopwords = nltk.corpus.stopwords.words('english')
lemmatize = nltk.WordNetLemmatizer()

f = open("../train_file.dat", "r")

#wordlist stores all the results of each sentiment
#freq store frequency of a word,  review stores rating (+1 or -1)
wordlist = []
freq = []
review = []

for line in f.readlines():
    #determine rating of each sentiment
    if line[0] == '+':
        review.append(1)
    else:
        review.append(-1)
    #temp_data stores splited words of each sentiment, temp_frep : frequency of a word
    temp_data = []
    temp_freq = []
    #convert to lowercase
    line = line.lower()
    #remove punctuation(except ') and number (Because there are words like I'm. These words will be removed by the rest of program)
    Re = re.compile(r'[^a-zA-Z \']+')
    line =  Re.sub(' ', line)
    #split the sentiment
    tokens = line.split()
    #stopwords : useless words 
    stopwords = nltk.corpus.stopwords.words('english')
    #remove useless word and the words contain ' (ex: I'm, she's)
    tokens = [t for t in tokens if t not in stopwords and '\'' not in t and len(t) > 1]
    #get the part of speech of all the words
    tokens_subject = nltk.pos_tag(tokens)
    #convert a word to its basic form, and add them in temp_data, and compute frequency
    for tag in tokens_subject:
        #get the part of speech of a words
        subject = get_subject(tag[1]) or wordnet.NOUN
        #convert a word to its basic form
        word = lemmatize.lemmatize(tag[0], subject)
        if word not in temp_data :
            temp_data.append(word)
            temp_freq.append(1)
        else:
            temp_freq[temp_data.index(word)] += 1
    wordlist.append(temp_data)
    freq.append(temp_freq)
f.close() 
 
a_data = np.array(wordlist, list)
a_freq = np.array(freq, list)
a_review = np.array(review, int)

#save data as the files
np.save("train_word", a_data)
np.save("train_freq", a_freq)
np.save("train_review", a_review)

#---------------------------------------------------------#
print(datetime.now() - startTime)
#---------------------------------------------------------#


