'''
Created on Apr 15, 2013

@author: Riles
'''

from bs4 import *
import re
import os
import csv

initialPosts = [] #initiate list
count = 0 #just for looping print out progress
for root, dirs, files in os.walk('C:/Users/Riles/Documents/Northwestern/Projects/PLAITLab/glitchDataset/CSVandHTMLfiles/forumsRaw/marketplaceThreadsCopy/'):
    for fn in range(len(files)): #loop through all files in the above directory
        count += 1 #just for looping print out progress
        print count #just for looping print out progress
        html_doc = open(os.path.join(root,files[fn]), 'r') #open file for reading
        soup = BeautifulSoup(html_doc) #initiate parser - parse doc
        poster = soup.find(href=re.compile("/profiles/")).get('href') #reg expression to get link matching provided string
        title = soup.h4.get_text() #get text from h4 header html element
        post = soup.p.get_text() #get text from paragraph html element
        initialPosts.append({'index':fn, 'filename':files[fn], 'poster':poster, 'title':title, 'post':post}) #append values to dictionary
print initialPosts[0] #just test print

f = open('initialPosts.csv','wb') #initiate new file to write to
fieldnames = ['index','filename','poster','title','post'] #specify the header values for our csv
csvwriter = csv.DictWriter(f, delimiter = ',', fieldnames=fieldnames) #prep dictionary writer with delim and header
csvwriter.writerow(dict((fn,fn) for fn in fieldnames)) #create our header row
for row in initialPosts: #write from our previously created dictionary values (in the initialPosts list)
     csvwriter.writerow(row)
f.close() #close connection with file to write to