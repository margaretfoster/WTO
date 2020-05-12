#!/usr/bin/env/ python

import sys
import nltk
import json
import csv
import numpy as np
import pandas as pd
import re
import codecs
import pandas
import string
import os
import subprocess
import unicodedata
import tempfile
import io
from wbCountryCodes import wb_codes # Import dict of World Bank country codes

from collections import defaultdict
from nltk import word_tokenize, pos_tag, ne_chunk
from nltk.tag import StanfordNERTagger
from nltk.chunk import conlltags2tree, tree2conlltags


if (len(sys.argv) < 3):
    print ("You need to specify an input file and an output file!")
    sys.exit(1)
    pass
    
infile=sys.argv[1] ## this is a csv of the paragraph data
outfile=sys.argv[2] ## filename to save to
tempdir = tempfile.mkdtemp()


def utf_8_encoder(unicode_csv_data):
    for line in unicode_csv_data:
        yield line.encode('utf-8')

## specify the list of entites to find:
## Take the list of named countries in the word bank code corpus

customnames = wb_codes.copy()
customnames = customnames.keys()
## Add non-country entities names:                                                                                                                                     
toadd = ['Committee', 'Secretariat',
         'Chairman', 'Chairperson',
         'Members', 'European Union',
         'Member', "Chairman "]

otherstates= ['African Group', 'Russian Federation',
                'Chinese Taipei']
## Organizations:
## orgs invited 

orgs= ['LDC Group','United Nations', 'ITTC',
       'ASEAN', 'Development Division',
       'African Union','Arab Maghreb Union',
       'UNECE', 'Economic Community of Central African States',
       'Economic Community of West African States',
       'Economic Cooperation Organisation',
       'Inter-Arab Investment Guarantee Corporation',
       'Islamic Development Bank',
       'Organisation of the Islamic Conference',
       'South Centre','the Pacific Islands Forum',
       'West African Economic and Monetary Union',
       'World Intellectual Property Organization',
       'League of Arab States','OPEC', "ITC",
       'Gulf Organization for Industrial Consulting',
       'Organisation Internationale de la Francophonie',
       'Common Fund for Commodities', 'OAPEC',
       'Groupe de la Banque Africaine de Developpement']


customnames.extend(toadd)
customnames.extend(otherstates)

print(customnames)
customnames.extend(orgs)
 
##Pass in location of the data
##Extract paratext from input csv into .txt files for tagging

def extract_corpus(): ## declare the function + required arguments
    with io.open(infile, mode='r', encoding = 'utf-8') as csvfile: 
        reader = csv.DictReader(utf_8_encoder(csvfile))

        for row in reader:
            para = row['text']
            #document = filter(lambda char: char in string.printable, unicodedata.normalize('NFKD', para))
            document = filter(lambda char: char in string.printable, para)
            fname = row[''] + '_' + row['doc'] + '.txt'
            outpath = os.path.join(tempdir, fname)
            with codecs.open(outpath, 'w') as f:
                f.write(para)

                
"""
Extract entities using the NLTK named entity chunker. 
Returns a dict mapping fileids to extracted PERSON, ORGANIZATION, GEOPOLITICAL, GEOGRAPHICAL 
entities, collapsed into a single section
## filtered to only include the custom list of country names and speaker entities.
"""
def nltk_entities(corpus):
    results = dict()
    fileids = corpus.fileids()

    for fileid in fileids:
        results[fileid] = set() # Ensure that for each fileid, all recorded entities are unique
        text = nltk.pos_tag(corpus.words(fileid))
        for entity in nltk.ne_chunk(text): # An entity is a (word, POStag) tuple
            if isinstance(entity, nltk.tree.Tree):
                etext = " ".join([word for word, tag in entity.leaves()])
                label = entity.label()
            else:
                continue
            if label == 'PERSON' or label == 'ORGANIZATION' or label == 'GPE' or label == 'GEO':
                    ##results[fileid].add(etext) ## adds the text
                    if etext in customnames : ## Keep this clause if you want it to keep only those entiites in the custom list
                         results[fileid].add(etext) ## Adds the text
        results[fileid] = ', '.join(results[fileid])
        print(fileid + " " + results[fileid])
    return results

                
"""
Write clean NER to output csv
"""

def write_csv(entities):
    with io.open(infile, mode='r', encoding = 'utf-8') as csvfile:
        reader = csv.DictReader(utf_8_encoder(csvfile))
        data = []
        for row in reader:
            # get entities from dict
            k = row['']
            print(k)

            para = row['']
            docid = row['doc']
            paratext = row['text']
            key = row['key'] ## paragraph number
            fileid=para+"_"+docid + ".txt"
            ents = entities[fileid].encode("ascii","ignore")
            print(ents)
#           data.append((para, docid, parnum, paratext, countryspeaker, date, numdate, ents))
            data.append([para, docid, key, paratext, ents])
            fieldnames= ['para', 'docid', 'key', 'paratext', 'ents']
            with open(outfile, mode = 'w') as csv_file:
                writer = csv.writer(csv_file)
                writer.writerow([n for n in fieldnames])
                for row in data:
                    writer.writerow(row)
        
def main():
    extract_corpus()
    kddcorpus = nltk.corpus.PlaintextCorpusReader(tempdir, '.*\.txt')
    nltkents = nltk_entities(kddcorpus)
    write_csv(nltkents)

main()
