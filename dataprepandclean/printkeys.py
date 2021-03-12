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

## copy country names from the wbCountryCodes list:
customents = wb_codes.copy()

entscustom = customents.keys()
## Add chairman, member:

toadd = ['Committee', 'Secretariat', 'Chairman',
         'Chairperson', 'Members', 'European Union']

entscustom.extend(toadd)
## Add extra list of speaking entities:
print(entscustom)

# def utf_8_encoder(unicode_csv_data):
#     for line in unicode_csv_data:
#         yield line.encode('utf-8')

#         """" 
# Pass in location of the data
# """
# """
# Extract paratext from input csv into .txt files for tagging
# """
# def extract_corpus(): ## declare the function + required arguments
#     with io.open(infile, mode='r', encoding = 'latin1') as csvfile: 
#         reader = csv.DictReader(utf_8_encoder(csvfile))

#         for row in reader:
#             para = row['text']
#             #document = filter(lambda char: char in string.printable, unicodedata.normalize('NFKD', para))
#             document = filter(lambda char: char in string.printable, para)
#             fname = row[''] + '_' + row['doc'] + '.txt'
#             outpath = os.path.join(tempdir, fname)
#             with codecs.open(outpath, 'w') as f:
#                 f.write(para)

                
# """
# Extract entities using the NLTK named entity chunker. 
# Returns a dict mapping fileids to extracted PERSON, ORGANIZATION, GEOPOLITICAL, GEOGRAPHICAL 
# entities, collapsed into a single section and filtered to only include country names.
# """
# def nltk_entities(corpus):
#     results = dict()
#     fileids = corpus.fileids()

#     for fileid in fileids:
#         results[fileid] = set() # Ensure that for each fileid, all recorded entities are unique
#         text = nltk.pos_tag(corpus.words(fileid))
#         for entity in nltk.ne_chunk(text): # An entity is a (word, POStag) tuple
#             if isinstance(entity, nltk.tree.Tree):
#                 etext = " ".join([word for word, tag in entity.leaves()])
#                 label = entity.label()
#             else:
#                 continue
#             if label == 'PERSON' or label == 'ORGANIZATION' or label == 'GPE' or label == 'GEO':
#                     results[fileid].add(etext)
#                 ##if etext in wb_codes.keys(): ## Add this line if you want only country names

#         results[fileid] = ', '.join(results[fileid])
#         print(fileid + " " + results[fileid])
#     return results



                
# """
# Write clean NER to output csv
# """

# def write_csv(entities):
#     with io.open(infile, mode='r', encoding = 'utf-8') as csvfile:
#         reader = csv.DictReader(utf_8_encoder(csvfile))
#         data = []
#         for row in reader:
#             # get entities from dict
#             k = row['']
#             print(k)

#             para = row['']
#             docid = row['doc']
#             paratext = row['text']
#             key = row['key'] ## paragraph number
# #            countryspeaker = row['country.speaker']
# #            date = row['date']
# #            numdate = row['numdate']
#             fileid=para+"_"+docid + ".txt"
#             ents = entities[fileid].encode("ascii","ignore")
#             print(ents)

# #           data.append((para, docid, parnum, paratext, countryspeaker, date, numdate, ents))
#             data.append([para, docid, key, ents])
#             with open(outfile, mode = 'w') as csv_file:
#                 writer = csv.writer(csv_file)
#                 for row in data:
#                     writer.writerow(row)
        
# def main():
#     extract_corpus()
#     kddcorpus = nltk.corpus.PlaintextCorpusReader(tempdir, '.*\.txt')
#     nltkents = nltk_entities(kddcorpus)
#     write_csv(nltkents)

# main()
