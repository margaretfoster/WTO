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
from wbCountryCodes import wb_codes

from collections import defaultdict
from nltk import word_tokenize, pos_tag, ne_chunk
from nltk.tag import StanfordNERTagger
from nltk.chunk import conlltags2tree, tree2conlltags

BASE = os.path.join(os.path.dirname(__file__), "..")
DOCS = os.path.join(BASE, "data", "docs")
CORPUS = os.path.join(BASE, "data", "corpus")

"""
Extract paratext from WTOData.csv into .txt files for tagging
"""
def extract_corpus(docs = DOCS, corpus = CORPUS):
    if not os.path.exists(corpus):
        os.mkdir(corpus)

    with open(r'/Users/Joyce/Desktop/2018/Johnson research/WTO/code/WTOData.csv', encoding = 'latin1') as csvfile:
        reader = csv.DictReader(csvfile)

        # skip header row
        i = 0
        for row in reader:
            if i == 0:
                i = i + 1
                continue    

            para = row['paratext']
            document = filter(lambda char: char in string.printable, unicodedata.normalize('NFKD', para))
            fname = row[''] + '_' + row['docid'] + '_' + row['parnum'] + '.txt'
            outpath = os.path.join(corpus, fname)
            with codecs.open(outpath, 'w') as f:
                f.write(para)

#TODO: Figure out why StanfordNERTagger is breaking
# from glob import glob
# FILELIST = [os.path.abspath(f) for f in glob(r'/Users/Joyce/Desktop/2018/Johnson research/WTO/data/corpus/*.txt')]
# def tag_paras(filelist = FILELIST):
#     model = r'/Users/Joyce/Desktop/stanford-corenlp-full-2018-10-05/stanford-corenlp-3.9.2-models.jar'
#     jar = r'/Users/Joyce/Desktop/stanford-corenlp-full-2018-10-05/stanford-corenlp-3.9.2.jar'
#     st = StanfordNERTagger(model, jar, 'latin1')
#     for f in filelist:
#         with open(f, 'r') as infile:
#             for line in infile:
#                 print(st.tag(line.split()))

"""
Extract entities using the NLTK named entity chunker.
"""
def nltk_entities(fileids, section, corpus):
    results = defaultdict(lambda: defaultdict(list))
    fileids = fileids or corpus.fileids()
    for fileid in fileids:
        print(fileid + '\n')
        if section is not None:
            text = nltk.pos_tag(nltk.word_tokenize(list(sectpull([fileid],section=section))[0][1]))
        else:
            text = nltk.pos_tag(corpus.words(fileid))
        for entity in nltk.ne_chunk(text):
            # an entity is a (word, POStag) tuple
            if isinstance(entity, nltk.tree.Tree):
                etext = " ".join([word for word, tag in entity.leaves()])
                label = entity.label()
            else:
                continue
            if label == 'PERSON':
                key = 'persons'
            elif label == 'ORGANIZATION':
                key = 'organizations'
            elif label == 'GPE':
                key = 'geopolitical'
            else:
                key = None
            # Only append unique entities 
            if (key and (etext not in results[fileid][key])): 
                results[fileid][key].append(etext)
    return results

"""
Extract entities using the NLTK named entity chunker. 
Returns a dict mapping fileids to extracted PERSON, ORGANIZATION, GEOPOLITICAL, GEOGRAPHICAL 
entities, collapsed into a single section.
"""
def nltk_entities_merged(corpus):
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
                if etext in wb_codes.keys(): # Only append country names
                    results[fileid].add(etext)
        results[fileid] = ', '.join(results[fileid])
        print(fileid + " " + results[fileid])
    return results


"""
Utility function to convert json file containing paragraph-level entities to a .csv
"""
def json_to_csv(jsonFile):
    with open(jsonFile) as jsondata:
        df = pandas.read_json(jsondata)
        df.to_csv(path_or_buf = r'/Users/Joyce/Desktop/Johnson research/WTO/data/NER.csv')


def write_csv(entities):
    with open(r'/Users/Joyce/Desktop/Johnson research/WTO/code/WTOData.csv', encoding = 'latin1') as csvfile:
        reader = csv.DictReader(csvfile)
        data = []
        # skip header row
        i = 0
        for row in reader:
            if i == 0:
                i = i + 1
                continue    
            # get entities from dict
            #k = row[''] + '_' + row['docid'] + '_' + row['parnum'] + '.txt'
            k = row['']
            print(k)

            para = row['']
            docid = row['docid']
            parnum = row['parnum']
            paratext = row['paratext']
            countryspeaker = row['country.speaker']
            date = row['date']
            numdate = row['numdate']
            ents = entities[k]
            # ents = ", ".join(entities[k])
            print(ents)
            data.append((para, docid, parnum, paratext, countryspeaker, date, numdate, ents))
        with open(r'/Users/Joyce/Desktop/Johnson research/WTO/data/WTODataNew.csv', mode = 'w', encoding = 'latin1') as csv_file:
            writer = csv.writer(csv_file)
            for para, docid, parnum, paratext, countryspeaker, date, numdate, ents in data:
                writer.writerow([para, docid, parnum, paratext, countryspeaker, date, numdate, ents])
        
def main():
    # extract_corpus()
    kddcorpus = nltk.corpus.PlaintextCorpusReader(CORPUS, '.*\.txt')
    nltkents = nltk_entities_merged(kddcorpus)

    write_csv(nltkents)

main()