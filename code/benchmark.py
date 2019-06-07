import nltk
import pandas as pd
import csv
import os
import string
import polyglot

from polyglot.text import Text
from nltk.tag import StanfordNERTagger
from nltk import word_tokenize, pos_tag, ne_chunk

BASE = os.path.join(os.path.dirname(__file__), "..")
DOCS = os.path.join(BASE, "data", "docs")
CORPUS = os.path.join(BASE, "data", "corpus")

"""
Reads WTO_NER_GroundTruth.
Returns a dict mapping fileids to human-identified entities.
"""
def read_csv():
    with open(r'/Users/Joyce/Dropbox/WTO/data/WTO_NER_GroundTruth.csv') as csvfile:
        reader = csv.DictReader(csvfile)
        results = dict()

        # skip header row
        i = 0
        for row in reader:
            if i == 0:
                i = i + 1
                continue    

            # This is used as a unique key to compare the same para-level data
            # between hand-coded and actual sets
            fname = row['X'] + '_' + row['docid'] + '_' + row['parnum'] + '.txt' 

            entitystring = row['Human-identified entities']
            entities = entitystring.split(',')
            for string in entities:
                string.strip(' ')
            results[fname] = entities
    return results

"""
Extract entities using the NLTK named entity chunker. 
Returns a dict mapping fileids to extracted PERSON, ORGANIZATION, GEOPOLITICAL, GEOGRAPHICAL 
entities.
"""
def nltk_entities(corpus, humancoded):
    results = dict()
    fileids = corpus.fileids()
    for fileid in fileids:
        if fileid in humancoded: # Only generate entities if we have human-coded copies
            results[fileid] = set() # Ensure that for each fileid, all recorded entities are unique
            text = nltk.pos_tag(corpus.words(fileid))
            for entity in nltk.ne_chunk(text): # An entity is a (word, POStag) tuple
                if isinstance(entity, nltk.tree.Tree):
                    etext = " ".join([word for word, tag in entity.leaves()])
                    label = entity.label()
                else:
                    continue
                if label == 'PERSON' or label == 'ORGANIZATION' or label == 'GPE' or label == 'GEO':
                    results[fileid].add(etext)
            print(fileid + ' ' + ', '.join(results[fileid]))
    return results

"""
Extract entities from each file using polyglot
"""
def polyglot_entities(corpus, humancoded):
    results = dict()
    fileids = corpus.fileids()
    for fileid in fileids:
        if fileid in humancoded:
            results[fileid] = set() # Ensure that for each fileid, all recorded entities are unique
            text = Text(corpus.raw(fileid), hint_language_code = 'en')
            for entity in text.entities:
                etext = " ".join(entity)
                if entity.tag == 'I-PER' or entity.tag == 'I-ORG' or entity.tag == 'I-locations':
                    results[fileid].add(etext)
            print(fileid + ' ' + ', '.join(results[fileid]))
    return results

"""
Extract entities from each file using StanfordNER
"""
def stanford_entities(corpus, humancoded):
    # Stanford Model Loading
    model = r'/Users/Joyce/Desktop/stanford-ner-2018-10-16/classifiers/english.all.3class.distsim.crf.ser.gz'
    jar = r'/Users/Joyce/Desktop/stanford-ner-2018-10-16/stanford-ner.jar'
    tagger  = StanfordNERTagger(model, jar)
    
    results = dict()
    fileids = corpus.fileids()

    for fileid in fileids:
        if fileid in humancoded:
            results[fileid] = set()
            text  = corpus.words(fileid)
            chunk = []
            for token, tag in tagger.tag(text):
                if tag == 'O':
                    if chunk:
                        # Flush the current chunk
                        etext =  " ".join([c[0] for c in chunk])
                        etag  = chunk[0][1]
                        chunk = []
                        if etag == 'PERSON' or etag == 'ORGANIZATION' or etag == 'LOCATION':
                            results[fileid].add(etext)
                else:
                    # Build chunk from tags
                    chunk.append((token, tag))
            print(fileid + ' ' + ', '.join(results[fileid]))
    return results

"""
Calculate precision, accuracy and recall based on 2 lists of handcoded and actual entities
"""
def metrics(truth,run):
    truth = truth
    run = run

    if float(len(truth)) == 0 and float(len(run)) == 0:
        return (1, 1, 1)

    # True positives
    TP = float(len(set(run) & set(truth)))
    if float(len(run)) >= float(TP):
        FP = len(run) - TP
    else:
        FP = TP - len(run)

    # True negatives
    TN = 0
    if len(truth) >= len(run):
        FN = len(truth) - len(run)
    else:
        FN = 0
    
    accuracy = 0
    recall = 0
    precision = 0

    if len(truth) != 0: # Avoid ZeroDivisionErrors
        accuracy = (float(TP)+float(TN))/float(len(truth))
        recall = (float(TP))/float(len(truth))
    if (float(FP)+float(TP)) != 0: # Avoid ZeroDivisionErrors
        precision = float(TP)/(float(FP)+float(TP))
    return (accuracy, recall, precision)

"""
Returns a float tuple (accuracy, recall, precision) given two dicts
"""
def benchmark(handcoded, actual):
    file_to_stats = dict()
    for k in handcoded:
        if k not in actual:
            raise Exception('File ID in handcoded set but not in machine-coded set')
        metric = metrics(handcoded[k], actual[k])
        file_to_stats[k] = metric

    accuracy = float(0)
    recall = float(0)
    precision = float(0)
    num = len(file_to_stats)
    for k in file_to_stats:
        accuracy += float(file_to_stats[k][0])
        recall += float(file_to_stats[k][1])
        precision += float(file_to_stats[k][2])

    accuracy /= float(num)
    recall /= float(num)
    precision /= float(num)

    print("The accuracy is %r" % accuracy)
    print("The recall is %r" % recall)
    print("The precision is %r" % precision)

    return (accuracy, recall, precision)

def main():
    # extract_corpus() # Run this in 11NER.py to generate corpus before running benchmark.py
    kddcorpus = nltk.corpus.PlaintextCorpusReader(CORPUS, '.*\.txt')

    hand_coded = read_csv()
    nltk_identified = nltk_entities(kddcorpus, hand_coded)
    #polyglot_identified = polyglot_entities(kddcorpus, hand_coded)
    #stanford_identified = stanford_entities(kddcorpus, hand_coded)

    nltkstats = benchmark(hand_coded, nltk_identified)
    #polyglotstats = benchmark(hand_coded, polyglot_identified)
    #stanfordstats = benchmark(hand_coded, stanford_identified)

main()
