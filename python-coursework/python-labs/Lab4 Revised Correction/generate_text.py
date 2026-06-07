#!/usr/bin/env python3

import sys
import random
import text_stats as ts


def text_generation(ipFile , startWord , maxWord):
    
    listOfWordsToProcess = ts.preProcessing(ipFile)    
    wListCountDict , filteredList = ts.filterWord(listOfWordsToProcess)
    nOccurWOrds = ts.nextOccuranceWords(startWord.lower() , filteredList)
    freq = ts.frequencyFollowingWords(nOccurWOrds)    
  
    
    totalSum = len(nOccurWOrds)  
    
    normalize_frequency = {key:float(value)/totalSum for key,value in freq.items()}
    
    sortedNormalizedFreq = sorted(normalize_frequency.items() , key = lambda item : item[1] , reverse = True)
    
    words = [word[0]  for word in sortedNormalizedFreq]
    weights = [weight[1] for weight in sortedNormalizedFreq]
    
    msgArray  = random.choices(words , weights , k = int(maxWord))
    
    genText = " ".join(msgArray)
    
    return genText
    
    
def main():
    if len(sys.argv) < 2:
        raise NameError("Please provide input text file a, starting word and maximum number of words")
    elif(len(sys.argv) < 3):
        raise NameError("Please provide starting word and maximum number of words")
    elif(len(sys.argv) < 4):
        raise NameError("Please provide maximum number of words")
        
    ipFile = sys.argv[1]
    startWord = sys.argv[2]
    maxWord = sys.argv[3]
    
    generated = text_generation(ipFile , startWord , maxWord)
    
    print("-------------------------------------------------------")
    print("/n")
    
    print(generated)
    
    print("/n")    
    print("-------------------------------------------------------")
    

if __name__ == "__main__" :
    main()    
    
    
    
    
    
