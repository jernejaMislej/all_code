#!/usr/bin/python3

import sys
import re
import csv
from itertools import product

printFlag=0
subject=""
sequence=""
sequenceDictionary={}

#function to generate datasets for all transitions length3
def generateTransitions3Datasets(condition, drug, eachComb):

    with open('../../../../processed_data/sequence_data_per_bout/'+ condition+'/Natural_'+ drug+'_'+ condition+'_'+eachComb+'Transition3Proportion','wt') as Transition3ProportionFile:

        writerTransition3ProportionFile = csv.writer(Transition3ProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

        for subject, sequences in sequenceDictionary.items():


            Transitions3Proportions=[]


            #set time of recording factor to zero, increase with each recording, going from 1 to 13
            timeOfRecording=0

            #go through all fish
            for recording in sequences:
        

                #extract all the bouts
                boutsList=re.findall('b.*?(?=b)',recording)

                #skip if there are no bouts(empty action sequence)
                if len(boutsList)==0:
                    Transitions3Proportions.append(100)
                    continue
                
                numberOfTransitions=0
                #go through all the bouts and append to the lists above
                for bout in boutsList:
                    #check that the bout has at least two turns in the bout(the bout could be empty or have one turn)
                    if len(bout)>3:
                        Transitions3Proportions.append(len(re.findall("(?="+eachComb+")",bout))/(len(bout)-3))
                    else:
                        Transitions3Proportions.append(100) #missing value here
                


            writerTransition3ProportionFile.writerow(Transitions3Proportions)




#function to generate datasets for all transitions length2
def generateTransitions2Datasets(condition, drug, eachComb):

    with open('../../../../processed_data/sequence_data_per_bout/'+ condition+'/Natural_'+ drug+'_'+ condition+'_'+eachComb+'Transition2Proportion','wt') as Transition2ProportionFile:

        writerTransition2ProportionFile = csv.writer(Transition2ProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

        for subject, sequences in sequenceDictionary.items():


            Transitions2Proportions=[]


            #set time of recording factor to zero, increase with each recording, going from 1 to 13
            timeOfRecording=0

            #go through all fish
            for recording in sequences:
        

                #extract all the bouts
                boutsList=re.findall('b.*?(?=b)',recording)

                #skip if there are no bouts(empty action sequence)
                if len(boutsList)==0:
                    Transitions2Proportions.append(100)
                    continue
                
                numberOfTransitions=0
                #go through all the bouts and append to the lists above
                for bout in boutsList:
                    #check that the bout has at least two turns in the bout(the bout could be empty or have one turn)
                    if len(bout)>2:
                        Transitions2Proportions.append(len(re.findall("(?="+eachComb+")",bout))/(len(bout)-2))
                    else:
                        Transitions2Proportions.append(100)#all empty bouts will have zero % of all turns
                


            writerTransition2ProportionFile.writerow(Transitions2Proportions)


#function to generate datasets for all turns
def generateTurnDatasets(turn, condition, drug):

    with open('../../../../processed_data/sequence_data_per_bout/'+ condition+'/Natural_'+ drug+'_'+ condition+'_'+turn+'Proportion','wt') as TurnProportionFile:



        writerTurnProportionFile = csv.writer(TurnProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)


        turnLetter=turn[0].lower()


        for subject, sequences in sequenceDictionary.items():


            TurnProportions=[]


            #set time of recording factor to zero, increase with each recording, going from 1 to 13
            timeOfRecording=0

            #go through all fish
            for recording in sequences:
        

                #extract all the bouts
                boutsList=re.findall('b.*?(?=b)',recording)


                #skip if there are no bouts(empty action sequence)
                if len(boutsList)==0:
                    TurnProportions.append(0)
                    continue

                #go through all the bouts and append to the lists above
                for bout in boutsList:
                    boutLen=len(bout)-1

                    #check if there are any truns in the bout(the bout could be empty)
                    if boutLen:
                        TurnProportions.append(len(re.findall(turnLetter,bout))/boutLen)
                    else:
                        TurnProportions.append(0)#all empty bouts will have zero % of all turns
                


            writerTurnProportionFile.writerow(TurnProportions)

    

    
#function to generate datasets for all conditions
def generateDatasets(condition, drug):

    with open('../../../../processed_data/sequence_data_per_bout/'+ condition+ '/Natural_'+ drug+'_'+condition+'_BoutLength','wt') as BoutLengthFile, open('../../../../processed_data/sequence_data_per_bout/'+ condition+ '/Natural_'+ drug+'_'+condition+'_TimeFactor','wt') as TimeFactorFile:



        writerBoutLengthFile = csv.writer(BoutLengthFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)


        writerTimeFactorFile = csv.writer(TimeFactorFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)



        for subject, sequences in sequenceDictionary.items():

            BoutLengths=[]


            TimeFactors=[]


            #set time of recording factor to zero, increase with each recording, going from 1 to 13
            timeOfRecording=0

            #go through all fish
            for recording in sequences:
        

                #extract all the bouts
                boutsList=re.findall('b.*?(?=b)',recording)

                #increase the time of recording
                timeOfRecording+=1


                #skip if there are no bouts(empty action sequence)
                if len(boutsList)==0:
                    TimeFactors.append(timeOfRecording)
                    BoutLengths.append(0)
                    continue

                #go through all the bouts and append to the lists above
                for bout in boutsList:
                    boutLen=len(bout)-1
                    BoutLengths.append(boutLen)    
                
                    TimeFactors.append(timeOfRecording)


            writerBoutLengthFile.writerow(BoutLengths)

            writerTimeFactorFile.writerow(TimeFactors)
        



drug=sys.argv[1][:-5]

#accumulate the sequences per 5-minute recording
with open(sys.argv[1],'r') as seqFile:

    for line in seqFile:
        if line.find('>')!=-1 and line.find(sys.argv[3])!=-1:
            res=re.search("Individual:..",line)        
            if res:            
                subject=res.group(0)
            printFlag=1
            continue
        if printFlag==1:
            if subject in sequenceDictionary:            
                sequenceDictionary[subject].append(line.rstrip().replace('L','').replace('R',''))
            else:
                sequenceDictionary[subject]=[line.rstrip().replace('L','').replace('R','')]
            printFlag=0


#make a list of turns
AllTurns=['Scoots','JBends','CBends','OBends','EBends','GBends','HBends','IBends']
condition=sys.argv[2]

drug+='_'+sys.argv[3]

#call the functions for each turn and condition and drug
for eachTurn in AllTurns:
    generateTurnDatasets(eachTurn, condition, drug)

#call function for each condition and drug only, (bout length and time factor)
generateDatasets(condition, drug)

#call function for each condition and drug and transition, 64 transitions length 2
letters=['s','j','c','o','e','g','h','i']
comb2=["".join(x) for x in product(letters, repeat=2)]
for eachComb in comb2:
    generateTransitions2Datasets(condition, drug, eachComb)

#call function for each condition and drug and transition, 512 transitions length 3 (max is 452 for DarkPTZ)
comb3=["".join(x) for x in product(letters, repeat=3)]
for eachComb in comb3:
    generateTransitions3Datasets(condition, drug, eachComb)






