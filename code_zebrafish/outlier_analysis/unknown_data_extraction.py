import sys
import re
import csv

printFlag=0
subject=""
sequence=""
sequenceDictionary={}

#function to generate datasets for all turns
def generateTurnDatasets(turn, condition):

    with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_'+turn+'Proportion_Unknown','wt') as \
    TurnProportionUnknownFile:



        writerTurnProportionUnknownFile = csv.writer(TurnProportionUnknownFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

        turnLetter=turn[0].lower()


        for subject, sequences in sequenceDictionary.items():

            TurnProportionsUnknown=[sys.argv[1]+"_"+subject]
            timeOfRecording=0
    
            #go through all fish
            for recording in sequences:
                    
                timeOfRecording+=1
                                    
                recording=recording.replace('b','')
                #skip if there are no bouts(empty action sequence)
                if len(recording)==0:
                    TurnProportionsUnknown.append(0)
                else:
                    TurnProportionsUnknown.append(round(float(len(re.findall(turnLetter,recording)))/len(recording),5))   
            writerTurnProportionUnknownFile.writerow(TurnProportionsUnknown)

    
#function to generate datasets for all conditions
def generateDatasets(condition):

    with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+\
    '_BoutCount_Unknown','wt') as BoutCountUnknownFile, \
    open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+\
    '_SequenceLength_Unknown','wt') as SequenceLengthUnknownFile:



        writerBoutCountUnknownFile = csv.writer(BoutCountUnknownFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

        writerSequenceLengthUnknownFile = csv.writer(SequenceLengthUnknownFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
 

        for subject, sequences in sequenceDictionary.items():
            
   
            timeOfRecording=0
            
            BoutCountsUnknown=[sys.argv[1]+"_"+subject]

            SequenceLengthUnknown=[sys.argv[1]+"_"+subject] 
                           
            #go through all fish
            for recording in sequences:
                
                timeOfRecording+=1
                
                #extract all the bouts                    
                boutsList=re.findall('b.*?(?=b)',recording)
                 #skip if there are no bouts(empty action sequence)
                if len(boutsList)==0:
                    SequenceLengthUnknown.append(0)
                    BoutCountsUnknown.append(0)
                else:
                    BoutCountsUnknown.append(len(boutsList))    
                    SequenceLengthUnknown.append(len(recording))  
            writerBoutCountUnknownFile.writerow(BoutCountsUnknown)
            writerSequenceLengthUnknownFile.writerow(SequenceLengthUnknown)                        
                         
        

#accumulate the sequences per 5-minute recording
with open(sys.argv[1],'r') as seqFile:

    for line in seqFile:
        if line.find('>')!=-1:
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



#call the functions for each turn and condition
generateDatasets(condition)
for eachTurn in AllTurns:
    generateTurnDatasets(eachTurn, condition)

