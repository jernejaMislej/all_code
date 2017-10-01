import sys
import re
import csv

printFlag=0
subject=""
sequence=""
sequenceDictionary={}
AripiprazoleOutlierIndividuals=['Individual:3;','Individual:27','Individual:42','Individual:23','Individual:47']
AripiprazoleOutlierRecordings=[12,4,6,11,8]
CariprazineOutlierIndividuals=['Individual:2;', 'Individual:2;', 'Individual:12','Individual:16','Individual:24','Individual:24']
CariprazineOutlierRecordings=[5,9,12,9,3,12]

#function to generate datasets for all turns
def generateTurnDatasets(turn, condition):

    with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_'+turn+'Proportion_Normal','wt') as \
    TurnProportionNormalFile, open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_'+turn+\
    'Proportion_Outlier','wt') as TurnProportionOutlierFile:



        writerTurnProportionNormalFile = csv.writer(TurnProportionNormalFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
        writerTurnProportionOutlierFile = csv.writer(TurnProportionOutlierFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)


        turnLetter=turn[0].lower()


        for subject, sequences in sequenceDictionary.items():

            if subject in AripiprazoleOutlierIndividuals or subject in CariprazineOutlierIndividuals:

                TurnProportionsNormal=[sys.argv[1]+"_"+subject]
                TurnProportionsOutlier=[sys.argv[1]+"_"+subject]
    
                timeOfRecording=0
    
                #go through all fish
                for recording in sequences:
                    
                    timeOfRecording+=1
                    
                    recording=recording.replace('b','')
    
                    if ((sys.argv[1]=='AripiprazoleDarkApoLow') and (subject in AripiprazoleOutlierIndividuals) and \
                    (AripiprazoleOutlierRecordings[AripiprazoleOutlierIndividuals.index(subject)]==timeOfRecording)) or \
                    ((sys.argv[1]=='CariprazineDarkApoLow') and (subject in CariprazineOutlierIndividuals) and \
                    ((CariprazineOutlierRecordings[CariprazineOutlierIndividuals.index(subject)]==timeOfRecording) or \
                    ((CariprazineOutlierRecordings[CariprazineOutlierIndividuals.index(subject)+1]==timeOfRecording) and \
                     (CariprazineOutlierIndividuals[(CariprazineOutlierIndividuals.index(subject)+1)]==subject)))): 
    
                        #skip if there are no bouts(empty action sequence)
                        if len(recording)==0:
                            TurnProportionsOutlier.append(0)
                        else:
                            TurnProportionsOutlier.append(round(float(len(re.findall(turnLetter,recording)))/len(recording),3))
                                
                    else:
                        #skip if there are no bouts(empty action sequence)
                        if len(recording)==0:
                            TurnProportionsNormal.append(0)
                        else:
                            TurnProportionsNormal.append(round(float(len(re.findall(turnLetter,recording)))/len(recording),3))
    
                writerTurnProportionNormalFile.writerow(TurnProportionsNormal)
                writerTurnProportionOutlierFile.writerow(TurnProportionsOutlier)
            else:
                TurnProportionsNormal=[sys.argv[1]+"_"+subject]
                timeOfRecording=0
    
                #go through all fish
                for recording in sequences:
                    
                    timeOfRecording+=1
                    
                    recording=recording.replace('b','')
                    #skip if there are no bouts(empty action sequence)
                    if len(recording)==0:
                        TurnProportionsNormal.append(0)
                    else:
                        TurnProportionsNormal.append(round(float(len(re.findall(turnLetter,recording)))/len(recording),3))                 
                writerTurnProportionNormalFile.writerow(TurnProportionsNormal)

    
#function to generate datasets for all conditions
def generateDatasets(condition):

    with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+\
    '_BoutCount_Normal','wt') as BoutCountNormalFile, \
    open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+\
    '_SequenceLength_Normal','wt') as SequenceLengthNormalFile, \
    open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+\
    '_BoutCount_Outlier','wt') as BoutCountOutlierFile, \
    open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+\
    '_SequenceLength_Outlier','wt') as SequenceLengthOutlierFile:



        writerBoutCountNormalFile = csv.writer(BoutCountNormalFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

        writerSequenceLengthNormalFile = csv.writer(SequenceLengthNormalFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)


        writerBoutCountOutlierFile = csv.writer(BoutCountOutlierFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

        writerSequenceLengthOutlierFile = csv.writer(SequenceLengthOutlierFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
        

        for subject, sequences in sequenceDictionary.items():
            
            if subject in AripiprazoleOutlierIndividuals or subject in CariprazineOutlierIndividuals:
                 
                BoutCountsNormal=[sys.argv[1]+"_"+subject]
    
                SequenceLengthNormal=[sys.argv[1]+"_"+subject]    
    
                BoutCountsOutlier=[sys.argv[1]+"_"+subject]
    
                SequenceLengthOutlier=[sys.argv[1]+"_"+subject]
    
                timeOfRecording=0
                
                for recording in sequences:
                    
                    timeOfRecording+=1
                    
                    #extract all the bouts                    
                    boutsList=re.findall('b.*?(?=b)',recording)
    
                    
                    if ((sys.argv[1]=='AripiprazoleDarkApoLow') and (subject in AripiprazoleOutlierIndividuals) and (AripiprazoleOutlierRecordings[AripiprazoleOutlierIndividuals.index(subject)]==timeOfRecording)) or (((sys.argv[1]=='CariprazineDarkApoLow')) and (subject in CariprazineOutlierIndividuals) and ((CariprazineOutlierRecordings[CariprazineOutlierIndividuals.index(subject)]==timeOfRecording) or (CariprazineOutlierRecordings[CariprazineOutlierIndividuals.index(subject)+1]==timeOfRecording and CariprazineOutlierIndividuals[CariprazineOutlierIndividuals.index(subject)+1]==subject))): 
        
        
                        #skip if there are no bouts(empty action sequence)
                        if len(boutsList)==0:
                            SequenceLengthOutlier.append(0)
                            BoutCountsOutlier.append(0)
                        else:
                            BoutCountsOutlier.append(len(boutsList))    
                            SequenceLengthOutlier.append(len(recording))
 
                                   
                    else:
        
                        #skip if there are no bouts(empty action sequence)
                        if len(boutsList)==0:
                            SequenceLengthNormal.append(0)
                            BoutCountsNormal.append(0)
                        else:
                            BoutCountsNormal.append(len(boutsList))    
                            SequenceLengthNormal.append(len(recording))

                writerBoutCountOutlierFile.writerow(BoutCountsOutlier)
                writerSequenceLengthOutlierFile.writerow(SequenceLengthOutlier) 
                writerBoutCountNormalFile.writerow(BoutCountsNormal)
                writerSequenceLengthNormalFile.writerow(SequenceLengthNormal)                
            else:
                timeOfRecording=0
                
                BoutCountsNormal=[sys.argv[1]+"_"+subject]
    
                SequenceLengthNormal=[sys.argv[1]+"_"+subject] 
                               
                #go through all fish
                for recording in sequences:
                    
                    timeOfRecording+=1
                    
                    #extract all the bouts                    
                    boutsList=re.findall('b.*?(?=b)',recording)
                     #skip if there are no bouts(empty action sequence)
                    if len(boutsList)==0:
                        SequenceLengthNormal.append(0)
                        BoutCountsNormal.append(0)
                    else:
                        BoutCountsNormal.append(len(boutsList))    
                        SequenceLengthNormal.append(len(recording))  
                writerBoutCountNormalFile.writerow(BoutCountsNormal)
                writerSequenceLengthNormalFile.writerow(SequenceLengthNormal)                        
                         
        

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

