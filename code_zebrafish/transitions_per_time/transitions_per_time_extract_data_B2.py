#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
word2Dictionary={}
word3Dictionary={}
subject=""
sequence=""
sequenceDictionary={}

#check condition, set the drug name and extreme length
condition=sys.argv[2]

if condition == "DarkApoLow":
    drug=sys.argv[1][:-10]
    
elif condition == "DarkPTZ":
    drug=sys.argv[1][:-7]
      
elif condition == "DarkApoHigh":
    drug=sys.argv[1][:-11]

elif condition == "Dark":
    drug=sys.argv[1][:-4]
    
elif condition == "Light":
    drug=sys.argv[1][:-5]
    
elif condition == "LightDark":
    drug=sys.argv[1][:-9]


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



drug+='_'+sys.argv[3]

no_subjects=len(sequenceDictionary)

subjectOrder=0

for subject in sorted(sequenceDictionary):

    #set time of recording factor to zero, increase with each recording, going from 1 to 13
    timeOfRecording=0
        
    #go through all fish
    for recording in sequenceDictionary[subject]:


        #extract all the bouts
        boutsList=re.findall('b.*?(?=b)',recording)

        if len(boutsList)>0:
            for bout in boutsList:
                if len(bout)>2:
                    for pos in range(1,len(bout)-1):
                        if bout[pos:pos+2] in word2Dictionary:
                            word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder]=word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder]+1
                        else:
                            word2Dictionary[bout[pos:pos+2]]=[[0 for x in range(no_subjects)] for y in range(13)]
                            word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder]=1
                if len(bout)>3:
                    for pos in range(1,len(bout)-2):
                        if bout[pos:pos+3] in word3Dictionary:
                            word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder]=word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder]+1
                        else:
                            word3Dictionary[bout[pos:pos+3]]=[[0 for x in range(no_subjects)] for y in range(13)]
                            word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder]=1


        
        #increase the time of recording and subject
        timeOfRecording+=1
    subjectOrder+=1
        
 
''' 
for key,value in word2Dictionary.items():
    print(key)
    print(value)

for key,value in word3Dictionary.items():
    print(key)
    print(value)
'''
#word2

#turn into proportions of all transitions for each time frame for each subject
word2_values=[ v for v in sorted(word2Dictionary.values()) ]
word2_values_t=list(zip(*word2_values))
for time_frame in range(0,13):
    word2_values_t_t=list(zip(*word2_values_t[time_frame]))
    for subj in range(0,len(word2_values_t_t)):
        if sum(word2_values_t_t[subj]) != 0:
               word2_values_t_t[subj]=[float(x)/sum(word2_values_t_t[subj]) for x in word2_values_t_t[subj]]
    word2_values_t[time_frame]=list(zip(*word2_values_t_t))
    
word2_values=list(zip(*word2_values_t))
dict_value=0
for keys in sorted(word2Dictionary, key=word2Dictionary.get):
    word2Dictionary[keys]=word2_values[dict_value]
    dict_value+=1


#take mean for all subjects in each time frame


for key,value in word2Dictionary.items():
    
    word2Dictionary[key]=[sum(x_list)/len(x_list) for x_list in value]

#word3
          
#turn into proportions of all transitions for each time frame for each subject
word3_values=[ v for v in sorted(word3Dictionary.values()) ]
word3_values_t=list(zip(*word3_values))
for time_frame in range(0,len(word3_values_t)):
    word3_values_t_t=list(zip(*word3_values_t[time_frame]))
    for subj in range(0,len(word3_values_t_t)):
        if sum(word3_values_t_t[subj]) != 0:
               word3_values_t_t[subj]=[float(x)/sum(word3_values_t_t[subj]) for x in word3_values_t_t[subj]]
    word3_values_t[time_frame]=list(zip(*word3_values_t_t))
    
word3_values=list(zip(*word3_values_t))
dict_value=0
for keys in sorted(word3Dictionary, key=word3Dictionary.get):
    word3Dictionary[keys]=word3_values[dict_value]
    dict_value+=1


#take mean for all subjects in each time frame
# if the analysis should be more precise and subject should be modeled, this is not to be done


for key,value in word3Dictionary.items():
    
    word3Dictionary[key]=[sum(x_list)/len(x_list) for x_list in value]               



with open('../../../../processed_data/simple_word_search/B2/'+condition+'/Natural_'+drug+ '_' + condition +'_word2','wt') as word2File:

    writerword2File = csv.writer(word2File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
    writerword2File.writerow(sorted(word2Dictionary, key=word2Dictionary.get))
    for value in list(zip(*sorted(word2Dictionary.values()))):
        writerword2File.writerow(value)
    
    
with open('../../../../processed_data/simple_word_search/B2/'+condition+'/Natural_'+drug+ '_' + condition +'_word3','wt') as word3File:

    writerword3File = csv.writer(word3File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
    writerword3File.writerow(sorted(word3Dictionary, key=word3Dictionary.get))
    for value in list(zip(*sorted(word3Dictionary.values()))):
        writerword3File.writerow(value)
