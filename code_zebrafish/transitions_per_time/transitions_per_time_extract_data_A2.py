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
extreme_length=5
condition=sys.argv[2]

if condition == "DarkApoLow":
    extreme_length=3
    drug=sys.argv[1][:-10]
    
elif condition == "DarkPTZ":
    extreme_length=7  
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
                            if word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder] == "Na":
                                word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder] = 1
                            else:
                                word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder]=word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder]+1
                        else:
                            word2Dictionary[bout[pos:pos+2]]=[["Na" for x in range(no_subjects)] for y in range(13)]
                            word2Dictionary[bout[pos:pos+2]][timeOfRecording][subjectOrder]=1
                if len(bout)>3:
                    for pos in range(1,len(bout)-2):
                        if bout[pos:pos+3] in word3Dictionary:
                            if word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder] == "Na":
                                word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder] = 1
                            else:
                                word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder]=word3Dictionary[bout[pos:pos+3]][timeOfRecording][subjectOrder]+1
                        else:
                            word3Dictionary[bout[pos:pos+3]]=[["Na" for x in range(no_subjects)] for y in range(13)]
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
word2_values=[ v for v in [word2Dictionary[key] for key in sorted(word2Dictionary)]]
word2_values_t=list(zip(*word2_values))
for time_frame in range(0,13):
    word2_values_t_t=list(zip(*word2_values_t[time_frame]))
    for subj in range(0,len(word2_values_t_t)):
        if word2_values_t_t[subj].count("Na") == len(word2_values_t_t[subj]):
            continue
        else:
            for index in [index for index, value in enumerate(word2_values_t_t[subj]) if value == "Na"]:
                word2_values_t_t_list=list(word2_values_t_t[subj])
                word2_values_t_t_list[index]=0
                word2_values_t_t[subj]=tuple(word2_values_t_t_list)    
        if "Na" not in word2_values_t_t[subj] and sum(word2_values_t_t[subj]) != 0:
               word2_values_t_t[subj]=[float(x)/sum(word2_values_t_t[subj]) for x in word2_values_t_t[subj]]
    word2_values_t[time_frame]=list(zip(*word2_values_t_t))
    
word2_values=list(zip(*word2_values_t))
dict_value=0
for keys in sorted(word2Dictionary):
    word2Dictionary[keys]=word2_values[dict_value]
    dict_value+=1

#print(word2Dictionary["ss"])


#dont take mean for all subjects in each time frame, but melt

#make the time frame variable
word2Dictionary["TimeFactor"]=[[y for x in range(no_subjects)] for y in range(1,14)]
#flatten all
for key,value in word2Dictionary.items():   
    word2Dictionary[key]=[item for sublist in value for item in sublist]


#word3
          
#turn into proportions of all transitions for each time frame for each subject
word3_values=[ v for v in [word3Dictionary[key] for key in sorted(word3Dictionary)]]
word3_values_t=list(zip(*word3_values))
for time_frame in range(0,len(word3_values_t)):
    word3_values_t_t=list(zip(*word3_values_t[time_frame]))
    for subj in range(0,len(word3_values_t_t)):
        if word3_values_t_t[subj].count("Na") == len(word3_values_t_t[subj]):
            continue
        else:
            for index in [index for index, value in enumerate(word3_values_t_t[subj]) if value == "Na"]:
                word3_values_t_t_list=list(word3_values_t_t[subj])
                word3_values_t_t_list[index]=0
                word3_values_t_t[subj]=tuple(word3_values_t_t_list)    

        if "Na" not in word3_values_t_t[subj] and sum(word3_values_t_t[subj]) != 0:
               word3_values_t_t[subj]=[float(x)/sum(word3_values_t_t[subj]) for x in word3_values_t_t[subj]]
    word3_values_t[time_frame]=list(zip(*word3_values_t_t))
    
word3_values=list(zip(*word3_values_t))
dict_value=0
for keys in sorted(word3Dictionary):
    word3Dictionary[keys]=word3_values[dict_value]
    dict_value+=1


#dont take mean for all subjects in each time frame
word3Dictionary["TimeFactor"]=[[y for x in range(no_subjects)] for y in range(1,14)]

for key,value in word3Dictionary.items():
    
    word3Dictionary[key]=[item for sublist in value for item in sublist]             



with open('../../../../processed_data/simple_word_search/A2/'+condition+'/Disease_'+drug+ '_' + condition +'_word2','wt') as word2File:

    writerword2File = csv.writer(word2File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
    un_dictionary=list(zip(*sorted(word2Dictionary.items())))
    writerword2File.writerow(un_dictionary[0])
    for value in list(zip(*un_dictionary[1])):
        writerword2File.writerow(value)
    
with open('../../../../processed_data/simple_word_search/A2/'+condition+'/Disease_'+drug+ '_' + condition +'_word3','wt') as word3File:

    writerword3File = csv.writer(word3File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
    un_dictionary=list(zip(*sorted(word3Dictionary.items())))
    writerword3File.writerow(un_dictionary[0])
    for value in list(zip(*un_dictionary[1])):
        writerword3File.writerow(value)



