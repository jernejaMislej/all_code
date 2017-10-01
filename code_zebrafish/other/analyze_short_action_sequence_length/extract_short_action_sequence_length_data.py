#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
sequenceDictionary={}
subject=""
sequence=""

#accumulate the sequences per 5-minute recording
with open(sys.argv[1],'r') as seqFile:

	for line in seqFile:
		if line.find('>')!=-1 and line.find('Control')!=-1:
			res=re.search("Individual:..",line)		
			if res:			
				subject=res.group(0)
			printFlag=1
			continue
		if printFlag==1:
			if subject in sequenceDictionary:			
				sequenceDictionary[subject].append(line.rstrip().replace('b',''))
			else:
				sequenceDictionary[subject]=[line.rstrip().replace('b','')]
			printFlag=0


with open('../../processed_data/analyze_short_action_sequence_length/'+sys.argv[1]+'Lengths','wt') as seqLengthsFile, open('../../processed_data/analyze_short_action_sequence_length/'+sys.argv[1]+'LengthsMean','wt') as seqLengthsMeanFile, open('../../processed_data/analyze_short_action_sequence_length/'+sys.argv[1]+'ProportionOfShort','wt') as ProportionOfShortFile, open('../../processed_data/analyze_short_action_sequence_length/'+sys.argv[1]+'ScootProportionOfShort','wt') as ScootProportionOfShortFile, open('../../processed_data/analyze_short_action_sequence_length/'+sys.argv[1]+'OBendProportionOfShort','wt') as OBendProportionOfShortFile:

	writer_seqLengthsFile = csv.writer(seqLengthsFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_seqLengthsMeanFile = csv.writer(seqLengthsMeanFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_ProportionOfShortFile = csv.writer(ProportionOfShortFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_ScootProportionOfShortFile = csv.writer(ScootProportionOfShortFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_OBendProportionOfShortFile = csv.writer(OBendProportionOfShortFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	for subject in sorted(sequenceDictionary):
		lengthsList=[subject]

		lengthsMeanList=[subject]

		ProportionOfShort=[subject,0,0,0,0,0]

		ScootProportionOfShort=[subject,0,0,0,0]

		OBendProportionOfShort=[subject,0,0,0,0]

		for recording in sequenceDictionary[subject]:
			seqLen=len(recording)
			lengthsList.append(seqLen)
			
			if seqLen==0:
				ProportionOfShort[1]+=1
			else:
				if seqLen<=100:
					ProportionOfShort[5]+=1
					ScootProportionOfShort[4]+=len(re.findall('s',recording))/seqLen
					OBendProportionOfShort[4]+=len(re.findall('o',recording))/seqLen
				if seqLen<=20:
					ProportionOfShort[4]+=1
					ScootProportionOfShort[3]+=len(re.findall('s',recording))/seqLen
					OBendProportionOfShort[3]+=len(re.findall('o',recording))/seqLen
				if seqLen<=10:
					ProportionOfShort[3]+=1
					ScootProportionOfShort[2]+=len(re.findall('s',recording))/seqLen
					OBendProportionOfShort[2]+=len(re.findall('o',recording))/seqLen
				if seqLen<=4:
					ProportionOfShort[2]+=1
					ScootProportionOfShort[1]+=len(re.findall('s',recording))/seqLen
					OBendProportionOfShort[1]+=len(re.findall('o',recording))/seqLen
			'''
			if seqLen==0:
				ProportionOfShort[1]+=1
			elif seqLen<=4:
				ProportionOfShort[2]+=1
				ScootProportionOfShort[1]+=len(re.findall('s',recording))/seqLen
			elif seqLen<=10:
				ProportionOfShort[3]+=1
				ScootProportionOfShort[2]+=len(re.findall('s',recording))/seqLen
			elif seqLen<=20:
				ProportionOfShort[4]+=1
				ScootProportionOfShort[3]+=len(re.findall('s',recording))/seqLen
			elif seqLen<=100:
				ProportionOfShort[5]+=1
				ScootProportionOfShort[4]+=len(re.findall('s',recording))/seqLen
			'''
		if len(lengthsList)-1:
			lengthsMeanList.append(round(sum(lengthsList[1:len(lengthsList)])/(len(lengthsList)-1),2))
		else:
			lengthsMeanList.append(0)

		if ProportionOfShort[2]:
			ScootProportionOfShort[1]=ScootProportionOfShort[1]/ProportionOfShort[2]
			OBendProportionOfShort[1]=OBendProportionOfShort[1]/ProportionOfShort[2]
		if ProportionOfShort[3]:
			ScootProportionOfShort[2]=ScootProportionOfShort[2]/ProportionOfShort[3]
			OBendProportionOfShort[2]=OBendProportionOfShort[2]/ProportionOfShort[3]
		if ProportionOfShort[4]:
			ScootProportionOfShort[3]=ScootProportionOfShort[3]/ProportionOfShort[4]
			OBendProportionOfShort[3]=OBendProportionOfShort[3]/ProportionOfShort[4]
		if ProportionOfShort[5]:
			ScootProportionOfShort[4]=ScootProportionOfShort[4]/ProportionOfShort[5]
			OBendProportionOfShort[4]=OBendProportionOfShort[4]/ProportionOfShort[5]

		ProportionOfShort[1]=ProportionOfShort[1]/(13)
		ProportionOfShort[2]=ProportionOfShort[2]/(13)
		ProportionOfShort[3]=ProportionOfShort[3]/(13)
		ProportionOfShort[4]=ProportionOfShort[4]/(13)
		ProportionOfShort[5]=ProportionOfShort[5]/(13)	

		writer_seqLengthsFile.writerow(lengthsList)
		
		writer_seqLengthsMeanFile.writerow(lengthsMeanList)

		writer_ProportionOfShortFile.writerow(ProportionOfShort)
		
		writer_ScootProportionOfShortFile.writerow(ScootProportionOfShort)
		
		writer_OBendProportionOfShortFile.writerow(OBendProportionOfShort)
