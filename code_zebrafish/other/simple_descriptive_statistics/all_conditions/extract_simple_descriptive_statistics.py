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
				sequenceDictionary[subject].append(line.rstrip())
			else:
				sequenceDictionary[subject]=[line.rstrip()]
			printFlag=0



with open('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/all/'+sys.argv[1]+'Lengths','wt') as seqLengthsFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/all/'+sys.argv[1]+'NumberOfBouts','wt') as numberOfBoutsFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/all/'+sys.argv[1]+'meanLengthOfBout','wt') as meanLengthOfBoutFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/all/'+sys.argv[1]+'sdLengthOfBout','wt') as sdLengthOfBoutFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/all/'+sys.argv[1]+'LengthsMean','wt') as seqLengthsMeanFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/all/'+sys.argv[1]+'NumberOfBoutsMean','wt') as numberOfBoutsMeanFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/all/'+sys.argv[1]+'meanLengthOfBoutMean','wt') as meanLengthOfBoutMeanFile, open('../../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/all/'+sys.argv[1]+'sdLengthOfBoutMean','wt') as sdLengthOfBoutMeanFile:

	writer_seqLengthsFile = csv.writer(seqLengthsFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	writer_numberOfBoutsFile = csv.writer(numberOfBoutsFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	writer_meanLengthOfBoutFile = csv.writer(meanLengthOfBoutFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	writer_sdLengthOfBoutFile = csv.writer(sdLengthOfBoutFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_seqLengthsMeanFile = csv.writer(seqLengthsMeanFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	writer_numberOfBoutsMeanFile = csv.writer(numberOfBoutsMeanFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	writer_meanLengthOfBoutMeanFile = csv.writer(meanLengthOfBoutMeanFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	writer_sdLengthOfBoutMeanFile = csv.writer(sdLengthOfBoutMeanFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	for subject in sorted(sequenceDictionary):
		lengthsList=[subject]
		numberOfBouts=[subject]
		meanLengthOfBout=[subject]
		sdLengthOfBout=[subject]

		lengthsMeanList=[subject]
		numberOfBoutsMean=[subject]
		meanLengthOfBoutMean=[subject]
		sdLengthOfBoutMean=[subject]
		count=0
		for recording in sequenceDictionary[subject]:
			lengthsList.append(len(recording))
			boutsList=re.findall('b.*?(?=b)',recording)
			numberOfBouts.append(len(boutsList))
			boutLengths=[]
			for bout in boutsList:
				bout=bout.replace('L','')
				bout=bout.replace('R','')
				#print(bout)
				boutLengths.append(len(bout)-1)
			if len(boutsList):
				meanLengthOfBout.append(round(sum(boutLengths)/len(boutsList),2))
				sdLengthOfBout.append(round((sum([(element-sum(boutLengths)/len(boutsList))**2 for element in boutLengths])/len(boutLengths))**(0.5),2))
			else:
				meanLengthOfBout.append(0)
				sdLengthOfBout.append(0)
			count=count+1
		for missing in range(1,(13-count)):
			lengthsList.append(0)
			numberOfBouts.append(0)
			meanLengthOfBout.append(0)
			sdLengthOfBout.append(0)

		if len(lengthsList)-1:
			lengthsMeanList.append(round(sum(lengthsList[1:len(lengthsList)])/(len(lengthsList)-1),2))
		else:
			lengthsMeanList.append(0)
		if len(numberOfBouts)-1:
			numberOfBoutsMean.append(round(sum(numberOfBouts[1:len(numberOfBouts)])/(len(numberOfBouts)-1),2))
		else:
			numberOfBoutsMean.append(0)
		if len(meanLengthOfBout)-1:
			meanLengthOfBoutMean.append(round(sum(meanLengthOfBout[1:len(meanLengthOfBout)])/(len(meanLengthOfBout)-1),2))
		else:
			meanLengthOfBoutMean.append(0)
		if len(sdLengthOfBout)-1:
			sdLengthOfBoutMean.append(round(sum(sdLengthOfBout[1:len(sdLengthOfBout)])/(len(sdLengthOfBout)-1),2))
		else:
			sdLengthOfBoutMean.append(0)

		writer_seqLengthsFile.writerow(lengthsList)
		writer_numberOfBoutsFile.writerow(numberOfBouts)
		writer_meanLengthOfBoutFile.writerow(meanLengthOfBout)
		writer_sdLengthOfBoutFile.writerow(sdLengthOfBout)

		writer_seqLengthsMeanFile.writerow(lengthsMeanList)
		writer_numberOfBoutsMeanFile.writerow(numberOfBoutsMean)
		writer_meanLengthOfBoutMeanFile.writerow(meanLengthOfBoutMean)
		writer_sdLengthOfBoutMeanFile.writerow(sdLengthOfBoutMean)

