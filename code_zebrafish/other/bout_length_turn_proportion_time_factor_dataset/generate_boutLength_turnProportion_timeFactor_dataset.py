#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
subject=""
sequence=""
sequenceDictionary={}


#function to generate datasets for all turns
def generateTurnDatasets(turn, condition):

	with open('../../../processed_data/bout_length_turn_proportion_time_factor_dataset/'+condition+'/'+sys.argv[1]+'_'+turn+'Proportion','wt') as TurnProportionFile:



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
def generateDatasets(condition):

	with open('../../../processed_data/bout_length_turn_proportion_time_factor_dataset/'+condition+'/'+sys.argv[1]+'_BoutLength','wt') as BoutLengthFile, open('../../../processed_data/bout_length_turn_proportion_time_factor_dataset/'+condition+'/'+sys.argv[1]+'_TimeFactor','wt') as TimeFactorFile:



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







