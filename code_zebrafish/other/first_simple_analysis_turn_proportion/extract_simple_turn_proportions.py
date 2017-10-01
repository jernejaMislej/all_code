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



with open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'ScootProportion','wt') as ScootProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'OBendProportion','wt') as OBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'JBendProportion','wt') as JBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'CBendProportion','wt') as CBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'EBendProportion','wt') as EBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'GBendProportion','wt') as GBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'HBendProportion','wt') as HBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'IBendProportion','wt') as IBendProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'LeftProportion','wt') as LeftProportionFile, open('../../../processed_data/first_simple_analysis_turn_proportion/'+sys.argv[1]+'RightProportion','wt') as RightProportionFile:

	writer_ScootProportionFile = csv.writer(ScootProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_OBendProportionFile = csv.writer(OBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_JBendProportionFile = csv.writer(JBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_CBendProportionFile = csv.writer(CBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_EBendProportionFile = csv.writer(EBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_GBendProportionFile = csv.writer(GBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_HBendProportionFile = csv.writer(HBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_IBendProportionFile = csv.writer(IBendProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_LeftProportionFile = csv.writer(LeftProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	writer_RightProportionFile = csv.writer(RightProportionFile, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)

	for subject in sorted(sequenceDictionary):

		ScootProportion=[subject]

		OBendProportion=[subject]

		JBendProportion=[subject]

		CBendProportion=[subject]

		EBendProportion=[subject]

		GBendProportion=[subject]

		HBendProportion=[subject]

		IBendProportion=[subject]

		LeftProportion=[subject]

		RightProportion=[subject]

		for recording in sequenceDictionary[subject]:

			seqLen=len(recording)
			
			if seqLen==0:
				ScootProportion.append(0)
				OBendProportion.append(0)
				JBendProportion.append(0)
				CBendProportion.append(0)
				EBendProportion.append(0)
				GBendProportion.append(0)
				HBendProportion.append(0)
				IBendProportion.append(0)
				LeftProportion.append(0)
				RightProportion.append(0)
			else:
				ScootProportion.append(len(re.findall('s',recording))/seqLen)
				OBendProportion.append(len(re.findall('o',recording))/seqLen)
				JBendProportion.append(len(re.findall('j',recording))/seqLen)
				CBendProportion.append(len(re.findall('c',recording))/seqLen)
				EBendProportion.append(len(re.findall('e',recording))/seqLen)
				GBendProportion.append(len(re.findall('g',recording))/seqLen)
				HBendProportion.append(len(re.findall('h',recording))/seqLen)
				IBendProportion.append(len(re.findall('i',recording))/seqLen)
				LeftProportion.append(len(re.findall('L',recording))/seqLen)
				RightProportion.append(len(re.findall('R',recording))/seqLen)

		
		writer_ScootProportionFile.writerow(ScootProportion)
		
		writer_OBendProportionFile.writerow(OBendProportion)

		writer_JBendProportionFile.writerow(JBendProportion)

		writer_CBendProportionFile.writerow(CBendProportion)

		writer_EBendProportionFile.writerow(EBendProportion)

		writer_GBendProportionFile.writerow(GBendProportion)

		writer_HBendProportionFile.writerow(HBendProportion)

		writer_IBendProportionFile.writerow(IBendProportion)

		writer_LeftProportionFile.writerow(LeftProportion)

		writer_RightProportionFile.writerow(RightProportion)
