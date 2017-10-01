#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
motif2Dictionary={}
motif3Dictionary={}
motif4Dictionary={}
motif5Dictionary={}

#process the sequences per 5-minute recording
with open(sys.argv[1],'r') as seqFile:

	for line in seqFile:
		if line.find('>')!=-1 and line.find(sys.argv[3])!=-1:
			printFlag=1
			continue
		if printFlag==1:
			if len(line)>1:
				#extract all the bouts
				boutsList=re.findall('b.*?(?=b)',line.rstrip().replace('R','').replace('L',''))
				#if there are bouts, search for motif inside them
				if len(boutsList)>0:
					for bout in boutsList:
						if len(bout)>2:
							for pos in range(1,len(bout)-1):
								if bout[pos:pos+2] in motif2Dictionary:
									motif2Dictionary[bout[pos:pos+2]]=motif2Dictionary[bout[pos:pos+2]]+1
								else:
									motif2Dictionary[bout[pos:pos+2]]=1
						if len(bout)>3:
							for pos in range(1,len(bout)-2):
								if bout[pos:pos+3] in motif3Dictionary:
									motif3Dictionary[bout[pos:pos+3]]=motif3Dictionary[bout[pos:pos+3]]+1
								else:
									motif3Dictionary[bout[pos:pos+3]]=1
						if len(bout)>4:
							for pos in range(1,len(bout)-3):
								if bout[pos:pos+4] in motif4Dictionary:
									motif4Dictionary[bout[pos:pos+4]]=motif4Dictionary[bout[pos:pos+4]]+1
								else:
									motif4Dictionary[bout[pos:pos+4]]=1
						if len(bout)>5:
							for pos in range(1,len(bout)-4):
								if bout[pos:pos+5] in motif5Dictionary:
									motif5Dictionary[bout[pos:pos+5]]=motif5Dictionary[bout[pos:pos+5]]+1
								else:
									motif5Dictionary[bout[pos:pos+5]]=1
																																	
						
			printFlag=0



condition=sys.argv[2]

drug=sys.argv[1][:-9] #drug
drug+='_'+sys.argv[3] #Control or dosage of Natural substrate

with open('../../../../processed_data/simple_motif_search/'+condition+'/Natural_'+drug+ '_' + condition +'_motif2','wt') as Motif2File:

	writerMotif2File = csv.writer(Motif2File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif2Dictionary.items():
   		writerMotif2File.writerow([key, value])

with open('../../../../processed_data/simple_motif_search/'+condition+'/Natural_'+drug+ '_' + condition +'_motif3','wt') as Motif3File:

	writerMotif3File = csv.writer(Motif3File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif3Dictionary.items():
   		writerMotif3File.writerow([key, value])
   		
with open('../../../../processed_data/simple_motif_search/'+condition+'/Natural_'+drug+ '_' + condition +'_motif4','wt') as Motif4File:

	writerMotif4File = csv.writer(Motif4File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif4Dictionary.items():
   		writerMotif4File.writerow([key, value])
   		
with open('../../../../processed_data/simple_motif_search/'+condition+'/Natural_'+drug+ '_' + condition +'_motif5','wt') as Motif5File:

	writerMotif5File = csv.writer(Motif5File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif5Dictionary.items():
   		writerMotif5File.writerow([key, value])

