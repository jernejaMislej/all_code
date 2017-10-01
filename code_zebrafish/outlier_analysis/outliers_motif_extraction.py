#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
motif2Dictionary_Normal={}
motif3Dictionary_Normal={}
motif4Dictionary_Normal={}
motif5Dictionary_Normal={}
motif6Dictionary_Normal={}
motif7Dictionary_Normal={}
motif8Dictionary_Normal={}
motif9Dictionary_Normal={}
motif10Dictionary_Normal={}

motif2Dictionary_Outlier={}
motif3Dictionary_Outlier={}
motif4Dictionary_Outlier={}
motif5Dictionary_Outlier={}
motif6Dictionary_Outlier={}
motif7Dictionary_Outlier={}
motif8Dictionary_Outlier={}
motif9Dictionary_Outlier={}
motif10Dictionary_Outlier={}

AripiprazoleSequenceIdentifiers=['Recording:12-1;Individual:3;', 'Recording:4-1;Individual:27;','Recording:6-1;Individual:42;',\
								'Recording:11-1;Individual:23','Recording:8-1;Individual:47;']
CariprazineSequenceIdentifiers=['Recording:5-1;Individual:2;D','Recording:9-1;Individual:2;D','Recording:12-1;Individual:12',\
							'Recording:9-1;Individual:16;','Recording:12-1;Individual:24','Recording:3-1;Individual:24;']

#process the sequences per 5-minute recording
with open(sys.argv[1],'r') as seqFile_Normal:

	printFlag_Normal=0
	printFlag_Outlier=0

	for line in seqFile_Normal:
		if line.find('>')!=-1:
			if ((sys.argv[1]=='AripiprazoleDarkApoLow') and (line[55:83] in AripiprazoleSequenceIdentifiers)) or \
				((sys.argv[1]=='CariprazineDarkApoLow') and (line[55:83] in CariprazineSequenceIdentifiers)):
					printFlag_Outlier=1
			else:
				printFlag_Normal=1
			continue
		if printFlag_Normal==1:
			if len(line)>1:
				#extract all the bouts
				boutsList=re.findall('b.*?(?=b)',line.rstrip().replace('R','').replace('L',''))
				#if there are bouts, search for motif inside them
				if len(boutsList)>0:
					for bout in boutsList:
						if len(bout)>2:
							for pos in range(1,len(bout)-1):
								if bout[pos:pos+2] in motif2Dictionary_Normal:
									motif2Dictionary_Normal[bout[pos:pos+2]]=motif2Dictionary_Normal[bout[pos:pos+2]]+1
								else:
									motif2Dictionary_Normal[bout[pos:pos+2]]=1
						if len(bout)>3:
							for pos in range(1,len(bout)-2):
								if bout[pos:pos+3] in motif3Dictionary_Normal:
									motif3Dictionary_Normal[bout[pos:pos+3]]=motif3Dictionary_Normal[bout[pos:pos+3]]+1
								else:
									motif3Dictionary_Normal[bout[pos:pos+3]]=1
						if len(bout)>4:
							for pos in range(1,len(bout)-3):
								if bout[pos:pos+4] in motif4Dictionary_Normal:
									motif4Dictionary_Normal[bout[pos:pos+4]]=motif4Dictionary_Normal[bout[pos:pos+4]]+1
								else:
									motif4Dictionary_Normal[bout[pos:pos+4]]=1
						if len(bout)>5:
							for pos in range(1,len(bout)-4):
								if bout[pos:pos+5] in motif5Dictionary_Normal:
									motif5Dictionary_Normal[bout[pos:pos+5]]=motif5Dictionary_Normal[bout[pos:pos+5]]+1
								else:
									motif5Dictionary_Normal[bout[pos:pos+5]]=1
						if len(bout)>6:
							for pos in range(1,len(bout)-5):
								if bout[pos:pos+6] in motif6Dictionary_Normal:
									motif6Dictionary_Normal[bout[pos:pos+6]]=motif6Dictionary_Normal[bout[pos:pos+6]]+1
								else:
									motif6Dictionary_Normal[bout[pos:pos+6]]=1
						if len(bout)>7:
							for pos in range(1,len(bout)-6):
								if bout[pos:pos+7] in motif7Dictionary_Normal:
									motif7Dictionary_Normal[bout[pos:pos+7]]=motif7Dictionary_Normal[bout[pos:pos+7]]+1
								else:
									motif7Dictionary_Normal[bout[pos:pos+7]]=1
						if len(bout)>8:
							for pos in range(1,len(bout)-7):
								if bout[pos:pos+8] in motif8Dictionary_Normal:
									motif8Dictionary_Normal[bout[pos:pos+8]]=motif8Dictionary_Normal[bout[pos:pos+8]]+1
								else:
									motif8Dictionary_Normal[bout[pos:pos+8]]=1
						if len(bout)>9:
							for pos in range(1,len(bout)-8):
								if bout[pos:pos+9] in motif9Dictionary_Normal:
									motif9Dictionary_Normal[bout[pos:pos+9]]=motif9Dictionary_Normal[bout[pos:pos+9]]+1
								else:
									motif9Dictionary_Normal[bout[pos:pos+9]]=1
						if len(bout)>10:
							for pos in range(1,len(bout)-9):
								if bout[pos:pos+10] in motif10Dictionary_Normal:
									motif10Dictionary_Normal[bout[pos:pos+10]]=motif10Dictionary_Normal[bout[pos:pos+10]]+1
								else:
									motif10Dictionary_Normal[bout[pos:pos+10]]=1																											
						
			printFlag_Normal=0
			
		elif printFlag_Outlier==1:
			if len(line)>1:
				#extract all the bouts
				boutsList=re.findall('b.*?(?=b)',line.rstrip().replace('R','').replace('L',''))
				#if there are bouts, search for motif inside them
				if len(boutsList)>0:
					for bout in boutsList:
						if len(bout)>2:
							for pos in range(1,len(bout)-1):
								if bout[pos:pos+2] in motif2Dictionary_Outlier:
									motif2Dictionary_Outlier[bout[pos:pos+2]]=motif2Dictionary_Outlier[bout[pos:pos+2]]+1
								else:
									motif2Dictionary_Outlier[bout[pos:pos+2]]=1
						if len(bout)>3:
							for pos in range(1,len(bout)-2):
								if bout[pos:pos+3] in motif3Dictionary_Outlier:
									motif3Dictionary_Outlier[bout[pos:pos+3]]=motif3Dictionary_Outlier[bout[pos:pos+3]]+1
								else:
									motif3Dictionary_Outlier[bout[pos:pos+3]]=1
						if len(bout)>4:
							for pos in range(1,len(bout)-3):
								if bout[pos:pos+4] in motif4Dictionary_Outlier:
									motif4Dictionary_Outlier[bout[pos:pos+4]]=motif4Dictionary_Outlier[bout[pos:pos+4]]+1
								else:
									motif4Dictionary_Outlier[bout[pos:pos+4]]=1
						if len(bout)>5:
							for pos in range(1,len(bout)-4):
								if bout[pos:pos+5] in motif5Dictionary_Outlier:
									motif5Dictionary_Outlier[bout[pos:pos+5]]=motif5Dictionary_Outlier[bout[pos:pos+5]]+1
								else:
									motif5Dictionary_Outlier[bout[pos:pos+5]]=1
						if len(bout)>6:
							for pos in range(1,len(bout)-5):
								if bout[pos:pos+6] in motif6Dictionary_Outlier:
									motif6Dictionary_Outlier[bout[pos:pos+6]]=motif6Dictionary_Outlier[bout[pos:pos+6]]+1
								else:
									motif6Dictionary_Outlier[bout[pos:pos+6]]=1
						if len(bout)>7:
							for pos in range(1,len(bout)-6):
								if bout[pos:pos+7] in motif7Dictionary_Outlier:
									motif7Dictionary_Outlier[bout[pos:pos+7]]=motif7Dictionary_Outlier[bout[pos:pos+7]]+1
								else:
									motif7Dictionary_Outlier[bout[pos:pos+7]]=1
						if len(bout)>8:
							for pos in range(1,len(bout)-7):
								if bout[pos:pos+8] in motif8Dictionary_Outlier:
									motif8Dictionary_Outlier[bout[pos:pos+8]]=motif8Dictionary_Outlier[bout[pos:pos+8]]+1
								else:
									motif8Dictionary_Outlier[bout[pos:pos+8]]=1
						if len(bout)>9:
							for pos in range(1,len(bout)-8):
								if bout[pos:pos+9] in motif9Dictionary_Outlier:
									motif9Dictionary_Outlier[bout[pos:pos+9]]=motif9Dictionary_Outlier[bout[pos:pos+9]]+1
								else:
									motif9Dictionary_Outlier[bout[pos:pos+9]]=1
						if len(bout)>10:
							for pos in range(1,len(bout)-9):
								if bout[pos:pos+10] in motif10Dictionary_Outlier:
									motif10Dictionary_Outlier[bout[pos:pos+10]]=motif10Dictionary_Outlier[bout[pos:pos+10]]+1
								else:
									motif10Dictionary_Outlier[bout[pos:pos+10]]=1
									
			printFlag_Outlier=0									


condition=sys.argv[2]


with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif2_Normal','wt') as Motif2File_Normal:

	writerMotif2File_Normal = csv.writer(Motif2File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif2Dictionary_Normal.items():
   		writerMotif2File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif3_Normal','wt') as Motif3File_Normal:

	writerMotif3File_Normal = csv.writer(Motif3File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif3Dictionary_Normal.items():
   		writerMotif3File_Normal.writerow([key, value])
   		
with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif4_Normal','wt') as Motif4File_Normal:

	writerMotif4File_Normal = csv.writer(Motif4File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif4Dictionary_Normal.items():
   		writerMotif4File_Normal.writerow([key, value])
   		
with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif5_Normal','wt') as Motif5File_Normal:

	writerMotif5File_Normal = csv.writer(Motif5File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif5Dictionary_Normal.items():
   		writerMotif5File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif6_Normal','wt') as Motif6File_Normal:

	writerMotif6File_Normal = csv.writer(Motif6File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif6Dictionary_Normal.items():
   		writerMotif6File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif7_Normal','wt') as Motif7File_Normal:

	writerMotif7File_Normal = csv.writer(Motif7File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif7Dictionary_Normal.items():
   		writerMotif7File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif8_Normal','wt') as Motif8File_Normal:

	writerMotif8File_Normal = csv.writer(Motif8File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif8Dictionary_Normal.items():
   		writerMotif8File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif9_Normal','wt') as Motif9File_Normal:

	writerMotif9File_Normal = csv.writer(Motif9File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif9Dictionary_Normal.items():
   		writerMotif9File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif10_Normal','wt') as Motif10File_Normal:

	writerMotif10File_Normal = csv.writer(Motif10File_Normal, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif10Dictionary_Normal.items():
   		writerMotif10File_Normal.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif2_Outlier','wt') as Motif2File_Outlier:

	writerMotif2File_Outlier = csv.writer(Motif2File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif2Dictionary_Outlier.items():
   		writerMotif2File_Outlier.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif3_Outlier','wt') as Motif3File_Outlier:

	writerMotif3File_Outlier = csv.writer(Motif3File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif3Dictionary_Outlier.items():
   		writerMotif3File_Outlier.writerow([key, value])
   		
with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif4_Outlier','wt') as Motif4File_Outlier:

	writerMotif4File_Outlier = csv.writer(Motif4File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif4Dictionary_Outlier.items():
   		writerMotif4File_Outlier.writerow([key, value])
   		
with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif5_Outlier','wt') as Motif5File_Outlier:

	writerMotif5File_Outlier = csv.writer(Motif5File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif5Dictionary_Outlier.items():
   		writerMotif5File_Outlier.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif6_Outlier','wt') as Motif6File_Outlier:

	writerMotif6File_Outlier = csv.writer(Motif6File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif6Dictionary_Outlier.items():
   		writerMotif6File_Outlier.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif7_Outlier','wt') as Motif7File_Outlier:

	writerMotif7File_Outlier = csv.writer(Motif7File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif7Dictionary_Outlier.items():
   		writerMotif7File_Outlier.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif8_Outlier','wt') as Motif8File_Outlier:

	writerMotif8File_Outlier = csv.writer(Motif8File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif8Dictionary_Outlier.items():
   		writerMotif8File_Outlier.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif9_Outlier','wt') as Motif9File_Outlier:

	writerMotif9File_Outlier = csv.writer(Motif9File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif9Dictionary_Outlier.items():
   		writerMotif9File_Outlier.writerow([key, value])

with open('../../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/'+condition+'/'+sys.argv[1]+'_motif10_Outlier','wt') as Motif10File_Outlier:

	writerMotif10File_Outlier = csv.writer(Motif10File_Outlier, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
	for key, value in motif10Dictionary_Outlier.items():
   		writerMotif10File_Outlier.writerow([key, value])

  	  		

