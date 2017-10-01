#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
subject=""
sequence=""
sequenceDictionary={}


#accumulate the sequences per 5-minute recording and replace the m with the preceding turn type

with open(sys.argv[1],'r') as seqFile_read, open(sys.argv[1]+'_copy','wt') as seqFile_write:

	for line in seqFile_read:
		if line[0]=='>' or line[0:9]=='Recording':
			seqFile_write.write(line.rstrip())
			printFlag=1
			continue

		elif line == '\n':
			if printFlag:
				seqFile_write.write("\n")
				sequence_list=list(sequence)
				for index_pair in [(m.start(0), m.end(0)) for m in re.finditer(".m+",sequence)]:
					sequence_list[index_pair[0]:index_pair[1]]=list(sequence[index_pair[0]]+sequence[index_pair[0]]*(len(sequence[index_pair[0]:index_pair[1]])-1))
				seqFile_write.write("".join(sequence_list))
				seqFile_write.write("\n\n")
				printFlag=0
				sequence=""

		if printFlag:
			sequence+=line.rstrip()
