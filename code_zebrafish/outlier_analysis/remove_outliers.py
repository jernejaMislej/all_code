import sys
import re
import csv


with open(sys.argv[1],'r') as seqFile, open(sys.argv[2],'r') as outliersFile, open("filtered/"+sys.argv[1],'wt') as filteredFile:
    
    #make list
    sequence_file_list = seqFile.readlines()
    sequence_file_list = [line for line in sequence_file_list if line.rstrip() ]
    
    for line in outliersFile:
        
        res=re.search("Individual:..",line)        
        individual_number=res.group(0)
        
        line=line.strip()
        
        recording_number=int(line[len(line)-2:len(line)].replace("_",""))

        identifier="Recording:"+str(recording_number)+"-1;"+str(individual_number)
                
                
        item_index=sequence_file_list.index([x for x in sequence_file_list if identifier in x][0])
        
        sequence_file_list.remove(sequence_file_list[item_index])
        sequence_file_list.remove(sequence_file_list[item_index])
        
 
    for idx in range(2,int(len(sequence_file_list)+0.5*len(sequence_file_list)),3):
        sequence_file_list.insert(idx,"\n")
      
    filteredFile.write("".join(sequence_file_list))     