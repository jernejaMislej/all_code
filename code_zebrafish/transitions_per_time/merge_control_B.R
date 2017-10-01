#load data
args <- commandArgs(trailingOnly = TRUE)

files<-list.files()

files_word2<-files[grep("Control.*word2",files)]
files_word3<-files[grep("Control.*word3",files)]


#args[1]="Natural"
#args[2]="Light"

strats_word_2<-unique(substr(files_word2,nchar(files_word2)-7,nchar(files_word2)))

strats_word_3<-unique(substr(files_word3,nchar(files_word3)-7,nchar(files_word3)))

for (strata in strats_word_2){
	
	files_word2_strata<-files_word2[grep(strata,files_word2)]
	
	all_dataset_word2<-read.table(files_word2_strata[1],header=TRUE,sep=",")
	
	for (fil in files_word2_strata[-1]){
		
		dataset<-read.table(fil,header=TRUE,sep=",")
		
		all_dataset_word2<-cbind(all_dataset_word2,dataset)
		
	}
	all_dataset_word2<-t(rowsum(t(all_dataset_word2), group = rownames(t(all_dataset_word2))))/12
	write.table(all_dataset_word2,file=paste0(args[1],"_Control_",args[2],"_word2_",strata),sep=",")
	
}

for (strata in strats_word_3){
	
	files_word3_strata<-files_word3[grep(strata,files_word3)]
	
	all_dataset_word3<-read.table(files_word3_strata[1],header=TRUE,sep=",")
	
	for (fil in files_word3_strata[-1]){
		
		dataset<-read.table(fil,header=TRUE,sep=",")
		
		all_dataset_word3<-cbind(all_dataset_word3,dataset)
		
	}
	all_dataset_word3<-t(rowsum(t(all_dataset_word3), group = rownames(t(all_dataset_word3))))/12
	write.table(all_dataset_word3,file=paste0(args[1],"_Control_",args[2],"_word3_",strata),sep=",")
	
}


