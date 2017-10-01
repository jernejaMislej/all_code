#load data
args <- commandArgs(trailingOnly = TRUE)

files<-list.files()

files_word2<-files[grep("Control.*word2",files)]
files_word3<-files[grep("Control.*word3",files)]

#args[1]="Natural"
#args[2]="Light"

all_dataset_word2<-read.table(files_word2[1],header=TRUE,sep=",",row.names=NULL)
all_dataset_word2<-all_dataset_word2[all_dataset_word2$ss!="Na",]
all_dataset_word2[,-1]<-apply(all_dataset_word2[,-1],2,function(x) as.numeric(x))

for (fil in files_word2[-1]){
	
	dataset<-read.table(fil,header=TRUE,sep=",",row.names=NULL)
	dataset<-dataset[dataset$ss!="Na",]
	dataset[,-1]<-apply(dataset[,-1],2,function(x) as.numeric(x))
	
	all_dataset_word2<-rbind(all_dataset_word2[,intersect(colnames(all_dataset_word2),colnames(dataset))],
			dataset[,intersect(colnames(all_dataset_word2),colnames(dataset))])
	
}
all_dataset_word2<-all_dataset_word2[order(all_dataset_word2$TimeFactor),]


all_dataset_word3<-read.table(files_word3[1],header=TRUE,sep=",",row.names=NULL)
all_dataset_word3<-all_dataset_word3[all_dataset_word3$sss!="Na",]
all_dataset_word3[,-1]<-apply(all_dataset_word3[,-1],2,function(x) as.numeric(x))

for (fil in files_word3[-1]){
	
	dataset<-read.table(fil,header=TRUE,sep=",",row.names=NULL)
	dataset<-dataset[dataset$sss!="Na",]
	dataset[,-1]<-apply(dataset[,-1],2,function(x) as.numeric(x))
	
	all_dataset_word3<-rbind(all_dataset_word3[,intersect(colnames(all_dataset_word3),colnames(dataset))],
			dataset[,intersect(colnames(all_dataset_word3),colnames(dataset))])
	
}
all_dataset_word3<-all_dataset_word3[order(all_dataset_word3$TimeFactor),]

write.table(all_dataset_word2,file=paste0(args[1],"_Control_",args[2],"_word2"),sep=",")
write.table(all_dataset_word3,file=paste0(args[1],"_Control_",args[2],"_word3"),sep=",")




