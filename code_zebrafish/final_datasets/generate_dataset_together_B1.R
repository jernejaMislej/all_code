
#load data 

files<-list.files()
subjects_nr<-0

args <- commandArgs(trailingOnly = TRUE)
condition<-args[1]

cut_point<-42

if(condition == "DarkApoLow"){
	cut_point<-47
} else if(condition == "DarkPTZ"){
	cut_point<-44
} else if(condition == "DarkApoHigh"){
	cut_point<-48
} else if(condition == "Dark"){
	cut_point<-41
} else if(condition == "LightDark"){
	cut_point<-46
}


dataset<-read.table(files[1],header=TRUE)
dataset$Group<-rep(substr(substr(files[1],9,100),1,nchar(files[1])-cut_point),times=length(dataset[,1]))#adjust for each of 6 conditions!!!
all_dataset<-dataset

for (fil in files[-1]){
	
	dataset<-read.table(fil,header=TRUE)	
	
	#add identifier
	dataset$Group<-rep(substr(substr(fil,9,100),1,nchar(fil)-cut_point),times=length(dataset[,1]))
	
	#make missing variables , having zero value 
	variables_missing_dataset<-colnames(dataset)[!(colnames(dataset) %in% 
						intersect(colnames(dataset),colnames(all_dataset)))]
	
	variables_missing_dataset_all<-colnames(all_dataset)[!(colnames(all_dataset) %in% 
						intersect(colnames(dataset),colnames(all_dataset)))]
	
	dataset_missing<-as.data.frame(matrix(0, 13, length(variables_missing_dataset_all)))
	colnames(dataset_missing)<-variables_missing_dataset_all
	
	dataset_all_missing<-as.data.frame(matrix(0, 13, length(variables_missing_dataset)))
	colnames(dataset_all_missing)<-variables_missing_dataset
	
	dataset<-cbind(dataset,dataset_missing)
	all_dataset<-cbind(all_dataset,dataset_all_missing)
	
	dataset<-dataset[,ordered(colnames(dataset))]
	all_dataset<-all_dataset[,ordered(colnames(all_dataset))]
	
	
	all_dataset<-rbind(all_dataset,dataset)
	
}


#save full dataset
write.table(all_dataset,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/final_dataset/B1/",condition,
				"_final_dataset_full.txt"),row.names=F,col.names=T)


#remove variables with no information and save compact dataset
all_dataset<-cbind(all_dataset$Group, all_dataset[,-(1:length(colnames(all_dataset)))[colnames(all_dataset)=="Group"]][,apply(
						all_dataset[,-(1:length(colnames(all_dataset)))[colnames(all_dataset)=="Group"]],2,FUN=sd,na.rm=T)>0.005])

write.table(all_dataset,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/final_dataset/B1/",condition,
				"_final_dataset_short.txt"),row.names=F,col.names=T)

