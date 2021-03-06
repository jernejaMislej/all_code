
#A1 is random subjects, stratified per bout length
#A2 is random subjects, not stratified per bout length
#B1 is mean over random subjects, stratified per bout length
#B2 is mean over random subjects, not stratified per bout length


#args<-c("Natural_Control_Light")
args <- commandArgs(trailingOnly = TRUE)

#load data
dataset<-read.table(paste0(args[1],".txt"))

#check which condition to set the right bout length cut point for the extremely long bouts

extreme_length=5

#from the input file, extract bout count, turn proportions

#get the total count 
count_all_bouts<-c()

for(time_frame in 1:13){
	
	count_all_bouts<-rbind(count_all_bouts, cbind(aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x))}),rep(time_frame,times=length(sort(unique(dataset$Subject[dataset$TimeFactor==time_frame]))))))
}

colnames(count_all_bouts)[c(2,3)]<-c("TotalBoutCount","TimeFactor")

#look at the counts of bouts per length

count_bouts_per_length_all<-c()

for (bout_length in 1:extreme_length){
	
	count_bouts_per_length<-c()
	
	for(time_frame in 1:13){
		
		count_bouts_per_length<-c(count_bouts_per_length, aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x[x==bout_length]))})[,2])
	}	
	
	count_bouts_per_length_all<-cbind(count_bouts_per_length_all, count_bouts_per_length)
}

#add extremely long ones
count_bouts_per_length<-c()

for(time_frame in 1:13){
	
	count_bouts_per_length<-c(count_bouts_per_length, aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x[x>=extreme_length+1]))})[,2])
}	

count_bouts_per_length_all<-cbind(count_bouts_per_length_all, count_bouts_per_length)


colnames(count_bouts_per_length_all)<-paste0("Length",0:extreme_length+1,"BoutCount")

colnames(count_bouts_per_length_all)[length(colnames(count_bouts_per_length_all))]<-paste0("Length",extreme_length+1,"PlusBoutCount")


#mean turns per action sequence, stratified per bout length

#turns in short bouts
turnTypes<-c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")
turnCounter<-4#the number of column of the turns in the dataset

#list of dataframes of turn proportions(8 turns) for each length
turnMeans_all<-vector("list", extreme_length+1)

# there is a problem with extracting turn proportions, since the dataset is aggregated per subject in time, if a subject does not have any of those bouts of that length, then that row will be missing if additionally aggregated by subject in time 
# for bout length, so instead of a missing row NA is insterted

for(turnType in turnTypes){
	
	
	for (bout_length in 1:extreme_length){
		
		turnMeans_per_length<-c()
		
		for(time_frame in 1:13){
			
			turnMeans_per_length<-c(turnMeans_per_length, sapply(split(cbind(dataset[dataset$TimeFactor==time_frame,turnCounter], dataset[dataset$TimeFactor==time_frame,c("BoutLength")]), 
									dataset[dataset$TimeFactor==time_frame,c("Subject")]),function(x){
								end<-length(x)/2
								start<-length(x)/2 +1
								end2<-length(x)
								return(mean(x[1:end][x[start:end2]==bout_length], na.rm=TRUE))}))		
		}	
		
		turnMeans_all[[bout_length]]<-cbind(turnMeans_all[[bout_length]], turnMeans_per_length)
	}
	
	#go to next turn in the dataset
	turnCounter<-turnCounter+1
}

#add for the extreme values

turnCounter<-4#the number of column of the turns in the dataset


for(turnType in turnTypes){
	
	turnMeans_per_length<-c()
	
	for(time_frame in 1:13){
		
		turnMeans_per_length<-c(turnMeans_per_length, sapply(split(cbind(dataset[dataset$TimeFactor==time_frame,turnCounter], dataset[dataset$TimeFactor==time_frame,c("BoutLength")]), 
								dataset[dataset$TimeFactor==time_frame,c("Subject")]),function(x){
							end<-length(x)/2
							start<-length(x)/2 +1
							end2<-length(x)
							return(mean(x[1:end][x[start:end2]>=extreme_length+1], na.rm=TRUE))}))		
	}	
	
	turnMeans_all[[extreme_length+1]]<-cbind(turnMeans_all[[extreme_length+1]], turnMeans_per_length)
	
	#go to next turn in the dataset
	turnCounter<-turnCounter+1
}

#make column names
for (dataset in 1:extreme_length){
	
	colnames(turnMeans_all[[dataset]])<-paste0("Length",dataset,"Bout",turnTypes,"Proportion")
	
}
colnames(turnMeans_all[[extreme_length+1]])<-paste0("Length",extreme_length+1,"PlusBout",turnTypes,"Proportion")

#bind all together

dataset_all<-cbind(count_all_bouts, count_bouts_per_length_all)
for (bout_length in 0:extreme_length){
	
	dataset_all<-cbind(dataset_all, turnMeans_all[[bout_length+1]])
	
}

#get mean for each time frame
dataset_all_mean<-aggregate(dataset_all,by=list(dataset_all$TimeFactor),FUN=mean,na.rm=T)[,c(4,3,5:(4+extreme_length+1+8*(extreme_length+1)))]

#get dataset for transitions of length 2 and 3 (stratified, so large)

#load data 
#list of dataframes of transitions for each length
dataset_word2_all<-vector("list", extreme_length)
dataset_word3_all<-vector("list", extreme_length-1)

for(dataset in 1:extreme_length){
	dataset_word2_all[[dataset]]<-read.table(paste0("../simple_word_search/B1/",args[2],"/",args[1],"_word2_strata_",dataset+1),
			header=TRUE,sep=",")
	dataset_word2_all[[dataset]]<-dataset_word2_all[[dataset]][,-length(dataset_word2_all[[dataset]][1,])]
}

extreme_length_1=extreme_length-1
for(dataset in 1:extreme_length_1){
	dataset_word3_all[[dataset]]<-read.table(paste0("../simple_word_search/B1/",args[2],"/",args[1],"_word3_strata_",dataset+2),
			header=TRUE,sep=",")
	dataset_word3_all[[dataset]]<-dataset_word3_all[[dataset]][,-length(dataset_word3_all[[dataset]][1,])]
	
}

#make column names
for (dataset in 2:extreme_length){
	
	colnames(dataset_word2_all[[dataset-1]])<-paste0("Length",dataset,"Transition_",colnames(dataset_word2_all[[dataset-1]]),"_Proportion")
	
}
colnames(dataset_word2_all[[extreme_length]])<-paste0("Length",extreme_length+1,"PlusTransition_",
		colnames(dataset_word2_all[[extreme_length]]),"_Proportion")

#make column names
for (dataset in 3:extreme_length){
	
	colnames(dataset_word3_all[[dataset-2]])<-paste0("Length",dataset,"Transition_",colnames(dataset_word3_all[[dataset-2]]),"_Proportion")
	
}
colnames(dataset_word3_all[[extreme_length-1]])<-paste0("Length",extreme_length+1,"PlusTransition_",
		colnames(dataset_word3_all[[extreme_length-1]]),"_Proportion")

#bind all together

for (bout_length in 1:extreme_length){
	
	dataset_all_mean<-cbind(dataset_all_mean, dataset_word2_all[[bout_length]])
	
}
for (bout_length in 1:extreme_length_1){
	
	dataset_all_mean<-cbind(dataset_all_mean, dataset_word3_all[[bout_length]])
	
}

#save data
write.table(dataset_all_mean,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/final_dataset_separated/B1/",args[2],"/",
				args[1],"_final_dataset_separated.txt"),row.names=F,col.names=T)
