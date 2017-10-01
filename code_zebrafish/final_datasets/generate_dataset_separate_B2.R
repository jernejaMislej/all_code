
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


#mean turns per action sequence, not stratified per bout length

#turns in short bouts
turnTypes<-c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")
turnCounter<-4 #the number of column of the turns in the dataset

#list of dataframes of turn proportions(8 turns) for each length
#turnMeans_all<-vector("list", extreme_length+1)

# there is a problem with extracting turn proportions, since the dataset is aggregated per subject in time, if a subject does not have any of those bouts of that length, then that row will be missing if additionally aggregated by subject in time 
# for bout length, so instead of a missing row NA is insterted

turnMeans_all<-c()

for(turnType in turnTypes){
	
	turnMeans<-c()
	
	for(time_frame in 1:13){
		
		turnMeans<-c(turnMeans, aggregate(dataset[dataset$TimeFactor==time_frame,c(1,turnCounter)],
						by=list(dataset[dataset$TimeFactor==time_frame,1]),FUN=mean)[,3])
		
	}	
	
	turnMeans_all<-cbind(turnMeans_all,turnMeans)	
	#go to next turn in the dataset
	turnCounter<-turnCounter+1
}



#make column names
colnames(turnMeans_all)<-paste0(turnTypes,"Proportion")



#bind all together

dataset_all<-cbind(count_all_bouts, count_bouts_per_length_all,turnMeans_all)

#take mean of all subjects per time frame
dataset_all_mean<-aggregate(dataset_all,by=list(dataset_all$TimeFactor),FUN=mean,na.rm=T)[,c(4,3,5:(12+extreme_length+1))]


#get dataset for transitions of length 2 and 3 (not stratified, mean random subject)

#load data mean per subject
dataset_word2<-read.table(paste0("../simple_word_search/B2/",args[2],"/",args[1],"_word2"),header=TRUE,sep=",")
dataset_word3<-read.table(paste0("../simple_word_search/B2/",args[2],"/",args[1],"_word3"),header=TRUE,sep=",")

#name the data per transition,
colnames(dataset_word2)<-paste0("Transition_",colnames(dataset_word2),"_Proportion")
colnames(dataset_word3)<-paste0("Transition_",colnames(dataset_word3),"_Proportion")


#bind together
dataset_all_mean<-cbind(dataset_all_mean, dataset_word2, dataset_word3)

#save data
write.table(dataset_all_mean,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/final_dataset_separated/B2/",args[2],"/",
				args[1],"_final_dataset_separated.txt"),row.names=F,col.names=T)
