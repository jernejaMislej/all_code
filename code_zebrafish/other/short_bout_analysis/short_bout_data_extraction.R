
#args<-c("Natural_Control_Light")
args <- commandArgs(trailingOnly = TRUE)

#load data
dataset<-read.table(paste0(args[1],".txt"))


#get the total count to adjust for it
count_all_bouts<-c()

for(time_frame in 1:13){
	
	count_all_bouts<-rbind(count_all_bouts, cbind(aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x))}),rep(time_frame,times=length(sort(unique(dataset$Subject[dataset$TimeFactor==time_frame]))))))
}

#look at the counts of short bouts

#length 1
count_length_1_bouts<-c()

for(time_frame in 1:13){
	
	count_length_1_bouts<-c(count_length_1_bouts, aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x[x==1]))})[,2])
}

#length 2
count_length_2_bouts<-c()

for(time_frame in 1:13){
	
	count_length_2_bouts<-c(count_length_2_bouts, aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x[x==2]))})[,2])
}

#length 3
count_length_3_bouts<-c()

for(time_frame in 1:13){
	
	count_length_3_bouts<-c(count_length_3_bouts, aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x[x==3]))})[,2])
}


#mean turns within the short bout length



#turns in short bouts
turnTypes<-c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")
turnCounter<-4#the number of column of the turns in the dataset

turnMeans_length1<-c()
turnMeans_length2<-c()
turnMeans_length3<-c()

# there is a problem with extracting turn proportions, since the dataset is aggregated per subject in time, if a subject does not have any of those bouts of that length, then that row will be missing if additionally aggregated by subject in time 
# for bout length, so instead of a missing row NA is insterted

for(turnType in turnTypes){
	
	#length 1
	
	proportion_length_1_turns<-c()
	
	for(time_frame in 1:13){
		
		proportion_length_1_turns<-c(proportion_length_1_turns, sapply(split(cbind(dataset[dataset$TimeFactor==time_frame,turnCounter], dataset[dataset$TimeFactor==time_frame,c("BoutLength")]), 
								dataset[dataset$TimeFactor==time_frame,c("Subject")]),function(x){
									end<-length(x)/2
									start<-length(x)/2 +1
									end2<-length(x)
									return(mean(x[1:end][x[start:end2]==1]))}))
	}

	turnMeans_length1<-cbind(turnMeans_length1, proportion_length_1_turns)


	#length 2
	
	proportion_length_2_turns<-c()
	
	for(time_frame in 1:13){
		
		proportion_length_2_turns<-c(proportion_length_2_turns, sapply(split(cbind(dataset[dataset$TimeFactor==time_frame,turnCounter], dataset[dataset$TimeFactor==time_frame,c("BoutLength")]), 
								dataset[dataset$TimeFactor==time_frame,c("Subject")]),function(x){
							end<-length(x)/2
							start<-length(x)/2 +1
							end2<-length(x)
							return(mean(x[1:end][x[start:end2]==2]))}))
	}
	
	turnMeans_length2<-cbind(turnMeans_length2, proportion_length_2_turns)
	
	
	#length 3
	
	proportion_length_3_turns<-c()
	
	for(time_frame in 1:13){
		
		proportion_length_3_turns<-c(proportion_length_3_turns, sapply(split(cbind(dataset[dataset$TimeFactor==time_frame,turnCounter], dataset[dataset$TimeFactor==time_frame,c("BoutLength")]), 
								dataset[dataset$TimeFactor==time_frame,c("Subject")]),function(x){
							end<-length(x)/2
							start<-length(x)/2 +1
							end2<-length(x)
							return(mean(x[1:end][x[start:end2]==3]))}))
	}
	
	turnMeans_length3<-cbind(turnMeans_length3, proportion_length_3_turns)
	
	
	#go to next turn in the dataset
	turnCounter<-turnCounter+1
}







write.table(cbind(count_all_bouts, count_length_1_bouts, count_length_2_bouts, count_length_3_bouts, turnMeans_length1, turnMeans_length2, 
				turnMeans_length3),file=paste0("~/git/zebrafish_action_sequence_project/processed_data/short_bout_analysis/",args[2],"/",
				args[1],"_count_bouts.txt"),row.names=F,col.names=F)
