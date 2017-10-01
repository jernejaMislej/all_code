
#args<-c("Natural_Control_Light")
args <- commandArgs(trailingOnly = TRUE)

#load data
dataset<-read.table(paste0(args[1],".txt"))

#check which condition to set the right bout length cut point for the extremely long bouts

extreme_length=5

if(args[2] == "DarkApoLow"){
	extreme_length=3
	
} else if(args[2] == "DarkPTZ"){
	extreme_length=7	
}
#LightDark=9

#get the total count 
count_all_bouts<-c()

for(time_frame in 1:13){
	
	count_all_bouts<-rbind(count_all_bouts, cbind(aggregate(BoutLength ~ Subject, data = dataset[dataset$TimeFactor==time_frame,], function(x){return(length(x))}),rep(time_frame,times=length(sort(unique(dataset$Subject[dataset$TimeFactor==time_frame]))))))
}

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






#mean turns within the short bout length

#turns in short bouts
turnTypes<-c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")
turnCounter<-4#the number of column of the turns in the dataset

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


#create dataset for transitions of length 2






#combine count_all_bouts, count_bouts_per_length_all,all items(per length-dataset per all turns) in turnMeans_all

write.table(cbind(count_all_bouts, count_length_1_bouts, count_length_2_bouts, count_length_3_bouts, turnMeans_length1, turnMeans_length2, 
				turnMeans_length3),file=paste0("~/git/zebrafish_action_sequence_project/processed_data/short_bout_analysis/",args[2],"/",
				args[1],"_count_bouts.txt"),row.names=F,col.names=F)
