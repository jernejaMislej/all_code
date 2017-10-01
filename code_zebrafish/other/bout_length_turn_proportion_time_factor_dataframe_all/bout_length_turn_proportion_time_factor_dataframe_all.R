#Load packages
library(reshape2)

#args is the experiment condition and type
args <- commandArgs(trailingOnly = TRUE)
#args<-c("Natural_Control_Dark")

baseFile=paste('../../../processed_data/bout_length_turn_proportion_time_factor_dataset_all/',args[2],'/',args[1], sep="")

file_BoutLength=paste(baseFile,'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'_TimeFactor', sep="")
All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))

#melt
All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]


colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")

turnType<-"Scoots"

file_TurnProportion=paste(baseFile,'_',turnType,'Proportion', sep="")

#read data for the given turn type
All_TurnProportion <- read.csv(file_TurnProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))

#melt
All_TurnProportion_Melted<-na.omit(melt(t(All_TurnProportion)))[,c(2,3)]


colnames(All_TurnProportion_Melted)<-c("Subject","TurnProportion")


#combine
All<-cbind(All_BoutLength_Melted$Subject, All_BoutLength_Melted$BoutLength, 
		All_TimeFactor_Melted$TimeFactor, All_TurnProportion_Melted$TurnProportion)

All[,1]<-factor(All[,1])

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")


for (turnType in turnTypes) {
	
	file_TurnProportion=paste(baseFile,'_',turnType,'Proportion', sep="")
	
	#read data for the given turn type
	All_TurnProportion <- read.csv(file_TurnProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
	
	#melt
	All_TurnProportion_Melted<-na.omit(melt(t(All_TurnProportion)))[,c(2,3)]
	
	colnames(All_TurnProportion_Melted)<-c("Subject","TurnProportion")
	
	
	#combine
	All<-cbind(All, All_TurnProportion_Melted$TurnProportion)
	
}

colnames(All)<-c("Subject","BoutLength","TimeFactor","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")

All<-as.data.frame(All)

for (subj in 1:144) {
	
	timePoints<-c()
	
	for (timeFrame in 1:13) {
		
		if(length(All$TimeFactor[All$Subject==subj & All$TimeFactor==timeFrame])){
			
			timePoints<-c(timePoints, 1:length(All$TimeFactor[All$Subject==subj & All$TimeFactor==timeFrame]))
			
		}
	}
	
	All$TimePoint[All$Subject==subj]<-timePoints
	
}

AllMelted<-melt(All,id="Subject")


save(AllMelted,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/bout_length_turn_proportion_time_factor_dataframe_all/",args[1],".Rda"))

write.table(All,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/bout_length_turn_proportion_time_factor_dataframe_all/",args[1],".txt"))
