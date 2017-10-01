#Load packages
library(reshape2)

#args is the experiment condition and type
args <- commandArgs(trailingOnly = TRUE)
#args<-c("Natural_Control_Light", "Light")

baseFile=paste('../../../processed_data/sequence_data_per_bout/',args[2],'/',args[1], sep="")

file_BoutLength=paste(baseFile,'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'_TimeFactor', sep="")
All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))

#melt
All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]


colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")

#add turn proportion data

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


# add transitions data
turnLetters=c('s','j','c','o','e','g','h','i')
transitions2=apply(expand.grid(p1 = turnLetters, p2 = turnLetters, stringsAsFactors = FALSE),1,function(x){return(paste0(x[1],x[2]))})
transitions3=apply(expand.grid(p1 = turnLetters, p2 = turnLetters, p3 = turnLetters, stringsAsFactors = FALSE),1,function(x){return(paste0(x[1],x[2],x[3]))})

for (transition in transitions2) {
	
	file_Transition2Proportion=paste(baseFile,'_',transition,'Transition2Proportion', sep="")
	
	#read data for the given turn type
	All_Transition2Proportion <- read.csv(file_Transition2Proportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
	
	#melt
	All_Transition2Proportion_Melted<-na.omit(melt(t(All_Transition2Proportion)))[,c(3)]
		
	All_Transition2Proportion_Melted[All_Transition2Proportion_Melted==100]<-NA
	
	#combine
	All<-cbind(All, All_Transition2Proportion_Melted)
	
	colnames(All)[length(colnames(All))]<-paste0(transition,"Transition")	
	
}

for (transition in transitions3) {
	
	file_Transition3Proportion=paste(baseFile,'_',transition,'Transition3Proportion', sep="")
	
	#read data for the given turn type
	All_Transition3Proportion <- read.csv(file_Transition3Proportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
	
	#melt
	All_Transition3Proportion_Melted<-na.omit(melt(t(All_Transition3Proportion)))[,c(2,3)]
	
	colnames(All_Transition3Proportion_Melted)<-c("Subject",transition)
	
	All_Transition3Proportion_Melted[All_Transition3Proportion_Melted[,2]==100,2]<-NA
	
	#combine
	All<-cbind(All, All_Transition3Proportion_Melted[,c(transition)])
	
	
}


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

#AllMelted<-melt(All,id="Subject")


save(All,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/dataset_per_bout/",args[1],".Rda"))

#write.table(All,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/dataset_per_bout/",args[1],".txt"))
