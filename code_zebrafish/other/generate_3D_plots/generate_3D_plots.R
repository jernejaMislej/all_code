library(reshape2)
library(rgl)
library(plotrix)
library(scatterplot3d)

#args is the experiment condition
args <- commandArgs(trailingOnly = TRUE)

args<-c("Light")
args<-c("Dark")
args<-c("LightDark")
args<-c("DarkApoLow")
args<-c("DarkApoHigh")
args<-c("DarkPTZ")



baseFile=paste('../../processed_data/bout_length_turn_proportion_time_factor_dataset/',args[1], sep="")

file_BoutLength=paste(baseFile,'/All',args[1],'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'/All',args[1],'_TimeFactor', sep="")
All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))


#melt
All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]

colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")

turnTypes=c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")

turnType<-turnTypes[1]

for (turnType in turnTypes) {
	
	file_TurnProportion=paste(baseFile,'/All',args[1],'_',turnType,'Proportion', sep="")
	
	#read data for the given turn type
	All_TurnProportion <- read.csv(file_TurnProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
	
	
	#melt
	All_TurnProportion_Melted<-na.omit(melt(t(All_TurnProportion)))[,c(2,3)]
	
		
	colnames(All_TurnProportion_Melted)<-c("Subject","TurnProportion")
		
	#combine
	All<-cbind(All_BoutLength_Melted$Subject, All_BoutLength_Melted$BoutLength, 
			All_TurnProportion_Melted$TurnProportion, All_TimeFactor_Melted$TimeFactor)
	
	All[,1]<-factor(All[,1])
	
	colnames(All)<-c("Subject","BoutLength","TurnProportion","TimeFactor")
	
	All<-as.data.frame(All)
	
	#--------the data is read it----------------------------
	
	#--------extra data-------------------------------------
	AllBoutMean_perTimeFactor<-aggregate(BoutLength~TimeFactor,data=All, FUN=mean)[,2]
	
	AllBoutCount_perTimeFacor<-aggregate(BoutLength~TimeFactor,data=All, FUN=length)[,2]
	AllBoutCount<-c()
	
	for(timeFrame in 1:13){
		AllBoutCount<-c(AllBoutCount,rep(AllBoutCount_perTimeFacor[timeFrame],times=AllBoutCount_perTimeFacor[timeFrame]))
	}
	
	AllTimeFramesColors_vector<-rainbow(13)
	AllTimeFramesColors<-c()
	
	for(timeFrame in 1:13){
		AllTimeFramesColors<-c(AllTimeFramesColors,rep(AllTimeFramesColors_vector[timeFrame],times=AllBoutCount_perTimeFacor[timeFrame]))
	}	
	
	#--------plot-------------------------------------------
	
	#interactive 3D
	
	#Turn proportion, bout length, time factor
	plot3d(All$TurnProportion,All$BoutLength,All$TimeFactor, size=10, 
			xlab=paste(turnType," proportion"),ylab="Bout length",zlab="Time factor")
	
	#Turn proportion, time factor, bout count
	plot3d(All$TurnProportion,All$TimeFactor,AllBoutCount, size=10, 
			xlab=paste(turnType," proportion"),ylab="Time factor",zlab="Bout count")	
	
	plot3d(AllBoutMean_perTimeFactor, AllBoutCount_perTimeFacor, c(1:13), size=10, 
			xlab="Bout mean",ylab="Bout count",zlab="Time factor",type="l",lwd="2")
	
	
}