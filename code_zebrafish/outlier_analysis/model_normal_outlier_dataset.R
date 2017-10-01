#Load packages
library(reshape2)
library(randomForest)
library(scatterplot3d)

#args is the experiment condition
#args <- commandArgs(trailingOnly = TRUE)
args<-c("DarkApoLow")

baseFile=paste('../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/',args[1], sep="")

file_BoutCountNormal=paste(baseFile,'/All',args[1],'_BoutCount_Normal', sep="")
file_SequenceLengthNormal=paste(baseFile,'/All',args[1],'_SequenceLength_Normal', sep="")
All_BoutCountNormal <- read.csv(file_BoutCountNormal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]
All_SequenceLengthNormal <- read.csv(file_SequenceLengthNormal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]

file_BoutCountOutlier=paste(baseFile,'/All',args[1],'_BoutCount_Outlier', sep="")
file_SequenceLengthOutlier=paste(baseFile,'/All',args[1],'_SequenceLength_Outlier', sep="")
All_BoutCountOutlier <- read.csv(file_BoutCountOutlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]
All_SequenceLengthOutlier <- read.csv(file_SequenceLengthOutlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]

#melt
All_BoutCountNormal_Melted<-na.omit(melt(t(All_BoutCountNormal)))[,c(2,3)]
All_SequenceLengthNormal_Melted<-na.omit(melt(t(All_SequenceLengthNormal)))[,c(2,3)]

All_BoutCountOutlier_Melted<-na.omit(melt(t(All_BoutCountOutlier)))[,c(2,3)]
All_SequenceLengthOutlier_Melted<-na.omit(melt(t(All_SequenceLengthOutlier)))[,c(2,3)]


colnames(All_BoutCountNormal_Melted)<-c("Subject","NormalBoutCount")
colnames(All_SequenceLengthNormal_Melted)<-c("Subject","NormalSequenceLength")

colnames(All_BoutCountOutlier_Melted)<-c("Subject","OutlierBoutCount")
colnames(All_SequenceLengthOutlier_Melted)<-c("Subject","OutlierSequenceLength")

turnType<-"Scoots"

file_TurnProportionNormal=paste(baseFile,'/All',args[1],'_',turnType,'Proportion_Normal', sep="")
file_TurnProportionOutlier=paste(baseFile,'/All',args[1],'_',turnType,'Proportion_Outlier', sep="")

#read data for the given turn type
All_TurnProportionNormal <- read.csv(file_TurnProportionNormal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]
All_TurnProportionOutlier <- read.csv(file_TurnProportionOutlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]



#melt
All_TurnProportionNormal_Melted<-na.omit(melt(t(All_TurnProportionNormal)))[,c(2,3)]

All_TurnProportionOutlier_Melted<-na.omit(melt(t(All_TurnProportionOutlier)))[,c(2,3)]


colnames(All_TurnProportionNormal_Melted)<-c("Subject","NormalTurnProportion")

colnames(All_TurnProportionOutlier_Melted)<-c("Subject","OutlierTurnProportion")



#combine
AllNormal<-cbind(All_BoutCountNormal_Melted$Subject, All_BoutCountNormal_Melted$NormalBoutCount, 
		All_SequenceLengthNormal_Melted$NormalSequenceLength, All_TurnProportionNormal_Melted$NormalTurnProportion)
AllOutlier<-cbind(All_BoutCountOutlier_Melted$Subject, All_BoutCountOutlier_Melted$OutlierBoutCount, 
		All_SequenceLengthOutlier_Melted$OutlierSequenceLength, All_TurnProportionOutlier_Melted$OutlierTurnProportion)

AllNormal[,1]<-factor(AllNormal[,1])
AllOutlier[,1]<-factor(AllOutlier[,1])

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionNormal=paste(baseFile,'/All',args[1],'_',turnType,'Proportion_Normal', sep="")
	file_TurnProportionOutlier=paste(baseFile,'/All',args[1],'_',turnType,'Proportion_Outlier', sep="")
	
	#read data for the given turn type
	All_TurnProportionNormal <- read.csv(file_TurnProportionNormal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]
	All_TurnProportionOutlier <- read.csv(file_TurnProportionOutlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:14))[,-1]
	
	
	
	#melt
	All_TurnProportionNormal_Melted<-na.omit(melt(t(All_TurnProportionNormal)))[,c(2,3)]
	
	All_TurnProportionOutlier_Melted<-na.omit(melt(t(All_TurnProportionOutlier)))[,c(2,3)]
	
	
	colnames(All_TurnProportionNormal_Melted)<-c("Subject","NormalTurnProportion")
	
	colnames(All_TurnProportionOutlier_Melted)<-c("Subject","OutlierTurnProportion")
	
	
	
	#combine
	AllNormal<-cbind(AllNormal, All_TurnProportionNormal_Melted$NormalTurnProportion)
	AllOutlier<-cbind(AllOutlier, All_TurnProportionOutlier_Melted$OutlierTurnProportion)
	
}

colnames(AllNormal)<-c("Subject","BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
colnames(AllOutlier)<-c("Subject","BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")

AllNormal<-as.data.frame(AllNormal)
AllOutlier<-as.data.frame(AllOutlier)

AllNormal$Class<-array(1,length(AllNormal$BoutCount))
AllOutlier$Class<-array(0,length(AllOutlier$BoutCount))


AllNormal<-AllNormal[AllNormal$BoutCount!=0,-1]
AllOutlier<-AllOutlier[AllOutlier$BoutCount!=0,-1]

#random undersample of the oversized normal:
AllNormal<-AllNormal[round(runif(length(AllNormal$BoutCount))*length(AllNormal$BoutCount))[1:800],]

lengthOutlierBoutCount<-length(AllOutlier$BoutCount)
#SMOTE oversample the undersized outliers for factor 10:
AllOutlier_overSampled<-c()
for (outlier in 1:lengthOutlierBoutCount){
	for(outlier2 in c(1:lengthOutlierBoutCount)[-outlier]){
		AllOutlier_overSampled<-rbind(AllOutlier_overSampled,c(AllOutlier[outlier,c(1:10)] + runif(1)*(AllOutlier[outlier,c(1:10)]-AllOutlier[outlier2,c(1:10)]),0))
		AllOutlier_overSampled<-rbind(AllOutlier_overSampled,c(AllOutlier[outlier,c(1:10)] + runif(1)*(AllOutlier[outlier,c(1:10)]-AllOutlier[outlier2,c(1:10)]),0))
		AllOutlier_overSampled<-rbind(AllOutlier_overSampled,c(AllOutlier[outlier,c(1:10)] + runif(1)*(AllOutlier[outlier,c(1:10)]-AllOutlier[outlier2,c(1:10)]),0))
		AllOutlier_overSampled<-rbind(AllOutlier_overSampled,c(AllOutlier[outlier,c(1:10)] + runif(1)*(AllOutlier[outlier,c(1:10)]-AllOutlier[outlier2,c(1:10)]),0))	
		AllOutlier_overSampled<-rbind(AllOutlier_overSampled,c(AllOutlier[outlier,c(1:10)] + runif(1)*(AllOutlier[outlier,c(1:10)]-AllOutlier[outlier2,c(1:10)]),0))
	}	
}

AllOutlier_overSampled<-as.data.frame(AllOutlier_overSampled)

AllOutlier_overSampled$BoutCount<-round(unlist(AllOutlier_overSampled$BoutCount))
AllOutlier_overSampled$SequenceLength<-round(unlist(AllOutlier_overSampled$SequenceLength))
AllOutlier_overSampled$ScootsProportion<-abs(unlist(AllOutlier_overSampled$ScootsProportion))
AllOutlier_overSampled$JBendsProportion<-abs(unlist(AllOutlier_overSampled$JBendsProportion))
AllOutlier_overSampled$CBendsProportion<-abs(unlist(AllOutlier_overSampled$CBendsProportion))
AllOutlier_overSampled$OBendsProportion<-abs(unlist(AllOutlier_overSampled$OBendsProportion))
AllOutlier_overSampled$EBendsProportion<-abs(unlist(AllOutlier_overSampled$EBendsProportion))
AllOutlier_overSampled$GBendsProportion<-abs(unlist(AllOutlier_overSampled$GBendsProportion))
AllOutlier_overSampled$HBendsProportion<-abs(unlist(AllOutlier_overSampled$HBendsProportion))
AllOutlier_overSampled$IBendsProportion<-abs(unlist(AllOutlier_overSampled$IBendsProportion))

#keep just the sane ones, length will result around 440
AllOutlier_overSampled<-AllOutlier_overSampled[AllOutlier_overSampled$BoutCount>600 & AllOutlier_overSampled$SequenceLength>1100 & 
				AllOutlier_overSampled$OBendsProportion>0.2 & 
				AllOutlier_overSampled$CBendsProportion>0.1 & 
				AllOutlier_overSampled$ScootsProportion<0.4,]

colnames(AllOutlier_overSampled)[11]<-"Class"


#out of  around 800 normal instances, take random of around 400 for learning and the rest for testing
separationIndexes<-round(runif(length(AllNormal$BoutCount))*length(AllNormal$BoutCount))[1:400]
separationIndexes<-separationIndexes[!duplicated(separationIndexes)]

#combine with around 200 artificial outliers and half of the real ones
AllLearning<-rbind(AllNormal[separationIndexes,], AllOutlier_overSampled[1:200,],AllOutlier[c(1:5),])
#proportion of 0: 0.3912214
#proportion of 1: 0.6087786

#combine the rest of normal(400) with the rest of artificial outliers plus the other half of the real outliers
AllTesting<-rbind(AllNormal[-separationIndexes,], AllOutlier[c(6:11),], AllOutlier_overSampled[201:length(AllOutlier_overSampled$BoutCount),])
#proportion of 0: 0.3374656
#proportion of 1: 0.6625344


#using all atributes
fitData<-randomForest(as.factor(unlist(Class)) ~ BoutCount + SequenceLength + ScootsProportion + JBendsProportion + CBendsProportion + OBendsProportion +
				EBendsProportion + GBendsProportion + HBendsProportion + IBendsProportion,
		data=AllLearning, 
		importance=TRUE, 
		ntree=2000)

# using sequence length, bout count and C, O bends and Scoots only......The results of modelling are the same, predictions for DarkApoLow
# are the same, but the number of predictions in all conditions goes down from 2238 to 1931, while the DarkApoLow only will go down insignificantly
# from 177 to 172 
fitData<-randomForest(as.factor(unlist(Class)) ~ BoutCount + SequenceLength + ScootsProportion + CBendsProportion + OBendsProportion,
		data=AllLearning, 
		importance=TRUE, 
		ntree=2000)

predictData<-predict(fitData, AllTesting[,-11])

#> table(predictData,unlist(AllTesting$Class))

#				AllTesting$Class
#predictData    0    1
#0  			245  3
#1   			0   478


#class. acc.: 0.9958678
#sensitivity: 0.993763
#specificity: 100
#precision: 100



#load the unknown dataset for the rest of DarkApoLow
file_BoutCountUnknown=paste(baseFile,'/All',args[1],'_BoutCount_Unknown', sep="")
file_SequenceLengthUnknown=paste(baseFile,'/All',args[1],'_SequenceLength_Unknown', sep="")
All_BoutCountUnknown <- read.csv(file_BoutCountUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))
All_SequenceLengthUnknown <- read.csv(file_SequenceLengthUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))

#melt
All_BoutCountUnknown_Melted<-melt(All_BoutCountUnknown)

All_SequenceLengthUnknown_Melted<-melt(All_SequenceLengthUnknown)

colnames(All_BoutCountUnknown_Melted)<-c("Subject","UnknownBoutCount")
colnames(All_SequenceLengthUnknown_Melted)<-c("Subject","UnknownSequenceLength")

turnType<-"Scoots"

file_TurnProportionUnknown=paste(baseFile,'/All',args[1],'_',turnType,'Proportion_Unknown', sep="")

#read data for the given turn type
All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))


#melt
All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)


colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")



#combine
AllUnknown<-cbind(All_BoutCountUnknown_Melted$UnknownBoutCount, 
		All_SequenceLengthUnknown_Melted$UnknownSequenceLength, All_TurnProportionUnknown_Melted$UnknownTurnProportion)

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionUnknown=paste(baseFile,'/All',args[1],'_',turnType,'Proportion_Unknown', sep="")
	
	#read data for the given turn type
	All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))	
	
	
	#melt
	All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)
	
	colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")
	
	
	
	#combine
	AllUnknown<-cbind(AllUnknown, All_TurnProportionUnknown_Melted$UnknownTurnProportion)
	
}

colnames(AllUnknown)<-c("BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
row.names(AllUnknown)<-paste0(rep(paste0(row.names(All_BoutCountUnknown),"_recording_"),times=13),rep(1:13,each=480))

AllUnknown<-as.data.frame(AllUnknown)



AllUnknown<-AllUnknown[AllUnknown$BoutCount!=0,]


#predict for the unknown
predict_UnknownData<-predict(fitData, AllUnknown)

PredictedOutliers<-AllUnknown[predict_UnknownData==0,]
PredictedNormal<-AllUnknown[predict_UnknownData==1,]

#PredictedOutliers<-PredictedOutliers[PredictedOutliers$BoutCount>500,]


write(row.names(PredictedOutliers), file="../../results/output/outliers_exploration/AllDarkApoLowPredictedOutliers")

#-------------------------------------------------------------------------------------------------


#load the unknown dataset for DarkApoHigh

baseFile='../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/DarkApoHigh'

#load the unknown dataset for the rest of DarkApoLow
file_BoutCountUnknown=paste(baseFile,'/AllDarkApoHigh_BoutCount_Unknown', sep="")
file_SequenceLengthUnknown=paste(baseFile,'/AllDarkApoHigh_SequenceLength_Unknown', sep="")
All_BoutCountUnknown <- read.csv(file_BoutCountUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))
All_SequenceLengthUnknown <- read.csv(file_SequenceLengthUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))

#melt
All_BoutCountUnknown_Melted<-melt(All_BoutCountUnknown)

All_SequenceLengthUnknown_Melted<-melt(All_SequenceLengthUnknown)

colnames(All_BoutCountUnknown_Melted)<-c("Subject","UnknownBoutCount")
colnames(All_SequenceLengthUnknown_Melted)<-c("Subject","UnknownSequenceLength")

turnType<-"Scoots"

file_TurnProportionUnknown=paste(baseFile,'/AllDarkApoHigh_',turnType,'Proportion_Unknown', sep="")

#read data for the given turn type
All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))


#melt
All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)


colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")



#combine
AllUnknown<-cbind(All_BoutCountUnknown_Melted$UnknownBoutCount, 
		All_SequenceLengthUnknown_Melted$UnknownSequenceLength, All_TurnProportionUnknown_Melted$UnknownTurnProportion)

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionUnknown=paste(baseFile,'/AllDarkApoHigh_',turnType,'Proportion_Unknown', sep="")
	
	#read data for the given turn type
	All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))	
	
	
	#melt
	All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)
	
	colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")
	
	
	
	#combine
	AllUnknown<-cbind(AllUnknown, All_TurnProportionUnknown_Melted$UnknownTurnProportion)
	
}

colnames(AllUnknown)<-c("BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
row.names(AllUnknown)<-paste0(rep(paste0(row.names(All_BoutCountUnknown),"_recording_"),times=13),rep(1:13,each=576))

AllUnknown<-as.data.frame(AllUnknown)



AllUnknown<-AllUnknown[AllUnknown$BoutCount!=0,]


#predict for the unknown
predict_UnknownData<-predict(fitData, AllUnknown)

PredictedOutliers<-AllUnknown[predict_UnknownData==0,]
PredictedNormal<-AllUnknown[predict_UnknownData==1,]

#output
write(row.names(PredictedOutliers), file="../../results/output/outliers_exploration/AllDarkApoHighPredictedOutliers")

#plots
turnType<-"Scoots"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"JBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$JBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$JBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"CBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$CBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$CBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"OBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$OBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$OBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"EBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$EBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$EBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"GBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$GBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$GBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"HBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$HBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$HBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"IBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$IBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
ploty$points3d(PredictedOutliers$IBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()



#-------------------------------------------------------------------------------------------------


#load the unknown dataset for DarkPTZ

baseFile='../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/DarkPTZ'

#load the unknown dataset for the rest of DarkApoLow
file_BoutCountUnknown=paste(baseFile,'/AllDarkPTZ_BoutCount_Unknown', sep="")
file_SequenceLengthUnknown=paste(baseFile,'/AllDarkPTZ_SequenceLength_Unknown', sep="")
All_BoutCountUnknown <- read.csv(file_BoutCountUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))
All_SequenceLengthUnknown <- read.csv(file_SequenceLengthUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))

#melt
All_BoutCountUnknown_Melted<-melt(All_BoutCountUnknown)

All_SequenceLengthUnknown_Melted<-melt(All_SequenceLengthUnknown)

colnames(All_BoutCountUnknown_Melted)<-c("Subject","UnknownBoutCount")
colnames(All_SequenceLengthUnknown_Melted)<-c("Subject","UnknownSequenceLength")

turnType<-"Scoots"

file_TurnProportionUnknown=paste(baseFile,'/AllDarkPTZ_',turnType,'Proportion_Unknown', sep="")

#read data for the given turn type
All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))


#melt
All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)


colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")



#combine
AllUnknown<-cbind(All_BoutCountUnknown_Melted$UnknownBoutCount, 
		All_SequenceLengthUnknown_Melted$UnknownSequenceLength, All_TurnProportionUnknown_Melted$UnknownTurnProportion)

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionUnknown=paste(baseFile,'/AllDarkPTZ_',turnType,'Proportion_Unknown', sep="")
	
	#read data for the given turn type
	All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))	
	
	
	#melt
	All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)
	
	colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")
	
	
	
	#combine
	AllUnknown<-cbind(AllUnknown, All_TurnProportionUnknown_Melted$UnknownTurnProportion)
	
}

colnames(AllUnknown)<-c("BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
row.names(AllUnknown)<-paste0(rep(paste0(row.names(All_BoutCountUnknown),"_recording_"),times=13),rep(1:13,each=576))

AllUnknown<-as.data.frame(AllUnknown)



AllUnknown<-AllUnknown[AllUnknown$BoutCount!=0,]


#predict for the unknown
predict_UnknownData<-predict(fitData, AllUnknown)

PredictedOutliers<-AllUnknown[predict_UnknownData==0,]
PredictedNormal<-AllUnknown[predict_UnknownData==1,]

#output
write(row.names(PredictedOutliers), file="../../results/output/outliers_exploration/AllDarkPTZPredictedOutliers")

#plots
turnType<-"Scoots"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"JBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$JBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$JBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"CBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$CBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$CBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"OBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$OBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$OBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"EBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$EBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$EBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"GBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$GBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$GBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"HBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$HBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$HBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"IBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$IBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkPTZ Predicted")
ploty$points3d(PredictedOutliers$IBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()



#-------------------------------------------------------------------------------------------------


#load the unknown dataset for Dark

args[1]<-"Dark"

baseFile='../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/Dark'

#load the unknown dataset for the rest of DarkApoLow
file_BoutCountUnknown=paste(baseFile,'/AllDark_BoutCount_Unknown', sep="")
file_SequenceLengthUnknown=paste(baseFile,'/AllDark_SequenceLength_Unknown', sep="")
All_BoutCountUnknown <- read.csv(file_BoutCountUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))
All_SequenceLengthUnknown <- read.csv(file_SequenceLengthUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))

#melt
All_BoutCountUnknown_Melted<-melt(All_BoutCountUnknown)

All_SequenceLengthUnknown_Melted<-melt(All_SequenceLengthUnknown)

colnames(All_BoutCountUnknown_Melted)<-c("Subject","UnknownBoutCount")
colnames(All_SequenceLengthUnknown_Melted)<-c("Subject","UnknownSequenceLength")

turnType<-"Scoots"

file_TurnProportionUnknown=paste(baseFile,'/AllDark_',turnType,'Proportion_Unknown', sep="")

#read data for the given turn type
All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))


#melt
All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)


colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")



#combine
AllUnknown<-cbind(All_BoutCountUnknown_Melted$UnknownBoutCount, 
		All_SequenceLengthUnknown_Melted$UnknownSequenceLength, All_TurnProportionUnknown_Melted$UnknownTurnProportion)

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionUnknown=paste(baseFile,'/AllDark_',turnType,'Proportion_Unknown', sep="")
	
	#read data for the given turn type
	All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))	
	
	
	#melt
	All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)
	
	colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")
	
	
	
	#combine
	AllUnknown<-cbind(AllUnknown, All_TurnProportionUnknown_Melted$UnknownTurnProportion)
	
}

colnames(AllUnknown)<-c("BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
row.names(AllUnknown)<-paste0(rep(paste0(row.names(All_BoutCountUnknown),"_recording_"),times=13),rep(1:13,each=576))

AllUnknown<-as.data.frame(AllUnknown)



AllUnknown<-AllUnknown[AllUnknown$BoutCount!=0,]


#predict for the unknown
predict_UnknownData<-predict(fitData, AllUnknown)

PredictedOutliers<-AllUnknown[predict_UnknownData==0,]
PredictedNormal<-AllUnknown[predict_UnknownData==1,]

PredictedNormal<-rbind(PredictedNormal,PredictedOutliers[PredictedOutliers$OBendsProportion<0.2,])
PredictedOutliers<-PredictedOutliers[PredictedOutliers$OBendsProportion>=0.2,]


#output
write(row.names(PredictedOutliers), file="../../results/output/outliers_exploration/AllDarkPredictedOutliers")

#plots
turnType<-"Scoots"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"JBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$JBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$JBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"CBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$CBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$CBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"OBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$OBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$OBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"EBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$EBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$EBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"GBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$GBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$GBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"HBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$HBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$HBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"IBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$IBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Dark Predicted")
ploty$points3d(PredictedOutliers$IBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()




#-------------------------------------------------------------------------------------------------


#load the unknown dataset for Light

args[1]<-"Light"

baseFile='../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/Light'

#load the unknown dataset for the rest of LightApoLow
file_BoutCountUnknown=paste(baseFile,'/AllLight_BoutCount_Unknown', sep="")
file_SequenceLengthUnknown=paste(baseFile,'/AllLight_SequenceLength_Unknown', sep="")
All_BoutCountUnknown <- read.csv(file_BoutCountUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))
All_SequenceLengthUnknown <- read.csv(file_SequenceLengthUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))

#melt
All_BoutCountUnknown_Melted<-melt(All_BoutCountUnknown)

All_SequenceLengthUnknown_Melted<-melt(All_SequenceLengthUnknown)

colnames(All_BoutCountUnknown_Melted)<-c("Subject","UnknownBoutCount")
colnames(All_SequenceLengthUnknown_Melted)<-c("Subject","UnknownSequenceLength")

turnType<-"Scoots"

file_TurnProportionUnknown=paste(baseFile,'/AllLight_',turnType,'Proportion_Unknown', sep="")

#read data for the given turn type
All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))


#melt
All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)


colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")



#combine
AllUnknown<-cbind(All_BoutCountUnknown_Melted$UnknownBoutCount, 
		All_SequenceLengthUnknown_Melted$UnknownSequenceLength, All_TurnProportionUnknown_Melted$UnknownTurnProportion)

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionUnknown=paste(baseFile,'/AllLight_',turnType,'Proportion_Unknown', sep="")
	
	#read data for the given turn type
	All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))	
	
	
	#melt
	All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)
	
	colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")
	
	
	
	#combine
	AllUnknown<-cbind(AllUnknown, All_TurnProportionUnknown_Melted$UnknownTurnProportion)
	
}

colnames(AllUnknown)<-c("BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
row.names(AllUnknown)<-paste0(rep(paste0(row.names(All_BoutCountUnknown),"_recording_"),times=13),rep(1:13,each=576))

AllUnknown<-as.data.frame(AllUnknown)



AllUnknown<-AllUnknown[AllUnknown$BoutCount!=0,]


#predict for the unknown
predict_UnknownData<-predict(fitData, AllUnknown)

PredictedOutliers<-AllUnknown[predict_UnknownData==0,]
PredictedNormal<-AllUnknown[predict_UnknownData==1,]

PredictedNormal<-rbind(PredictedNormal,PredictedOutliers[PredictedOutliers$OBendsProportion<0.2,])
PredictedOutliers<-PredictedOutliers[PredictedOutliers$OBendsProportion>=0.2,]


#output
write(row.names(PredictedOutliers), file="../../results/output/outliers_exploration/AllLightPredictedOutliers")

#plots
turnType<-"Scoots"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"JBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$JBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$JBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"CBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$CBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$CBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"OBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$OBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$OBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"EBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$EBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$EBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"GBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$GBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$GBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"HBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$HBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$HBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"IBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$IBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="Light Predicted")
ploty$points3d(PredictedOutliers$IBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()





#-------------------------------------------------------------------------------------------------


#load the unknown dataset for LightDark

args[1]<-"LightDark"

baseFile='../../processed_data/outlier_analysis_bout_count_turn_proportion_dataset/LightDark'

#load the unknown dataset for the rest of LightDarkApoLow
file_BoutCountUnknown=paste(baseFile,'/AllLightDark_BoutCount_Unknown', sep="")
file_SequenceLengthUnknown=paste(baseFile,'/AllLightDark_SequenceLength_Unknown', sep="")
All_BoutCountUnknown <- read.csv(file_BoutCountUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))
All_SequenceLengthUnknown <- read.csv(file_SequenceLengthUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))

#melt
All_BoutCountUnknown_Melted<-melt(All_BoutCountUnknown)

All_SequenceLengthUnknown_Melted<-melt(All_SequenceLengthUnknown)

colnames(All_BoutCountUnknown_Melted)<-c("Subject","UnknownBoutCount")
colnames(All_SequenceLengthUnknown_Melted)<-c("Subject","UnknownSequenceLength")

turnType<-"Scoots"

file_TurnProportionUnknown=paste(baseFile,'/AllLightDark_',turnType,'Proportion_Unknown', sep="")

#read data for the given turn type
All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))


#melt
All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)


colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")



#combine
AllUnknown<-cbind(All_BoutCountUnknown_Melted$UnknownBoutCount, 
		All_SequenceLengthUnknown_Melted$UnknownSequenceLength, All_TurnProportionUnknown_Melted$UnknownTurnProportion)

turnTypes=c("JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {
	
	file_TurnProportionUnknown=paste(baseFile,'/AllLightDark_',turnType,'Proportion_Unknown', sep="")
	
	#read data for the given turn type
	All_TurnProportionUnknown <- read.csv(file_TurnProportionUnknown, header = FALSE, sep = ",", row.names = 1, fill=TRUE, col.names=c(1:14))	
	
	
	#melt
	All_TurnProportionUnknown_Melted<-melt(All_TurnProportionUnknown)
	
	colnames(All_TurnProportionUnknown_Melted)<-c("Subject","UnknownTurnProportion")
	
	
	
	#combine
	AllUnknown<-cbind(AllUnknown, All_TurnProportionUnknown_Melted$UnknownTurnProportion)
	
}

colnames(AllUnknown)<-c("BoutCount","SequenceLength","ScootsProportion","JBendsProportion","CBendsProportion",
		"OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")
row.names(AllUnknown)<-paste0(rep(paste0(row.names(All_BoutCountUnknown),"_recording_"),times=13),rep(1:13,each=576))

AllUnknown<-as.data.frame(AllUnknown)



AllUnknown<-AllUnknown[AllUnknown$BoutCount!=0,]


#predict for the unknown
predict_UnknownData<-predict(fitData, AllUnknown)

PredictedOutliers<-AllUnknown[predict_UnknownData==0,]
PredictedNormal<-AllUnknown[predict_UnknownData==1,]

PredictedNormal<-rbind(PredictedNormal,PredictedOutliers[PredictedOutliers$OBendsProportion<0.2,])
PredictedOutliers<-PredictedOutliers[PredictedOutliers$OBendsProportion>=0.2,]


#output
write(row.names(PredictedOutliers), file="../../results/output/outliers_exploration/AllLightDarkPredictedOutliers")

#plots
turnType<-"Scoots"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"JBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$JBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$JBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"CBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$CBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$CBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"OBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$OBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$OBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"EBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$EBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$EBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"GBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$GBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$GBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"HBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$HBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$HBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()

turnType<-"IBends"
png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_Predicted.png", sep=""),width=1000)
ploty<-scatterplot3d(PredictedNormal$IBendsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
		color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
		xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="LightDark Predicted")
ploty$points3d(PredictedOutliers$IBendsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
dev.off()







