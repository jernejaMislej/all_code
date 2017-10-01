#Load packages
library(reshape2)
library(rgl)
library(plotrix)
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



turnTypes=c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")

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
	AllNormal<-cbind(All_BoutCountNormal_Melted$Subject, All_BoutCountNormal_Melted$NormalBoutCount, 
			All_TurnProportionNormal_Melted$NormalTurnProportion, All_SequenceLengthNormal_Melted$NormalSequenceLength)
	AllOutlier<-cbind(All_BoutCountOutlier_Melted$Subject, All_BoutCountOutlier_Melted$OutlierBoutCount, 
			All_TurnProportionOutlier_Melted$OutlierTurnProportion, All_SequenceLengthOutlier_Melted$OutlierSequenceLength)
	
	AllNormal[,1]<-factor(AllNormal[,1])
	AllOutlier[,1]<-factor(AllOutlier[,1])

	colnames(AllNormal)<-c("Subject","NormalBoutCount","NormalTurnProportion","NormalSequenceLength")
	colnames(AllOutlier)<-c("Subject","OutlierBoutCount","OutlierTurnProportion","OutlierSequenceLength")

	AllNormal<-as.data.frame(AllNormal)
	AllOutlier<-as.data.frame(AllOutlier)
	
	AllNormal<-AllNormal[AllNormal$NormalBoutCount!=0,]
	AllOutlier<-AllOutlier[AllOutlier$OutlierBoutCount!=0,]
	
	png(paste("../../results/plots/outliers_exploration/",args[1],"_",turnType,"_allConditionsPredicted.png", sep=""),width=1000)
	ploty<-scatterplot3d(AllNormal$NormalTurnProportion,AllNormal$NormalBoutCount,AllNormal$NormalSequenceLength,pch=16,
			color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
			xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
	ploty$points3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,col="green")
	ploty$points3d(AllOutlier$OutlierTurnProportion,AllOutlier$OutlierBoutCount,AllOutlier$OutlierSequenceLength,pch=16,col="red")
	ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
	dev.off()
	
	
	ploty<-scatterplot3d(PredictedNormal$ScootsProportion,PredictedNormal$BoutCount,PredictedNormal$SequenceLength,pch=16,
			color="green", xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length",angle=-240, 
			xlim=c(0, 1), ylim=c(0,1400),zlim=c(0,2800), main="DarkApoHigh Predicted")
	ploty$points3d(PredictedOutliers$ScootsProportion,PredictedOutliers$BoutCount,PredictedOutliers$SequenceLength,pch=16,col="blue")
	
	
	
	#plot the 3d scatter plot
	plot3d(AllOutlier$OutlierTurnProportion,AllOutlier$OutlierBoutCount,AllOutlier$OutlierSequenceLength, col="red", size=10, xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length")
	plot3d(AllNormal$NormalTurnProportion,AllNormal$NormalBoutCount,AllNormal$NormalSequenceLength, col="green", size=10, add=TRUE, xlab=paste(turnType," proportion"),ylab="Bout counts",zlab="Sequence length")
	
	
	
	theta = seq(0,180,len=90)
	phi = seq(0,90,len=90)
	
	for(i in 1:90){
		snapshot3d(file=paste0(i,".png"))
		view3d(theta=theta[i],phi=phi[i])
	}
	
}

file_motif2_Normal=paste(baseFile,'/All',args[1],'_motif2_Normal', sep="")
file_motif3_Normal=paste(baseFile,'/All',args[1],'_motif3_Normal', sep="")
file_motif4_Normal=paste(baseFile,'/All',args[1],'_motif4_Normal', sep="")
file_motif5_Normal=paste(baseFile,'/All',args[1],'_motif5_Normal', sep="")
file_motif6_Normal=paste(baseFile,'/All',args[1],'_motif6_Normal', sep="")
file_motif7_Normal=paste(baseFile,'/All',args[1],'_motif7_Normal', sep="")
file_motif8_Normal=paste(baseFile,'/All',args[1],'_motif8_Normal', sep="")
file_motif9_Normal=paste(baseFile,'/All',args[1],'_motif9_Normal', sep="")
file_motif10_Normal=paste(baseFile,'/All',args[1],'_motif10_Normal', sep="")

All_motif2_Normal <- read.csv(file_motif2_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif3_Normal <- read.csv(file_motif3_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif4_Normal <- read.csv(file_motif4_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif5_Normal <- read.csv(file_motif5_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif6_Normal <- read.csv(file_motif6_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif7_Normal <- read.csv(file_motif7_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif8_Normal <- read.csv(file_motif8_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif9_Normal <- read.csv(file_motif9_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif10_Normal <- read.csv(file_motif10_Normal, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)




file_motif2_Outlier=paste(baseFile,'/All',args[1],'_motif2_Outlier', sep="")
file_motif3_Outlier=paste(baseFile,'/All',args[1],'_motif3_Outlier', sep="")
file_motif4_Outlier=paste(baseFile,'/All',args[1],'_motif4_Outlier', sep="")
file_motif5_Outlier=paste(baseFile,'/All',args[1],'_motif5_Outlier', sep="")
file_motif6_Outlier=paste(baseFile,'/All',args[1],'_motif6_Outlier', sep="")
file_motif7_Outlier=paste(baseFile,'/All',args[1],'_motif7_Outlier', sep="")
file_motif8_Outlier=paste(baseFile,'/All',args[1],'_motif8_Outlier', sep="")
file_motif9_Outlier=paste(baseFile,'/All',args[1],'_motif9_Outlier', sep="")
file_motif10_Outlier=paste(baseFile,'/All',args[1],'_motif10_Outlier', sep="")

All_motif2_Outlier <- read.csv(file_motif2_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif3_Outlier <- read.csv(file_motif3_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif4_Outlier <- read.csv(file_motif4_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif5_Outlier <- read.csv(file_motif5_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif6_Outlier <- read.csv(file_motif6_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif7_Outlier <- read.csv(file_motif7_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif8_Outlier <- read.csv(file_motif8_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif9_Outlier <- read.csv(file_motif9_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif10_Outlier <- read.csv(file_motif10_Outlier, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)

#merge motifs and their counts
All_motif2_Normal_Merged <-aggregate(V2~V1,data=All_motif2_Normal,FUN=sum)
All_motif3_Normal_Merged <-aggregate(V2~V1,data=All_motif3_Normal,FUN=sum)
All_motif4_Normal_Merged <-aggregate(V2~V1,data=All_motif4_Normal,FUN=sum)
All_motif5_Normal_Merged <-aggregate(V2~V1,data=All_motif5_Normal,FUN=sum)
All_motif6_Normal_Merged <-aggregate(V2~V1,data=All_motif6_Normal,FUN=sum)
All_motif7_Normal_Merged <-aggregate(V2~V1,data=All_motif7_Normal,FUN=sum)
All_motif8_Normal_Merged <-aggregate(V2~V1,data=All_motif8_Normal,FUN=sum)
All_motif9_Normal_Merged <-aggregate(V2~V1,data=All_motif9_Normal,FUN=sum)
All_motif10_Normal_Merged <-aggregate(V2~V1,data=All_motif10_Normal,FUN=sum)


colnames(All_motif2_Normal_Merged)<-c("Motif","Count")
colnames(All_motif3_Normal_Merged)<-c("Motif","Count")
colnames(All_motif4_Normal_Merged)<-c("Motif","Count")
colnames(All_motif5_Normal_Merged)<-c("Motif","Count")
colnames(All_motif6_Normal_Merged)<-c("Motif","Count")
colnames(All_motif7_Normal_Merged)<-c("Motif","Count")
colnames(All_motif8_Normal_Merged)<-c("Motif","Count")
colnames(All_motif9_Normal_Merged)<-c("Motif","Count")
colnames(All_motif10_Normal_Merged)<-c("Motif","Count")

#add a column representing percentages
All_motif2_Normal_Merged$CountPrecentage<-round((All_motif2_Normal_Merged$Count/sum(All_motif2_Normal_Merged[,2]))*100,2)
All_motif3_Normal_Merged$CountPrecentage<-round((All_motif3_Normal_Merged$Count/sum(All_motif3_Normal_Merged[,2]))*100,2)
All_motif4_Normal_Merged$CountPrecentage<-round((All_motif4_Normal_Merged$Count/sum(All_motif4_Normal_Merged[,2]))*100,2)
All_motif5_Normal_Merged$CountPrecentage<-round((All_motif5_Normal_Merged$Count/sum(All_motif5_Normal_Merged[,2]))*100,2)
All_motif6_Normal_Merged$CountPrecentage<-round((All_motif6_Normal_Merged$Count/sum(All_motif6_Normal_Merged[,2]))*100,2)
All_motif7_Normal_Merged$CountPrecentage<-round((All_motif7_Normal_Merged$Count/sum(All_motif7_Normal_Merged[,2]))*100,2)
All_motif8_Normal_Merged$CountPrecentage<-round((All_motif8_Normal_Merged$Count/sum(All_motif8_Normal_Merged[,2]))*100,2)
All_motif9_Normal_Merged$CountPrecentage<-round((All_motif9_Normal_Merged$Count/sum(All_motif9_Normal_Merged[,2]))*100,2)
All_motif10_Normal_Merged$CountPrecentage<-round((All_motif10_Normal_Merged$Count/sum(All_motif10_Normal_Merged[,2]))*100,2)


#write the results for the simple motif search
resultsFile=paste("../../results/output/outliers_exploration/All",args[1], sep="")
write.table(All_motif2_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif2_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif3_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif3_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif4_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif4_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif5_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif5_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif6_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif6_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif7_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif7_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif8_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif8_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif9_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif9_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif10_Normal_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif10_Normal",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")


#make boxplots for each motif length for the first 10 counts

#extract the first 10
All_motif2_Normal_Merged_first10<-na.omit(All_motif2_Normal_Merged[with(All_motif2_Normal_Merged,order(-Count)),][1:10,])
All_motif3_Normal_Merged_first10<-na.omit(All_motif3_Normal_Merged[with(All_motif3_Normal_Merged,order(-Count)),][1:10,])
All_motif4_Normal_Merged_first10<-na.omit(All_motif4_Normal_Merged[with(All_motif4_Normal_Merged,order(-Count)),][1:10,])
All_motif5_Normal_Merged_first10<-na.omit(All_motif5_Normal_Merged[with(All_motif5_Normal_Merged,order(-Count)),][1:10,])
All_motif6_Normal_Merged_first10<-na.omit(All_motif6_Normal_Merged[with(All_motif6_Normal_Merged,order(-Count)),][1:10,])
All_motif7_Normal_Merged_first10<-na.omit(All_motif7_Normal_Merged[with(All_motif7_Normal_Merged,order(-Count)),][1:10,])
All_motif8_Normal_Merged_first10<-na.omit(All_motif8_Normal_Merged[with(All_motif8_Normal_Merged,order(-Count)),][1:10,])
All_motif9_Normal_Merged_first10<-na.omit(All_motif9_Normal_Merged[with(All_motif9_Normal_Merged,order(-Count)),][1:10,])
All_motif10_Normal_Merged_first10<-na.omit(All_motif10_Normal_Merged[with(All_motif10_Normal_Merged,order(-Count)),][1:10,])

#plot
png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif2_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif2_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif2_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif2_Normal_Merged[,2])," motif counts of length 2_Normal",sep=""), xlab="Motifs of length 2", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif2_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif2_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif2_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif2_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif2_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif3_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif3_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif3_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif3_Normal_Merged[,2])," motif counts of length 3_Normal",sep=""), xlab="Motifs of length 3", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif3_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif3_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif3_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif3_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif3_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif4_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif4_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif4_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif4_Normal_Merged[,2])," motif counts of length 4_Normal",sep=""), xlab="Motifs of length 4", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif4_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif4_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif4_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif4_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif4_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif5_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif5_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif5_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif5_Normal_Merged[,2])," motif counts of length 5_Normal",sep=""), xlab="Motifs of length 5", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif5_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif5_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif5_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif5_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif5_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif6_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif6_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif6_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif6_Normal_Merged[,2])," motif counts of length 6_Normal",sep=""), xlab="Motifs of length 6", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif6_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif6_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif6_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif6_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif6_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif7_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif7_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif7_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif7_Normal_Merged[,2])," motif counts of length 7_Normal",sep=""), xlab="Motifs of length 7", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif7_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif7_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif7_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif7_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif7_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif8_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif8_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif8_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif8_Normal_Merged[,2])," motif counts of length 8_Normal",sep=""), xlab="Motifs of length 8", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif8_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif8_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif8_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif8_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif8_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif9_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif9_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif9_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif9_Normal_Merged[,2])," motif counts of length 9_Normal",sep=""), xlab="Motifs of length 9", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif9_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif9_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif9_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif9_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif9_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif10_Normal_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif10_Normal_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif10_Normal_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif10_Normal_Merged[,2])," motif counts of length 10_Normal",sep=""), xlab="Motifs of length 10", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif10_Normal_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif10_Normal_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif10_Normal_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif10_Normal_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif10_Normal_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

#merge motifs and their counts
All_motif2_Outlier_Merged <-aggregate(V2~V1,data=All_motif2_Outlier,FUN=sum)
All_motif3_Outlier_Merged <-aggregate(V2~V1,data=All_motif3_Outlier,FUN=sum)
All_motif4_Outlier_Merged <-aggregate(V2~V1,data=All_motif4_Outlier,FUN=sum)
All_motif5_Outlier_Merged <-aggregate(V2~V1,data=All_motif5_Outlier,FUN=sum)
All_motif6_Outlier_Merged <-aggregate(V2~V1,data=All_motif6_Outlier,FUN=sum)
All_motif7_Outlier_Merged <-aggregate(V2~V1,data=All_motif7_Outlier,FUN=sum)
All_motif8_Outlier_Merged <-aggregate(V2~V1,data=All_motif8_Outlier,FUN=sum)
All_motif9_Outlier_Merged <-aggregate(V2~V1,data=All_motif9_Outlier,FUN=sum)
All_motif10_Outlier_Merged <-aggregate(V2~V1,data=All_motif10_Outlier,FUN=sum)


colnames(All_motif2_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif3_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif4_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif5_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif6_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif7_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif8_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif9_Outlier_Merged)<-c("Motif","Count")
colnames(All_motif10_Outlier_Merged)<-c("Motif","Count")

#add a column representing percentages
All_motif2_Outlier_Merged$CountPrecentage<-round((All_motif2_Outlier_Merged$Count/sum(All_motif2_Outlier_Merged[,2]))*100,2)
All_motif3_Outlier_Merged$CountPrecentage<-round((All_motif3_Outlier_Merged$Count/sum(All_motif3_Outlier_Merged[,2]))*100,2)
All_motif4_Outlier_Merged$CountPrecentage<-round((All_motif4_Outlier_Merged$Count/sum(All_motif4_Outlier_Merged[,2]))*100,2)
All_motif5_Outlier_Merged$CountPrecentage<-round((All_motif5_Outlier_Merged$Count/sum(All_motif5_Outlier_Merged[,2]))*100,2)
All_motif6_Outlier_Merged$CountPrecentage<-round((All_motif6_Outlier_Merged$Count/sum(All_motif6_Outlier_Merged[,2]))*100,2)
All_motif7_Outlier_Merged$CountPrecentage<-round((All_motif7_Outlier_Merged$Count/sum(All_motif7_Outlier_Merged[,2]))*100,2)
All_motif8_Outlier_Merged$CountPrecentage<-round((All_motif8_Outlier_Merged$Count/sum(All_motif8_Outlier_Merged[,2]))*100,2)
All_motif9_Outlier_Merged$CountPrecentage<-round((All_motif9_Outlier_Merged$Count/sum(All_motif9_Outlier_Merged[,2]))*100,2)
All_motif10_Outlier_Merged$CountPrecentage<-round((All_motif10_Outlier_Merged$Count/sum(All_motif10_Outlier_Merged[,2]))*100,2)


#write the results for the simple motif search
resultsFile=paste("../../results/output/outliers_exploration/All",args[1], sep="")
write.table(All_motif2_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif2_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif3_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif3_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif4_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif4_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif5_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif5_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif6_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif6_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif7_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif7_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif8_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif8_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif9_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif9_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif10_Outlier_Merged, file=paste("../../results/output/outliers_exploration/All",args[1],"_motif10_Outlier",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")


#make boxplots for each motif length for the first 10 counts

#extract the first 10
All_motif2_Outlier_Merged_first10<-na.omit(All_motif2_Outlier_Merged[with(All_motif2_Outlier_Merged,order(-Count)),][1:10,])
All_motif3_Outlier_Merged_first10<-na.omit(All_motif3_Outlier_Merged[with(All_motif3_Outlier_Merged,order(-Count)),][1:10,])
All_motif4_Outlier_Merged_first10<-na.omit(All_motif4_Outlier_Merged[with(All_motif4_Outlier_Merged,order(-Count)),][1:10,])
All_motif5_Outlier_Merged_first10<-na.omit(All_motif5_Outlier_Merged[with(All_motif5_Outlier_Merged,order(-Count)),][1:10,])
All_motif6_Outlier_Merged_first10<-na.omit(All_motif6_Outlier_Merged[with(All_motif6_Outlier_Merged,order(-Count)),][1:10,])
All_motif7_Outlier_Merged_first10<-na.omit(All_motif7_Outlier_Merged[with(All_motif7_Outlier_Merged,order(-Count)),][1:10,])
All_motif8_Outlier_Merged_first10<-na.omit(All_motif8_Outlier_Merged[with(All_motif8_Outlier_Merged,order(-Count)),][1:10,])
All_motif9_Outlier_Merged_first10<-na.omit(All_motif9_Outlier_Merged[with(All_motif9_Outlier_Merged,order(-Count)),][1:10,])
All_motif10_Outlier_Merged_first10<-na.omit(All_motif10_Outlier_Merged[with(All_motif10_Outlier_Merged,order(-Count)),][1:10,])

#plot
png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif2_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif2_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif2_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif2_Outlier_Merged[,2])," motif counts of length 2_Outlier",sep=""), xlab="Motifs of length 2", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif2_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif2_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif2_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif2_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif2_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif3_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif3_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif3_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif3_Outlier_Merged[,2])," motif counts of length 3_Outlier",sep=""), xlab="Motifs of length 3", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif3_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif3_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif3_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif3_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif3_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif4_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif4_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif4_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif4_Outlier_Merged[,2])," motif counts of length 4_Outlier",sep=""), xlab="Motifs of length 4", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif4_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif4_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif4_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif4_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif4_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif5_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif5_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif5_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif5_Outlier_Merged[,2])," motif counts of length 5_Outlier",sep=""), xlab="Motifs of length 5", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif5_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif5_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif5_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif5_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif5_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif6_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif6_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif6_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif6_Outlier_Merged[,2])," motif counts of length 6_Outlier",sep=""), xlab="Motifs of length 6", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif6_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif6_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif6_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif6_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif6_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif7_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif7_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif7_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif7_Outlier_Merged[,2])," motif counts of length 7_Outlier",sep=""), xlab="Motifs of length 7", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif7_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif7_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif7_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif7_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif7_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif8_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif8_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif8_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif8_Outlier_Merged[,2])," motif counts of length 8_Outlier",sep=""), xlab="Motifs of length 8", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif8_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif8_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif8_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif8_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif8_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif9_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif9_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif9_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif9_Outlier_Merged[,2])," motif counts of length 9_Outlier",sep=""), xlab="Motifs of length 9", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif9_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif9_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif9_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif9_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif9_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/outliers_exploration/',args[1],"_barplot_first10_motif10_Outlier_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif10_Outlier_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif10_Outlier_Merged_first10$Motif, main=paste(args[1],", first 10 precentages of total ",sum(All_motif10_Outlier_Merged[,2])," motif counts of length 10_Outlier",sep=""), xlab="Motifs of length 10", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif10_Outlier_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif10_Outlier_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif10_Outlier_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif10_Outlier_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif10_Outlier_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()


