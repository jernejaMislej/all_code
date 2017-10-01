#Load packages
library(reshape2)
library(car)
library(pracma)

#args is the experiment condition
args <- commandArgs(trailingOnly = TRUE)

baseFile=paste('../../processed_data/bout_length_turn_proportion_time_factor_dataset_all/',args[1],'/',args[2], sep="")
file_BoutLength=paste(baseFile,'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'_TimeFactor', sep="")
file_ScootsProportion=paste(baseFile,'_ScootsProportion', sep="")
file_JBendsProportion=paste(baseFile,'_JBendsProportion', sep="")
file_CBendsProportion=paste(baseFile,'_CBendsProportion', sep="")
file_OBendsProportion=paste(baseFile,'_OBendsProportion', sep="")
file_EBendsProportion=paste(baseFile,'_EBendsProportion', sep="")
file_GBendsProportion=paste(baseFile,'_GBendsProportion', sep="")
file_HBendsProportion=paste(baseFile,'_HBendsProportion', sep="")
file_IBendsProportion=paste(baseFile,'_IBendsProportion', sep="")


#read data for the given turn type
All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_ScootsProportion <- read.csv(file_ScootsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_JBendsProportion <- read.csv(file_JBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_CBendsProportion <- read.csv(file_CBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_OBendsProportion <- read.csv(file_OBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_EBendsProportion <- read.csv(file_EBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_GBendsProportion <- read.csv(file_GBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_HBendsProportion <- read.csv(file_HBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_IBendsProportion <- read.csv(file_IBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))


#melt the datasets
All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]
All_ScootsProportion_Melted<-na.omit(melt(t(All_ScootsProportion)))[,c(2,3)]
All_JBendsProportion_Melted<-na.omit(melt(t(All_JBendsProportion)))[,c(2,3)]
All_CBendsProportion_Melted<-na.omit(melt(t(All_CBendsProportion)))[,c(2,3)]
All_OBendsProportion_Melted<-na.omit(melt(t(All_OBendsProportion)))[,c(2,3)]
All_EBendsProportion_Melted<-na.omit(melt(t(All_EBendsProportion)))[,c(2,3)]
All_GBendsProportion_Melted<-na.omit(melt(t(All_GBendsProportion)))[,c(2,3)]
All_HBendsProportion_Melted<-na.omit(melt(t(All_HBendsProportion)))[,c(2,3)]
All_IBendsProportion_Melted<-na.omit(melt(t(All_IBendsProportion)))[,c(2,3)]





colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")
colnames(All_ScootsProportion_Melted)<-c("Subject","ScootsProportion")
colnames(All_JBendsProportion_Melted)<-c("Subject","JBendsProportion")
colnames(All_CBendsProportion_Melted)<-c("Subject","CBendsProportion")
colnames(All_OBendsProportion_Melted)<-c("Subject","OBendsProportion")
colnames(All_EBendsProportion_Melted)<-c("Subject","EBendsProportion")
colnames(All_GBendsProportion_Melted)<-c("Subject","GBendsProportion")
colnames(All_HBendsProportion_Melted)<-c("Subject","HBendsProportion")
colnames(All_IBendsProportion_Melted)<-c("Subject","IBendsProportion")

All<-cbind(All_BoutLength_Melted$Subject, All_BoutLength_Melted$BoutLength, All_TimeFactor_Melted$TimeFactor, All_ScootsProportion_Melted$ScootsProportion, All_JBendsProportion_Melted$JBendsProportion, All_CBendsProportion_Melted$CBendsProportion, All_OBendsProportion_Melted$OBendsProportion, All_EBendsProportion_Melted$EBendsProportion, All_GBendsProportion_Melted$GBendsProportion, All_HBendsProportion_Melted$HBendsProportion, All_IBendsProportion_Melted$IBendsProportion)

All[,1]<-factor(All[,1])

colnames(All)<-c("Subject","BoutLength","TimeFactor","ScootsProportion","JBendsProportion","CBendsProportion","OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")

All<-as.data.frame(All)
All<-All[All$BoutLength>0,]

#aggregate mean turn proportions per bout length
mean_ScootsProportions<-aggregate(ScootsProportion~BoutLength, data=All, FUN=mean)
mean_JBendsProportions<-aggregate(JBendsProportion~BoutLength, data=All, FUN=mean)
mean_CBendsProportions<-aggregate(CBendsProportion~BoutLength, data=All, FUN=mean)
mean_OBendsProportions<-aggregate(OBendsProportion~BoutLength, data=All, FUN=mean)
mean_EBendsProportions<-aggregate(EBendsProportion~BoutLength, data=All, FUN=mean)
mean_GBendsProportions<-aggregate(GBendsProportion~BoutLength, data=All, FUN=mean)
mean_HBendsProportions<-aggregate(HBendsProportion~BoutLength, data=All, FUN=mean)
mean_IBendsProportions<-aggregate(IBendsProportion~BoutLength, data=All, FUN=mean)


#make plot ScootsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_ScootsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_ScootsProportions[,1],mean_ScootsProportions[,2],type="l", main=paste("In ",args[1]," Mean Scoots proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_ScootsProportions[,1])
dev.off()

#make plot JBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_JBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_JBendsProportions[,1],mean_JBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean JBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_JBendsProportions[,1])
dev.off()

#make plot CBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_CBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_CBendsProportions[,1],mean_CBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean CBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_CBendsProportions[,1])
dev.off()

#make plot OBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_OBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_OBendsProportions[,1],mean_OBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean OBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_OBendsProportions[,1])
dev.off()

#make plot EBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_EBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_EBendsProportions[,1],mean_EBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean EBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_EBendsProportions[,1])
dev.off()

#make plot GBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_GBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_GBendsProportions[,1],mean_GBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean GBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_GBendsProportions[,1])
dev.off()

#make plot HBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_HBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_HBendsProportions[,1],mean_HBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean HBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_HBendsProportions[,1])
dev.off()

#make plot IBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/mean_turn_proportion_per_bout_length/",args[1],"/",args[2],"_plot_Mean_IBendsProportion_per_BoutLength.png", sep=""),width=1000)
plot(mean_IBendsProportions[,1],mean_IBendsProportions[,2],type="l", main=paste("In ",args[1]," Mean IBends proportion per bout length", sep=""), ylab= "Turn proportion", xlab="Bout length", col="blue")
axis(1, at=mean_IBendsProportions[,1])
dev.off()


# asses which fall out of "normal"