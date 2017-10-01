#Load packages
library(reshape2)
library(car)
library(pracma)

#args is the experiment condition
args <- commandArgs(trailingOnly = TRUE)

baseFile=paste('../../processed_data/bout_length_turn_proportion_time_factor_dataset_all/',args[1],'/',args[2], sep="")
file_BoutLength=paste(baseFile,'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'_TimeFactor', sep="")

#read data for the given turn type
All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))


#melt the datasets
All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]




colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")

All<-cbind(All_BoutLength_Melted$Subject, All_BoutLength_Melted$BoutLength, All_TimeFactor_Melted$TimeFactor)

All[,1]<-factor(All[,1])

colnames(All)<-c("Subject","BoutLength","TimeFactor")

All<-as.data.frame(All)

#aggregate mean turn proportions per bout length and extract the zero and one length bout count (LightDark has no zero bout length!)
count_bouts_per_length<-aggregate(TimeFactor~BoutLength, data=All, FUN=length)
colnames(count_bouts_per_length)<-c("BoutLength","BoutCount")


#make a table of the counts per bout length , later maybe add for time factor aswell
write.table(count_bouts_per_length, file=paste("../../results/output/bout_cout_per_bout_length/", args[2],"_bout_count_per_bout_length",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")



#might not be valid anymore, since after cleaning there are more conditions with no zero length bouts
#if(args[1]!="LightDark"){
#	zero_length_bouts<-count_bouts_per_length$BoutCount[1]
#	count_bouts_per_length<-count_bouts_per_length[-1,]
#	length_one_bouts<-count_bouts_per_length$BoutCount[1]
#	count_bouts_per_length<-count_bouts_per_length[-1,]
#}else{
#	zero_length_bouts<-0
#	length_one_bouts<-count_bouts_per_length$BoutCount[1]
#	count_bouts_per_length<-count_bouts_per_length[-1,]		
#}
#
#
#
#
#png(paste("../../results/plots/bout_count_per_bout_length/",args[1],"_plot_count_per_BoutLength.png", sep=""),width=1000)
#plot(count_bouts_per_length[,1],count_bouts_per_length[,2],type="l", main=paste("In ",args[1],", bout count per bout length \n \n with ", zero_length_bouts, " zero length bouts and ", length_one_bouts, " length one bouts", sep=""), ylab= "Total count of bouts", xlab="Bout length", col="blue")
#axis(1, at=count_bouts_per_length[,1])
#dev.off()

