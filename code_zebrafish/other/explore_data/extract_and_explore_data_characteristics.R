#Load packages
library(reshape2)
library(car)
library(pracma)

#args is the experiment condition
args <- commandArgs(trailingOnly = TRUE)

baseFile=paste('../../processed_data/bout_length_turn_proportion_time_factor_dataset/',args[1], sep="")
file_BoutLength=paste(baseFile,'/All',args[1],'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'/All',args[1],'_TimeFactor', sep="")
file_ScootsProportion=paste(baseFile,'/All',args[1],'_ScootsProportion', sep="")
file_JBendsProportion=paste(baseFile,'/All',args[1],'_JBendsProportion', sep="")
file_CBendsProportion=paste(baseFile,'/All',args[1],'_CBendsProportion', sep="")
file_OBendsProportion=paste(baseFile,'/All',args[1],'_OBendsProportion', sep="")
file_EBendsProportion=paste(baseFile,'/All',args[1],'_EBendsProportion', sep="")
file_GBendsProportion=paste(baseFile,'/All',args[1],'_GBendsProportion', sep="")
file_HBendsProportion=paste(baseFile,'/All',args[1],'_HBendsProportion', sep="")
file_IBendsProportion=paste(baseFile,'/All',args[1],'_IBendsProportion', sep="")


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

#analyze SD for both variables through all 144 subjects and all time
All_BoutLength_SD<-sd(All$BoutLength)
All_ScootsProportion_SD<-sd(All$ScootsProportion)
All_JBendsProportion_SD<-sd(All$JBendsProportion)
All_CBendsProportion_SD<-sd(All$CBendsProportion)
All_OBendsProportion_SD<-sd(All$OBendsProportion)
All_EBendsProportion_SD<-sd(All$EBendsProportion)
All_GBendsProportion_SD<-sd(All$GBendsProportion)
All_HBendsProportion_SD<-sd(All$HBendsProportion)
All_IBendsProportion_SD<-sd(All$IBendsProportion)


resultsFile=paste("../../results/output/data_exploration/output_",args[1], sep="")
write(paste("SD of bout length through all 144 subjects and all time:",sep=""), file = resultsFile, append = FALSE, sep = "\n")
write(All_BoutLength_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of Scoots proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_ScootsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of JBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_JBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of CBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_CBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of OBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_OBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of EBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_EBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of GBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_GBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of HBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_HBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")
write("SD of IBends proportion through all 144 subjects and all time:", file = resultsFile, append = TRUE, sep = "\n")
write(All_IBendsProportion_SD, file = resultsFile, append = TRUE, sep = "\n")


#make histograms for both variables through all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_histogram_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$BoutLength, main=paste("Bout length through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_ScootsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$ScootsProportion, main=paste("Scoots proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_JBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$JBendsProportion, main=paste("JBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_CBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$CBendsProportion, main=paste("CBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_OBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$OBendsProportion, main=paste("OBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_EBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$EBendsProportion, main=paste("EBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_GBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$GBendsProportion, main=paste("GBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_HBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$HBendsProportion, main=paste("HBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()

png(paste("../../results/plots/data_exploration/",args[1],"_histogram_HBendsProportion_AllTime_AllSubjects.png", sep=""),width=1000)
hist(All$HBendsProportion, main=paste("HBends proportion through all time for all 144 subjects in condition ",args[1], sep=""))
dev.off()


#make plot ScootsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_ScootsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$ScootsProportion, main=paste("In ",args[1]," Scoots proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot JBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_JBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$JBendsProportion, main=paste("In ",args[1]," JBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot CBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_CBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$CBendsProportion, main=paste("In ",args[1]," CBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot OBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_OBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$OBendsProportion, main=paste("In ",args[1]," OBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot EBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_EBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$EBendsProportion, main=paste("In ",args[1]," EBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot GBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_GBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$GBendsProportion, main=paste("In ",args[1]," GBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot HBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_HBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$HBendsProportion, main=paste("In ",args[1]," HBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()

#make plot IBendsProportions per BoutLength for all 144 subjects and all time
png(paste("../../results/plots/data_exploration/",args[1],"_plot_IBendsProportion_per_BoutLength_AllTime_AllSubjects.png", sep=""),width=1000)
plot(All$BoutLength, All$IBendsProportion, main=paste("In ",args[1]," IBends proportion per bout length through all time for all 144 subjects", sep=""))
dev.off()



#analyze short bouts

#go through various lengths of short bouts, extract counts per 5 minute time grouping and plot the increase/decrease
shortLengths_vs_timeFactor=matrix(, nrow = 5, ncol = 13)
colors=c("blue","green","red","purple","cyan")
png(paste("../../results/plots/data_exploration/",args[1],"_plot_BoutCounts_per_BoutLength_throughTime_AllSubjects.png", sep=""),width=2000, height=1000)
plot(1:15, c(1:14,max(aggregate(BoutLength~TimeFactor, data=All, function(x){return(length(x[x==2]))}))),type="l", pch=21, col="white")
for (short_length in 2:6){
	for (time_factor in 1:13){ 
		toPrint=strcat("short length ",toString(short_length))
		toPrint=strcat(toPrint," and time factor ")
		toPrint=strcat(toPrint,toString(time_factor))

		write(paste("Bout length ", toString(short_length), " and time factor ", toString(time_factor),sep=""), file = resultsFile, append = TRUE, sep = "\n")

		write(length(All[All$BoutLength==short_length & All$TimeFactor==time_factor,1]), file = resultsFile, append = TRUE, sep = "\n")

		shortLengths_vs_timeFactor[short_length-1,time_factor]=length(All[All$BoutLength==short_length & All$TimeFactor==time_factor,1])

	}


	lines(1:13,shortLengths_vs_timeFactor[short_length-1,], col=colors[short_length-1], type="l", pch=21, bg=colors[short_length-1], ylab="bout length counts", xlab="5 minute recording", main=paste("number of short bouts through the experiment",args[1], sep=""))
	axis(1, at=1:13)
	


}

legend(14,length(All[All$BoutLength==2 & All$TimeFactor==1,1]), paste(rep("bout length", times=5), 2:6), lty=c(1,1,1,1,1), lwd=c(2.5,2.5,2.5,2.5,2.5),col=c("blue","green","red","purple","cyan")) 
dev.off()



#plot the bout length=1 separately since the count is so high
shortLengths_vs_timeFactor=matrix(, nrow = 1, ncol = 13)
colors=c("gray")
png(paste("../../results/plots/data_exploration/",args[1],"_plot_BoutCounts_per_BoutLengthOne_throughTime_AllSubjects.png", sep=""),width=2000, height=1000)
plot(1:15, c(1:14,max(aggregate(BoutLength~TimeFactor, data=All, function(x){return(length(x[x==1]))}))),type="l", pch=21, col="white")

for (time_factor in 1:13){ 
	
	shortLengths_vs_timeFactor[1,time_factor]=length(All[All$BoutLength==1 & All$TimeFactor==time_factor,1])

}


lines(1:13,shortLengths_vs_timeFactor[1,], col=colors[1], type="l", pch=21, bg=colors[1], ylab="bout length counts", xlab="5 minute recording", main=paste("number of short bouts through the experiment",args[1], sep=""))
axis(1, at=1:13)
	


legend(14,length(All[All$BoutLength==1 & All$TimeFactor==1,1]), "bout length 1", lty=c(1), lwd=c(2.5),col=c("gray")) 
dev.off()
