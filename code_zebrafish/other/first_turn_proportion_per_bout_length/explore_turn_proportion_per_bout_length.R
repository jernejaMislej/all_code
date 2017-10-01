#Load packages
library(reshape2)
library(car)
library(pracma)
library(Hmisc)
library(ppcor)

#args is the experiment condition
args <- commandArgs(trailingOnly = TRUE)

baseFile=paste('../../processed_data/bout_length_turn_proportion_time_factor_dataset/',args[1], sep="")
file_BoutLength=paste(baseFile,'/All',args[1],'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'/All',args[1],'_TimeFactor', sep="")

turnTypes=c("Scoots","JBends","CBends","OBends","EBends","GBends","HBends","IBends")

for (turnType in turnTypes) {

	file_TurnProportion=paste(baseFile,'/All',args[1],'_',turnType,'Proportion', sep="")


	#read data for the given turn type
	All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
	All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
	All_TurnProportion <- read.csv(file_TurnProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))

	#melt the datasets
	All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
	All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]
	All_TurnProportion_Melted<-na.omit(melt(t(All_TurnProportion)))[,c(2,3)]

	colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
	colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")
	colnames(All_TurnProportion_Melted)<-c("Subject","TurnProportion")

	All<-cbind(All_BoutLength_Melted$Subject, All_BoutLength_Melted$BoutLength, All_TurnProportion_Melted$TurnProportion, All_TimeFactor_Melted$TimeFactor)

	All[,1]<-factor(All[,1])

	colnames(All)<-c("Subject","BoutLength","TurnProportion","TimeFactor")

	All<-as.data.frame(All)

	All<-All[All$BoutLength!=0,]


	#create datasets with aggregated turn proportion and time factor per bout length and combine them
	new_dataset<-aggregate(TurnProportion~BoutLength,data=All, function(x){return(x)})

	new_dataset<-sapply(new_dataset[,2], '[', seq(max(sapply(new_dataset[,2],length))))

	colnames(new_dataset)<-unique(sort(All$BoutLength))

	new_dataset<-na.omit(melt(new_dataset))

	new_dataset<-new_dataset[,-1]


	new_dataset2<-aggregate(TimeFactor~BoutLength,data=All, function(x){return(x)})

	new_dataset2<-sapply(new_dataset2[,2], '[', seq(max(sapply(new_dataset2[,2],length))))

	colnames(new_dataset2)<-unique(sort(All$BoutLength))

	new_dataset2<-na.omit(melt(new_dataset2))[,3]


	new_dataset_melted<-cbind(new_dataset,new_dataset2)

	colnames(new_dataset_melted)<-c("BoutLength","TurnProportion","TimeFactor")

	#> head(new_dataset_melted)
  	#	BoutLength TurnProportion TimeFactor
	#1          1              1          1
	#2          1              0          1
	#3          1              0          1




	#extract the bouts count for partial correlation.....per time frame the count of each bout length
	frequency_data<-aggregate(TimeFactor~BoutLength, data=new_dataset_melted, function(x){return(list(aggregate(x,by=list(x),FUN=length)))})



	#analyze for each bout length per time frame and adjust for the frequency per time frame
	bout_length_count<-0
	
	write.table(" ", file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Proportion_per_BoutLength_dataset',sep=""), append = FALSE, row.names=F,col.names=F, sep = "\n")
	write(" ", file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Correlate_Proportion_per_BoutLength_and_per_TimeFrame_output',sep=""), append=FALSE, sep="\n")

	for (bout_length in unique(sort(All$BoutLength))){

		#save boxplot of turn proportions through time for each bout length
		png(paste('../../results/plots/first_turn_proportion_per_bout_length/',args[1],"_",turnType,'Proportion_per_BoutLength_',toString(bout_length),'_boxplot.png', sep=""),width=1000)
		boxplot(new_dataset_melted[new_dataset_melted[,1]==bout_length,2]~new_dataset_melted[new_dataset_melted[,1]==bout_length,3], main=paste(turnType, " proportions per bout length ", toString(bout_length)," per time frame", sep=""))
		dev.off()


		#generate aggregate dataset with means per time frame per bout length
		bout_length_count<-bout_length_count+1

		turn_proportion_per_bout_length<-cbind(aggregate(TurnProportion~TimeFactor,data=new_dataset_melted[new_dataset_melted[,1]==bout_length,],FUN=mean),frequency_data[[2]][[bout_length_count]][,2])

		colnames(turn_proportion_per_bout_length)<-c("TimeFactor","TurnProportion","Count")


		#save dataset 
		write.table(paste("Bout Length ",toString(bout_length),sep=" "), file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Proportion_per_BoutLength_dataset',sep=""), row.names=F,col.names=F, append = TRUE, sep = ",")
		write.table(as.data.frame(do.call(cbind, turn_proportion_per_bout_length)), file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Proportion_per_BoutLength_dataset',sep=""), row.names=F,col.names=T, append = TRUE, sep=",")
		write.table(" ", file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Proportion_per_BoutLength_dataset',sep=""), row.names=F,col.names=F, append = TRUE, sep=",")


		#save plot of means through time
		png(paste('../../results/plots/first_turn_proportion_per_bout_length/',args[1],"_",turnType,'Proportion_per_BoutLength_',toString(bout_length),'_plot.png', sep=""),width=1000)
		plot(turn_proportion_per_bout_length$TimeFactor, turn_proportion_per_bout_length$TurnProportion, main=paste(turnType, " proportions per bout length ", toString(bout_length)," per time frame", sep=""))
		dev.off()


		#if enough time frames present correlate
		if(length(turn_proportion_per_bout_length[,1])>2){	

			partial_correlation<-pcor.test(turn_proportion_per_bout_length$TurnProportion, turn_proportion_per_bout_length$TimeFactor, turn_proportion_per_bout_length$Count, method="spearman")
			if(partial_correlation$p.value<0.05 & !is.na(partial_correlation$p.value)){
				message(turn_proportion_per_bout_length)
				message(bout_length)
				message(" ")
				message(partial_correlation$estimate)
				message(partial_correlation$p.value)
				message(" ")
				message(" ")
				message(" ")

				write(paste("Bout Length ",toString(bout_length),sep=" "), file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Correlate_Proportion_per_BoutLength_and_per_TimeFrame_output',sep=""), append=TRUE, sep="\n")	
				write(partial_correlation$estimate, file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Correlate_Proportion_per_BoutLength_and_per_TimeFrame_output',sep=""), append=TRUE, sep="\n")
				write(partial_correlation$p.value, file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Correlate_Proportion_per_BoutLength_and_per_TimeFrame_output',sep=""), append=TRUE, sep="\n")
				write(" ", file=paste("../../results/output/first_turn_proportion_per_bout_length/",args[1],"_",turnType,'_Correlate_Proportion_per_BoutLength_and_per_TimeFrame_output',sep=""), append=TRUE, sep="\n")
			}
		
		}

	}
}
