library(lme4)
library(MASS)


#load data

files<-list.files()
all_dataset<-c()
subjects_nr<-0
condition<-substr(list.files()[1],nchar(list.files()[1])-19,nchar(list.files()[1])-16)#adjust for each of 6 conditions!!!
for (fil in files){
	
	dataset<-read.table(fil)
	
	#add identifier
	dataset$Group<-rep(substr(substr(fil,9,100),1,nchar(fil)-29),times=length(dataset[,1]))
	
	#increase subject to ensure unique subjects per group
	dataset[,1]<-dataset[,1]+subjects_nr
	
	all_dataset<-rbind(all_dataset,dataset)
	
	subjects_nr<-max(dataset[,1])
}

#rearrange and rename
colnames(all_dataset)[1:30]<-c("Subject","AllBoutCount","TimeFactor","Length1BoutCount","Length2BoutCount","Length3BoutCount",
		"Length1Scoots","Length1JBends","Length1CBends","Length1OBends","Length1EBends","Length1GBends","Length1HBends","Length1IBends",
		"Length2Scoots","Length2JBends","Length2CBends","Length2OBends","Length2EBends","Length2GBends","Length2HBends","Length2IBends",
		"Length3Scoots","Length3JBends","Length3CBends","Length3OBends","Length3EBends","Length3GBends","Length3HBends","Length3IBends")

#increase by one to avoid zeros
all_dataset$AllBoutCount<-all_dataset$AllBoutCount+1
all_dataset$Length1BoutCount<-all_dataset$Length1BoutCount+1
all_dataset$Length2BoutCount<-all_dataset$Length2BoutCount+1
all_dataset$Length3BoutCount<-all_dataset$Length3BoutCount+1

#extract permille
all_dataset$Length1BoutCountPerMille<-round(all_dataset$Length1BoutCount*1000/all_dataset$AllBoutCount)
all_dataset$Length2BoutCountPerMille<-round(all_dataset$Length2BoutCount*1000/all_dataset$AllBoutCount)
all_dataset$Length3BoutCountPerMille<-round(all_dataset$Length3BoutCount*1000/all_dataset$AllBoutCount)

#rearrange
all_dataset<-all_dataset[,c(31,1,3,2,4,5,6,32,33,34,7:30)]

#make Subject and Group factor
all_dataset$Subject<-as.factor(all_dataset$Subject)


all_dataset$Group<-as.factor(all_dataset$Group)


#all_dataset$Condition<-as.factor(all_dataset$Condition)#drug and group are merged into group
#all_dataset$Drug<-as.factor(all_dataset$Drug)

#make control as the reference of all
contrasts(all_dataset$Group) <- contr.treatment(levels(all_dataset$Group),base=which(levels(all_dataset$Group) == 'Control'))

#start with calculating the mean through subjects in the time frame and plot

#get the means
all_dataset_means<-aggregate(all_dataset[,-2],by=list(all_dataset$TimeFactor,all_dataset$Group),FUN=mean,na.rm=T)[,-c(1,3)]

colnames(all_dataset_means)[1]<-"Group"

#dont need to standardize permilles...
#standardize
#all_dataset_means<-cbind(all_dataset_means[,c(1,2)],do.call(rbind, by(all_dataset_means[,c(3:33)],all_dataset_means$Group,function(x) scale(x))))

attach(all_dataset_means)

#mean total bout count
for(group in levels(Group)){

	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
					,"gray40","gray35","gray30")
			
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Mean_total_bout_count_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",3],all_dataset_means[Group==group,3],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",3]),min(all_dataset_means[Group==group,3])),
			max(max(all_dataset_means[Group=="Control",3]),max(all_dataset_means[Group==group,3]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",3]),min(all_dataset_means[Group==group,3])),
			max(max(all_dataset_means[Group=="Control",3]),max(all_dataset_means[Group==group,3]))),
			xlab="Control",ylab=group,main="Mean total bout count, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",3]),min(all_dataset_means[Group==group,3])),
			max(max(all_dataset_means[Group=="Control",3]),max(all_dataset_means[Group==group,3]))),
			c(min(min(all_dataset_means[Group=="Control",3]),min(all_dataset_means[Group==group,3])),
			max(max(all_dataset_means[Group=="Control",3]),max(all_dataset_means[Group==group,3]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",3]),min(all_dataset_means[Group==group,3])), 
			max(max(all_dataset_means[Group=="Control",3]),max(all_dataset_means[Group==group,3])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}
	
	
	
#Length1BoutCountPerMille
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length1PerMille_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",7],all_dataset_means[Group==group,7],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",7]),min(all_dataset_means[Group==group,7])),
					max(max(all_dataset_means[Group=="Control",7]),max(all_dataset_means[Group==group,7]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",7]),min(all_dataset_means[Group==group,7])),
					max(max(all_dataset_means[Group=="Control",7]),max(all_dataset_means[Group==group,7]))),
			xlab="Control",ylab=group,main="Mean permille of lenght1 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",7]),min(all_dataset_means[Group==group,7])),
					max(max(all_dataset_means[Group=="Control",7]),max(all_dataset_means[Group==group,7]))),
			c(min(min(all_dataset_means[Group=="Control",7]),min(all_dataset_means[Group==group,7])),
					max(max(all_dataset_means[Group=="Control",7]),max(all_dataset_means[Group==group,7]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",7]),min(all_dataset_means[Group==group,7])), 
			max(max(all_dataset_means[Group=="Control",7]),max(all_dataset_means[Group==group,7])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length2BoutCountPerMille
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length2PerMille_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",8],all_dataset_means[Group==group,8],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",8]),min(all_dataset_means[Group==group,8])),
					max(max(all_dataset_means[Group=="Control",8]),max(all_dataset_means[Group==group,8]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",8]),min(all_dataset_means[Group==group,8])),
					max(max(all_dataset_means[Group=="Control",8]),max(all_dataset_means[Group==group,8]))),
			xlab="Control",ylab=group,main="Mean permille of lenght2 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",8]),min(all_dataset_means[Group==group,8])),
					max(max(all_dataset_means[Group=="Control",8]),max(all_dataset_means[Group==group,8]))),
			c(min(min(all_dataset_means[Group=="Control",8]),min(all_dataset_means[Group==group,8])),
					max(max(all_dataset_means[Group=="Control",8]),max(all_dataset_means[Group==group,8]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",8]),min(all_dataset_means[Group==group,8])), 
			max(max(all_dataset_means[Group=="Control",8]),max(all_dataset_means[Group==group,8])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length3BoutCountPerMille
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length3PerMille_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",9],all_dataset_means[Group==group,9],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",9]),min(all_dataset_means[Group==group,9])),
					max(max(all_dataset_means[Group=="Control",9]),max(all_dataset_means[Group==group,9]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",9]),min(all_dataset_means[Group==group,9])),
					max(max(all_dataset_means[Group=="Control",9]),max(all_dataset_means[Group==group,9]))),
			xlab="Control",ylab=group,main="Mean permille of lenght3 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",9]),min(all_dataset_means[Group==group,9])),
					max(max(all_dataset_means[Group=="Control",9]),max(all_dataset_means[Group==group,9]))),
			c(min(min(all_dataset_means[Group=="Control",9]),min(all_dataset_means[Group==group,9])),
					max(max(all_dataset_means[Group=="Control",9]),max(all_dataset_means[Group==group,9]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",9]),min(all_dataset_means[Group==group,9])), 
			max(max(all_dataset_means[Group=="Control",9]),max(all_dataset_means[Group==group,9])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length1 Scoots proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length1Scoots_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",10],all_dataset_means[Group==group,10],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",10]),min(all_dataset_means[Group==group,10])),
					max(max(all_dataset_means[Group=="Control",10]),max(all_dataset_means[Group==group,10]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",10]),min(all_dataset_means[Group==group,10])),
					max(max(all_dataset_means[Group=="Control",10]),max(all_dataset_means[Group==group,10]))),
			xlab="Control",ylab=group,main="Mean proportion of scoots in lenght1 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",10]),min(all_dataset_means[Group==group,10])),
					max(max(all_dataset_means[Group=="Control",10]),max(all_dataset_means[Group==group,10]))),
			c(min(min(all_dataset_means[Group=="Control",10]),min(all_dataset_means[Group==group,10])),
					max(max(all_dataset_means[Group=="Control",10]),max(all_dataset_means[Group==group,10]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",10]),min(all_dataset_means[Group==group,10])), 
			max(max(all_dataset_means[Group=="Control",10]),max(all_dataset_means[Group==group,10])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}


#Length1 JBends proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length1JBends_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",11],all_dataset_means[Group==group,11],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",11]),min(all_dataset_means[Group==group,11])),
					max(max(all_dataset_means[Group=="Control",11]),max(all_dataset_means[Group==group,11]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",11]),min(all_dataset_means[Group==group,11])),
					max(max(all_dataset_means[Group=="Control",11]),max(all_dataset_means[Group==group,11]))),
			xlab="Control",ylab=group,main="Mean proportion of Jbends in lenght1 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",11]),min(all_dataset_means[Group==group,11])),
					max(max(all_dataset_means[Group=="Control",11]),max(all_dataset_means[Group==group,11]))),
			c(min(min(all_dataset_means[Group=="Control",11]),min(all_dataset_means[Group==group,11])),
					max(max(all_dataset_means[Group=="Control",11]),max(all_dataset_means[Group==group,11]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",11]),min(all_dataset_means[Group==group,11])), 
			max(max(all_dataset_means[Group=="Control",11]),max(all_dataset_means[Group==group,11])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length1 CBends proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length1CBends_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",12],all_dataset_means[Group==group,12],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",12]),min(all_dataset_means[Group==group,12])),
					max(max(all_dataset_means[Group=="Control",12]),max(all_dataset_means[Group==group,12]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",12]),min(all_dataset_means[Group==group,12])),
					max(max(all_dataset_means[Group=="Control",12]),max(all_dataset_means[Group==group,12]))),
			xlab="Control",ylab=group,main="Mean proportion of Cbends in lenght1 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",12]),min(all_dataset_means[Group==group,12])),
					max(max(all_dataset_means[Group=="Control",12]),max(all_dataset_means[Group==group,12]))),
			c(min(min(all_dataset_means[Group=="Control",12]),min(all_dataset_means[Group==group,12])),
					max(max(all_dataset_means[Group=="Control",12]),max(all_dataset_means[Group==group,12]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",12]),min(all_dataset_means[Group==group,12])), 
			max(max(all_dataset_means[Group=="Control",12]),max(all_dataset_means[Group==group,12])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}


#Length1 OBends proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length1OBends_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",13],all_dataset_means[Group==group,13],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",13]),min(all_dataset_means[Group==group,13])),
					max(max(all_dataset_means[Group=="Control",13]),max(all_dataset_means[Group==group,13]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",13]),min(all_dataset_means[Group==group,13])),
					max(max(all_dataset_means[Group=="Control",13]),max(all_dataset_means[Group==group,13]))),
			xlab="Control",ylab=group,main="Mean proportion of Obends in lenght1 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",13]),min(all_dataset_means[Group==group,13])),
					max(max(all_dataset_means[Group=="Control",13]),max(all_dataset_means[Group==group,13]))),
			c(min(min(all_dataset_means[Group=="Control",13]),min(all_dataset_means[Group==group,13])),
					max(max(all_dataset_means[Group=="Control",13]),max(all_dataset_means[Group==group,13]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",13]),min(all_dataset_means[Group==group,13])), 
			max(max(all_dataset_means[Group=="Control",13]),max(all_dataset_means[Group==group,13])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length1 GBends proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length1GBends_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",15],all_dataset_means[Group==group,15],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",15]),min(all_dataset_means[Group==group,15])),
					max(max(all_dataset_means[Group=="Control",15]),max(all_dataset_means[Group==group,15]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",15]),min(all_dataset_means[Group==group,15])),
					max(max(all_dataset_means[Group=="Control",15]),max(all_dataset_means[Group==group,15]))),
			xlab="Control",ylab=group,main="Mean proportion of Gbends in lenght1 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",15]),min(all_dataset_means[Group==group,15])),
					max(max(all_dataset_means[Group=="Control",15]),max(all_dataset_means[Group==group,15]))),
			c(min(min(all_dataset_means[Group=="Control",15]),min(all_dataset_means[Group==group,15])),
					max(max(all_dataset_means[Group=="Control",15]),max(all_dataset_means[Group==group,15]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",15]),min(all_dataset_means[Group==group,15])), 
			max(max(all_dataset_means[Group=="Control",15]),max(all_dataset_means[Group==group,15])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length2 Scoots proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length2Scoots_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",18],all_dataset_means[Group==group,18],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",18]),min(all_dataset_means[Group==group,18])),
					max(max(all_dataset_means[Group=="Control",18]),max(all_dataset_means[Group==group,18]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",18]),min(all_dataset_means[Group==group,18])),
					max(max(all_dataset_means[Group=="Control",18]),max(all_dataset_means[Group==group,18]))),
			xlab="Control",ylab=group,main="Mean proportion of scoots in lenght2 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",18]),min(all_dataset_means[Group==group,18])),
					max(max(all_dataset_means[Group=="Control",18]),max(all_dataset_means[Group==group,18]))),
			c(min(min(all_dataset_means[Group=="Control",18]),min(all_dataset_means[Group==group,18])),
					max(max(all_dataset_means[Group=="Control",18]),max(all_dataset_means[Group==group,18]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",18]),min(all_dataset_means[Group==group,18])), 
			max(max(all_dataset_means[Group=="Control",18]),max(all_dataset_means[Group==group,18])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length2 CBends proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length2CBends_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",20],all_dataset_means[Group==group,20],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",20]),min(all_dataset_means[Group==group,20])),
					max(max(all_dataset_means[Group=="Control",20]),max(all_dataset_means[Group==group,20]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",20]),min(all_dataset_means[Group==group,20])),
					max(max(all_dataset_means[Group=="Control",20]),max(all_dataset_means[Group==group,20]))),
			xlab="Control",ylab=group,main="Mean proportion of Cbends in lenght2 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",20]),min(all_dataset_means[Group==group,20])),
					max(max(all_dataset_means[Group=="Control",20]),max(all_dataset_means[Group==group,20]))),
			c(min(min(all_dataset_means[Group=="Control",20]),min(all_dataset_means[Group==group,20])),
					max(max(all_dataset_means[Group=="Control",20]),max(all_dataset_means[Group==group,20]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",20]),min(all_dataset_means[Group==group,20])), 
			max(max(all_dataset_means[Group=="Control",20]),max(all_dataset_means[Group==group,20])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}

#Length3 CBends proportion
for(group in levels(Group)){
	
	#plot
	colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
			,"gray40","gray35","gray30")
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Length3CBends_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
	
	plot(all_dataset_means[Group=="Control",28],all_dataset_means[Group==group,28],cex=1.6,col=colrs,pch=19,
			xlim=c(min(min(all_dataset_means[Group=="Control",28]),min(all_dataset_means[Group==group,28])),
					max(max(all_dataset_means[Group=="Control",28]),max(all_dataset_means[Group==group,28]))),
			ylim=c(min(min(all_dataset_means[Group=="Control",28]),min(all_dataset_means[Group==group,28])),
					max(max(all_dataset_means[Group=="Control",28]),max(all_dataset_means[Group==group,28]))),
			xlab="Control",ylab=group,main="Mean proportion of Cbends in lenght3 bouts, control vs drug")
	
	lines(c(min(min(all_dataset_means[Group=="Control",28]),min(all_dataset_means[Group==group,28])),
					max(max(all_dataset_means[Group=="Control",28]),max(all_dataset_means[Group==group,28]))),
			c(min(min(all_dataset_means[Group=="Control",28]),min(all_dataset_means[Group==group,28])),
					max(max(all_dataset_means[Group=="Control",28]),max(all_dataset_means[Group==group,28]))),type="l")
	
	legend(min(min(all_dataset_means[Group=="Control",28]),min(all_dataset_means[Group==group,28])), 
			max(max(all_dataset_means[Group=="Control",28]),max(all_dataset_means[Group==group,28])), 
			c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
	
	dev.off()
}


#finish with plotting means over 12 subjects

#take all subject data and do analysis
detach(all_dataset_means)

#check the model with poisson, cant really do that due to overdispersion
#model_poisson<-glmer(BoutCount~TimeFactor+Group+TimeFactor*Group+(1|Subject/TimeFactor), family = poisson(link = "log"))
#summary(model_possion)

#check if there is a linear relationship between the mean and var for the dependant variable in all groups
for(grp in levels(Group)){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Mean_vs_Var_", grp,"_",condition,".png"),width=1500,height=750)
	plot(aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=mean)[,2]~aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=var)[,2],
			xlab="Var",ylab="Mean")
	dev.off()
	
	message(grp)
	message(summary(lm(aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=mean)[,2]~aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=var)[,2]))$coefficients[8])
	
}

#check if there is a quadratic relationship between the mean and var for the dependant variable in all groups

for(grp in levels(Group)){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Mean_sqred_vs_Var_", grp,"_",condition,".png"),width=1500,height=750)
	plot(((aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=mean)[,2])*(aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=mean)[,2]))~aggregate(BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=var)[,2],
			xlab="Var",ylab="Mean")
	dev.off()
	
	message(grp)
	message(summary(lm(((aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=mean)[,2])*(aggregate(Length1BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=mean)[,2]))~aggregate(BoutCount~TimeFactor,data=all_dataset[Group==grp,],FUN=var)[,2]))$coefficients[8])
	
}


#first model the total bout counts with time

#negative binomial
model_nb_theta<-summary(glm.nb(AllBoutCount~TimeFactor+Group+TimeFactor*Group))[[18]]#estimate theta

model_nb<-glmer(AllBoutCount~TimeFactor+Group+TimeFactor*Group+(1|Subject), family = negative.binomial(model_nb_theta))

summary(model_nb)

#quasipoisson
model_quasipoisson<-glmmPQL(AllBoutCount~TimeFactor+Group+TimeFactor*Group, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)

#save coefficients, set non-significant to 0
model_quasipoisson_coeff<-summary(model_quasipoisson)$tTable[-1,1]
model_quasipoisson_coeff[summary(model_quasipoisson)$tTable[-1,5]>=0.05]<-0

#plot for all groups(control + 36)	

png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Control_vs_drugs_total_bout_count_",condition,".png"),width=1500,height=750)

plot(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01),
		rep(0,times=length(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01))),
		xlim=c(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3),
		ylim=c(min(model_quasipoisson_coeff[39:(37+36)])-0.3,max(model_quasipoisson_coeff[39:(37+36)])+0.3),
		ylab="change in time",xlab="overall change",type="l",
		main="regression coefficient of total bout count,\n relative to control")
lines(rep(0,times=length(seq(min(model_quasipoisson_coeff[39:(37+36)])-0.3,max(model_quasipoisson_coeff[39:(37+36)])+0.3,0.01))),
		seq(min(model_quasipoisson_coeff[39:(37+36)])-0.3,max(model_quasipoisson_coeff[39:(37+36)])+0.3,0.01))

points(model_quasipoisson_coeff[2:4],model_quasipoisson_coeff[(2+36):(4+36)], cex=2.1, pch=c(15,17,19),col="royalblue4")#Aripiprazole
points(model_quasipoisson_coeff[5:7],model_quasipoisson_coeff[(5+36):(7+36)], cex=2.1, pch=c(15,17,19),col="seagreen4")#Cariprazine
points(model_quasipoisson_coeff[8:10],model_quasipoisson_coeff[(8+36):(10+36)], cex=2.1, pch=c(15,17,19),col="saddlebrown")#Clozapine
points(model_quasipoisson_coeff[11:13],model_quasipoisson_coeff[(11+36):(13+36)], cex=2.1, pch=c(15,17,19),col="red")#CNO
points(model_quasipoisson_coeff[14:16],model_quasipoisson_coeff[(14+36):(16+36)], cex=2.1, pch=c(15,17,19),col="purple1")#Haloperidol
points(model_quasipoisson_coeff[17:19],model_quasipoisson_coeff[(17+36):(19+36)], cex=2.1, pch=c(15,17,19),col="turquoise2")#NDMC
points(model_quasipoisson_coeff[20:22],model_quasipoisson_coeff[(20+36):(22+36)], cex=2.1, pch=c(15,17,19),col="chartreuse")#NDMCHigh
points(model_quasipoisson_coeff[23:25],model_quasipoisson_coeff[(23+36):(25+36)], cex=2.1, pch=c(15,17,19),col="deeppink")#OSU6162
points(model_quasipoisson_coeff[26:28],model_quasipoisson_coeff[(26+36):(28+36)], cex=2.1, pch=c(15,17,19),col="orange1")#PCAP1
points(model_quasipoisson_coeff[29:31],model_quasipoisson_coeff[(29+36):(31+36)], cex=2.1, pch=c(15,17,19),col="yellow1")#PCAP2
points(model_quasipoisson_coeff[32:34],model_quasipoisson_coeff[(32+36):(34+36)], cex=2.1, pch=c(15,17,19),col="paleturquoise1")#PCAP814
points(model_quasipoisson_coeff[35:37],model_quasipoisson_coeff[(35+36):(37+36)], cex=2.1, pch=c(15,17,19),col="lightpink")#PCAP931
points(0,0,pch=19,cex=2.1,col="black")

legend(min(model_quasipoisson_coeff[2:37])-0.3, max(model_quasipoisson_coeff[39:(37+36)])+0.3, 
		c("Control","Aripiprazole","Cariprazine","Clozapine","CNO","Haloperidol",
				"NDMC","NDMCHigh","OSU6162","PCAP1","PCAP2", "PCAP814","PCAP931", "1 microM","3 microM","10 microM"), cex=1, 
		col=c("black","royalblue4","seagreen4","saddlebrown","red","purple1","turquoise2", "chartreuse", "deeppink", "orange1", 
				"yellow1", "paleturquoise1", "lightpink","gray","gray","gray"), pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19,2,1,0))


dev.off()

#then look if the bout count of short bouts will change in time inside the total increase/decrease of bout count

#permille are not always normally distributed, so treated as counts


#length 1

#negative binomial
model_nb_theta<-summary(glm.nb(Length1BoutCountPerMille~TimeFactor+Group+TimeFactor*Group))[[18]]#estimate theta

model_nb<-glmer(Length1BoutCountPerMille~TimeFactor+Group+TimeFactor*Group+(1|Subject), family = negative.binomial(model_nb_theta))

summary(model_nb)

#quasipoisson
model_quasipoisson<-glmmPQL(Length1BoutCountPerMille~TimeFactor+Group+TimeFactor*Group, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)

#save coefficients, set non-significant to 0
model_quasipoisson_coeff<-summary(model_quasipoisson)$tTable[-1,1]
model_quasipoisson_coeff[summary(model_quasipoisson)$tTable[-1,5]>=0.05]<-0

#plot for all groups(control + 36)
png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Control_vs_drugs_permille_length1_",condition,".png"),width=1500,height=750)

plot(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01),
		rep(0,times=length(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01))),
		xlim=c(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3),
		ylim=c(min(model_quasipoisson_coeff[39:(37+36)])-0.3,max(model_quasipoisson_coeff[39:(37+36)])+0.3),
		ylab="change in time",xlab="overall change",type="l",
		main="regression coefficient of permille of length 1 bouts,\n relative to control")
lines(rep(0,times=length(seq(min(model_quasipoisson_coeff[39:(37+36)])-0.3,max(model_quasipoisson_coeff[39:(37+36)])+0.3,0.01))),
		seq(min(model_quasipoisson_coeff[39:(37+36)])-0.3,max(model_quasipoisson_coeff[39:(37+36)])+0.3,0.01))

points(model_quasipoisson_coeff[2:4],model_quasipoisson_coeff[(2+36):(4+36)], cex=2.1, pch=c(15,17,19),col="royalblue4")#Aripiprazole
points(model_quasipoisson_coeff[5:7],model_quasipoisson_coeff[(5+36):(7+36)], cex=2.1, pch=c(15,17,19),col="seagreen4")#Cariprazine
points(model_quasipoisson_coeff[8:10],model_quasipoisson_coeff[(8+36):(10+36)], cex=2.1, pch=c(15,17,19),col="saddlebrown")#Clozapine
points(model_quasipoisson_coeff[11:13],model_quasipoisson_coeff[(11+36):(13+36)], cex=2.1, pch=c(15,17,19),col="red")#CNO
points(model_quasipoisson_coeff[14:16],model_quasipoisson_coeff[(14+36):(16+36)], cex=2.1, pch=c(15,17,19),col="purple1")#Haloperidol
points(model_quasipoisson_coeff[17:19],model_quasipoisson_coeff[(17+36):(19+36)], cex=2.1, pch=c(15,17,19),col="turquoise2")#NDMC
points(model_quasipoisson_coeff[20:22],model_quasipoisson_coeff[(20+36):(22+36)], cex=2.1, pch=c(15,17,19),col="chartreuse")#NDMCHigh
points(model_quasipoisson_coeff[23:25],model_quasipoisson_coeff[(23+36):(25+36)], cex=2.1, pch=c(15,17,19),col="deeppink")#OSU6162
points(model_quasipoisson_coeff[26:28],model_quasipoisson_coeff[(26+36):(28+36)], cex=2.1, pch=c(15,17,19),col="orange1")#PCAP1
points(model_quasipoisson_coeff[29:31],model_quasipoisson_coeff[(29+36):(31+36)], cex=2.1, pch=c(15,17,19),col="yellow1")#PCAP2
points(model_quasipoisson_coeff[32:34],model_quasipoisson_coeff[(32+36):(34+36)], cex=2.1, pch=c(15,17,19),col="paleturquoise1")#PCAP814
points(model_quasipoisson_coeff[35:37],model_quasipoisson_coeff[(35+36):(37+36)], cex=2.1, pch=c(15,17,19),col="lightpink")#PCAP931
points(0,0,pch=19,cex=2.1,col="black")

legend(min(model_quasipoisson_coeff[2:37])-0.3, max(model_quasipoisson_coeff[39:(37+36)])+0.3, 
		c("Control","Aripiprazole","Cariprazine","Clozapine","CNO","Haloperidol",
				"NDMC","NDMCHigh","OSU6162","PCAP1","PCAP2", "PCAP814","PCAP931", "1 microM","3 microM","10 microM"), cex=1, 
		col=c("black","royalblue4","seagreen4","saddlebrown","red","purple1","turquoise2", "chartreuse", "deeppink", "orange1", 
				"yellow1", "paleturquoise1", "lightpink","gray","gray","gray"), pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19,2,1,0))


dev.off()
#length 2

#negative binomial
model_nb_theta<-summary(glm.nb(Length2BoutCountPerMille~TimeFactor+Group+TimeFactor*Group))[[18]]#estimate theta

model_nb<-glmer(Length2BoutCountPerMille~TimeFactor+Group+TimeFactor*Group+(1|Subject), family = negative.binomial(model_nb_theta))

summary(model_nb)

#quasipoisson
model_quasipoisson<-glmmPQL(Length1BoutCountPerMille~TimeFactor+Group+TimeFactor*Group, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)


#length 3

#negative binomial
model_nb_theta<-summary(glm.nb(Length3BoutCountPerMille~TimeFactor+Group+TimeFactor*Group))[[18]]#estimate theta

model_nb<-glmer(Length3BoutCountPerMille~TimeFactor+Group+TimeFactor*Group+(1|Subject), family = negative.binomial(model_nb_theta))

summary(model_nb)

#quasipoisson
model_quasipoisson<-glmmPQL(Length3BoutCountPerMille~TimeFactor+Group+TimeFactor*Group, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)


#then look if there is a change in the mean turn adjusting for the total bout count of that length

#turn permille are not normally distrubted, so treated as counts

#not sure if adjusting for the total count of that length is enough, or need to adjust for the time influence on the total count of that 
#length or even group


#length 1

#Scoots

#negative binomial
model_nb_theta<-summary(glm.nb(round(Length1Scoots*1000)~TimeFactor+Group+TimeFactor*Group+Length1BoutCount))[[19]]#estimate theta

model_nb<-glmer(round(Length1Scoots*1000)~TimeFactor+Group+TimeFactor*Group+(1|Subject)+Length1BoutCount, family = negative.binomial(model_nb_theta))

summary(model_nb)

#quasipoisson
model_quasipoisson<-glmmPQL(round(Length1Scoots*1000)~TimeFactor+Group+TimeFactor*Group+Length1BoutCount, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)$tTable[,c(1,5)]

#save coefficients, set non-significant to 0
model_quasipoisson_coeff<-summary(model_quasipoisson)$tTable[-1,1]
model_quasipoisson_coeff[summary(model_quasipoisson)$tTable[-1,5]>=0.05]<-0

png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Control_vs_drugs_length1Scoots_",condition,".png"),width=1500,height=750)

plot(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01),
		rep(0,times=length(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01))),
		xlim=c(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3),
		ylim=c(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3),
		ylab="change in time",xlab="overall change",type="l",
		main="regression coefficient of Scoots proportion\n in length1 bouts, relative to control")
lines(rep(0,times=length(seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))),
		seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))

points(model_quasipoisson_coeff[2:4],model_quasipoisson_coeff[(2+37):(4+37)], cex=2.1, pch=c(15,17,19),col="royalblue4")#Aripiprazole
points(model_quasipoisson_coeff[5:7],model_quasipoisson_coeff[(5+37):(7+37)], cex=2.1, pch=c(15,17,19),col="seagreen4")#Cariprazine
points(model_quasipoisson_coeff[8:10],model_quasipoisson_coeff[(8+37):(10+37)], cex=2.1, pch=c(15,17,19),col="saddlebrown")#Clozapine
points(model_quasipoisson_coeff[11:13],model_quasipoisson_coeff[(11+37):(13+37)], cex=2.1, pch=c(15,17,19),col="red")#CNO
points(model_quasipoisson_coeff[14:16],model_quasipoisson_coeff[(14+37):(16+37)], cex=2.1, pch=c(15,17,19),col="purple1")#Haloperidol
points(model_quasipoisson_coeff[17:19],model_quasipoisson_coeff[(17+37):(19+37)], cex=2.1, pch=c(15,17,19),col="turquoise2")#NDMC
points(model_quasipoisson_coeff[20:22],model_quasipoisson_coeff[(20+37):(22+37)], cex=2.1, pch=c(15,17,19),col="chartreuse")#NDMCHigh
points(model_quasipoisson_coeff[23:25],model_quasipoisson_coeff[(23+37):(25+37)], cex=2.1, pch=c(15,17,19),col="deeppink")#OSU6162
points(model_quasipoisson_coeff[26:28],model_quasipoisson_coeff[(26+37):(28+37)], cex=2.1, pch=c(15,17,19),col="orange1")#PCAP1
points(model_quasipoisson_coeff[29:31],model_quasipoisson_coeff[(29+37):(31+37)], cex=2.1, pch=c(15,17,19),col="yellow1")#PCAP2
points(model_quasipoisson_coeff[32:34],model_quasipoisson_coeff[(32+37):(34+37)], cex=2.1, pch=c(15,17,19),col="paleturquoise1")#PCAP814
points(model_quasipoisson_coeff[35:37],model_quasipoisson_coeff[(35+37):(37+37)], cex=2.1, pch=c(15,17,19),col="lightpink")#PCAP931
points(0,0,pch=19,cex=2.1,col="black")

legend(min(model_quasipoisson_coeff[2:37])-0.3, max(model_quasipoisson_coeff[39:(37+37)])+0.3, 
		c("Control","Aripiprazole","Cariprazine","Clozapine","CNO","Haloperidol",
				"NDMC","NDMCHigh","OSU6162","PCAP1","PCAP2", "PCAP814","PCAP931", "1 microM","3 microM","10 microM"), cex=1, 
		col=c("black","royalblue4","seagreen4","saddlebrown","red","purple1","turquoise2", "chartreuse", "deeppink", "orange1", 
				"yellow1", "paleturquoise1", "lightpink","gray","gray","gray"), pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19,2,1,0))


dev.off()



#CBends

#quasipoisson
model_quasipoisson<-glmmPQL(round(Length1CBends*1000)~TimeFactor+Group+TimeFactor*Group+Length1BoutCount, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)$tTable[,c(1,5)]


#save coefficients, set non-significant to 0
model_quasipoisson_coeff<-summary(model_quasipoisson)$tTable[-1,1]
model_quasipoisson_coeff[summary(model_quasipoisson)$tTable[-1,5]>=0.05]<-0

png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Control_vs_drugs_length1CBends_",condition,".png"),width=1500,height=750)

plot(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01),
		rep(0,times=length(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01))),
		xlim=c(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3),
		ylim=c(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3),
		ylab="change in time",xlab="overall change",type="l",
		main="regression coefficient of CBends proportion\n in length1 bouts, relative to control")
lines(rep(0,times=length(seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))),
		seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))

points(model_quasipoisson_coeff[2:4],model_quasipoisson_coeff[(2+37):(4+37)], cex=2.1, pch=c(15,17,19),col="royalblue4")#Aripiprazole
points(model_quasipoisson_coeff[5:7],model_quasipoisson_coeff[(5+37):(7+37)], cex=2.1, pch=c(15,17,19),col="seagreen4")#Cariprazine
points(model_quasipoisson_coeff[8:10],model_quasipoisson_coeff[(8+37):(10+37)], cex=2.1, pch=c(15,17,19),col="saddlebrown")#Clozapine
points(model_quasipoisson_coeff[11:13],model_quasipoisson_coeff[(11+37):(13+37)], cex=2.1, pch=c(15,17,19),col="red")#CNO
points(model_quasipoisson_coeff[14:16],model_quasipoisson_coeff[(14+37):(16+37)], cex=2.1, pch=c(15,17,19),col="purple1")#Haloperidol
points(model_quasipoisson_coeff[17:19],model_quasipoisson_coeff[(17+37):(19+37)], cex=2.1, pch=c(15,17,19),col="turquoise2")#NDMC
points(model_quasipoisson_coeff[20:22],model_quasipoisson_coeff[(20+37):(22+37)], cex=2.1, pch=c(15,17,19),col="chartreuse")#NDMCHigh
points(model_quasipoisson_coeff[23:25],model_quasipoisson_coeff[(23+37):(25+37)], cex=2.1, pch=c(15,17,19),col="deeppink")#OSU6162
points(model_quasipoisson_coeff[26:28],model_quasipoisson_coeff[(26+37):(28+37)], cex=2.1, pch=c(15,17,19),col="orange1")#PCAP1
points(model_quasipoisson_coeff[29:31],model_quasipoisson_coeff[(29+37):(31+37)], cex=2.1, pch=c(15,17,19),col="yellow1")#PCAP2
points(model_quasipoisson_coeff[32:34],model_quasipoisson_coeff[(32+37):(34+37)], cex=2.1, pch=c(15,17,19),col="paleturquoise1")#PCAP814
points(model_quasipoisson_coeff[35:37],model_quasipoisson_coeff[(35+37):(37+37)], cex=2.1, pch=c(15,17,19),col="lightpink")#PCAP931
points(0,0,pch=19,cex=2.1,col="black")

legend(min(model_quasipoisson_coeff[2:37])-0.3, max(model_quasipoisson_coeff[39:(37+37)])+0.3, 
		c("Control","Aripiprazole","Cariprazine","Clozapine","CNO","Haloperidol",
				"NDMC","NDMCHigh","OSU6162","PCAP1","PCAP2", "PCAP814","PCAP931", "1 microM","3 microM","10 microM"), cex=1, 
		col=c("black","royalblue4","seagreen4","saddlebrown","red","purple1","turquoise2", "chartreuse", "deeppink", "orange1", 
				"yellow1", "paleturquoise1", "lightpink","gray","gray","gray"), pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19,2,1,0))


dev.off()


#JBends

#quasipoisson
model_quasipoisson<-glmmPQL(round(Length1JBends*1000)~TimeFactor+Group+TimeFactor*Group+Length1BoutCount, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)$tTable[,c(1,5)]


#save coefficients, set non-significant to 0
model_quasipoisson_coeff<-summary(model_quasipoisson)$tTable[-1,1]
model_quasipoisson_coeff[summary(model_quasipoisson)$tTable[-1,5]>=0.05]<-0

png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Control_vs_drugs_length1JBends_",condition,".png"),width=1500,height=750)

plot(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01),
		rep(0,times=length(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01))),
		xlim=c(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3),
		ylim=c(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3),
		ylab="change in time",xlab="overall change",type="l",
		main="regression coefficient of JBends proportion\n in length1 bouts, relative to control")
lines(rep(0,times=length(seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))),
		seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))

points(model_quasipoisson_coeff[2:4],model_quasipoisson_coeff[(2+37):(4+37)], cex=2.1, pch=c(15,17,19),col="royalblue4")#Aripiprazole
points(model_quasipoisson_coeff[5:7],model_quasipoisson_coeff[(5+37):(7+37)], cex=2.1, pch=c(15,17,19),col="seagreen4")#Cariprazine
points(model_quasipoisson_coeff[8:10],model_quasipoisson_coeff[(8+37):(10+37)], cex=2.1, pch=c(15,17,19),col="saddlebrown")#Clozapine
points(model_quasipoisson_coeff[11:13],model_quasipoisson_coeff[(11+37):(13+37)], cex=2.1, pch=c(15,17,19),col="red")#CNO
points(model_quasipoisson_coeff[14:16],model_quasipoisson_coeff[(14+37):(16+37)], cex=2.1, pch=c(15,17,19),col="purple1")#Haloperidol
points(model_quasipoisson_coeff[17:19],model_quasipoisson_coeff[(17+37):(19+37)], cex=2.1, pch=c(15,17,19),col="turquoise2")#NDMC
points(model_quasipoisson_coeff[20:22],model_quasipoisson_coeff[(20+37):(22+37)], cex=2.1, pch=c(15,17,19),col="chartreuse")#NDMCHigh
points(model_quasipoisson_coeff[23:25],model_quasipoisson_coeff[(23+37):(25+37)], cex=2.1, pch=c(15,17,19),col="deeppink")#OSU6162
points(model_quasipoisson_coeff[26:28],model_quasipoisson_coeff[(26+37):(28+37)], cex=2.1, pch=c(15,17,19),col="orange1")#PCAP1
points(model_quasipoisson_coeff[29:31],model_quasipoisson_coeff[(29+37):(31+37)], cex=2.1, pch=c(15,17,19),col="yellow1")#PCAP2
points(model_quasipoisson_coeff[32:34],model_quasipoisson_coeff[(32+37):(34+37)], cex=2.1, pch=c(15,17,19),col="paleturquoise1")#PCAP814
points(model_quasipoisson_coeff[35:37],model_quasipoisson_coeff[(35+37):(37+37)], cex=2.1, pch=c(15,17,19),col="lightpink")#PCAP931
points(0,0,pch=19,cex=2.1,col="black")

legend(min(model_quasipoisson_coeff[2:37])-0.3, max(model_quasipoisson_coeff[39:(37+37)])+0.3, 
		c("Control","Aripiprazole","Cariprazine","Clozapine","CNO","Haloperidol",
				"NDMC","NDMCHigh","OSU6162","PCAP1","PCAP2", "PCAP814","PCAP931", "1 microM","3 microM","10 microM"), cex=1, 
		col=c("black","royalblue4","seagreen4","saddlebrown","red","purple1","turquoise2", "chartreuse", "deeppink", "orange1", 
				"yellow1", "paleturquoise1", "lightpink","gray","gray","gray"), pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19,2,1,0))


dev.off()


#OBends

#quasipoisson
model_quasipoisson<-glmmPQL(round(Length1OBends*1000)~TimeFactor+Group+TimeFactor*Group+Length1BoutCount, random=~1|Subject,family=quasipoisson,data=all_dataset)

summary(model_quasipoisson)$tTable[,c(1,5)]


#save coefficients, set non-significant to 0
model_quasipoisson_coeff<-summary(model_quasipoisson)$tTable[-1,1]
model_quasipoisson_coeff[summary(model_quasipoisson)$tTable[-1,5]>=0.05]<-0

png(paste0("~/git/zebrafish_action_sequence_project/results/plots/short_bout_analysis/",condition,"/Control_vs_drugs_length1OBends_",condition,".png"),width=1500,height=750)

plot(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01),
		rep(0,times=length(seq(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3,0.01))),
		xlim=c(min(model_quasipoisson_coeff[2:37])-0.3,max(model_quasipoisson_coeff[2:37])+0.3),
		ylim=c(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3),
		ylab="change in time",xlab="overall change",type="l",
		main="regression coefficient of OBends proportion\n in length1 bouts, relative to control")
lines(rep(0,times=length(seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))),
		seq(min(model_quasipoisson_coeff[39:(37+37)])-0.3,max(model_quasipoisson_coeff[39:(37+37)])+0.3,0.01))

points(model_quasipoisson_coeff[2:4],model_quasipoisson_coeff[(2+37):(4+37)], cex=2.1, pch=c(15,17,19),col="royalblue4")#Aripiprazole
points(model_quasipoisson_coeff[5:7],model_quasipoisson_coeff[(5+37):(7+37)], cex=2.1, pch=c(15,17,19),col="seagreen4")#Cariprazine
points(model_quasipoisson_coeff[8:10],model_quasipoisson_coeff[(8+37):(10+37)], cex=2.1, pch=c(15,17,19),col="saddlebrown")#Clozapine
points(model_quasipoisson_coeff[11:13],model_quasipoisson_coeff[(11+37):(13+37)], cex=2.1, pch=c(15,17,19),col="red")#CNO
points(model_quasipoisson_coeff[14:16],model_quasipoisson_coeff[(14+37):(16+37)], cex=2.1, pch=c(15,17,19),col="purple1")#Haloperidol
points(model_quasipoisson_coeff[17:19],model_quasipoisson_coeff[(17+37):(19+37)], cex=2.1, pch=c(15,17,19),col="turquoise2")#NDMC
points(model_quasipoisson_coeff[20:22],model_quasipoisson_coeff[(20+37):(22+37)], cex=2.1, pch=c(15,17,19),col="chartreuse")#NDMCHigh
points(model_quasipoisson_coeff[23:25],model_quasipoisson_coeff[(23+37):(25+37)], cex=2.1, pch=c(15,17,19),col="deeppink")#OSU6162
points(model_quasipoisson_coeff[26:28],model_quasipoisson_coeff[(26+37):(28+37)], cex=2.1, pch=c(15,17,19),col="orange1")#PCAP1
points(model_quasipoisson_coeff[29:31],model_quasipoisson_coeff[(29+37):(31+37)], cex=2.1, pch=c(15,17,19),col="yellow1")#PCAP2
points(model_quasipoisson_coeff[32:34],model_quasipoisson_coeff[(32+37):(34+37)], cex=2.1, pch=c(15,17,19),col="paleturquoise1")#PCAP814
points(model_quasipoisson_coeff[35:37],model_quasipoisson_coeff[(35+37):(37+37)], cex=2.1, pch=c(15,17,19),col="lightpink")#PCAP931
points(0,0,pch=19,cex=2.1,col="black")

legend(min(model_quasipoisson_coeff[2:37])-0.3, max(model_quasipoisson_coeff[39:(37+37)])+0.3, 
		c("Control","Aripiprazole","Cariprazine","Clozapine","CNO","Haloperidol",
				"NDMC","NDMCHigh","OSU6162","PCAP1","PCAP2", "PCAP814","PCAP931", "1 microM","3 microM","10 microM"), cex=1, 
		col=c("black","royalblue4","seagreen4","saddlebrown","red","purple1","turquoise2", "chartreuse", "deeppink", "orange1", 
				"yellow1", "paleturquoise1", "lightpink","gray","gray","gray"), pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19,2,1,0))


dev.off()


