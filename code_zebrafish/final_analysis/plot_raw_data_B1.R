args<-commandArgs(trailingOnly = TRUE)

#load data, start with full, if taking too long work with short
condition<-args[1]
dataset<-read.table(paste0(condition,"_final_dataset_selected.txt"),header=TRUE)
dataset_full<-read.table(paste0(condition,"_final_dataset_full.txt"),header=TRUE)

dataset$Group<-factor(dataset$Group)
dataset_full$Group<-factor(dataset_full$Group)



#make control as the reference of all
contrasts(dataset$Group) <- contr.treatment(levels(dataset$Group),base=which(levels(dataset$Group) == 'Control'))
contrasts(dataset_full$Group) <- contr.treatment(levels(dataset_full$Group),base=which(levels(dataset_full$Group) == 'Control'))

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(dataset)))
dataset<-cbind(dataset,apply(dataset[,grep("Length.*Count", colnames(dataset))],2,function(x) x/dataset$TotalBoutCount))

colnames(dataset)[c((length(dataset[1,])-(count_vars-1)):length(dataset[1,]))]<-paste0(colnames(dataset)[c((length(dataset[1,])-
									(count_vars-1)):length(dataset[1,]))],"Proportion")

count_vars<-length(grep("Length.*Count", colnames(dataset_full)))
dataset_full<-cbind(dataset_full,apply(dataset_full[,grep("Length.*Count", colnames(dataset_full))],2,function(x) x/dataset_full$TotalBoutCount))

colnames(dataset_full)[c((length(dataset_full[1,])-(count_vars-1)):length(dataset_full[1,]))]<-paste0(colnames(dataset_full)[c((length(dataset_full[1,])-
									(count_vars-1)):length(dataset_full[1,]))],"Proportion")


attach(dataset)


#plot
colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
		,"gray40","gray35","gray30")

#scatter plot per variable for all groups vs control
for(group in levels(Group)){
	
	for (variable in colnames(dataset)[-c(grep("Group", colnames(dataset)),grep("Time", colnames(dataset)))]){
		
		if( !length((1:13)[is.na(dataset[Group=="Control",variable])]) && !length((1:13)[is.na(dataset[Group==group,variable])])){
			
			png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/",
							condition,"/",variable,"_Control_vs_", group,"_",condition,".png"),width=1500,height=750)
			
			plot(dataset[Group=="Control",variable],dataset[Group==group,variable],cex=1.6,col=colrs,pch=19,
					xlim=c(min(min(dataset[Group=="Control",variable]),min(dataset[Group==group,variable])),
							max(max(dataset[Group=="Control",variable]),max(dataset[Group==group,variable]))),
					ylim=c(min(min(dataset[Group=="Control",variable]),min(dataset[Group==group,variable])),
							max(max(dataset[Group=="Control",variable]),max(dataset[Group==group,variable]))),
					xlab="Control",ylab=group,main=paste0("Mean ", variable,", control vs ",group))
			
			lines(c(min(min(dataset[Group=="Control",variable]),min(dataset[Group==group,variable])),
							max(max(dataset[Group=="Control",variable]),max(dataset[Group==group,variable]))),
					c(min(min(dataset[Group=="Control",variable]),min(dataset[Group==group,variable])),
							max(max(dataset[Group=="Control",variable]),max(dataset[Group==group,variable]))),type="l")
			
			legend(min(min(dataset[Group=="Control",variable]),min(dataset[Group==group,variable])), 
					max(max(dataset[Group=="Control",variable]),max(dataset[Group==group,variable])), 
					c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
			
			dev.off()
		}
		
	}
}

