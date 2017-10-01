#plot
colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
		,"gray40","gray35","gray30")

colrs2<-c("darkolivegreen1","darkolivegreen1","darkolivegreen2","darkolivegreen2","darkolivegreen3","darkolivegreen3",
		"darkolivegreen4","darkolivegreen4","darkolivegreen","darkolivegreen","darkgreen","darkgreen","darkgreen")

colrs3<-c("paleturquoise","paleturquoise","paleturquoise1","paleturquoise1","paleturquoise2","paleturquoise2","paleturquoise3","paleturquoise3",
		"paleturquoise4","paleturquoise4","cyan4","cyan4","cyan4")

colrs4<-c("navajowhite1","navajowhite1","navajowhite1","orange","orange","orange1","orange1","orange2","orange2","orange3","orange3",
		"orange4","orange4")


#compare Light and Dark 

dataset_full_Light<-read.table("Light_final_dataset_full.txt",header=TRUE)
dataset_full_Light$Group<-factor(dataset_full_Light$Group)


dataset_full_Dark<-read.table("Dark_final_dataset_full.txt",header=TRUE)
dataset_full_Dark$Group<-factor(dataset_full_Dark$Group)

common_var_Light_Dark<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_Light))


control_Light<-dataset_full_Light[dataset_full_Light$Group=="Control",common_var_Light_Dark]
control_Light$Group<-rep("Control_Light",times=13)
control_Light$Group<-as.factor(control_Light$Group)

control_Dark<-dataset_full_Dark[dataset_full_Dark$Group=="Control",common_var_Light_Dark]
control_Dark$Group<-rep("Control_Dark",times=13)
control_Dark$Group<-as.factor(control_Dark$Group)



control_Light_Dark<-rbind(control_Light,control_Dark)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(control_Light_Dark)))
control_Light_Dark<-cbind(control_Light_Dark,apply(control_Light_Dark[,grep("Length.*Count", colnames(control_Light_Dark))],2,function(x) x/control_Light_Dark$TotalBoutCount))

colnames(control_Light_Dark)[c((length(control_Light_Dark[1,])-(count_vars-1)):length(control_Light_Dark[1,]))]<-paste0(colnames(control_Light_Dark)[c((length(control_Light_Dark[1,])-
									(count_vars-1)):length(control_Light_Dark[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
control_Light_Dark[,c(grep("Proportion", colnames(control_Light_Dark)))]<-apply(control_Light_Dark[,c(grep("Proportion", colnames(control_Light_Dark)))],2,function(x) round(x*100))
control_Light_Dark[,c(grep("Count", colnames(control_Light_Dark)))]<-apply(control_Light_Dark[,c(grep("Count", colnames(control_Light_Dark)))],2,function(x) round(x))




#make control as the reference of all
contrasts(control_Light_Dark$Group) <- contr.treatment(levels(control_Light_Dark$Group),base=which(levels(control_Light_Dark$Group) == 'Control_Dark'))

attach(control_Light_Dark)


#scatter plot per variable 
for (variable in colnames(control_Light_Dark)[-c(grep("Group", colnames(control_Light_Dark)),grep("Time", colnames(control_Light_Dark)))]){
	
	if( !length((1:13)[is.na(control_Light_Dark[Group=="Control_Light",variable])]) && !length((1:13)[is.na(control_Light_Dark[Group=="Control_Dark",variable])])){
		
		png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Light_vs_Dark/",
						variable,"_LightControl_vs_DarkControl.png"),width=1500,height=750)
		
		plot(control_Light_Dark[Group=="Control_Light",variable],control_Light_Dark[Group=="Control_Dark",variable],cex=1.6,col=colrs,pch=19,
				xlim=c(min(min(control_Light_Dark[Group=="Control_Light",variable]),min(control_Light_Dark[Group=="Control_Dark",variable])),
						max(max(control_Light_Dark[Group=="Control_Light",variable]),max(control_Light_Dark[Group=="Control_Dark",variable]))),
				ylim=c(min(min(control_Light_Dark[Group=="Control_Light",variable]),min(control_Light_Dark[Group=="Control_Dark",variable])),
						max(max(control_Light_Dark[Group=="Control_Light",variable]),max(control_Light_Dark[Group=="Control_Dark",variable]))),
				xlab="control Light",ylab="control Dark",main=paste0("Mean ", variable,", control Light vs control Dark "))
		
		lines(c(min(min(control_Light_Dark[Group=="Control_Light",variable]),min(control_Light_Dark[Group=="Control_Dark",variable])),
						max(max(control_Light_Dark[Group=="Control_Light",variable]),max(control_Light_Dark[Group=="Control_Dark",variable]))),
				c(min(min(control_Light_Dark[Group=="Control_Light",variable]),min(control_Light_Dark[Group=="Control_Dark",variable])),
						max(max(control_Light_Dark[Group=="Control_Light",variable]),max(control_Light_Dark[Group=="Control_Dark",variable]))),type="l")
		
		legend(min(min(control_Light_Dark[Group=="Control_Light",variable]),min(control_Light_Dark[Group=="Control_Dark",variable])), 
				max(max(control_Light_Dark[Group=="Control_Light",variable]),max(control_Light_Dark[Group=="Control_Dark",variable])), 
				c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
		
		dev.off()
	}
	
}

detach(control_Light_Dark)

#compare Dark and all the disease in Dark(ApoLow, ApoHigh, PTZ)

dataset_full_DarkApoLow<-read.table("DarkApoLow_final_dataset_full.txt",header=TRUE)
dataset_full_DarkApoLow$Group<-factor(dataset_full_DarkApoLow$Group)


dataset_full_DarkApoHigh<-read.table("DarkApoHigh_final_dataset_full.txt",header=TRUE)
dataset_full_DarkApoHigh$Group<-factor(dataset_full_DarkApoHigh$Group)


dataset_full_DarkPTZ<-read.table("DarkPTZ_final_dataset_full.txt",header=TRUE)
dataset_full_DarkPTZ$Group<-factor(dataset_full_DarkPTZ$Group)



common_var_Dark_ApoLow<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_DarkApoLow))
common_var_Dark_ApoHigh<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_DarkApoHigh))
common_var_Dark_PTZ<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_DarkPTZ))



#compare Dark and ApoLow

control_Dark<-dataset_full_Dark[dataset_full_Dark$Group=="Control",common_var_Dark_ApoLow]
control_Dark$Group<-rep("Control_Dark",times=13)
control_Dark$Group<-as.factor(control_Dark$Group)

control_DarkApoLow<-dataset_full_DarkApoLow[dataset_full_DarkApoLow$Group=="Control",common_var_Dark_ApoLow]
control_DarkApoLow$Group<-rep("Control_DarkApoLow",times=13)
control_DarkApoLow$Group<-as.factor(control_DarkApoLow$Group)

control_Dark_ApoLow<-rbind(control_Dark,control_DarkApoLow)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(control_Dark_ApoLow)))
control_Dark_ApoLow<-cbind(control_Dark_ApoLow,apply(control_Dark_ApoLow[,grep("Length.*Count", colnames(control_Dark_ApoLow))],2,function(x) x/control_Dark_ApoLow$TotalBoutCount))

colnames(control_Dark_ApoLow)[c((length(control_Dark_ApoLow[1,])-(count_vars-1)):length(control_Dark_ApoLow[1,]))]<-paste0(colnames(control_Dark_ApoLow)[c((length(control_Dark_ApoLow[1,])-
									(count_vars-1)):length(control_Dark_ApoLow[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
control_Dark_ApoLow[,c(grep("Proportion", colnames(control_Dark_ApoLow)))]<-apply(control_Dark_ApoLow[,c(grep("Proportion", colnames(control_Dark_ApoLow)))],2,function(x) round(x*100))
control_Dark_ApoLow[,c(grep("Count", colnames(control_Dark_ApoLow)))]<-apply(control_Dark_ApoLow[,c(grep("Count", colnames(control_Dark_ApoLow)))],2,function(x) round(x))

#make control as the reference of all
contrasts(control_Dark_ApoLow$Group) <- contr.treatment(levels(control_Dark_ApoLow$Group),base=which(levels(control_Dark_ApoLow$Group) == 'Control_Dark'))

attach(control_Dark_ApoLow)

#scatter plot per variable 
for (variable in colnames(control_Dark_ApoLow)[-c(grep("Group", colnames(control_Dark_ApoLow)),grep("Time", colnames(control_Dark_ApoLow)))]){
	
	if( !length((1:13)[is.na(control_Dark_ApoLow[Group=="Control_Dark",variable])]) && !length((1:13)[is.na(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])])){
		
		png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Dark_vs_ApoLow/",
						variable,"_DarkControl_vs_ApoLowControl.png"),width=1500,height=750)
		
		plot(control_Dark_ApoLow[Group=="Control_Dark",variable],control_Dark_ApoLow[Group=="Control_DarkApoLow",variable],cex=1.6,col=colrs,pch=19,
				xlim=c(min(min(control_Dark_ApoLow[Group=="Control_Dark",variable]),min(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])),
						max(max(control_Dark_ApoLow[Group=="Control_Dark",variable]),max(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))),
				ylim=c(min(min(control_Dark_ApoLow[Group=="Control_Dark",variable]),min(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])),
						max(max(control_Dark_ApoLow[Group=="Control_Dark",variable]),max(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))),
				xlab="control healthy",ylab="control ApoLow",main=paste0("Mean ", variable,", control healthy vs control ApoLow "))
		
		lines(c(min(min(control_Dark_ApoLow[Group=="Control_Dark",variable]),min(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])),
						max(max(control_Dark_ApoLow[Group=="Control_Dark",variable]),max(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))),
				c(min(min(control_Dark_ApoLow[Group=="Control_Dark",variable]),min(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])),
						max(max(control_Dark_ApoLow[Group=="Control_Dark",variable]),max(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))),type="l")
		
		legend(min(min(control_Dark_ApoLow[Group=="Control_Dark",variable]),min(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])), 
				max(max(control_Dark_ApoLow[Group=="Control_Dark",variable]),max(control_Dark_ApoLow[Group=="Control_DarkApoLow",variable])), 
				c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
		
		dev.off()
	}
	
}

detach(control_Dark_ApoLow)


#compare Dark and ApoHigh

control_Dark<-dataset_full_Dark[dataset_full_Dark$Group=="Control",common_var_Dark_ApoHigh]
control_Dark$Group<-rep("Control_Dark",times=13)
control_Dark$Group<-as.factor(control_Dark$Group)

control_DarkApoHigh<-dataset_full_DarkApoHigh[dataset_full_DarkApoHigh$Group=="Control",common_var_Dark_ApoHigh]
control_DarkApoHigh$Group<-rep("Control_DarkApoHigh",times=13)
control_DarkApoHigh$Group<-as.factor(control_DarkApoHigh$Group)

control_Dark_ApoHigh<-rbind(control_Dark,control_DarkApoHigh)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(control_Dark_ApoHigh)))
control_Dark_ApoHigh<-cbind(control_Dark_ApoHigh,apply(control_Dark_ApoHigh[,grep("Length.*Count", colnames(control_Dark_ApoHigh))],2,function(x) x/control_Dark_ApoHigh$TotalBoutCount))

colnames(control_Dark_ApoHigh)[c((length(control_Dark_ApoHigh[1,])-(count_vars-1)):length(control_Dark_ApoHigh[1,]))]<-paste0(colnames(control_Dark_ApoHigh)[c((length(control_Dark_ApoHigh[1,])-
									(count_vars-1)):length(control_Dark_ApoHigh[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
control_Dark_ApoHigh[,c(grep("Proportion", colnames(control_Dark_ApoHigh)))]<-apply(control_Dark_ApoHigh[,c(grep("Proportion", colnames(control_Dark_ApoHigh)))],2,function(x) round(x*100))
control_Dark_ApoHigh[,c(grep("Count", colnames(control_Dark_ApoHigh)))]<-apply(control_Dark_ApoHigh[,c(grep("Count", colnames(control_Dark_ApoHigh)))],2,function(x) round(x))

#make control as the reference of all
contrasts(control_Dark_ApoHigh$Group) <- contr.treatment(levels(control_Dark_ApoHigh$Group),base=which(levels(control_Dark_ApoHigh$Group) == 'Control_Dark'))

attach(control_Dark_ApoHigh)

#scatter plot per variable 
for (variable in colnames(control_Dark_ApoHigh)[-c(grep("Group", colnames(control_Dark_ApoHigh)),grep("Time", colnames(control_Dark_ApoHigh)))]){
	
	if( !length((1:13)[is.na(control_Dark_ApoHigh[Group=="Control_Dark",variable])]) && !length((1:13)[is.na(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])])){
		
		png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Dark_vs_ApoHigh/",
						variable,"_DarkControl_vs_ApoHighControl.png"),width=1500,height=750)
		
		plot(control_Dark_ApoHigh[Group=="Control_Dark",variable],control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable],cex=1.6,col=colrs,pch=19,
				xlim=c(min(min(control_Dark_ApoHigh[Group=="Control_Dark",variable]),min(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])),
						max(max(control_Dark_ApoHigh[Group=="Control_Dark",variable]),max(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))),
				ylim=c(min(min(control_Dark_ApoHigh[Group=="Control_Dark",variable]),min(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])),
						max(max(control_Dark_ApoHigh[Group=="Control_Dark",variable]),max(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))),
				xlab="control healthy",ylab="control ApoHigh",main=paste0("Mean ", variable,", control healthy vs control ApoHigh "))
		
		lines(c(min(min(control_Dark_ApoHigh[Group=="Control_Dark",variable]),min(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])),
						max(max(control_Dark_ApoHigh[Group=="Control_Dark",variable]),max(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))),
				c(min(min(control_Dark_ApoHigh[Group=="Control_Dark",variable]),min(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])),
						max(max(control_Dark_ApoHigh[Group=="Control_Dark",variable]),max(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))),type="l")
		
		legend(min(min(control_Dark_ApoHigh[Group=="Control_Dark",variable]),min(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])), 
				max(max(control_Dark_ApoHigh[Group=="Control_Dark",variable]),max(control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])), 
				c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
		
		dev.off()
	}
	
}

detach(control_Dark_ApoHigh)


#compare Dark and PTZ

control_Dark<-dataset_full_Dark[dataset_full_Dark$Group=="Control",common_var_Dark_PTZ]
control_Dark$Group<-rep("Control_Dark",times=13)
control_Dark$Group<-as.factor(control_Dark$Group)

control_DarkPTZ<-dataset_full_DarkPTZ[dataset_full_DarkPTZ$Group=="Control",common_var_Dark_PTZ]
control_DarkPTZ$Group<-rep("Control_DarkPTZ",times=13)
control_DarkPTZ$Group<-as.factor(control_DarkPTZ$Group)

control_Dark_PTZ<-rbind(control_Dark,control_DarkPTZ)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(control_Dark_PTZ)))
control_Dark_PTZ<-cbind(control_Dark_PTZ,apply(control_Dark_PTZ[,grep("Length.*Count", colnames(control_Dark_PTZ))],2,function(x) x/control_Dark_PTZ$TotalBoutCount))

colnames(control_Dark_PTZ)[c((length(control_Dark_PTZ[1,])-(count_vars-1)):length(control_Dark_PTZ[1,]))]<-paste0(colnames(control_Dark_PTZ)[c((length(control_Dark_PTZ[1,])-
									(count_vars-1)):length(control_Dark_PTZ[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
control_Dark_PTZ[,c(grep("Proportion", colnames(control_Dark_PTZ)))]<-apply(control_Dark_PTZ[,c(grep("Proportion", colnames(control_Dark_PTZ)))],2,function(x) round(x*100))
control_Dark_PTZ[,c(grep("Count", colnames(control_Dark_PTZ)))]<-apply(control_Dark_PTZ[,c(grep("Count", colnames(control_Dark_PTZ)))],2,function(x) round(x))

#make control as the reference of all
contrasts(control_Dark_PTZ$Group) <- contr.treatment(levels(control_Dark_PTZ$Group),base=which(levels(control_Dark_PTZ$Group) == 'Control_Dark'))

attach(control_Dark_PTZ)

#scatter plot per variable 
for (variable in colnames(control_Dark_PTZ)[-c(grep("Group", colnames(control_Dark_PTZ)),grep("Time", colnames(control_Dark_PTZ)))]){
	
	if( !length((1:13)[is.na(control_Dark_PTZ[Group=="Control_Dark",variable])]) && !length((1:13)[is.na(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])])){
		
		png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Dark_vs_PTZ/",
						variable,"_DarkControl_vs_PTZControl.png"),width=1500,height=750)
		
		plot(control_Dark_PTZ[Group=="Control_Dark",variable],control_Dark_PTZ[Group=="Control_DarkPTZ",variable],cex=1.6,col=colrs,pch=19,
				xlim=c(min(min(control_Dark_PTZ[Group=="Control_Dark",variable]),min(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])),
						max(max(control_Dark_PTZ[Group=="Control_Dark",variable]),max(control_Dark_PTZ[Group=="Control_DarkPTZ",variable]))),
				ylim=c(min(min(control_Dark_PTZ[Group=="Control_Dark",variable]),min(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])),
						max(max(control_Dark_PTZ[Group=="Control_Dark",variable]),max(control_Dark_PTZ[Group=="Control_DarkPTZ",variable]))),
				xlab="control healthy",ylab="control PTZ",main=paste0("Mean ", variable,", control healthy vs control PTZ "))
		
		lines(c(min(min(control_Dark_PTZ[Group=="Control_Dark",variable]),min(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])),
						max(max(control_Dark_PTZ[Group=="Control_Dark",variable]),max(control_Dark_PTZ[Group=="Control_DarkPTZ",variable]))),
				c(min(min(control_Dark_PTZ[Group=="Control_Dark",variable]),min(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])),
						max(max(control_Dark_PTZ[Group=="Control_Dark",variable]),max(control_Dark_PTZ[Group=="Control_DarkPTZ",variable]))),type="l")
		
		legend(min(min(control_Dark_PTZ[Group=="Control_Dark",variable]),min(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])), 
				max(max(control_Dark_PTZ[Group=="Control_Dark",variable]),max(control_Dark_PTZ[Group=="Control_DarkPTZ",variable])), 
				c("Time 0 minutes","Time 65 minutes","identity line"), cex=1.3, col=c("gray90","gray30"), pch = c(19,19,NA),lty=c(NA,NA,1))
		
		dev.off()
	}
	
}

detach(control_Dark_PTZ)

#compare drugs with healthy vs disease

#load data, start with full, if taking too long work with selected
dataset_full_Dark<-read.table("Dark_final_dataset_full.txt",header=TRUE)
dataset_full_Dark$Group<-factor(dataset_full_Dark$Group)

dataset_full_DarkApoLow<-read.table("DarkApoLow_final_dataset_full.txt",header=TRUE)
dataset_full_DarkApoLow$Group<-factor(dataset_full_DarkApoLow$Group)


dataset_full_DarkApoHigh<-read.table("DarkApoHigh_final_dataset_full.txt",header=TRUE)
dataset_full_DarkApoHigh$Group<-factor(dataset_full_DarkApoHigh$Group)


dataset_full_DarkPTZ<-read.table("DarkPTZ_final_dataset_full.txt",header=TRUE)
dataset_full_DarkPTZ$Group<-factor(dataset_full_DarkPTZ$Group)



common_var_Dark_ApoLow<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_DarkApoLow))
common_var_Dark_ApoHigh<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_DarkApoHigh))
common_var_Dark_PTZ<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_DarkPTZ))




#compare Dark and ApoLow

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoLow]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoLow_common<-dataset_full_DarkApoLow[,common_var_Dark_ApoLow]
levels(dataset_full_DarkApoLow_common$Group)<-paste0(levels(dataset_full_DarkApoLow_common$Group),"_DarkApoLow")

full_Dark_ApoLow<-rbind(dataset_full_Dark_common,dataset_full_DarkApoLow_common)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoLow)))
full_Dark_ApoLow<-cbind(full_Dark_ApoLow,apply(full_Dark_ApoLow[,grep("Length.*Count", colnames(full_Dark_ApoLow))],2,function(x) x/full_Dark_ApoLow$TotalBoutCount))

colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-(count_vars-1)):length(full_Dark_ApoLow[1,]))]<-paste0(colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-
									(count_vars-1)):length(full_Dark_ApoLow[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))],2,function(x) round(x*100))
full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))],2,function(x) round(x))

#make control as the reference of all
contrasts(full_Dark_ApoLow$Group) <- contr.treatment(levels(full_Dark_ApoLow$Group),base=which(levels(full_Dark_ApoLow$Group) == 'Control_Dark'))

attach(full_Dark_ApoLow)


#scatter plot per variable for all groups vs control
for(group in levels(Group)[1:37][-13]){
	
	for (variable in colnames(full_Dark_ApoLow)[-c(grep("Group", colnames(full_Dark_ApoLow)),grep("Time", colnames(full_Dark_ApoLow)))]){
		
		if( !length((1:13)[is.na(full_Dark_ApoLow[Group=="Control_Dark",variable])]) && !length((1:13)[is.na(full_Dark_ApoLow[Group==group,variable])])
			&& !length((1:13)[is.na(full_Dark_ApoLow[Group=="Control_DarkApoLow",variable])]) && !length((1:13)[is.na(full_Dark_ApoLow[Group==paste0(group,"ApoLow"),variable])])){
			
			x_min=min(min(full_Dark_ApoLow[Group=="Control_Dark",variable]),min(full_Dark_ApoLow[Group==group,variable]),
					min(full_Dark_ApoLow[Group==paste0(group,"ApoLow"),variable]),min(full_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))
			x_max=max(max(full_Dark_ApoLow[Group=="Control_Dark",variable]),max(full_Dark_ApoLow[Group==group,variable]),
					max(full_Dark_ApoLow[Group==paste0(group,"ApoLow"),variable]),max(full_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))
			y_min=min(min(full_Dark_ApoLow[Group=="Control_Dark",variable]),min(full_Dark_ApoLow[Group==group,variable]),
					min(full_Dark_ApoLow[Group==paste0(group,"ApoLow"),variable]),min(full_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))
			y_max=max(max(full_Dark_ApoLow[Group=="Control_Dark",variable]),max(full_Dark_ApoLow[Group==group,variable]),
					max(full_Dark_ApoLow[Group==paste0(group,"ApoLow"),variable]),max(full_Dark_ApoLow[Group=="Control_DarkApoLow",variable]))
						
			png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Healthy_vs_Disease/ApoLow/",
							variable,"_Control_vs_", group,"_ApoLow.png"),width=1500,height=750)
			par(mar=c(5, 5, 5, 25), xpd=TRUE)
			
			plot(full_Dark_ApoLow[Group=="Control_Dark",variable],full_Dark_ApoLow[Group==group,variable],cex=1.6,col=colrs2,pch=19,
					xlim=c(x_min,x_max),ylim=c(y_min,y_max),
					xlab=" healthy control",ylab=paste0("disease control, ",substr(group,1,nchar(group)-5),"_healthy, ", substr(group,1,nchar(group)-5),"_disease "),main=paste0("Mean ", variable))

			
			points(full_Dark_ApoLow[Group=="Control_Dark",variable],full_Dark_ApoLow[Group==paste0(group,"ApoLow"),variable],cex=1.6,col=colrs3,pch=19)
			
			points(full_Dark_ApoLow[Group=="Control_Dark",variable],full_Dark_ApoLow[Group=="Control_DarkApoLow",variable],cex=1.6,col=colrs4,pch=19)
			
			
			lines(c(x_min,x_max),c(x_min,x_max),type="l")
			
			leg<-legend("topright", inset=c(-0.3,0), 
					c(paste0(substr(group,1,nchar(group)-5)," healthy in\n time 0 to 65 minutes\n"),
							paste0(substr(group,1,nchar(group)-5)," disease in\n time 0 to 65 minutes\n"),
							"Control disease in\n time 0 to 65 minutes\n ","Identity line"), 
					cex=1.3, col=c(colrs2[1],colrs3[1], colrs4[1],"black"), pch = c(19,19,19,NA),lty=c(NA,NA,NA,1))
			
			points(x=leg$text$x[c(1,2,3)]-leg$text$x[1]/80,y=leg$text$y[c(1,2,3)],col=c(colrs2[7],colrs3[7], colrs4[7]),pch=19,cex=1.3)
			points(x=leg$text$x[c(1,2,3)]-leg$text$x[1]/280,y=leg$text$y[c(1,2,3)],col=c(colrs2[13],colrs3[13], colrs4[13]),pch=19,cex=1.3)
			
			dev.off()
			
			
		}
		
	}
	
}

detach(full_Dark_ApoLow)


#compare Dark and ApoHigh

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoHigh]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoHigh_common<-dataset_full_DarkApoHigh[,common_var_Dark_ApoHigh]
levels(dataset_full_DarkApoHigh_common$Group)<-paste0(levels(dataset_full_DarkApoHigh_common$Group),"_DarkApoHigh")

full_Dark_ApoHigh<-rbind(dataset_full_Dark_common,dataset_full_DarkApoHigh_common)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoHigh)))
full_Dark_ApoHigh<-cbind(full_Dark_ApoHigh,apply(full_Dark_ApoHigh[,grep("Length.*Count", colnames(full_Dark_ApoHigh))],2,function(x) x/full_Dark_ApoHigh$TotalBoutCount))

colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-(count_vars-1)):length(full_Dark_ApoHigh[1,]))]<-paste0(colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-
									(count_vars-1)):length(full_Dark_ApoHigh[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))],2,function(x) round(x*100))
full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))],2,function(x) round(x))

#make control as the reference of all
contrasts(full_Dark_ApoHigh$Group) <- contr.treatment(levels(full_Dark_ApoHigh$Group),base=which(levels(full_Dark_ApoHigh$Group) == 'Control_Dark'))

attach(full_Dark_ApoHigh)


#scatter plot per variable for all groups vs control
for(group in levels(Group)[1:37][-13]){
	
	for (variable in colnames(full_Dark_ApoHigh)[-c(grep("Group", colnames(full_Dark_ApoHigh)),grep("Time", colnames(full_Dark_ApoHigh)))]){
		
		if( !length((1:13)[is.na(full_Dark_ApoHigh[Group=="Control_Dark",variable])]) && !length((1:13)[is.na(full_Dark_ApoHigh[Group==group,variable])])
				&& !length((1:13)[is.na(full_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable])]) && !length((1:13)[is.na(full_Dark_ApoHigh[Group==paste0(group,"ApoHigh"),variable])])){
			
			x_min=min(min(full_Dark_ApoHigh[Group=="Control_Dark",variable]),min(full_Dark_ApoHigh[Group==group,variable]),
					min(full_Dark_ApoHigh[Group==paste0(group,"ApoHigh"),variable]),min(full_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))
			x_max=max(max(full_Dark_ApoHigh[Group=="Control_Dark",variable]),max(full_Dark_ApoHigh[Group==group,variable]),
					max(full_Dark_ApoHigh[Group==paste0(group,"ApoHigh"),variable]),max(full_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))
			y_min=min(min(full_Dark_ApoHigh[Group=="Control_Dark",variable]),min(full_Dark_ApoHigh[Group==group,variable]),
					min(full_Dark_ApoHigh[Group==paste0(group,"ApoHigh"),variable]),min(full_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))
			y_max=max(max(full_Dark_ApoHigh[Group=="Control_Dark",variable]),max(full_Dark_ApoHigh[Group==group,variable]),
					max(full_Dark_ApoHigh[Group==paste0(group,"ApoHigh"),variable]),max(full_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]))
			
			png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Healthy_vs_Disease/ApoHigh/",
							variable,"_Control_vs_", group,"_ApoHigh.png"),width=1500,height=750)
			par(mar=c(5, 5, 5, 25), xpd=TRUE)
			
			plot(full_Dark_ApoHigh[Group=="Control_Dark",variable],full_Dark_ApoHigh[Group==group,variable],cex=1.6,col=colrs2,pch=19,
					xlim=c(x_min,x_max),ylim=c(y_min,y_max),
					xlab=" healthy control",ylab=paste0("disease control, ",substr(group,1,nchar(group)-5),"_healthy, ", substr(group,1,nchar(group)-5),"_disease "),main=paste0("Mean ", variable))
			
			
			points(full_Dark_ApoHigh[Group=="Control_Dark",variable],full_Dark_ApoHigh[Group==paste0(group,"ApoHigh"),variable],cex=1.6,col=colrs3,pch=19)
			
			points(full_Dark_ApoHigh[Group=="Control_Dark",variable],full_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable],cex=1.6,col=colrs4,pch=19)
			
			
			lines(c(x_min,x_max),c(x_min,x_max),type="l")
			
			leg<-legend("topright", inset=c(-0.3,0), 
					c(paste0(substr(group,1,nchar(group)-5)," healthy in\n time 0 to 65 minutes\n"),
							paste0(substr(group,1,nchar(group)-5)," disease in\n time 0 to 65 minutes\n"),
							"Control disease in\n time 0 to 65 minutes\n ","Identity line"), 
					cex=1.3, col=c(colrs2[1],colrs3[1], colrs4[1],"black"), pch = c(19,19,19,NA),lty=c(NA,NA,NA,1))
			
			points(x=leg$text$x[c(1,2,3)]-leg$text$x[1]/80,y=leg$text$y[c(1,2,3)],col=c(colrs2[7],colrs3[7], colrs4[7]),pch=19,cex=1.3)
			points(x=leg$text$x[c(1,2,3)]-leg$text$x[1]/280,y=leg$text$y[c(1,2,3)],col=c(colrs2[13],colrs3[13], colrs4[13]),pch=19,cex=1.3)
			
			dev.off()
		}
		
	}
	
}

detach(full_Dark_ApoHigh)



#compare Dark and PTZ

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_PTZ]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkPTZ_common<-dataset_full_DarkPTZ[,common_var_Dark_PTZ]
levels(dataset_full_DarkPTZ_common$Group)<-paste0(levels(dataset_full_DarkPTZ_common$Group),"_DarkPTZ")


full_Dark_PTZ<-rbind(dataset_full_Dark_common,dataset_full_DarkPTZ_common)

#add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_PTZ)))
full_Dark_PTZ<-cbind(full_Dark_PTZ,apply(full_Dark_PTZ[,grep("Length.*Count", colnames(full_Dark_PTZ))],2,function(x) x/full_Dark_PTZ$TotalBoutCount))

colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-(count_vars-1)):length(full_Dark_PTZ[1,]))]<-paste0(colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-
									(count_vars-1)):length(full_Dark_PTZ[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))],2,function(x) round(x*100))
full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))],2,function(x) round(x))

#make control as the reference of all
contrasts(full_Dark_PTZ$Group) <- contr.treatment(levels(full_Dark_PTZ$Group),base=which(levels(full_Dark_PTZ$Group) == 'Control_Dark'))

attach(full_Dark_PTZ)


#scatter plot per variable for all groups vs control
for(group in levels(Group)[1:37][-13]){
	
	for (variable in colnames(full_Dark_PTZ)[-c(grep("Group", colnames(full_Dark_PTZ)),grep("Time", colnames(full_Dark_PTZ)))]){
		
		if( !length((1:13)[is.na(full_Dark_PTZ[Group=="Control_Dark",variable])]) && !length((1:13)[is.na(full_Dark_PTZ[Group==group,variable])])
				&& !length((1:13)[is.na(full_Dark_PTZ[Group=="Control_DarkPTZ",variable])]) && !length((1:13)[is.na(full_Dark_PTZ[Group==paste0(group,"PTZ"),variable])])){
			
			x_min=min(min(full_Dark_PTZ[Group=="Control_Dark",variable]),min(full_Dark_PTZ[Group==group,variable]),
					min(full_Dark_PTZ[Group==paste0(group,"PTZ"),variable]),min(full_Dark_PTZ[Group=="Control_DarkPTZ",variable]))
			x_max=max(max(full_Dark_PTZ[Group=="Control_Dark",variable]),max(full_Dark_PTZ[Group==group,variable]),
					max(full_Dark_PTZ[Group==paste0(group,"PTZ"),variable]),max(full_Dark_PTZ[Group=="Control_DarkPTZ",variable]))
			y_min=min(min(full_Dark_PTZ[Group=="Control_Dark",variable]),min(full_Dark_PTZ[Group==group,variable]),
					min(full_Dark_PTZ[Group==paste0(group,"PTZ"),variable]),min(full_Dark_PTZ[Group=="Control_DarkPTZ",variable]))
			y_max=max(max(full_Dark_PTZ[Group=="Control_Dark",variable]),max(full_Dark_PTZ[Group==group,variable]),
					max(full_Dark_PTZ[Group==paste0(group,"PTZ"),variable]),max(full_Dark_PTZ[Group=="Control_DarkPTZ",variable]))
			
			png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/scatter_plot_per_variable/Healthy_vs_Disease/PTZ/",
							variable,"_Control_vs_", group,"_PTZ.png"),width=1500,height=750)
			par(mar=c(5, 5, 5, 25), xpd=TRUE)
			
			plot(full_Dark_PTZ[Group=="Control_Dark",variable],full_Dark_PTZ[Group==group,variable],cex=1.6,col=colrs2,pch=19,
					xlim=c(x_min,x_max),ylim=c(y_min,y_max),
					xlab=" healthy control",ylab=paste0("disease control, ",substr(group,1,nchar(group)-5),"_healthy, ", substr(group,1,nchar(group)-5),"_disease "),main=paste0("Mean ", variable))
			
			
			points(full_Dark_PTZ[Group=="Control_Dark",variable],full_Dark_PTZ[Group==paste0(group,"PTZ"),variable],cex=1.6,col=colrs3,pch=19)
			
			points(full_Dark_PTZ[Group=="Control_Dark",variable],full_Dark_PTZ[Group=="Control_DarkPTZ",variable],cex=1.6,col=colrs4,pch=19)
			
			
			lines(c(x_min,x_max),c(x_min,x_max),type="l")
			
			leg<-legend("topright", inset=c(-0.3,0), 
					c(paste0(substr(group,1,nchar(group)-5)," healthy in\n time 0 to 65 minutes\n"),
							paste0(substr(group,1,nchar(group)-5)," disease in\n time 0 to 65 minutes\n"),
							"Control disease in\n time 0 to 65 minutes\n ","Identity line"), 
					cex=1.3, col=c(colrs2[1],colrs3[1], colrs4[1],"black"), pch = c(19,19,19,NA),lty=c(NA,NA,NA,1))
			
			points(x=leg$text$x[c(1,2,3)]-leg$text$x[1]/80,y=leg$text$y[c(1,2,3)],col=c(colrs2[7],colrs3[7], colrs4[7]),pch=19,cex=1.3)
			points(x=leg$text$x[c(1,2,3)]-leg$text$x[1]/280,y=leg$text$y[c(1,2,3)],col=c(colrs2[13],colrs3[13], colrs4[13]),pch=19,cex=1.3)
			
			dev.off()
			
			
		}
		
	}
	
}

detach(full_Dark_PTZ)