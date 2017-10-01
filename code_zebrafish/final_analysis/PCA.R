library(FactoMineR)


colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
		,"gray40","gray35","gray30")

colrs2<-c("darkolivegreen1","darkolivegreen1","darkolivegreen2","darkolivegreen2","darkolivegreen3","darkolivegreen3",
		"darkolivegreen4","darkolivegreen4","darkolivegreen","darkolivegreen","darkgreen","darkgreen","darkgreen")

colrs3<-c("paleturquoise","paleturquoise","paleturquoise1","paleturquoise1","paleturquoise2","paleturquoise2","paleturquoise3","paleturquoise3",
		"paleturquoise4","paleturquoise4","cyan4","cyan4","cyan4")

colrs4<-c("navajowhite1","navajowhite1","navajowhite1","orange","orange","orange1","orange1","orange2","orange2","orange3","orange3",
		"orange4","orange4")


#load data
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




#ApoLow

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoLow]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoLow_common<-dataset_full_DarkApoLow[,common_var_Dark_ApoLow]
levels(dataset_full_DarkApoLow_common$Group)<-paste0(levels(dataset_full_DarkApoLow_common$Group),"_DarkApoLow")


full_Dark_ApoLow<-rbind(dataset_full_Dark_common,dataset_full_DarkApoLow_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoLow)))
full_Dark_ApoLow<-cbind(full_Dark_ApoLow,apply(full_Dark_ApoLow[,grep("Length.*Count", colnames(full_Dark_ApoLow))],2,function(x) x/full_Dark_ApoLow$TotalBoutCount))

colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-(count_vars-1)):length(full_Dark_ApoLow[1,]))]<-paste0(colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-
									(count_vars-1)):length(full_Dark_ApoLow[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))],2,function(x) round(x*100))
full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA

Dark_ApoLow_predictors<-full_Dark_ApoLow[,c(-137,-11)]
#remove columns with all zeros
Dark_ApoLow_predictors_scaled<-scale(Dark_ApoLow_predictors[,apply(Dark_ApoLow_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoLow_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1
#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots/ApoLow/",
					drug,"Control_ApoLow.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),2]
	

	drug<-substr(drug,1,nchar(drug)-1)
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	
	if(counter_for_order<5){
	
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,10),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
				
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
				paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
				paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
				paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.1
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
	}else{
			
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,10),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.1
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
			
		
	}

	

	counter_for_order<-counter_for_order+1
	
}




#ApoHigh
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoHigh]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoHigh_common<-dataset_full_DarkApoHigh[,common_var_Dark_ApoHigh]
levels(dataset_full_DarkApoHigh_common$Group)<-paste0(levels(dataset_full_DarkApoHigh_common$Group),"_DarkApoHigh")


full_Dark_ApoHigh<-rbind(dataset_full_Dark_common,dataset_full_DarkApoHigh_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoHigh)))
full_Dark_ApoHigh<-cbind(full_Dark_ApoHigh,apply(full_Dark_ApoHigh[,grep("Length.*Count", colnames(full_Dark_ApoHigh))],2,function(x) x/full_Dark_ApoHigh$TotalBoutCount))

colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-(count_vars-1)):length(full_Dark_ApoHigh[1,]))]<-paste0(colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-
									(count_vars-1)):length(full_Dark_ApoHigh[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))],2,function(x) round(x*100))
full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA

Dark_ApoHigh_predictors<-full_Dark_ApoHigh[,c(-187,-16)]
#remove columns with all zeros
Dark_ApoHigh_predictors_scaled<-scale(Dark_ApoHigh_predictors[,apply(Dark_ApoHigh_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoHigh_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots/ApoHigh/",
					drug,"Control_ApoHigh.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
	
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
	
	
		leg<-legend("topright", inset=c(-0.32,0), 
			c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
					paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
					paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
					paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
			cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
	
	
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.15
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
	
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.15
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	

	counter_for_order<-counter_for_order+1

	
}




#PTZ
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_PTZ]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkPTZ_common<-dataset_full_DarkPTZ[,common_var_Dark_PTZ]
levels(dataset_full_DarkPTZ_common$Group)<-paste0(levels(dataset_full_DarkPTZ_common$Group),"_DarkPTZ")


full_Dark_PTZ<-rbind(dataset_full_Dark_common,dataset_full_DarkPTZ_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_PTZ)))
full_Dark_PTZ<-cbind(full_Dark_PTZ,apply(full_Dark_PTZ[,grep("Length.*Count", colnames(full_Dark_PTZ))],2,function(x) x/full_Dark_PTZ$TotalBoutCount))

colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-(count_vars-1)):length(full_Dark_PTZ[1,]))]<-paste0(colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-
									(count_vars-1)):length(full_Dark_PTZ[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))],2,function(x) round(x*100))
full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA

Dark_PTZ_predictors<-full_Dark_PTZ[,c(-190,-16)]
#remove columns with all zeros
Dark_PTZ_predictors_scaled<-scale(Dark_PTZ_predictors[,apply(Dark_PTZ_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_PTZ_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots/PTZ/",
					drug,"Control_PTZ.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,14),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
					paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
					paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
			cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
	
	
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.14
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
	
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,14),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.14
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1


}



#check pca models and plots with the turn variables only





#ApoLow

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoLow]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoLow_common<-dataset_full_DarkApoLow[,common_var_Dark_ApoLow]
levels(dataset_full_DarkApoLow_common$Group)<-paste0(levels(dataset_full_DarkApoLow_common$Group),"_DarkApoLow")


full_Dark_ApoLow<-rbind(dataset_full_Dark_common,dataset_full_DarkApoLow_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoLow)))
full_Dark_ApoLow<-cbind(full_Dark_ApoLow,apply(full_Dark_ApoLow[,grep("Length.*Count", colnames(full_Dark_ApoLow))],2,function(x) x/full_Dark_ApoLow$TotalBoutCount))

colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-(count_vars-1)):length(full_Dark_ApoLow[1,]))]<-paste0(colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-
									(count_vars-1)):length(full_Dark_ApoLow[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))],2,function(x) round(x*100))
full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
#take only turn predictors this time

Dark_ApoLow_predictors<-full_Dark_ApoLow[,c(-137,-11)]

Dark_ApoLow_predictors<-Dark_ApoLow_predictors[,-(grep("Count", colnames(Dark_ApoLow_predictors)))]
#remove columns with all zeros
Dark_ApoLow_predictors_scaled<-scale(Dark_ApoLow_predictors[,apply(Dark_ApoLow_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoLow_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1
#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only/ApoLow/",
					drug,"Control_ApoLow.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),2]
	
	
	drug<-substr(drug,1,nchar(drug)-1)
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,10),
				ylim=c(-10,15),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.1
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,10),
				ylim=c(-10,15),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.1
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	
	counter_for_order<-counter_for_order+1
	
}




#ApoHigh
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoHigh]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoHigh_common<-dataset_full_DarkApoHigh[,common_var_Dark_ApoHigh]
levels(dataset_full_DarkApoHigh_common$Group)<-paste0(levels(dataset_full_DarkApoHigh_common$Group),"_DarkApoHigh")


full_Dark_ApoHigh<-rbind(dataset_full_Dark_common,dataset_full_DarkApoHigh_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoHigh)))
full_Dark_ApoHigh<-cbind(full_Dark_ApoHigh,apply(full_Dark_ApoHigh[,grep("Length.*Count", colnames(full_Dark_ApoHigh))],2,function(x) x/full_Dark_ApoHigh$TotalBoutCount))

colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-(count_vars-1)):length(full_Dark_ApoHigh[1,]))]<-paste0(colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-
									(count_vars-1)):length(full_Dark_ApoHigh[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))],2,function(x) round(x*100))
full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA

Dark_ApoHigh_predictors<-full_Dark_ApoHigh[,c(-187,-16)]
#take only turn predictors this time
Dark_ApoHigh_predictors<-Dark_ApoHigh_predictors[,-(grep("Count", colnames(Dark_ApoHigh_predictors)))]

#remove columns with all zeros
Dark_ApoHigh_predictors_scaled<-scale(Dark_ApoHigh_predictors[,apply(Dark_ApoHigh_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoHigh_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only/ApoHigh/",
					drug,"Control_ApoHigh.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.15
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.15
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1
	
	
}





#PTZ
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_PTZ]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkPTZ_common<-dataset_full_DarkPTZ[,common_var_Dark_PTZ]
levels(dataset_full_DarkPTZ_common$Group)<-paste0(levels(dataset_full_DarkPTZ_common$Group),"_DarkPTZ")


full_Dark_PTZ<-rbind(dataset_full_Dark_common,dataset_full_DarkPTZ_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_PTZ)))
full_Dark_PTZ<-cbind(full_Dark_PTZ,apply(full_Dark_PTZ[,grep("Length.*Count", colnames(full_Dark_PTZ))],2,function(x) x/full_Dark_PTZ$TotalBoutCount))

colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-(count_vars-1)):length(full_Dark_PTZ[1,]))]<-paste0(colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-
									(count_vars-1)):length(full_Dark_PTZ[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))],2,function(x) round(x*100))
full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA

Dark_PTZ_predictors<-full_Dark_PTZ[,c(-190,-16)]
#take only turn predictors this time
Dark_PTZ_predictors<-Dark_PTZ_predictors[,-(grep("Count", colnames(Dark_PTZ_predictors)))]

#remove columns with all zeros
Dark_PTZ_predictors_scaled<-scale(Dark_PTZ_predictors[,apply(Dark_PTZ_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_PTZ_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only/PTZ/",
					drug,"Control_PTZ.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,14),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.14
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,14),
				ylim=c(-10,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.14
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1
	
	
}


#check with the stratified variables


#load data
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




#ApoLow

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoLow]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoLow_common<-dataset_full_DarkApoLow[,common_var_Dark_ApoLow]
levels(dataset_full_DarkApoLow_common$Group)<-paste0(levels(dataset_full_DarkApoLow_common$Group),"_DarkApoLow")


full_Dark_ApoLow<-rbind(dataset_full_Dark_common,dataset_full_DarkApoLow_common)


count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoLow)))
full_Dark_ApoLow<-cbind(full_Dark_ApoLow,apply(full_Dark_ApoLow[,grep("Length.*Count", colnames(full_Dark_ApoLow))],2,function(x) x/full_Dark_ApoLow$TotalBoutCount))

colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-(count_vars-1)):length(full_Dark_ApoLow[1,]))]<-paste0(colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-
									(count_vars-1)):length(full_Dark_ApoLow[1,]))],"Proportion")


full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))],2,function(x) round(x*100))
full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
full_Dark_ApoLow[is.na(full_Dark_ApoLow)]<-0

Dark_ApoLow_predictors<-full_Dark_ApoLow[,c(-599,-361)]
#remove columns with all zeros
Dark_ApoLow_predictors_scaled<-scale(Dark_ApoLow_predictors[,apply(Dark_ApoLow_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoLow_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1
#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_stratified/ApoLow/",
					drug,"Control_ApoLow.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),2]
	
	
	drug<-substr(drug,1,nchar(drug)-1)
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-25,10),
				ylim=c(-15,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-25,10),
				ylim=c(-15,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	
	counter_for_order<-counter_for_order+1
	
}





#ApoHigh
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoHigh]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoHigh_common<-dataset_full_DarkApoHigh[,common_var_Dark_ApoHigh]
levels(dataset_full_DarkApoHigh_common$Group)<-paste0(levels(dataset_full_DarkApoHigh_common$Group),"_DarkApoHigh")


full_Dark_ApoHigh<-rbind(dataset_full_Dark_common,dataset_full_DarkApoHigh_common)


count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoHigh)))
full_Dark_ApoHigh<-cbind(full_Dark_ApoHigh,apply(full_Dark_ApoHigh[,grep("Length.*Count", colnames(full_Dark_ApoHigh))],2,function(x) x/full_Dark_ApoHigh$TotalBoutCount))

colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-(count_vars-1)):length(full_Dark_ApoHigh[1,]))]<-paste0(colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-
									(count_vars-1)):length(full_Dark_ApoHigh[1,]))],"Proportion")


full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))],2,function(x) round(x*100))
full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
full_Dark_ApoHigh[is.na(full_Dark_ApoHigh)]<-0
Dark_ApoHigh_predictors<-full_Dark_ApoHigh[,c(-838,-503)]
#remove columns with all zeros
Dark_ApoHigh_predictors_scaled<-scale(Dark_ApoHigh_predictors[,apply(Dark_ApoHigh_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoHigh_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_stratified/ApoHigh/",
					drug,"Control_ApoHigh.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-26,12),
				ylim=c(-16,12),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-26,12),
				ylim=c(-16,12),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1
	
	
}




#PTZ
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_PTZ]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkPTZ_common<-dataset_full_DarkPTZ[,common_var_Dark_PTZ]
levels(dataset_full_DarkPTZ_common$Group)<-paste0(levels(dataset_full_DarkPTZ_common$Group),"_DarkPTZ")


full_Dark_PTZ<-rbind(dataset_full_Dark_common,dataset_full_DarkPTZ_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_PTZ)))
full_Dark_PTZ<-cbind(full_Dark_PTZ,apply(full_Dark_PTZ[,grep("Length.*Count", colnames(full_Dark_PTZ))],2,function(x) x/full_Dark_PTZ$TotalBoutCount))

colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-(count_vars-1)):length(full_Dark_PTZ[1,]))]<-paste0(colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-
									(count_vars-1)):length(full_Dark_PTZ[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))],2,function(x) round(x*100))
full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
full_Dark_PTZ[is.na(full_Dark_PTZ)]<-0

Dark_PTZ_predictors<-full_Dark_PTZ[,c(-859,-514)]


#remove columns with all zeros
Dark_PTZ_predictors_scaled<-scale(Dark_PTZ_predictors[,apply(Dark_PTZ_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_PTZ_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_stratified/PTZ/",
					drug,"Control_PTZ.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-15,26),
				ylim=c(-12,15),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.34
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-15,26),
				ylim=c(-12,15),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.34
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1
	
	
}



#check pca models and plots with the turn variables only






#ApoLow

dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoLow]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoLow_common<-dataset_full_DarkApoLow[,common_var_Dark_ApoLow]
levels(dataset_full_DarkApoLow_common$Group)<-paste0(levels(dataset_full_DarkApoLow_common$Group),"_DarkApoLow")


full_Dark_ApoLow<-rbind(dataset_full_Dark_common,dataset_full_DarkApoLow_common)


count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoLow)))
full_Dark_ApoLow<-cbind(full_Dark_ApoLow,apply(full_Dark_ApoLow[,grep("Length.*Count", colnames(full_Dark_ApoLow))],2,function(x) x/full_Dark_ApoLow$TotalBoutCount))

colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-(count_vars-1)):length(full_Dark_ApoLow[1,]))]<-paste0(colnames(full_Dark_ApoLow)[c((length(full_Dark_ApoLow[1,])-
									(count_vars-1)):length(full_Dark_ApoLow[1,]))],"Proportion")


full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Proportion", colnames(full_Dark_ApoLow)))],2,function(x) round(x*100))
full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))]<-apply(full_Dark_ApoLow[,c(grep("Count", colnames(full_Dark_ApoLow)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
full_Dark_ApoLow[is.na(full_Dark_ApoLow)]<-0

Dark_ApoLow_predictors<-full_Dark_ApoLow[,c(-599,-361)]
Dark_ApoLow_predictors<-Dark_ApoLow_predictors[,-(grep("Count", colnames(Dark_ApoLow_predictors)))]

#remove columns with all zeros
Dark_ApoLow_predictors_scaled<-scale(Dark_ApoLow_predictors[,apply(Dark_ApoLow_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoLow_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1
#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only_stratified/ApoLow/",
					drug,"Control_ApoLow.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoLow$Group),2]
	
	
	drug<-substr(drug,1,nchar(drug)-1)
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-25,10),
				ylim=c(-15,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-25,10),
				ylim=c(-15,10),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hypoactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hypoactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hypoactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	
	counter_for_order<-counter_for_order+1
	
}





#ApoHigh
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_ApoHigh]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkApoHigh_common<-dataset_full_DarkApoHigh[,common_var_Dark_ApoHigh]
levels(dataset_full_DarkApoHigh_common$Group)<-paste0(levels(dataset_full_DarkApoHigh_common$Group),"_DarkApoHigh")


full_Dark_ApoHigh<-rbind(dataset_full_Dark_common,dataset_full_DarkApoHigh_common)


count_vars<-length(grep("Length.*Count", colnames(full_Dark_ApoHigh)))
full_Dark_ApoHigh<-cbind(full_Dark_ApoHigh,apply(full_Dark_ApoHigh[,grep("Length.*Count", colnames(full_Dark_ApoHigh))],2,function(x) x/full_Dark_ApoHigh$TotalBoutCount))

colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-(count_vars-1)):length(full_Dark_ApoHigh[1,]))]<-paste0(colnames(full_Dark_ApoHigh)[c((length(full_Dark_ApoHigh[1,])-
									(count_vars-1)):length(full_Dark_ApoHigh[1,]))],"Proportion")


full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Proportion", colnames(full_Dark_ApoHigh)))],2,function(x) round(x*100))
full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))]<-apply(full_Dark_ApoHigh[,c(grep("Count", colnames(full_Dark_ApoHigh)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
full_Dark_ApoHigh[is.na(full_Dark_ApoHigh)]<-0
Dark_ApoHigh_predictors<-full_Dark_ApoHigh[,c(-838,-503)]
Dark_ApoHigh_predictors<-Dark_ApoHigh_predictors[,-(grep("Count", colnames(Dark_ApoHigh_predictors)))]

#remove columns with all zeros
Dark_ApoHigh_predictors_scaled<-scale(Dark_ApoHigh_predictors[,apply(Dark_ApoHigh_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_ApoHigh_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only_stratified/ApoHigh/",
					drug,"Control_ApoHigh.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_ApoHigh$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-26,12),
				ylim=c(-16,12),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-26,12),
				ylim=c(-16,12),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and hyperactive assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in hyperactive,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in hyperactive,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.26
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1
	
	
}




#PTZ
dataset_full_Dark_common<-dataset_full_Dark[,common_var_Dark_PTZ]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_DarkPTZ_common<-dataset_full_DarkPTZ[,common_var_Dark_PTZ]
levels(dataset_full_DarkPTZ_common$Group)<-paste0(levels(dataset_full_DarkPTZ_common$Group),"_DarkPTZ")


full_Dark_PTZ<-rbind(dataset_full_Dark_common,dataset_full_DarkPTZ_common)

#dont add variables as proportions of total bout count

count_vars<-length(grep("Length.*Count", colnames(full_Dark_PTZ)))
full_Dark_PTZ<-cbind(full_Dark_PTZ,apply(full_Dark_PTZ[,grep("Length.*Count", colnames(full_Dark_PTZ))],2,function(x) x/full_Dark_PTZ$TotalBoutCount))

colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-(count_vars-1)):length(full_Dark_PTZ[1,]))]<-paste0(colnames(full_Dark_PTZ)[c((length(full_Dark_PTZ[1,])-
									(count_vars-1)):length(full_Dark_PTZ[1,]))],"Proportion")


#dont round up the counts and turn proportions in to percents, since it will be scaled
full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Proportion", colnames(full_Dark_PTZ)))],2,function(x) round(x*100))
full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))]<-apply(full_Dark_PTZ[,c(grep("Count", colnames(full_Dark_PTZ)))],2,function(x) round(x))

#make a dataframe of predictors only and scale for PCA
full_Dark_PTZ[is.na(full_Dark_PTZ)]<-0

Dark_PTZ_predictors<-full_Dark_PTZ[,c(-859,-514)]
Dark_PTZ_predictors<-Dark_PTZ_predictors[,-(grep("Count", colnames(Dark_PTZ_predictors)))]


#remove columns with all zeros
Dark_PTZ_predictors_scaled<-scale(Dark_PTZ_predictors[,apply(Dark_PTZ_predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-prcomp(Dark_PTZ_predictors_scaled, center = TRUE, scale. = TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

counter_for_order<-1

#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only_stratified/PTZ/",
					drug,"Control_PTZ.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),1]
	PCA2_Drug_Controls<-pca_model[[5]][grep(paste0(drug,"|Control"),full_Dark_PTZ$Group),2]
	
	drug<-substr(drug,1,nchar(drug)-1)
	
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	if(counter_for_order<5){
		
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-15,26),
				ylim=c(-12,15),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#14:26 is 1microM in healthy
		points(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=19)
		
		#27:39 is 3microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=17)
		
		#40:52 is control in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=5,col=colrs,pch="*")
		
		#53:65 is 10microM in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=2,col=colrs3,pch=15)
		
		#66:78 is 1microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=19)
		
		#79:91 is 3microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=17)
		
		#92:104 is control in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.34
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-15,26),
				ylim=c(-12,15),xlab="PCA1",ylab="PCA2",
				main=paste0("Principal components of ",drug," and control\n in healthy and convulsant assays."))
		
		#27:39 is 1microM in healthy
		points(PCA1_Drug_Controls[27:39], PCA2_Drug_Controls[27:39],cex=2,col=colrs2,pch=19)
		
		#40:52 is 3microM in healthy
		points(PCA1_Drug_Controls[40:52], PCA2_Drug_Controls[40:52],cex=2,col=colrs2,pch=17)
		
		#1:13 is control in healthy
		points(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=5,col=colrs,pch="*")
		
		#66:78 is 10microM in disease
		points(PCA1_Drug_Controls[66:78], PCA2_Drug_Controls[66:78],cex=2,col=colrs3,pch=15)
		
		#79:91 is 1microM in disease
		points(PCA1_Drug_Controls[79:91], PCA2_Drug_Controls[79:91],cex=2,col=colrs3,pch=19)
		
		#92:104 is 3microM in disease
		points(PCA1_Drug_Controls[92:104], PCA2_Drug_Controls[92:104],cex=2,col=colrs3,pch=17)
		
		#53:65 is control in disease
		points(PCA1_Drug_Controls[53:65], PCA2_Drug_Controls[53:65],cex=5,col=colrs4,pch="*")
		
		
		leg<-legend("topright", inset=c(-0.32,0), 
				c(paste0("     10 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     1 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 microM ",drug," in convulsant,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in convulsant,\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.34
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	counter_for_order<-counter_for_order+1
	
	
}
