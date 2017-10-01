library(FactoMineR)
library(scales)


colrs<-c("gray90","gray85","gray80","gray75","gray70","gray65","gray60","gray55","gray50","gray45"
		,"gray40","gray35","gray30")

colrs2<-c("darkolivegreen1","darkolivegreen1","darkolivegreen2","darkolivegreen2","darkolivegreen3","darkolivegreen3",
		"darkolivegreen4","darkolivegreen4","darkolivegreen","darkolivegreen","darkgreen","darkgreen","darkgreen")

colrs3<-c("paleturquoise","paleturquoise","paleturquoise1","paleturquoise1","paleturquoise2","paleturquoise2","paleturquoise3","paleturquoise3",
		"paleturquoise4","paleturquoise4","cyan4","cyan4","cyan4")

colrs4<-c("navajowhite1","navajowhite1","navajowhite1","orange","orange","orange1","orange1","orange2","orange2","orange3","orange3",
		"orange4","orange4")

colrs_all<-c("cadetblue3","aquamarine4","blueviolet","darkgoldenrod4","brown4","darkgoldenrod1","deeppink1","thistle3","blue","lemonchiffon3","greenyellow","aquamarine")

args<-commandArgs(trailingOnly = TRUE)
condition<-args[1]

state="hypoactive"

if(condition=="DarkApoHigh"){
	
	state="hyperactive type I"
	
	
}else if(condition=="DarkPTZ"){
	
	state="hyperactive type II"
	
}


#load data
dataset_full_Dark<-read.table("Dark_final_dataset_full.txt",header=TRUE)
dataset_full_Dark$Group<-factor(dataset_full_Dark$Group)

dataset_full_Disease<-read.table(paste0(condition,"_final_dataset_full.txt"),header=TRUE)
dataset_full_Disease$Group<-factor(dataset_full_Disease$Group)

common_var<-intersect(colnames(dataset_full_Dark),colnames(dataset_full_Disease))


dataset_full_Dark_common<-dataset_full_Dark[,common_var]
levels(dataset_full_Dark_common$Group)<-paste0(levels(dataset_full_Dark_common$Group), "_Dark")

dataset_full_Disease_common<-dataset_full_Disease[,common_var]
levels(dataset_full_Disease_common$Group)<-paste0(levels(dataset_full_Disease_common$Group),paste0("_",condition))


full_together<-rbind(dataset_full_Dark_common,dataset_full_Disease_common)

#add variables as proportions of total bout count
count_vars<-length(grep("Length.*Count", colnames(full_together)))
full_together<-cbind(full_together,apply(full_together[,grep("Length.*Count", colnames(full_together))],2,function(x) x/full_together$TotalBoutCount))

colnames(full_together)[c((length(full_together[1,])-(count_vars-1)):length(full_together[1,]))]<-paste0(colnames(full_together)[c((length(full_together[1,])-
									(count_vars-1)):length(full_together[1,]))],"Proportion")


#round up the counts and turn proportions in to percents
full_together[,c(grep("Proportion", colnames(full_together)))]<-apply(full_together[,c(grep("Proportion", colnames(full_together)))],2,function(x) round(x*100))
full_together[,c(grep("Count", colnames(full_together)))]<-apply(full_together[,c(grep("Count", colnames(full_together)))],2,function(x) round(x))


#make a dataframe of predictors only and scale for PCA
#each condition has different indexes:

if(condition=="DarkApoLow"){
	
	predictors<-full_together[,c(-137,-11)]
	
}else if(condition=="DarkApoHigh"){
	
	predictors<-full_together[,c(-187,-16)]
	
}else{
	
	predictors<-full_together[,c(-190,-16)]
	
}
#also remove the redundant counts per length, since proportions are to be used, but leave total bout count
predictors<-predictors[,-grep("^Length.*Count$",colnames(predictors))]

#remove columns with all zeros and scale
predictors_scaled<-scale(predictors[,apply(predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-PCA(predictors_scaled, graph = FALSE, scale.unit=TRUE)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")
names_drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP3_","PCAP4_")


#first plot the controls
#healthy
xlimit0<--5
xlimit1<-9
ylimit0<--5.5
ylimit1<-5

#xlimit0<-min(pca_model$ind$coord[,1])
#xlimit1<-max(pca_model$ind$coord[,1])
#ylimit0<-min(pca_model$ind$coord[,2])
#ylimit1<-max(pca_model$ind$coord[,2])




PC1_control_healthy<-mean(pca_model$ind$coord[grep("Control_Dark$",full_together$Group),1])
PC2_control_healthy<-mean(pca_model$ind$coord[grep("Control_Dark$",full_together$Group),2])
png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots/",condition,"/All_together_doses_",condition,".png"),width=1500,height=750)
par(mar=c(5, 5, 5, 21), xpd=TRUE)
plot(PC1_control_healthy,PC2_control_healthy,xlim=c(xlimit0,xlimit1),ylim=c(ylimit0,ylimit1),
				pch=19, cex=6, col="grey30", xlab="PC1", ylab="PC2", main=paste0("PCA based plot of all compounds and their\ndirection in dosage examined in the ",state," condition"))

#disease
PC1_control_disease<-mean(pca_model$ind$coord[grep("Control_Dark..*",full_together$Group),1])
PC2_control_disease<-mean(pca_model$ind$coord[grep("Control_Dark..*",full_together$Group),2])
points(PC1_control_disease,PC2_control_disease, pch=15,cex=6,col="orangered2")

drug_count<-1
for(drug in drugs[-7]){
	#1microM
	dose<-"1microM_"
	PC1_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
	PC2_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	dose<-"3microM_"
	PC1_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
	PC2_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	dose<-"10microM_"
	PC1_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
	PC2_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	points(PC1_control_drug_1,PC2_control_drug_1, pch=19, cex=4, col=alpha(colrs_all[drug_count], 0.8))
	lines(c(PC1_control_drug_1,PC1_control_drug_3),c(PC2_control_drug_1,PC2_control_drug_3), lw=2, col=alpha(colrs_all[drug_count], 0.8))
	arrows(x0=PC1_control_drug_3,y0=PC2_control_drug_3,x1=PC1_control_drug_10,y1=PC2_control_drug_10,col=alpha(colrs_all[drug_count], 0.8),lw=2)
	
	dose<-"1microM_"
	PC1_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
	PC2_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	dose<-"3microM_"
	PC1_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
	PC2_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
	dose<-"10microM_"
	PC1_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
	PC2_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
	points(PC1_control_drug_1_d,PC2_control_drug_1_d, pch=15, cex=4, col=alpha(colrs_all[drug_count], 0.8))
	lines(c(PC1_control_drug_1_d,PC1_control_drug_3_d),c(PC2_control_drug_1_d,PC2_control_drug_3_d), lw=2, col=alpha(colrs_all[drug_count], 0.8))
	arrows(x0=PC1_control_drug_3_d,y0=PC2_control_drug_3_d,x1=PC1_control_drug_10_d,y1=PC2_control_drug_10_d,col=alpha(colrs_all[drug_count], 0.8),lw=2)
	
	
	drug_count<-drug_count+1
	
}
#NDMCIHigh
drug<-"NDMCHigh_"
dose<-"25microM_"
PC1_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
PC2_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
dose<-"50microM_"
PC1_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
PC2_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
dose<-"100microM_"
PC1_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
PC2_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
points(PC1_control_drug_1,PC2_control_drug_1, pch=19, cex=4, col=alpha(colrs_all[12], 0.8))
lines(c(PC1_control_drug_1,PC1_control_drug_3),c(PC2_control_drug_1,PC2_control_drug_3), lw=2, col=alpha(colrs_all[12], 0.8))
arrows(x0=PC1_control_drug_3,y0=PC2_control_drug_3,x1=PC1_control_drug_10,y1=PC2_control_drug_10,col=alpha(colrs_all[12], 0.8),lw=2)

dose<-"25microM_"
PC1_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
PC2_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
dose<-"50microM_"
PC1_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
PC2_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
dose<-"100microM_"
PC1_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
PC2_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
points(PC1_control_drug_1_d,PC2_control_drug_1_d, pch=15, cex=4, col=alpha(colrs_all[12], 0.8))
lines(c(PC1_control_drug_1_d,PC1_control_drug_3_d),c(PC2_control_drug_1_d,PC2_control_drug_3_d), lw=2, col=alpha(colrs_all[12], 0.8))
arrows(x0=PC1_control_drug_3_d,y0=PC2_control_drug_3_d,x1=PC1_control_drug_10_d,y1=PC2_control_drug_10_d,col=alpha(colrs_all[12], 0.8),lw=2)


leg<-legend("topright", inset=c(-0.27,0), 
		c("mean Aripiprazole in time","mean Cariprazine in time","mean Clozapine in time","mean CNO in time","mean Haloperidol in time","mean NDMC in time","mean OSU6162 in time","mean PCAP1 in time",
				"mean PCAP2 in time","mean PCAP3 in time","mean PCAP4 in time", "mean NDMCHigh in time", "healthy control", "disease control","healthy","disease induced"), 
		col=c(colrs_all, "grey30", "orangered2","black","black"), pch = c(rep(19,times=13), 15, 1, 0),cex=1.7)


dev.off()



#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)
counter_for_order<-1
#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots/",condition,"/",
					drug,"Control_",condition,".png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model$ind$coord[grep(paste0(drug,"|Control"),full_together$Group),1]
	PCA2_Drug_Controls<-pca_model$ind$coord[grep(paste0(drug,"|Control"),full_together$Group),2]
	
	
	drug<-substr(names_drugs[counter_for_order],1,nchar(names_drugs[counter_for_order])-1)
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,15),xlab="PC1",ylab="PC2",
				main=paste0("Principal components of ",drug," and control\n in healthy and ",state," assays."))
		
		
		
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
				c(paste0("     10 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     Control in ",state,",\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.2
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,15),xlab="PC1",ylab="PC2",
				main=paste0("Principal components of ",drug," and control\n in healthy and ",state," assays."))
		
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
				c(paste0("     10 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     Control in ",state,",\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.2
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	
	counter_for_order<-counter_for_order+1
	
}

#save the list of most important variables in order
order_important_vars<-names(rev(apply(pca_model$var$contrib,1,sum)[order(apply(pca_model$var$contrib,1,sum))]))
write(order_important_vars,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/final_dataset/B2/PCA_important_variables_",condition,".txt"))

#create a vector for distances
drugs_distances_mean<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))
drugs_distances_std<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))
drugs_distances_min<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))
drugs_distances_max<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))

names(drugs_distances_mean)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]
names(drugs_distances_std)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]
names(drugs_distances_min)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]
names(drugs_distances_max)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]

PC1_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),1]
PC2_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),2]
PC3_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),3]
PC4_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),4]
PC5_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),5]


for(compound in names(drugs_distances_mean)){
	
	PC1_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),1]
	PC2_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),2]
	PC3_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),3]
	PC4_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),4]
	PC5_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),5]
	
	distance<-sqrt( (PC1_control_healthy-PC1_compound)^2 + (PC2_control_healthy-PC2_compound)^2 + (PC3_control_healthy-PC3_compound)^2 + (PC4_control_healthy-PC4_compound)^2
					+ (PC5_control_healthy-PC5_compound)^2)
	
	drugs_distances_mean[compound]<-mean(distance)
	drugs_distances_std[compound]<-sd(distance)
	drugs_distances_min[compound]<-min(distance)
	drugs_distances_max[compound]<-max(distance)
	
}

drugs_distances_std<-round(drugs_distances_std[order(drugs_distances_mean)],2)
drugs_distances_min<-round(drugs_distances_min[order(drugs_distances_mean)],2)
drugs_distances_max<-round(drugs_distances_max[order(drugs_distances_mean)],2)
drugs_distances_mean<-round(drugs_distances_mean[order(drugs_distances_mean)],2)

csv_set_H<-cbind(drugs_distances_mean[grep("Dark$",names(drugs_distances_mean))],drugs_distances_std[grep("Dark$",names(drugs_distances_std))],
		drugs_distances_min[grep("Dark$",names(drugs_distances_min))], drugs_distances_max[grep("Dark$",names(drugs_distances_max))])
csv_set_H<-as.data.frame(csv_set_H)
colnames(csv_set_H)<-c("mean","SD","MIN","MAX")
rownames(csv_set_H)<-names(drugs_distances_mean[grep("Dark$",names(drugs_distances_mean))])

csv_set_D<-cbind(drugs_distances_mean[-grep("Dark$",names(drugs_distances_mean))],drugs_distances_std[-grep("Dark$",names(drugs_distances_std))],
		drugs_distances_min[-grep("Dark$",names(drugs_distances_min))], drugs_distances_max[-grep("Dark$",names(drugs_distances_max))])
csv_set_D<-as.data.frame(csv_set_D)
colnames(csv_set_D)<-c("mean","SD","MIN","MAX")
rownames(csv_set_D)<-names(drugs_distances_mean[-grep("Dark$",names(drugs_distances_mean))])

write.table(csv_set_H, file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/healthy_",condition,"set1.csv"),row.names=T,col.names=T)
write.table(csv_set_D, file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/disease_",condition,"set1.csv"),row.names=T,col.names=T)

#save the ranked compounds
write.table(drugs_distances_mean[grep("Dark$",names(drugs_distances_mean))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/mean_in_healthy_",condition,"set1.txt"),row.names=T,col.names=F)
write.table(drugs_distances_std[grep("Dark$",names(drugs_distances_std))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/std_in_healthy_",condition,"set1.txt"),row.names=T,col.names=F)
write.table(drugs_distances_min[grep("Dark$",names(drugs_distances_min))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/min_in_healthy_",condition,"set1.txt"),row.names=T,col.names=F)
write.table(drugs_distances_max[grep("Dark$",names(drugs_distances_max))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/max_in_healthy_",condition,"set1.txt"),row.names=T,col.names=F)

write.table(drugs_distances_mean[-grep("Dark$",names(drugs_distances_mean))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/mean_in_disease_",condition,"set1.txt"),row.names=T,col.names=F)
write.table(drugs_distances_std[-grep("Dark$",names(drugs_distances_std))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/std_in_disease_",condition,"set1.txt"),row.names=T,col.names=F)
write.table(drugs_distances_min[-grep("Dark$",names(drugs_distances_min))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/min_in_disease_",condition,"set1.txt"),row.names=T,col.names=F)
write.table(drugs_distances_max[-grep("Dark$",names(drugs_distances_max))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/max_in_disease_",condition,"set1.txt"),row.names=T,col.names=F)



#make a newick tree file for the first 5 PC
library(reshape)
PC_dataset<-as.data.frame(pca_model$ind$coord)
colnames(PC_dataset)<-c("PC1","PC2","PC3","PC4","PC5")
PC_dataset$Group<-full_together$Group
PC_dataset$TimeFactor<-full_together$TimeFactor

#reshape, start with healthy control
vars<-colnames(PC_dataset)[1]
reshaped_together<-cast(PC_dataset[PC_dataset$Group=="Control_Dark",c("TimeFactor","Group",vars)],Group~TimeFactor)

colnames(reshaped_together)[2:14]<-paste0(vars,1:13)
reshaped_together$Group<-"Control_healthy"

for (vars in colnames(PC_dataset)[-grep("TimeFactor|Group",colnames(PC_dataset))][-1]){
	
	reshaped_together<-cbind(reshaped_together, cast(PC_dataset[PC_dataset$Group=="Control_Dark",c("TimeFactor","Group",vars)],Group~TimeFactor)[-1])
	len1<-length(colnames(reshaped_together))-12
	len2<-length(colnames(reshaped_together))
	colnames(reshaped_together)[len1:len2]<-paste0(vars,1:13)
	
}

#add the rest
for (groupy in unique(PC_dataset$Group)[-grep("Control_Dark$",unique(PC_dataset$Group))]){
	
	vars<-colnames(PC_dataset)[-grep("TimeFactor|Group",colnames(PC_dataset))][1]
	reshaped_together_temp<-cast(PC_dataset[PC_dataset$Group==groupy,c("TimeFactor","Group",vars)],Group~TimeFactor)
	
	colnames(reshaped_together_temp)[2:14]<-paste0(vars,1:13)
	
	if(grepl("Dark$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-4),"healthy")
		
	}else if (grepl("DarkApoLow$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-10),state)
		
	}else if (grepl("DarkApoHigh$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-11),state)
		
	}else if (grepl("DarkPTZ$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-7),state)
		
	}
	
	
	for (vars in colnames(PC_dataset)[-grep("TimeFactor|Group",colnames(PC_dataset))][-1]){
		
		reshaped_together_temp<-cbind(reshaped_together_temp, cast(PC_dataset[PC_dataset$Group==groupy,c("TimeFactor","Group",vars)],Group~TimeFactor)[-1])
		len1<-length(colnames(reshaped_together_temp))-12
		len2<-length(colnames(reshaped_together_temp))
		colnames(reshaped_together_temp)[len1:len2]<-paste0(vars,1:13)
		
	}
	
	reshaped_together<-rbind(reshaped_together,reshaped_together_temp)
	
}


reshaped_together<-reshaped_together[,apply(reshaped_together,2,function(x) length(x[x!=0])>0)]

#save scaled
reshaped_together<-cbind(reshaped_together[,1],scale(reshaped_together[,-1]))
colnames(reshaped_together)[1]<-"Group"
write.table(reshaped_together,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/clusters/",condition,"_set1_cluster_scaled_PCA.txt"),row.names=F,col.names=T)

library(ctc)
#save as tree
reshaped_together<-as.data.frame(reshaped_together)
new_rownames<-gsub("PCAP814","PCAP3",reshaped_together$Group)
new_rownames<-gsub("PCAP931","PCAP4",new_rownames)
rownames(reshaped_together)<-new_rownames

reshaped_together<-reshaped_together[,-1]
reshaped_together<-reshaped_together[,apply(reshaped_together,2,function(x) length(x[is.na(x)])==0)]

d <- dist(reshaped_together)

hc <- hclust(d) 

write(hc2Newick(hc),file=paste0(condition,'set1_PCA.newick'))





#check pca models and plots with the turn variables only




#also remove the redundant counts per length, since proportions are to be used
predictors<-predictors[,-grep("Count",colnames(predictors))]
predictors_scaled<-scale(predictors[,apply(predictors,2,function(x) length(x[x!=0])>0)])

pca_model<-PCA(predictors_scaled, graph = FALSE, scale.unit=TRUE) 

#plot , the PCA1 and PCA2 are in the [[5]] list in the first and second column in the same order as input(hopefully)

drugs<-c("Aripiprazole_","Cariprazine_","Clozapine_","CNO_","Haloperidol_","NDMC_","NDMCHigh_","OSU6162_","PCAP1_","PCAP2_","PCAP814_","PCAP931_")

#create a vector for distances
drugs_distances_mean<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))
drugs_distances_std<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))
drugs_distances_min<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))
drugs_distances_max<-numeric(length(unique(full_together$Group)[-grep("Control",unique(full_together$Group))]))

names(drugs_distances_mean)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]
names(drugs_distances_std)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]
names(drugs_distances_min)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]
names(drugs_distances_max)<-unique(full_together$Group)[-grep("Control",unique(full_together$Group))]

PC1_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),1]
PC2_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),2]
PC3_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),3]
PC4_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),4]
PC5_control_healthy<-pca_model$ind$coord[grep("Control_Dark$",full_together$Group),5]


for(compound in names(drugs_distances_mean)){
	
	PC1_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),1]
	PC2_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),2]
	PC3_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),3]
	PC4_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),4]
	PC5_compound<-pca_model$ind$coord[grep(paste0(compound,"$"),full_together$Group),5]
	
	distance<-sqrt( (PC1_control_healthy-PC1_compound)^2 + (PC2_control_healthy-PC2_compound)^2 + (PC3_control_healthy-PC3_compound)^2 + (PC4_control_healthy-PC4_compound)^2
					+ (PC5_control_healthy-PC5_compound)^2)
	
	drugs_distances_mean[compound]<-mean(distance)
	drugs_distances_std[compound]<-sd(distance)
	drugs_distances_min[compound]<-min(distance)
	drugs_distances_max[compound]<-max(distance)
	
}

drugs_distances_std<-round(drugs_distances_std[order(drugs_distances_mean)],2)
drugs_distances_min<-round(drugs_distances_min[order(drugs_distances_mean)],2)
drugs_distances_max<-round(drugs_distances_max[order(drugs_distances_mean)],2)
drugs_distances_mean<-round(drugs_distances_mean[order(drugs_distances_mean)],2)

csv_set_H<-cbind(drugs_distances_mean[grep("Dark$",names(drugs_distances_mean))],drugs_distances_std[grep("Dark$",names(drugs_distances_std))],
		drugs_distances_min[grep("Dark$",names(drugs_distances_min))], drugs_distances_max[grep("Dark$",names(drugs_distances_max))])
csv_set_H<-as.data.frame(csv_set_H)
colnames(csv_set_H)<-c("mean","SD","MIN","MAX")
rownames(csv_set_H)<-names(drugs_distances_mean[grep("Dark$",names(drugs_distances_mean))])

csv_set_D<-cbind(drugs_distances_mean[-grep("Dark$",names(drugs_distances_mean))],drugs_distances_std[-grep("Dark$",names(drugs_distances_std))],
		drugs_distances_min[-grep("Dark$",names(drugs_distances_min))], drugs_distances_max[-grep("Dark$",names(drugs_distances_max))])
csv_set_D<-as.data.frame(csv_set_D)
colnames(csv_set_D)<-c("mean","SD","MIN","MAX")
rownames(csv_set_D)<-names(drugs_distances_mean[-grep("Dark$",names(drugs_distances_mean))])

write.table(csv_set_H, file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/healthy_",condition,"set2.csv"),row.names=T,col.names=T)
write.table(csv_set_D, file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/disease_",condition,"set2.csv"),row.names=T,col.names=T)


#save the ranked compounds
write.table(drugs_distances_mean[grep("Dark$",names(drugs_distances_mean))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/mean_in_healthy_",condition,"set2.txt"),row.names=T,col.names=F)
write.table(drugs_distances_std[grep("Dark$",names(drugs_distances_std))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/std_in_healthy_",condition,"set2.txt"),row.names=T,col.names=F)
write.table(drugs_distances_min[grep("Dark$",names(drugs_distances_min))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/min_in_healthy_",condition,"set2.txt"),row.names=T,col.names=F)
write.table(drugs_distances_max[grep("Dark$",names(drugs_distances_max))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/max_in_healthy_",condition,"set2.txt"),row.names=T,col.names=F)

write.table(drugs_distances_mean[-grep("Dark$",names(drugs_distances_mean))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/mean_in_disease_",condition,"set2.txt"),row.names=T,col.names=F)
write.table(drugs_distances_std[-grep("Dark$",names(drugs_distances_std))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/std_in_disease_",condition,"set2.txt"),row.names=T,col.names=F)
write.table(drugs_distances_min[-grep("Dark$",names(drugs_distances_min))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/min_in_disease_",condition,"set2.txt"),row.names=T,col.names=F)
write.table(drugs_distances_max[-grep("Dark$",names(drugs_distances_max))],
		file=paste0("~/git/zebrafish_action_sequence_project/results/output/ranked_drugs_PCA/max_in_disease_",condition,"set2.txt"),row.names=T,col.names=F)




counter_for_order<-1
#go through all drugs
for(drug in drugs){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots_turn_only/",condition,"/",
					drug,"Control_",condition,"_turn_only.png"),width=1500,height=750)
	
	PCA1_Drug_Controls<-pca_model$ind$coord[grep(paste0(drug,"|Control"),full_together$Group),1]
	PCA2_Drug_Controls<-pca_model$ind$coord[grep(paste0(drug,"|Control"),full_together$Group),2]
	
	
	drug<-substr(names_drugs[counter_for_order],1,nchar(names_drugs[counter_for_order])-1)
	par(mar=c(5, 5, 5, 25), xpd=TRUE)
	
	
	if(counter_for_order<5){
		
		#1:13 is 10microM in healthy
		plot(PCA1_Drug_Controls[1:13], PCA2_Drug_Controls[1:13],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,15),xlab="PC1",ylab="PC2",
				main=paste0("Principal components of ",drug," and control\n in healthy and ",state," assays."))
		
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
				c(paste0("     10 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     Control in ",state,",\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.2
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
	}else{
		
		
		#14:26 is 10microM in healthy
		plot(PCA1_Drug_Controls[14:26], PCA2_Drug_Controls[14:26],cex=2,col=colrs2,pch=15,xlim=c(-10,15),
				ylim=c(-10,15),xlab="PC1",ylab="PC2",
				main=paste0("Principal components of ",drug," and control\n in healthy and ",state," assays."))
		
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
				c(paste0("     10 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in healthy,\n     timeframes 0 to 65 minutes\n"),paste0("     Control in healthy,\n    timeframes 0 to 65 minutes\n"),
						paste0("     10 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     1 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),
						paste0("     3 uM ",drug," in ",state,",\n     timeframes 0 to 65 minutes\n"),paste0("     Control in ",state,",\n     timeframes 0 to 65 minutes\n")), 
				cex=1.3, col=c(colrs2[1], colrs2[1], colrs2[1], colrs[1], colrs3[1], colrs3[1], colrs3[1], colrs4[1]), pch = c(15,19,17,8,15,19,17,8))
		
		
		x_p1<-leg$text$x-0.07
		x_p2<-leg$text$x+0.2
		y_p<-leg$text$y
		points(x_p1,y_p,col=c(colrs2[7], colrs2[7], colrs2[7], colrs[7], colrs3[7], colrs3[7], colrs3[7], colrs4[7]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		points(x_p2,y_p,col=c(colrs2[13], colrs2[13], colrs2[13], colrs[13], colrs3[13], colrs3[13], colrs3[13], colrs4[13]), pch = c(15,19,17,8,15,19,17,8),cex=1.3)
		
		dev.off()
		
		
	}
	
	
	
	counter_for_order<-counter_for_order+1
	
}


#save the list of most important variables in order
order_important_vars<-names(rev(apply(pca_model$var$contrib,1,sum)[order(apply(pca_model$var$contrib,1,sum))]))
write(order_important_vars,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/final_dataset/B2/PCA_important_variables_",condition,"_turn_only.txt"))


#make a newick tree file for the first 5 PC
PC_dataset<-as.data.frame(pca_model$ind$coord)
colnames(PC_dataset)<-c("PC1","PC2","PC3","PC4","PC5")
PC_dataset$Group<-full_together$Group
PC_dataset$TimeFactor<-full_together$TimeFactor

#reshape, start with healthy control
vars<-colnames(PC_dataset)[1]
reshaped_together<-cast(PC_dataset[PC_dataset$Group=="Control_Dark",c("TimeFactor","Group",vars)],Group~TimeFactor)

colnames(reshaped_together)[2:14]<-paste0(vars,1:13)
reshaped_together$Group<-"Control_healthy"

for (vars in colnames(PC_dataset)[-grep("TimeFactor|Group",colnames(PC_dataset))][-1]){
	
	reshaped_together<-cbind(reshaped_together, cast(PC_dataset[PC_dataset$Group=="Control_Dark",c("TimeFactor","Group",vars)],Group~TimeFactor)[-1])
	len1<-length(colnames(reshaped_together))-12
	len2<-length(colnames(reshaped_together))
	colnames(reshaped_together)[len1:len2]<-paste0(vars,1:13)
	
}

#add the rest
for (groupy in unique(PC_dataset$Group)[-grep("Control_Dark$",unique(PC_dataset$Group))]){
	
	vars<-colnames(PC_dataset)[-grep("TimeFactor|Group",colnames(PC_dataset))][1]
	reshaped_together_temp<-cast(PC_dataset[PC_dataset$Group==groupy,c("TimeFactor","Group",vars)],Group~TimeFactor)
	
	colnames(reshaped_together_temp)[2:14]<-paste0(vars,1:13)
	
	if(grepl("Dark$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-4),"healthy")
		
	}else if (grepl("DarkApoLow$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-10),state)
		
	}else if (grepl("DarkApoHigh$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-11),state)
		
	}else if (grepl("DarkPTZ$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-7),state)
		
	}
	
	
	for (vars in colnames(PC_dataset)[-grep("TimeFactor|Group",colnames(PC_dataset))][-1]){
		
		reshaped_together_temp<-cbind(reshaped_together_temp, cast(PC_dataset[PC_dataset$Group==groupy,c("TimeFactor","Group",vars)],Group~TimeFactor)[-1])
		len1<-length(colnames(reshaped_together_temp))-12
		len2<-length(colnames(reshaped_together_temp))
		colnames(reshaped_together_temp)[len1:len2]<-paste0(vars,1:13)
		
	}
	
	reshaped_together<-rbind(reshaped_together,reshaped_together_temp)
	
}


reshaped_together<-reshaped_together[,apply(reshaped_together,2,function(x) length(x[x!=0])>0)]

#save scaled
reshaped_together<-cbind(reshaped_together[,1],scale(reshaped_together[,-1]))
colnames(reshaped_together)[1]<-"Group"
write.table(reshaped_together,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/clusters/",condition,"_set2_cluster_scaled_PCA.txt"),row.names=F,col.names=T)

#save as tree
reshaped_together<-as.data.frame(reshaped_together)
new_rownames<-gsub("PCAP814","PCAP3",reshaped_together$Group)
new_rownames<-gsub("PCAP931","PCAP4",new_rownames)
rownames(reshaped_together)<-new_rownames

reshaped_together<-reshaped_together[,-1]
reshaped_together<-reshaped_together[,apply(reshaped_together,2,function(x) length(x[is.na(x)])==0)]

d <- dist(reshaped_together)

hc <- hclust(d) 

write(hc2Newick(hc),file=paste0(condition,'set2_PCA.newick'))

#make vector plots per dose, otherwise unvisible


#first plot the controls
#healthy
PC1_control_healthy<-mean(pca_model$ind$coord[grep("Control_Dark$",full_together$Group),1])
PC2_control_healthy<-mean(pca_model$ind$coord[grep("Control_Dark$",full_together$Group),2])
png(paste0("~/git/zebrafish_action_sequence_project/results/plots/final_analysis/PCA_plots/",condition,"/All_together_doses_",condition,"set2.png"),width=1500,height=750)
par(mar=c(5, 5, 5, 21), xpd=TRUE)
plot(PC1_control_healthy,PC2_control_healthy,xlim=c(xlimit0,xlimit1),ylim=c(ylimit0,ylimit1)
	, pch=19, cex=6, col="grey30", xlab="PC1", ylab="PC2", main=paste0("PCA based plot of all compounds and their\ndirection in dosage examined in the ",state," condition"))

#disease
PC1_control_disease<-mean(pca_model$ind$coord[grep("Control_Dark..*",full_together$Group),1])
PC2_control_disease<-mean(pca_model$ind$coord[grep("Control_Dark..*",full_together$Group),2])
points(PC1_control_disease,PC2_control_disease, pch=15,cex=6,col="orangered2")

drug_count<-1
for(drug in drugs[-7]){
	#1microM
	dose<-"1microM_"
	PC1_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
	PC2_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	dose<-"3microM_"
	PC1_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
	PC2_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	dose<-"10microM_"
	PC1_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
	PC2_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	points(PC1_control_drug_1,PC2_control_drug_1, pch=19, cex=4, col=alpha(colrs_all[drug_count], 0.8))
	lines(c(PC1_control_drug_1,PC1_control_drug_3),c(PC2_control_drug_1,PC2_control_drug_3), lw=2, col=alpha(colrs_all[drug_count], 0.8))
	arrows(x0=PC1_control_drug_3,y0=PC2_control_drug_3,x1=PC1_control_drug_10,y1=PC2_control_drug_10,col=alpha(colrs_all[drug_count], 0.8),lw=2)
	
	dose<-"1microM_"
	PC1_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
	PC2_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
	dose<-"3microM_"
	PC1_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
	PC2_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
	dose<-"10microM_"
	PC1_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
	PC2_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
	points(PC1_control_drug_1_d,PC2_control_drug_1_d, pch=15, cex=4, col=alpha(colrs_all[drug_count], 0.8))
	lines(c(PC1_control_drug_1_d,PC1_control_drug_3_d),c(PC2_control_drug_1_d,PC2_control_drug_3_d), lw=2, col=alpha(colrs_all[drug_count], 0.8))
	arrows(x0=PC1_control_drug_3_d,y0=PC2_control_drug_3_d,x1=PC1_control_drug_10_d,y1=PC2_control_drug_10_d,col=alpha(colrs_all[drug_count], 0.8),lw=2)
	
	
	drug_count<-drug_count+1
	
}
#NDMCIHigh
drug<-"NDMCHigh_"
dose<-"25microM_"
PC1_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
PC2_control_drug_1<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
dose<-"50microM_"
PC1_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
PC2_control_drug_3<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
dose<-"100microM_"
PC1_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),1])
PC2_control_drug_10<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
points(PC1_control_drug_1,PC2_control_drug_1, pch=19, cex=4, col=alpha(colrs_all[12], 0.8))
lines(c(PC1_control_drug_1,PC1_control_drug_3),c(PC2_control_drug_1,PC2_control_drug_3), lw=2, col=alpha(colrs_all[12], 0.8))
arrows(x0=PC1_control_drug_3,y0=PC2_control_drug_3,x1=PC1_control_drug_10,y1=PC2_control_drug_10,col=alpha(colrs_all[12], 0.8),lw=2)

dose<-"25microM_"
PC1_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
PC2_control_drug_1_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark$"),full_together$Group),2])
dose<-"50microM_"
PC1_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
PC2_control_drug_3_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
dose<-"100microM_"
PC1_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),1])
PC2_control_drug_10_d<-mean(pca_model$ind$coord[grep(paste0(drug,dose,"Dark..*"),full_together$Group),2])
points(PC1_control_drug_1_d,PC2_control_drug_1_d, pch=15, cex=4, col=alpha(colrs_all[12], 0.8))
lines(c(PC1_control_drug_1_d,PC1_control_drug_3_d),c(PC2_control_drug_1_d,PC2_control_drug_3_d), lw=2, col=alpha(colrs_all[12], 0.8))
arrows(x0=PC1_control_drug_3_d,y0=PC2_control_drug_3_d,x1=PC1_control_drug_10_d,y1=PC2_control_drug_10_d,col=alpha(colrs_all[12], 0.8),lw=2)


leg<-legend("topright", inset=c(-0.27,0), 
		c("mean Aripiprazole in time","mean Cariprazine in time","mean Clozapine in time","mean CNO in time","mean Haloperidol in time","mean NDMC in time","mean OSU6162 in time","mean PCAP1 in time",
				"mean PCAP2 in time","mean PCAP3 in time","mean PCAP4 in time", "mean NDMCHigh in time", "healthy control", "disease control","healthy","disease induced"), 
		col=c(colrs_all, "grey30", "orangered2","black","black"), pch = c(rep(19,times=13), 15, 1, 0),cex=1.7)


dev.off()



