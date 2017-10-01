library(lme4)
library(MASS)
library(AER)

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


#round up the counts and turn proportions in to percents
dataset[,c(grep("Proportion", colnames(dataset)))]<-apply(dataset[,c(grep("Proportion", colnames(dataset)))],2,function(x) round(x*100))
dataset[,c(grep("Count", colnames(dataset)))]<-apply(dataset[,c(grep("Count", colnames(dataset)))],2,function(x) round(x))

dataset_full[,c(grep("Proportion", colnames(dataset_full)))]<-apply(dataset_full[,c(grep("Proportion", colnames(dataset_full)))],2,function(x) round(x*100))
dataset_full[,c(grep("Count", colnames(dataset_full)))]<-apply(dataset_full[,c(grep("Count", colnames(dataset_full)))],2,function(x) round(x))


attach(dataset)

#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

transitions_2<-colnames(dataset)[grep(".*_.._Proportion", colnames(dataset))]
transitions_2_single<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==1]
transitions_2_double<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==2]


transitions_3<-colnames(dataset)[grep(".*_..._Proportion", colnames(dataset))]

transitions_3_single<-transitions_3[unlist(lapply(strsplit(substr(transitions_3,nchar(transitions_3)-13,nchar(transitions_3)-11),""),
						function(x) length(unique(x))))==1]
transitions_3_double<-transitions_3[unlist(lapply(strsplit(substr(transitions_3,nchar(transitions_3)-13,nchar(transitions_3)-11),""),
						function(x) length(unique(x))))==2]
transitions_3_triple<-transitions_3[unlist(lapply(strsplit(substr(transitions_3,nchar(transitions_3)-13,nchar(transitions_3)-11),""),
						function(x) length(unique(x))))==3]


#we need to adjust the standard errors due to over/underdispersion, model with quasipossion, the ones that are not really over/under
#disperesed will get the estimated dispersion calculated to aproximately one, which is the same as poison

#prepare a list for each group

drug_group_effect_sizes<-list()

list_nr<-1

variables<-c()

#total bout count and proportion of count per length dont need to be adjusted for anything
variables<-c(variables, c("TotalBoutCount",colnames(dataset)[grep("Count*Proportion", colnames(dataset))]))
for (variable in c("TotalBoutCount",colnames(dataset)[grep("Count*Proportion", colnames(dataset))])){
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group, family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[-1]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}


variables<-c(variables, colnames(dataset)[grep("Bends|Scoots", colnames(dataset))])
#turn proportions per length have to be adjusted for the total count of bouts per that length
for (variable in colnames(dataset)[grep("Bends|Scoots", colnames(dataset))]){
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group+dataset_full[,paste0(substr(variable,1,(nchar(variable)-20)),"BoutCount")], 
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-39)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-39)] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}


#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

#per amount of unique turns transition variables have already been extracted above
variables<-c(variables, transitions_2_single)
for (variable in transitions_2_single){
	
	turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter=="s"){
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter),"BendsProportion")
	}
	
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group+dataset_full[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+dataset_full[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-39,-40)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-39,-40)] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

variables<-c(variables, transitions_2_double)
for (variable in transitions_2_double){
	
	turn_letter1<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter1=="s"){
		adjusting_turn1<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn1<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter1),"BendsProportion")
	}
	
	turn_letter2<-substr(variable,nchar(variable)-11,nchar(variable)-11)
	
	if(turn_letter2=="s"){
		adjusting_turn2<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn2<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter2),"BendsProportion")
	}
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group+dataset_full[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+dataset_full[,adjusting_turn1]+dataset_full[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-39,-40,-41)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-39,-40,-41)] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

variables<-c(variables, transitions_3_single)
for (variable in transitions_3_single){
	
	turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter=="s"){
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
	}else{
		
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter),"BendsProportion")
	}
	
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group+dataset_full[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+dataset_full[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-39,-40)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-39,-40)] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}


variables<-c(variables, transitions_3_double)
for (variable in transitions_3_double){
	
	turn_letter1<-unique(strsplit(substr(variable,nchar(variable)-13,nchar(variable)-11),"")[[1]])[1]
	
	if(turn_letter1=="s"){
		adjusting_turn1<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
	}else{
		
		adjusting_turn1<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter1),"BendsProportion")
	}
	
	turn_letter2<-unique(strsplit(substr(variable,nchar(variable)-13,nchar(variable)-11),"")[[1]])[2]
	
	if(turn_letter2=="s"){
		adjusting_turn2<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
	}else{
		
		adjusting_turn2<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter2),"BendsProportion")
	}
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group+dataset_full[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+dataset_full[,adjusting_turn1]+dataset_full[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-39,-40,-41)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-39,-40,-41)] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

variables<-c(variables, transitions_3_triple)
for (variable in transitions_3_triple){
	
	turn_letter1<-substr(variable,nchar(variable)-11,nchar(variable)-11)
	
	if(turn_letter1=="s"){
		adjusting_turn1<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
	}else{
		
		adjusting_turn1<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter1),"BendsProportion")
	}
	
	turn_letter2<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter2=="s"){
		adjusting_turn2<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
	}else{
		
		adjusting_turn2<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter2),"BendsProportion")
	}
	
	turn_letter3<-substr(variable,nchar(variable)-13,nchar(variable)-13)
	
	if(turn_letter3=="s"){
		adjusting_turn3<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
	}else{
		
		adjusting_turn3<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter3),"BendsProportion")
	}
	
	model_quasipoisson<-glm(dataset[,variable]~TimeFactor+Group+TimeFactor*Group+dataset_full[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+dataset_full[,adjusting_turn1]+dataset_full[,adjusting_turn2]+dataset_full[,adjusting_turn3],
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-39,-40,-41,-42)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-39,-40,-41,-42)] >=0.05]<-0
	
	drug_group_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

#transform the effect sizes to be interpretable (exp(beta)-1) change referenced to control
names(drug_group_effect_sizes)<-variables
drug_group_effect_sizes_tranformed<-lapply(drug_group_effect_sizes,function(x) (exp(x)-1))

for(variable in variables){
	
	write.table(drug_group_effect_sizes_tranformed[variable],
			file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/separate/",condition,"/",variable,".txt"),
			row.names=T,col.names=T)
	
	write.table(drug_group_effect_sizes_tranformed[variable],
			file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/separate_with_values/",condition,"/",variable,".txt"),
			row.names=T,col.names=T)
	
	control_values<-dataset[Group=="Control",variable]
	control_values<-as.data.frame(control_values)
	colnames(control_values)<-"Control_values"
	
	write.table(control_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/separate_with_values/",condition,
					"/",variable,".txt"),append=T,row.names=F,col.names=T)
}




