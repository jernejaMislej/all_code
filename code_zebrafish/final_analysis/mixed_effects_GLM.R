library(lme4)
library(MASS)
#library(AER)
library(reshape)

args<-commandArgs(trailingOnly = TRUE)

condition<-args[1]
type<-args[2]


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


#make control as the reference of all
contrasts(full_together$Group) <- contr.treatment(levels(full_together$Group),base=which(levels(full_together$Group) == 'Control_Dark'))

attach(full_together)

full_effect_sizes<-list()

full_effect_sizes_all<-list()
full_p_values_all<-list()

list_nr<-1

variables<-c()

#total bout count and proportion of count per length dont need to be adjusted for anything
variables<-c(variables, c("TotalBoutCount",colnames(full_together)[grep("Count*Proportion", colnames(full_together))]))
for (variable in c("TotalBoutCount",colnames(full_together)[grep("Count*Proportion", colnames(full_together))])){
	
	model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group, family = quasipoisson(link = "log"))
	
	var_coeff<-model_quasipoisson$coefficients[-1]
	
	full_effect_sizes_all[[list_nr]]<-var_coeff
	full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1]
	
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1] >=0.05]<-0
	
	full_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}

if(type=="B2"){
	variables<-c(variables, colnames(full_together)[grep("Bends|Scoots", colnames(full_together))])
	#turn proportions per length have to be adjusted for the total count of bouts and proportion of bouts of certain length
	for (variable in colnames(full_together)[grep("Bends|Scoots", colnames(full_together))]){
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+TotalBoutCount+
						Length4BoutCountProportion+Length5BoutCountProportion+Length6PlusBoutCountProportion+Length2BoutCountProportion+
						Length1BoutCountProportion+Length3BoutCountProportion, family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[-c(1,76:82)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:82)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:82)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
	}
}else{
	variables<-c(variables, colnames(full_together)[grep("Bends|Scoots", colnames(full_together))])
	#turn proportions per length have to be adjusted for the total count of bouts per that length
	for (variable in colnames(full_together)[grep("Bends|Scoots", colnames(full_together))]){
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+full_together[,paste0(substr(variable,1,(nchar(variable)-20)),"BoutCount")], 
				family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[c(-1,-76)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
	}
	
	
}

#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

transitions_2<-colnames(full_together)[grep(".*_.._Proportion", colnames(full_together))]
transitions_2_single<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==1]
transitions_2_double<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==2]


transitions_3<-colnames(full_together)[grep(".*_..._Proportion", colnames(full_together))]

transitions_3_single<-transitions_3[unlist(lapply(strsplit(substr(transitions_3,nchar(transitions_3)-13,nchar(transitions_3)-11),""),
						function(x) length(unique(x))))==1]
transitions_3_double<-transitions_3[unlist(lapply(strsplit(substr(transitions_3,nchar(transitions_3)-13,nchar(transitions_3)-11),""),
						function(x) length(unique(x))))==2]
transitions_3_triple<-transitions_3[unlist(lapply(strsplit(substr(transitions_3,nchar(transitions_3)-13,nchar(transitions_3)-11),""),
						function(x) length(unique(x))))==3]


#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

#per amount of unique turns transition variables have already been extracted above
if(type=="B2"){
	
	
	variables<-c(variables, transitions_2_single)
	for (variable in transitions_2_single){
		
		turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
		
		if(turn_letter=="s"){
			adjusting_turn<-"ScootsProportion"
		}else{
			
			adjusting_turn<-paste0(toupper(turn_letter),"BendsProportion")
		}
		
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+TotalBoutCount+
						Length4BoutCountProportion+Length5BoutCountProportion+Length6PlusBoutCountProportion+Length2BoutCountProportion+
						Length1BoutCountProportion+Length3BoutCountProportion+full_together[,adjusting_turn], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[-c(1,76:83)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:83)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:83)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	variables<-c(variables, transitions_2_double[-grep("Transition_io_Proportion|Transition_oe_Proportion",transitions_2_double)])
	for (variable in transitions_2_double[-grep("Transition_io_Proportion|Transition_oe_Proportion",transitions_2_double)]){
		
		turn_letter1<-substr(variable,nchar(variable)-12,nchar(variable)-12)
		
		if(turn_letter1=="s"){
			adjusting_turn1<-"ScootsProportion"
		}else{
			
			adjusting_turn1<-paste0(toupper(turn_letter1),"BendsProportion")
		}
		
		turn_letter2<-substr(variable,nchar(variable)-11,nchar(variable)-11)
		
		if(turn_letter2=="s"){
			adjusting_turn2<-"ScootsProportion"
		}else{
			
			adjusting_turn2<-paste0(toupper(turn_letter2),"BendsProportion")
		}
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+TotalBoutCount+
						Length4BoutCountProportion+Length5BoutCountProportion+Length6PlusBoutCountProportion+Length2BoutCountProportion+
						Length1BoutCountProportion+Length3BoutCountProportion+full_together[,adjusting_turn1]+full_together[,adjusting_turn2], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[-c(1,76:84)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:84)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:84)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	variables<-c(variables, transitions_3_single[-grep("Transition_iii_Proportion",transitions_3_single)])
	for (variable in transitions_3_single[-grep("Transition_iii_Proportion",transitions_3_single)]){
		
		turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
		
		if(turn_letter=="s"){
			adjusting_turn<-"ScootsProportion"
		}else{
			
			adjusting_turn<-paste0(toupper(turn_letter),"BendsProportion")
		}
		
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+TotalBoutCount+
						Length4BoutCountProportion+Length5BoutCountProportion+Length6PlusBoutCountProportion+Length2BoutCountProportion+
						Length1BoutCountProportion+Length3BoutCountProportion+full_together[,adjusting_turn], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[-c(1,76:83)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:83)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:83)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	
	variables<-c(variables, transitions_3_double[-grep("Transition_sjj_Proportion|Transition_eii_Proportion",transitions_3_double)])
	for (variable in transitions_3_double[-grep("Transition_sjj_Proportion|Transition_eii_Proportion",transitions_3_double)]){
		
		turn_letter1<-unique(strsplit(substr(variable,nchar(variable)-13,nchar(variable)-11),"")[[1]])[1]
		
		if(turn_letter1=="s"){
			adjusting_turn1<-"ScootsProportion"
		}else{
			
			adjusting_turn1<-paste0(toupper(turn_letter1),"BendsProportion")
		}
		
		turn_letter2<-unique(strsplit(substr(variable,nchar(variable)-13,nchar(variable)-11),"")[[1]])[2]
		
		if(turn_letter2=="s"){
			adjusting_turn2<-"ScootsProportion"
		}else{
			
			adjusting_turn2<-paste0(toupper(turn_letter2),"BendsProportion")
		}
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+TotalBoutCount+
						Length4BoutCountProportion+Length5BoutCountProportion+Length6PlusBoutCountProportion+Length2BoutCountProportion+
						Length1BoutCountProportion+Length3BoutCountProportion+full_together[,adjusting_turn1]+full_together[,adjusting_turn2], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[-c(1,76:84)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:84)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:84)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	variables<-c(variables, transitions_3_triple)
	for (variable in transitions_3_triple){
				
				turn_letter1<-substr(variable,nchar(variable)-11,nchar(variable)-11)
				
				if(turn_letter1=="s"){
					adjusting_turn1<-"ScootsProportion"
				}else{
					
					adjusting_turn1<-paste0(toupper(turn_letter1),"BendsProportion")
				}
				
				turn_letter2<-substr(variable,nchar(variable)-12,nchar(variable)-12)
				
				if(turn_letter2=="s"){
					adjusting_turn2<-"ScootsProportion"
				}else{
					
					adjusting_turn2<-paste0(toupper(turn_letter2),"BendsProportion")
				}
				
				turn_letter3<-substr(variable,nchar(variable)-13,nchar(variable)-13)
				
				if(turn_letter3=="s"){
					adjusting_turn3<-"ScootsProportion"
				}else{
					
					adjusting_turn3<-paste0(toupper(turn_letter3),"BendsProportion")
				}
				
				model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+TotalBoutCount+
								Length4BoutCountProportion+Length5BoutCountProportion+Length6PlusBoutCountProportion+Length2BoutCountProportion+
								Length1BoutCountProportion+Length3BoutCountProportion+full_together[,adjusting_turn1]+full_together[,adjusting_turn2]+full_together[,adjusting_turn3],
						family = quasipoisson(link = "log"))
				
				var_coeff<-model_quasipoisson$coefficients[-c(1,76:85)]
				
				full_effect_sizes_all[[list_nr]]<-var_coeff
				full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:85)]
				
				var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-c(1,76:85)] >=0.05]<-0
				
				full_effect_sizes[[list_nr]]<-var_coeff
				
				list_nr<-list_nr+1
				
		}
	
	
	
}else {
	variables<-c(variables, transitions_2_single)
	for (variable in transitions_2_single){
		
		turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
		
		if(turn_letter=="s"){
			adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
		}else{
			
			adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter),"BendsProportion")
		}
		
		
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+full_together[,paste0(substr(variable,1,(nchar(variable)-24)),
								"BoutCount")]+full_together[,adjusting_turn], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[c(-1,-76,-77)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	variables<-c(variables, transitions_2_double[-grep("Length4Transition_so_Proportion",transitions_2_double)])
	for (variable in transitions_2_double[-grep("Length4Transition_so_Proportion",transitions_2_double)]){
		
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
		
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+full_together[,paste0(substr(variable,1,(nchar(variable)-24)),
								"BoutCount")]+full_together[,adjusting_turn1]+full_together[,adjusting_turn2], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[c(-1,-76,-77,-78)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77,-78)] 
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77,-78)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	variables<-c(variables, transitions_3_single[-grep("Transition_iii_Proportion",transitions_3_single)])
	for (variable in transitions_3_single[-grep("Transition_iii_Proportion",transitions_3_single)]){
		
		turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
		
		if(turn_letter=="s"){
			adjusting_turn<-paste0(substr(variable,1,nchar(variable)-25),"BoutScootsProportion")
		}else{
			
			adjusting_turn<-paste0(substr(variable,1,nchar(variable)-25),"Bout",toupper(turn_letter),"BendsProportion")
		}
		
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+full_together[,paste0(substr(variable,1,(nchar(variable)-25)),
								"BoutCount")]+full_together[,adjusting_turn], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[c(-1,-76,-77)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77)] 
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	
	variables<-c(variables, transitions_3_double[-grep("Transition_sjj_Proportion",transitions_3_double)])
	for (variable in transitions_3_double[-grep("Transition_sjj_Proportion",transitions_3_double)]){
		
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
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+full_together[,paste0(substr(variable,1,(nchar(variable)-25)),
								"BoutCount")]+full_together[,adjusting_turn1]+full_together[,adjusting_turn2], family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[c(-1,-76,-77,-78)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77,-78)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77,-78)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
		
	}
	
	variables<-c(variables, transitions_3_triple[-grep("Length6PlusTransition_hhh_Proportion",transitions_3_double)])
	for (variable in transitions_3_triple[-grep("Length6PlusTransition_hhh_Proportion",transitions_3_double)]){
		
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
		
		model_quasipoisson<-glm(full_together[,variable]~TimeFactor+Group+TimeFactor*Group+full_together[,paste0(substr(variable,1,(nchar(variable)-25)),
								"BoutCount")]+full_together[,adjusting_turn1]+full_together[,adjusting_turn2]+full_together[,adjusting_turn3],family = quasipoisson(link = "log"))
		
		var_coeff<-model_quasipoisson$coefficients[c(-1,-76,-77,-78,-79)]
		
		full_effect_sizes_all[[list_nr]]<-var_coeff
		full_p_values_all[[list_nr]]<-summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77,-78,-79)]
		
		var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-76,-77,-78,-79)] >=0.05]<-0
		
		full_effect_sizes[[list_nr]]<-var_coeff
		
		list_nr<-list_nr+1
		
	}
	
}

names(full_effect_sizes)<-variables
full_effect_sizes_tranformed<-lapply(full_effect_sizes,function(x) exp(x))

names(full_effect_sizes_all)<-variables
full_effect_sizes_tranformed_all<-lapply(full_effect_sizes_all,function(x) exp(x))

names(full_p_values_all)<-variables

for(variable in variables){
	
	casting<-cbind(full_together[,c(variable,"Group")],rep(1:13,times=74))
	colnames(casting)[c(2,3)]<-c("id","variable")
	casting<-casting[,c(3,2,1)]
	casting<-cast(casting,variable~id)[,-1]
	casting<-casting[,c((1:74)[colnames(casting)=="Control_Dark"],(1:74)[colnames(casting)!="Control_Dark"])]
	#at least one of the groups has to have at least 10 percent information
	information_criteria<-length((1:37)[apply(casting, 2, function(x) sum(x)>=10)])>0
	if(length(full_effect_sizes_tranformed[variable][full_effect_sizes_tranformed[variable][[1]]!=0])!=0 && information_criteria){
		
		write.table(full_effect_sizes_tranformed[variable],
				file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/effect_sizes/",type,"/unstandardized/",condition,"/",variable,".txt"),
				row.names=T,col.names=T)

		write.table(full_effect_sizes_tranformed_all[variable],
				file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/effect_sizes/",type,"/unstandardized/",condition,"/",variable,"all.txt"),
				row.names=T,col.names=T)
		
		write.table(full_p_values_all[variable],
				file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/effect_sizes/",type,"/unstandardized/",condition,"/",variable,"p_values.txt"),
				row.names=T,col.names=T)
		
		write.table(full_effect_sizes_tranformed[variable],
				file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/effect_sizes/",type,"/unstandardized/",condition,"/",variable,"_with_values.txt"),
				row.names=T,col.names=T)
		
		write.table(casting, file = paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/effect_sizes/",type,"/unstandardized/",condition,"/", 
						variable,"_with_values.txt"),append=T,row.names=F,col.names=T)
		
	}
}

detach(full_together)






