library(lme4)
library(MASS)
library(AER)

#compare Light Dark

control_Light_Dark<-read.table("Light_Dark_final_dataset_selected.txt",header=TRUE)
control_Light_Dark$Group<-factor(control_Light_Dark$Group)

dataset_full_Light<-read.table("Light_final_dataset_full.txt",header=TRUE)
dataset_full_Light$Group<-factor(dataset_full_Light$Group)

dataset_full_Dark<-read.table("Dark_final_dataset_full.txt",header=TRUE)
dataset_full_Dark$Group<-factor(dataset_full_Dark$Group)


#make a full dataset combined
common_var_Light_Dark_full<-intersect(colnames(dataset_full_Light),colnames(dataset_full_Dark))

control_Light_full<-dataset_full_Light[dataset_full_Light$Group=="Control",common_var_Light_Dark_full]
control_Light_full$Group<-rep("Control_Light",times=13)
control_Light_full$Group<-as.factor(control_Light_full$Group)

control_Dark_full<-dataset_full_Dark[dataset_full_Dark$Group=="Control",common_var_Light_Dark_full]
control_Dark_full$Group<-rep("Control_Dark",times=13)
control_Dark_full$Group<-as.factor(control_Dark_full$Group)

control_Light_Dark_full<-rbind(control_Light_full,control_Dark_full)

#add variables as proportions of total bout count and round up the counts and turn proportions in to percents

#selected
count_vars<-length(grep("Length.*Count", colnames(control_Light_Dark)))
control_Light_Dark<-cbind(control_Light_Dark,apply(control_Light_Dark[,grep("Length.*Count", colnames(control_Light_Dark))],2,function(x) x/control_Light_Dark$TotalBoutCount))

colnames(control_Light_Dark)[c((length(control_Light_Dark[1,])-(count_vars-1)):length(control_Light_Dark[1,]))]<-paste0(colnames(control_Light_Dark)[c((length(control_Light_Dark[1,])-
									(count_vars-1)):length(control_Light_Dark[1,]))],"Proportion")
control_Light_Dark[,c(grep("Proportion", colnames(control_Light_Dark)))]<-apply(control_Light_Dark[,c(grep("Proportion", colnames(control_Light_Dark)))],2,function(x) round(x*100))
control_Light_Dark[,c(grep("Count", colnames(control_Light_Dark)))]<-apply(control_Light_Dark[,c(grep("Count", colnames(control_Light_Dark)))],2,function(x) round(x))


#full
count_vars<-length(grep("Length.*Count", colnames(control_Light_Dark_full)))
control_Light_Dark_full<-cbind(control_Light_Dark_full,apply(control_Light_Dark_full[,grep("Length.*Count", colnames(control_Light_Dark_full))],2,function(x) x/control_Light_Dark_full$TotalBoutCount))

colnames(control_Light_Dark_full)[c((length(control_Light_Dark_full[1,])-(count_vars-1)):length(control_Light_Dark_full[1,]))]<-paste0(colnames(control_Light_Dark_full)[c((length(control_Light_Dark_full[1,])-
									(count_vars-1)):length(control_Light_Dark_full[1,]))],"Proportion")

control_Light_Dark_full[,c(grep("Proportion", colnames(control_Light_Dark_full)))]<-apply(control_Light_Dark_full[,c(grep("Proportion", colnames(control_Light_Dark_full)))],2,function(x) round(x*100))
control_Light_Dark_full[,c(grep("Count", colnames(control_Light_Dark_full)))]<-apply(control_Light_Dark_full[,c(grep("Count", colnames(control_Light_Dark_full)))],2,function(x) round(x))

#make control as the reference of all
contrasts(control_Light_Dark$Group) <- contr.treatment(levels(control_Light_Dark$Group),base=which(levels(control_Light_Dark$Group) == 'Control_Light'))
contrasts(control_Light_Dark_full$Group) <- contr.treatment(levels(control_Light_Dark_full$Group),base=which(levels(control_Light_Dark_full$Group) == 'Control_Light'))


attach(control_Light_Dark)

control_Light_Dark_effect_sizes<-list()

list_nr<-1

variables<-c()

#total bout count and proportion of count per length dont need to be adjusted for anything
variables<-c(variables, c("TotalBoutCount",colnames(control_Light_Dark)[grep("Count*Proportion", colnames(control_Light_Dark))]))
for (variable in c("TotalBoutCount",colnames(control_Light_Dark)[grep("Count*Proportion", colnames(control_Light_Dark))])){
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group, family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[-1]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}


variables<-c(variables, colnames(control_Light_Dark)[grep("Bends|Scoots", colnames(control_Light_Dark))])
#turn proportions per length have to be adjusted for the total count of bouts per that length
for (variable in colnames(control_Light_Dark)[grep("Bends|Scoots", colnames(control_Light_Dark))]){
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group+control_Light_Dark_full[,paste0(substr(variable,1,(nchar(variable)-20)),"BoutCount")], 
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4)] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}

#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

transitions_2<-colnames(control_Light_Dark)[grep(".*_.._Proportion", colnames(control_Light_Dark))]
transitions_2_single<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==1]
transitions_2_double<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==2]


transitions_3<-colnames(control_Light_Dark)[grep(".*_..._Proportion", colnames(control_Light_Dark))]

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
variables<-c(variables, transitions_2_single)
for (variable in transitions_2_single){
	
	turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter=="s"){
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter),"BendsProportion")
	}
	
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group+control_Light_Dark_full[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Light_Dark_full[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group+control_Light_Dark_full[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Light_Dark_full[,adjusting_turn1]+control_Light_Dark_full[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group+control_Light_Dark_full[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Light_Dark_full[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group+control_Light_Dark_full[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Light_Dark_full[,adjusting_turn1]+control_Light_Dark_full[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Light_Dark[,variable]~TimeFactor+Group+TimeFactor*Group+control_Light_Dark_full[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Light_Dark_full[,adjusting_turn1]+control_Light_Dark_full[,adjusting_turn2]+control_Light_Dark_full[,adjusting_turn3],
							family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6,-7)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6,-7)] >=0.05]<-0
	
	control_Light_Dark_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

names(control_Light_Dark_effect_sizes)<-variables
control_Light_Dark_effect_sizes_tranformed<-lapply(control_Light_Dark_effect_sizes,function(x) (exp(x)-1))

for(variable in variables){
	
	control_values<-control_Light_Dark[Group=="Control_Light",variable]
	control_values<-as.data.frame(control_values)
	colnames(control_values)<-paste0("Light_",variable)
	
	compare_values<-control_Light_Dark[Group=="Control_Dark",variable]
	compare_values<-as.data.frame(compare_values)
	colnames(compare_values)<-paste0("Dark_",variable)
	
	if(length(control_Light_Dark_effect_sizes_tranformed[variable][control_Light_Dark_effect_sizes_tranformed[variable][[1]]==0])!=3 && (sum(control_values)>=5 || sum(compare_values)>=5)){
		write.table(control_Light_Dark_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls/Light_vs_Dark/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_Light_Dark_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Light_vs_Dark/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Light_vs_Dark/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
		write.table(compare_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Light_vs_Dark/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
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

control_Dark_ApoLow_effect_sizes<-list()

list_nr<-1

variables<-c()

#total bout count and proportion of count per length dont need to be adjusted for anything
variables<-c(variables, c("TotalBoutCount",colnames(control_Dark_ApoLow)[grep("Count*Proportion", colnames(control_Dark_ApoLow))]))
for (variable in c("TotalBoutCount",colnames(control_Dark_ApoLow)[grep("Count*Proportion", colnames(control_Dark_ApoLow))])){
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group, family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[-1]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}


variables<-c(variables, colnames(control_Dark_ApoLow)[grep("Bends|Scoots", colnames(control_Dark_ApoLow))])
#turn proportions per length have to be adjusted for the total count of bouts per that length
for (variable in colnames(control_Dark_ApoLow)[grep("Bends|Scoots", colnames(control_Dark_ApoLow))]){
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoLow[,paste0(substr(variable,1,(nchar(variable)-20)),"BoutCount")], 
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4)] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}

#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

transitions_2<-colnames(control_Dark_ApoLow)[grep(".*_.._Proportion", colnames(control_Dark_ApoLow))]
transitions_2_single<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==1]
transitions_2_double<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==2]


transitions_3<-colnames(control_Dark_ApoLow)[grep(".*_..._Proportion", colnames(control_Dark_ApoLow))]

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
variables<-c(variables, transitions_2_single)
for (variable in transitions_2_single){
	
	turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter=="s"){
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter),"BendsProportion")
	}
	
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoLow[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Dark_ApoLow[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoLow[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Dark_ApoLow[,adjusting_turn1]+control_Dark_ApoLow[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoLow[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_ApoLow[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoLow[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_ApoLow[,adjusting_turn1]+control_Dark_ApoLow[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_ApoLow[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoLow[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_ApoLow[,adjusting_turn1]+control_Dark_ApoLow[,adjusting_turn2]+control_Dark_ApoLow[,adjusting_turn3],
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6,-7)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6,-7)] >=0.05]<-0
	
	control_Dark_ApoLow_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

names(control_Dark_ApoLow_effect_sizes)<-variables
control_Dark_ApoLow_effect_sizes_tranformed<-lapply(control_Dark_ApoLow_effect_sizes,function(x) (exp(x)-1))

for(variable in variables){
	
	control_values<-control_Dark_ApoLow[Group=="Control_Dark",variable]
	control_values<-as.data.frame(control_values)
	colnames(control_values)<-paste0("Dark_",variable)
	
	compare_values<-control_Dark_ApoLow[Group=="Control_DarkApoLow",variable]
	compare_values<-as.data.frame(compare_values)
	colnames(compare_values)<-paste0("DarkApoLow_",variable)
	
	if(length(control_Dark_ApoLow_effect_sizes_tranformed[variable][control_Dark_ApoLow_effect_sizes_tranformed[variable][[1]]==0])!=3 && (sum(control_values)>=5 || sum(compare_values)>=5)){
		write.table(control_Dark_ApoLow_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls/Healthy_vs_ApoLow/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_Dark_ApoLow_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_ApoLow/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_ApoLow/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
		write.table(compare_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_ApoLow/",
						variable,".txt"),append=T,row.names=F,col.names=T)
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

control_Dark_ApoHigh_effect_sizes<-list()

list_nr<-1

variables<-c()

#total bout count and proportion of count per length dont need to be adjusted for anything
variables<-c(variables, c("TotalBoutCount",colnames(control_Dark_ApoHigh)[grep("Count*Proportion", colnames(control_Dark_ApoHigh))]))
for (variable in c("TotalBoutCount",colnames(control_Dark_ApoHigh)[grep("Count*Proportion", colnames(control_Dark_ApoHigh))])){
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group, family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[-1]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}


variables<-c(variables, colnames(control_Dark_ApoHigh)[grep("Bends|Scoots", colnames(control_Dark_ApoHigh))])
#turn proportions per length have to be adjusted for the total count of bouts per that length
for (variable in colnames(control_Dark_ApoHigh)[grep("Bends|Scoots", colnames(control_Dark_ApoHigh))]){
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoHigh[,paste0(substr(variable,1,(nchar(variable)-20)),"BoutCount")], 
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4)] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}

#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

transitions_2<-colnames(control_Dark_ApoHigh)[grep(".*_.._Proportion", colnames(control_Dark_ApoHigh))]
transitions_2_single<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==1]
transitions_2_double<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==2]


transitions_3<-colnames(control_Dark_ApoHigh)[grep(".*_..._Proportion", colnames(control_Dark_ApoHigh))]

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
variables<-c(variables, transitions_2_single)
for (variable in transitions_2_single){
	
	turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter=="s"){
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter),"BendsProportion")
	}
	
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoHigh[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Dark_ApoHigh[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoHigh[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Dark_ApoHigh[,adjusting_turn1]+control_Dark_ApoHigh[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoHigh[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_ApoHigh[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoHigh[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_ApoHigh[,adjusting_turn1]+control_Dark_ApoHigh[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_ApoHigh[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_ApoHigh[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_ApoHigh[,adjusting_turn1]+control_Dark_ApoHigh[,adjusting_turn2]+control_Dark_ApoHigh[,adjusting_turn3],
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6,-7)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6,-7)] >=0.05]<-0
	
	control_Dark_ApoHigh_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

names(control_Dark_ApoHigh_effect_sizes)<-variables
control_Dark_ApoHigh_effect_sizes_tranformed<-lapply(control_Dark_ApoHigh_effect_sizes,function(x) (exp(x)-1))

for(variable in variables){
	
	control_values<-control_Dark_ApoHigh[Group=="Control_Dark",variable]
	control_values<-as.data.frame(control_values)
	colnames(control_values)<-paste0("Dark_",variable)
	
	compare_values<-control_Dark_ApoHigh[Group=="Control_DarkApoHigh",variable]
	compare_values<-as.data.frame(compare_values)
	colnames(compare_values)<-paste0("DarkApoHigh_",variable)
	
	if(length(control_Dark_ApoHigh_effect_sizes_tranformed[variable][control_Dark_ApoHigh_effect_sizes_tranformed[variable][[1]]==0])!=3 && (sum(control_values)>=5 || sum(compare_values)>=5)){
		write.table(control_Dark_ApoHigh_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls/Healthy_vs_ApoHigh/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_Dark_ApoHigh_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_ApoHigh/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_ApoHigh/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
		write.table(compare_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_ApoHigh/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
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

control_Dark_PTZ_effect_sizes<-list()

list_nr<-1

variables<-c()

#total bout count and proportion of count per length dont need to be adjusted for anything
variables<-c(variables, c("TotalBoutCount",colnames(control_Dark_PTZ)[grep("Count*Proportion", colnames(control_Dark_PTZ))]))
for (variable in c("TotalBoutCount",colnames(control_Dark_PTZ)[grep("Count*Proportion", colnames(control_Dark_PTZ))])){
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group, family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[-1]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][-1] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}


variables<-c(variables, colnames(control_Dark_PTZ)[grep("Bends|Scoots", colnames(control_Dark_PTZ))])
#turn proportions per length have to be adjusted for the total count of bouts per that length
for (variable in colnames(control_Dark_PTZ)[grep("Bends|Scoots", colnames(control_Dark_PTZ))]){
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_PTZ[,paste0(substr(variable,1,(nchar(variable)-20)),"BoutCount")], 
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4)] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
}

#turn transitions per bout length have to be adjusted for turn proportion per that bout length and bout count per that length

#tedious since have to know which unique turns are in transition to adjust for and how many, for example transition "sss" has to
# be adjusted for scoots only, while "sgj" has to be adjusted for scoot, jbends and gbends...

transitions_2<-colnames(control_Dark_PTZ)[grep(".*_.._Proportion", colnames(control_Dark_PTZ))]
transitions_2_single<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==1]
transitions_2_double<-transitions_2[unlist(lapply(strsplit(substr(transitions_2,nchar(transitions_2)-12,nchar(transitions_2)-11),""),
						function(x) length(unique(x))))==2]


transitions_3<-colnames(control_Dark_PTZ)[grep(".*_..._Proportion", colnames(control_Dark_PTZ))]

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
variables<-c(variables, transitions_2_single)
for (variable in transitions_2_single){
	
	turn_letter<-substr(variable,nchar(variable)-12,nchar(variable)-12)
	
	if(turn_letter=="s"){
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"BoutScootsProportion")
	}else{
		
		adjusting_turn<-paste0(substr(variable,1,nchar(variable)-24),"Bout",toupper(turn_letter),"BendsProportion")
	}
	
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_PTZ[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Dark_PTZ[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_PTZ[,paste0(substr(variable,1,(nchar(variable)-24)),
							"BoutCount")]+control_Dark_PTZ[,adjusting_turn1]+control_Dark_PTZ[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_PTZ[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_PTZ[,adjusting_turn], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5)] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_PTZ[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_PTZ[,adjusting_turn1]+control_Dark_PTZ[,adjusting_turn2], family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6)] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
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
	
	model_quasipoisson<-glm(control_Dark_PTZ[,variable]~TimeFactor+Group+TimeFactor*Group+control_Dark_PTZ[,paste0(substr(variable,1,(nchar(variable)-25)),
							"BoutCount")]+control_Dark_PTZ[,adjusting_turn1]+control_Dark_PTZ[,adjusting_turn2]+control_Dark_PTZ[,adjusting_turn3],
			family = quasipoisson)
	
	var_coeff<-model_quasipoisson$coefficients[c(-1,-4,-5,-6,-7)]
	var_coeff[summary(model_quasipoisson)$coef[, "Pr(>|t|)"][c(-1,-4,-5,-6,-7)] >=0.05]<-0
	
	control_Dark_PTZ_effect_sizes[[list_nr]]<-var_coeff
	
	list_nr<-list_nr+1
	
	
}

names(control_Dark_PTZ_effect_sizes)<-variables
control_Dark_PTZ_effect_sizes_tranformed<-lapply(control_Dark_PTZ_effect_sizes,function(x) (exp(x)-1))

for(variable in variables){
	
	control_values<-control_Dark_PTZ[Group=="Control_Dark",variable]
	control_values<-as.data.frame(control_values)
	colnames(control_values)<-paste0("Dark_",variable)
	
	compare_values<-control_Dark_PTZ[Group=="Control_DarkPTZ",variable]
	compare_values<-as.data.frame(compare_values)
	colnames(compare_values)<-paste0("DarkPTZ_",variable)
	
	if(length(control_Dark_PTZ_effect_sizes_tranformed[variable][control_Dark_PTZ_effect_sizes_tranformed[variable][[1]]==0])!=3 && (sum(control_values)>=5 || sum(compare_values)>=5)){
		write.table(control_Dark_PTZ_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls/Healthy_vs_PTZ/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_Dark_PTZ_effect_sizes_tranformed[variable],
				file=paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_PTZ/",variable,".txt"),
				row.names=T,col.names=T)
		
		write.table(control_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_PTZ/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
		write.table(compare_values, file = paste0("~/git/zebrafish_action_sequence_project/results/output/effect_sizes/comparing_controls_with_values/Healthy_vs_PTZ/",
						variable,".txt"),append=T,row.names=F,col.names=T)
		
	}
}

detach(control_Dark_PTZ)

