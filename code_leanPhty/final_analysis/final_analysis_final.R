library(pROC)
library(leaps)
library(car)

#java needs to be configurated to install and run glmulti, before running R, in the same terminal, run:
#R CMD javareconf -e
#echo $LD_LIBRARY_PATH
#echo $JAVA_LD_LIBRARY_PATH
#echo ${LD_LIBRARY_PATH}:${JAVA_LD_LIBRARY_PATH}
#export LD_LIBRARY_PATH=$(echo ${LD_LIBRARY_PATH}:${JAVA_LD_LIBRARY_PATH})
#echo $LD_LIBRARY_PATH

#load entire cleaned data (154009 lines)
VIP_data_all <- read.csv("../VIP_data/VIP_170206_cleaned.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

#load the subset
VIP_data_subset <- read.csv("../VIP_data/VIP_170206_cleaned_subset.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
attach(VIP_data_subset)
#extract only swedish....66228 (33114 for each visit)
#get those that have the ursprungsland in both visits and the value is 1 for Swedish
VIP_data_subset<-VIP_data_subset[!is.na(ursprungsland[visit==1]) & !is.na(ursprungsland[visit==2]),]
VIP_data_subset<-VIP_data_subset[VIP_data_subset$ursprungsland==1,]
VIP_data_subset_visit1<-VIP_data_subset[VIP_data_subset$visit==1,]
VIP_data_subset_visit2<-VIP_data_subset[VIP_data_subset$visit==2,]
detach(VIP_data_subset)

#create the independant dataset of the first visit that is not in the subset already with Swedish only
attach(VIP_data_all)
VIP_data_independant<-VIP_data_all[!is.na(besok1) & besok1==1 & !(Subject_id %in% VIP_data_subset$Subject_id) & !is.na(ursprungsland) & ursprungsland==1, ]
detach(VIP_data_all)
#length(VIP_data_independant[,1])#....47107

#load all variables, adjusted for TEI

#independent
source(file="load_variables_independent_dataset_TEI_adjusted.R")

#visit1
source(file="load_variables_visit1_dataset_TEI_adjusted.R")

#visit2
source(file="load_variables_visit2_dataset_TEI_adjusted.R")

#training data 
VIP_data_independant_complete_cases <- na.omit(VIP_data_independant[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
				"basic_residuals_bmi", "g6", "POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
				"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
				"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
				"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
				"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
				"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
				"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
				"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
				"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
				"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
				"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer","fibesum1_TEI_adjusted_norm_sd","alkosum1_TEI_adjusted_norm_sd")])
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi < 25]<-0
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 25 & VIP_data_independant_complete_cases$bmi < 30]<-1
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 30]<-2
VIP_data_independant_complete_cases$bmi_category<-as.factor(VIP_data_independant_complete_cases$bmi_category)
VIP_data_independant_complete_cases$g6<-as.factor(VIP_data_independant_complete_cases$g6)
length(VIP_data_independant_complete_cases[,1])
#[1] 44735

#validation data used for prediction also
VIP_data_subset_visit1_complete_cases <- na.omit(VIP_data_subset_visit1[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi","g6",
						"POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer","fibesum1_TEI_adjusted_norm_sd","alkosum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi < 25]<-0
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 25 & VIP_data_subset_visit1_complete_cases$bmi < 30]<-1
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit1_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit1_complete_cases$bmi_category)
VIP_data_subset_visit1_complete_cases$sm_status<-VIP_data_subset_visit1[VIP_data_subset_visit1$enummer %in% VIP_data_subset_visit1_complete_cases$enummer,"sm_status"]
VIP_data_subset_visit1_complete_cases$g6<-as.factor(VIP_data_subset_visit1_complete_cases$g6)

VIP_data_subset_visit2_complete_cases <- na.omit(VIP_data_subset_visit2[,c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
						"basic_residuals_bmi","g6",
						"POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
						"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
						"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
						"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
						"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd", "MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd",
						"selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
						"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd",
						"B2sum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd",
						"askosum1_TEI_adjusted_norm_sd","Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd",
						"VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd","JODIsum1_TEI_adjusted_norm_sd",
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer","fibesum1_TEI_adjusted_norm_sd","alkosum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi < 25]<-0
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 25 & VIP_data_subset_visit2_complete_cases$bmi < 30]<-1
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit2_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit2_complete_cases$bmi_category)
VIP_data_subset_visit2_complete_cases$sm_status<-VIP_data_subset_visit2[VIP_data_subset_visit2$enummer %in% VIP_data_subset_visit2_complete_cases$enummer,"sm_status"]
VIP_data_subset_visit2_complete_cases$g6<-as.factor(VIP_data_subset_visit2_complete_cases$g6)


#keep subjects that were in the complete cases in both visits
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]

length(VIP_data_subset_visit2_complete_cases[,1])
#30381

#make the full model
full_model<-lm(log(bmi)~age + agesq + gender_factor + year + ffq_factor + POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd+g6+alkosum1_TEI_adjusted_norm_sd, data = VIP_data_independant_complete_cases)
summary(full_model)
vif(full_model)

#select the best, based on R²
attach(VIP_data_independant_complete_cases)

all_predictors<-cbind(age,agesq,gender_factor,year,ffq_factor,POLYsum1_TEI_adjusted_norm_sd,MONOsum1_TEI_adjusted_norm_sd,mfetsum1_TEI_adjusted_norm_sd,
		fettsum1_TEI_adjusted_norm_sd,sacksum1_TEI_adjusted_norm_sd,kolhsum1_TEI_adjusted_norm_sd,FA_TEI_adjusted_norm_sd,
		protsum1_anim_TEI_adjusted_norm_sd,protsum1_veg_TEI_adjusted_norm_sd,
		fibesum1_TEI_adjusted_norm_sd,DISAsum1_TEI_adjusted_norm_sd,MOSAsum1_TEI_adjusted_norm_sd,TRANSsum1_TEI_adjusted_norm_sd,
		NATRsum1_TEI_adjusted_norm_sd,kolesum1_TEI_adjusted_norm_sd,ensum1_norm_sd, MAGNsum1_TEI_adjusted_norm_sd,
		FOSFsum1_TEI_adjusted_norm_sd,selesum1_TEI_adjusted_norm_sd,ZINCsum1_TEI_adjusted_norm_sd,retisum1_TEI_adjusted_norm_sd,
		karosum1_TEI_adjusted_norm_sd,TIAMsum1_TEI_adjusted_norm_sd,Folasum1_TEI_adjusted_norm_sd,B2sum1_TEI_adjusted_norm_sd,
		NIACsum1_TEI_adjusted_norm_sd,B6sum1_TEI_adjusted_norm_sd,B12sum1_TEI_adjusted_norm_sd,askosum1_TEI_adjusted_norm_sd,
		Dsum1_TEI_adjusted_norm_sd,tokosum1_TEI_adjusted_norm_sd,VITKsum1_TEI_adjusted_norm_sd,jernsum1_TEI_adjusted_norm_sd,
		JODIsum1_TEI_adjusted_norm_sd,kalcsum1_TEI_adjusted_norm_sd,KALIsum1_TEI_adjusted_norm_sd,g6,alkosum1_TEI_adjusted_norm_sd)

model_selection<-leaps(y=bmi_norm_sd,x=all_predictors,names=colnames(all_predictors),method="adjr2",strictly.compatible=F)

model_selection$which[order(-model_selection$adjr2)[1],]
#exclude:
#kolesum1_TEI_adjusted_norm_sd
#karosum1_TEI_adjusted_norm_sd
#Dsum1_TEI_adjusted_norm_sd
#B2sum1_TEI_adjusted_norm_sd
#MOSAsum1_TEI_adjusted_norm_sd
#fettsum1_TEI_adjusted_norm_sd

detach(VIP_data_independant_complete_cases)


selected_model<-lm(log(bmi)~age + agesq + gender_factor + year + ffq_factor + POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd + TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd+g6, data = VIP_data_independant_complete_cases)
summary(selected_model)
vif(selected_model)

#make matrices of bmi classes, to calculate AUC
#independent
bmi_classes_independent<-c()
bmi_classes_independent[VIP_data_independant_complete_cases$bmi_category==0]<-1
bmi_classes_independent[VIP_data_independant_complete_cases$bmi_category!=0]<-0
bmi_classes_independent<-cbind(bmi_classes_independent,bmi_classes_independent, bmi_classes_independent)
bmi_classes_independent[VIP_data_independant_complete_cases$bmi_category!=1,2]<-0
bmi_classes_independent[VIP_data_independant_complete_cases$bmi_category==1,2]<-1
bmi_classes_independent[VIP_data_independant_complete_cases$bmi_category!=2,3]<-0
bmi_classes_independent[VIP_data_independant_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_independent)<-c(1,2,3)

#full model
fitted_categorized<-full_model$fitted.values
fitted_categorized[fitted_categorized < log(25)]<-0
fitted_categorized[fitted_categorized >= log(25) & fitted_categorized < log(30)]<-1
fitted_categorized[fitted_categorized >= log(30)]<-2
fitted_categorized<-as.factor(fitted_categorized)

bmi_classes_independent_fitted<-c()
bmi_classes_independent_fitted[fitted_categorized==0]<-1
bmi_classes_independent_fitted[fitted_categorized!=0]<-0
bmi_classes_independent_fitted<-cbind(bmi_classes_independent_fitted,bmi_classes_independent_fitted, bmi_classes_independent_fitted)
bmi_classes_independent_fitted[fitted_categorized!=1,2]<-0
bmi_classes_independent_fitted[fitted_categorized==1,2]<-1
bmi_classes_independent_fitted[fitted_categorized!=2,3]<-0
bmi_classes_independent_fitted[fitted_categorized==2,3]<-1
colnames(bmi_classes_independent_fitted)<-c(1,2,3)

auc_independent_01<-pROC::auc(pROC::roc(bmi_classes_independent[,1],bmi_classes_independent_fitted[,1]))
auc_independent_02<-pROC::auc(pROC::roc(bmi_classes_independent[,2],bmi_classes_independent_fitted[,2]))
auc_independent_12<-pROC::auc(pROC::roc(bmi_classes_independent[,3],bmi_classes_independent_fitted[,3]))


mean(c(auc_independent_01,auc_independent_02,auc_independent_12))



#selected model
fitted_selected_categorized<-selected_model$fitted.values
fitted_selected_categorized[fitted_selected_categorized < log(25)]<-0
fitted_selected_categorized[fitted_selected_categorized >= log(25) & fitted_selected_categorized < log(30)]<-1
fitted_selected_categorized[fitted_selected_categorized >= log(30)]<-2
fitted_selected_categorized<-as.factor(fitted_selected_categorized)

bmi_classes_independent_fitted_selected<-c()
bmi_classes_independent_fitted_selected[fitted_selected_categorized==0]<-1
bmi_classes_independent_fitted_selected[fitted_selected_categorized!=0]<-0
bmi_classes_independent_fitted_selected<-cbind(bmi_classes_independent_fitted_selected,bmi_classes_independent_fitted_selected, bmi_classes_independent_fitted_selected)
bmi_classes_independent_fitted_selected[fitted_selected_categorized!=1,2]<-0
bmi_classes_independent_fitted_selected[fitted_selected_categorized==1,2]<-1
bmi_classes_independent_fitted_selected[fitted_selected_categorized!=2,3]<-0
bmi_classes_independent_fitted_selected[fitted_selected_categorized==2,3]<-1
colnames(bmi_classes_independent_fitted_selected)<-c(1,2,3)

auc_independent_01<-pROC::auc(pROC::roc(bmi_classes_independent[,1],bmi_classes_independent_fitted_selected[,1]))
auc_independent_02<-pROC::auc(pROC::roc(bmi_classes_independent[,2],bmi_classes_independent_fitted_selected[,2]))
auc_independent_12<-pROC::auc(pROC::roc(bmi_classes_independent[,3],bmi_classes_independent_fitted_selected[,3]))


mean(c(auc_independent_01,auc_independent_02,auc_independent_12))



#validate

selected_variables<-c("age","agesq","gender_factor","year","ffq_factor","POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
		"sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd","protsum1_anim_TEI_adjusted_norm_sd",
		"protsum1_veg_TEI_adjusted_norm_sd","fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd","NATRsum1_TEI_adjusted_norm_sd",
		"ensum1_norm_sd","MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd","selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd",
		"retisum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd",
		"B12sum1_TEI_adjusted_norm_sd","askosum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd","VITKsum1_TEI_adjusted_norm_sd", "jernsum1_TEI_adjusted_norm_sd",
		"JODIsum1_TEI_adjusted_norm_sd","kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","g6","fettsum1_TEI_adjusted_norm_sd")

#visit1
VIP_data_subset_visit1_complete_cases_selected<-VIP_data_subset_visit1_complete_cases[,selected_variables]

predicted_values_visit1<-predict(selected_model, VIP_data_subset_visit1_complete_cases_selected, se.fit = TRUE)
predicted_values_visit1_intervals<-predict(selected_model, VIP_data_subset_visit1_complete_cases_selected, interval = "prediction",level = 0.95)

residuals_visit1<-log(VIP_data_subset_visit1_complete_cases$bmi)-predicted_values_visit1$fit

R_sq<-1-var(na.omit(residuals_visit1))/var(log(VIP_data_subset_visit1_complete_cases$bmi))

N<-length(VIP_data_subset_visit1_complete_cases[,1])
k<-length(selected_variables)
R_sq_adj<-1-(1-R_sq)*(N-1)/(N-k-1)


bmi_classes_visit1<-c()
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==0]<-1
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=0]<-0
bmi_classes_visit1<-cbind(bmi_classes_visit1,bmi_classes_visit1, bmi_classes_visit1)
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=1,2]<-0
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==1,2]<-1
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category!=2,3]<-0
bmi_classes_visit1[VIP_data_subset_visit1_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_visit1)<-c(1,2,3)

predicted_categorized_visit1<-predicted_values_visit1$fit
predicted_categorized_visit1[predicted_categorized_visit1 < log(25)]<-0
predicted_categorized_visit1[predicted_categorized_visit1 >= log(25) & predicted_categorized_visit1 < log(30)]<-1
predicted_categorized_visit1[predicted_categorized_visit1 >= log(30)]<-2
predicted_categorized_visit1<-as.factor(predicted_categorized_visit1)

bmi_classes_visit1_predicted<-c()
bmi_classes_visit1_predicted[predicted_categorized_visit1==0]<-1
bmi_classes_visit1_predicted[predicted_categorized_visit1!=0]<-0
bmi_classes_visit1_predicted<-cbind(bmi_classes_visit1_predicted,bmi_classes_visit1_predicted, bmi_classes_visit1_predicted)
bmi_classes_visit1_predicted[predicted_categorized_visit1!=1,2]<-0
bmi_classes_visit1_predicted[predicted_categorized_visit1==1,2]<-1
bmi_classes_visit1_predicted[predicted_categorized_visit1!=2,3]<-0
bmi_classes_visit1_predicted[predicted_categorized_visit1==2,3]<-1
colnames(bmi_classes_visit1_predicted)<-c(1,2,3)



auc_visit1_01<-pROC::auc(pROC::roc(bmi_classes_visit1[,1],bmi_classes_visit1_predicted[,1]))
auc_visit1_02<-pROC::auc(pROC::roc(bmi_classes_visit1[,2],bmi_classes_visit1_predicted[,2]))
auc_visit1_12<-pROC::auc(pROC::roc(bmi_classes_visit1[,3],bmi_classes_visit1_predicted[,3]))

mean(c(auc_visit1_01,auc_visit1_02,auc_visit1_12))


#visit2
VIP_data_subset_visit2_complete_cases_selected<-VIP_data_subset_visit2_complete_cases[,selected_variables]

predicted_values_visit2<-predict(selected_model, VIP_data_subset_visit2_complete_cases_selected, se.fit = TRUE)
predicted_values_visit2_intervals<-predict(selected_model, VIP_data_subset_visit2_complete_cases_selected, interval = "prediction",level = 0.95)

residuals_visit2<-log(VIP_data_subset_visit2_complete_cases$bmi)-predicted_values_visit2$fit

R_sq<-1-var(na.omit(residuals_visit2))/var(log(VIP_data_subset_visit2_complete_cases$bmi))

N<-length(VIP_data_subset_visit2_complete_cases[,1])
k<-length(selected_variables)
R_sq_adj<-1-(1-R_sq)*(N-1)/(N-k-1)


#make matrix of bmi classes visit2
bmi_classes_visit2<-c()
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==0]<-1
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=0]<-0
bmi_classes_visit2<-cbind(bmi_classes_visit2,bmi_classes_visit2, bmi_classes_visit2)
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=1,2]<-0
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==1,2]<-1
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category!=2,3]<-0
bmi_classes_visit2[VIP_data_subset_visit2_complete_cases$bmi_category==2,3]<-1
colnames(bmi_classes_visit2)<-c(1,2,3)

predicted_categorized_visit2<-predicted_values_visit2$fit
predicted_categorized_visit2[predicted_categorized_visit2 < log(25)]<-0
predicted_categorized_visit2[predicted_categorized_visit2 >= log(25) & predicted_categorized_visit2 < log(30)]<-1
predicted_categorized_visit2[predicted_categorized_visit2 >= log(30)]<-2
predicted_categorized_visit2<-as.factor(predicted_categorized_visit2)

bmi_classes_visit2_predicted<-c()
bmi_classes_visit2_predicted[predicted_categorized_visit2==0]<-1
bmi_classes_visit2_predicted[predicted_categorized_visit2!=0]<-0
bmi_classes_visit2_predicted<-cbind(bmi_classes_visit2_predicted,bmi_classes_visit2_predicted, bmi_classes_visit2_predicted)
bmi_classes_visit2_predicted[predicted_categorized_visit2!=1,2]<-0
bmi_classes_visit2_predicted[predicted_categorized_visit2==1,2]<-1
bmi_classes_visit2_predicted[predicted_categorized_visit2!=2,3]<-0
bmi_classes_visit2_predicted[predicted_categorized_visit2==2,3]<-1
colnames(bmi_classes_visit2_predicted)<-c(1,2,3)



auc_visit2_01<-pROC::auc(pROC::roc(bmi_classes_visit2[,1],bmi_classes_visit2_predicted[,1]))
auc_visit2_02<-pROC::auc(pROC::roc(bmi_classes_visit2[,2],bmi_classes_visit2_predicted[,2]))
auc_visit2_12<-pROC::auc(pROC::roc(bmi_classes_visit2[,3],bmi_classes_visit2_predicted[,3]))

mean(c(auc_visit2_01,auc_visit2_02,auc_visit2_12))


#make the plots
boxplot(predicted_values_visit1$fit~as.numeric(VIP_data_subset_visit1_complete_cases$bmi_category),col=c("gray85","gray60","gray35"))
boxplot(predicted_values_visit2$fit~as.numeric(VIP_data_subset_visit2_complete_cases$bmi_category),col=c("gray85","gray60","gray35"))




#construct and save the continuous and discrete phenotypes......caution messy code below
source(file="load_phenotypes.R")



#link with GLACIER and make a phenotype dataset for GCTA
link_data<- read.table("../Documents/femvfemk_qdates.txt", header = TRUE,row.names=NULL,sep="\t")

#get the intersect of subjects that have GWAS data in two visits
subjects_total1<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$enummer %in% link_data$enummer]

subjects_total2<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$enummer %in% link_data$enummer]

subject_total<-intersect(subjects_total1,subjects_total2)

#get the enummers for those subjects(from the first set)
enummers_total<-VIP_data_subset_visit1_complete_cases$enummer[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]




#glacier_id  persistant phenotype  identifierC1 identifierC2 ....
final_dataset<-data.frame(matrix(ncol=15,nrow=length(enummers_total)))
colnames(final_dataset)<-c("glacier_ID", "pat_code_dna", "resistance_phenotype", "bmi_basic_fit_log", "bmi_log", 
		"susceptible_case_control_compliant_1", "resistance_case_control_compliant_1", "resistance_case_control_susceptible_1",
		"susceptible_case_control_compliant_2", "resistance_case_control_susceptible_2", "resistance_case_control_compliant_2", 
		"case_bmi_low_control_bmi_middle", "case_bmi_high_control_bmi_middle",
		"case_bmi_high_control_susceptible", "case_bmi_low_control_resistance")

#id columns
final_dataset$glacier_ID<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% enummers_total]
final_dataset$pat_code_dna<-link_data$pat_code_dna[!is.na(link_data$pat_code_dna) & link_data$enummer %in% enummers_total]

#make a new continous phenotype, taking the mean of the two residuals at each visit
final_dataset$resistance_phenotype<-as.numeric((VIP_data_subset_visit1_complete_cases$resistance_continuous[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
			VIP_data_subset_visit2_complete_cases$resistance_continuous[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2)

#make a mean of the residuals of model of log(bmi) with the basic variables:
basic_model<-lm(log(bmi)~age + agesq + gender_factor + year + ffq_factor, data=VIP_data_independant_complete_cases)
predicted_values_visit1_basic<-predict(basic_model, VIP_data_subset_visit1_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor")], se.fit = TRUE)
predicted_values_visit2_basic<-predict(basic_model, VIP_data_subset_visit2_complete_cases[,c("age","agesq","gender_factor","year","ffq_factor")], se.fit = TRUE)
residuals_visit1_basic<-log(VIP_data_subset_visit1_complete_cases$bmi)-predicted_values_visit1_basic$fit
residuals_visit2_basic<-log(VIP_data_subset_visit2_complete_cases$bmi)-predicted_values_visit2_basic$fit
final_dataset$bmi_basic_fit_log<-(residuals_visit1_basic[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
			residuals_visit2_basic[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2


#make a mean of the log(bmi) of the two visits:
final_dataset$bmi_log<-(log(VIP_data_subset_visit1_complete_cases$bmi[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])+
			log(VIP_data_subset_visit2_complete_cases$bmi[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total]))/2

#add the rest of phenotypes
final_dataset$resistance_case_control_susceptible_1<-as.numeric(resistance_case_control_susceptible_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$resistance_case_control_compliant_1<-as.numeric(resistance_case_control_compliant_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$susceptible_case_control_compliant_1<-as.numeric(susceptible_case_control_compliant_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])

final_dataset$resistance_case_control_compliant_2<-as.numeric(resistance_case_control_compliant_data_2[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$susceptible_case_control_compliant_2<-as.numeric(susceptible_case_control_compliant_data_2[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$resistance_case_control_susceptible_2<-as.numeric(resistance_case_control_susceptible_data_2[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])

#additional
final_dataset$case_bmi_low_control_bmi_middle<-as.numeric(case_bmi_low_control_bmi_middle_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$case_bmi_high_control_bmi_middle<-as.numeric(case_bmi_high_control_bmi_middle_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$case_bmi_high_control_susceptible<-as.numeric(case_bmi_high_control_susceptible_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])
final_dataset$case_bmi_low_control_resistance<-as.numeric(case_bmi_low_control_resistance_data[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total])


#set family id to 0 for GCTA
final_dataset$glacier_ID<-0


final_dataset_copy<-final_dataset
#remove column names for GCTA
colnames(final_dataset)<-NULL
write.table(final_dataset, "../Results/persistantly_lean_subjects/final_persistant_phenotype_dataset_final2209.phen", row.names=FALSE,sep="\t",na="NA")




#get the lengths
attach(final_dataset_copy)

length(subject_total)
#3762

length(resistance_case_control_susceptible_1[resistance_case_control_susceptible_1=="0" & !is.na(resistance_case_control_susceptible_1)])
#410
length(susceptible_case_control_compliant_1[susceptible_case_control_compliant_1=="1" & !is.na(susceptible_case_control_compliant_1)])
#796
length(resistance_case_control_compliant_1[resistance_case_control_compliant_1=="0" & !is.na(resistance_case_control_compliant_1)])
#1921


length(resistance_case_control_susceptible_2[resistance_case_control_susceptible_2=="0" & !is.na(resistance_case_control_susceptible_2)])
#28
length(resistance_case_control_compliant_2[resistance_case_control_compliant_2=="0" & !is.na(resistance_case_control_compliant_2)])
#3635
length(susceptible_case_control_compliant_2[susceptible_case_control_compliant_2=="1" & !is.na(susceptible_case_control_compliant_2)])
#64


length(case_bmi_low_control_bmi_middle[case_bmi_low_control_bmi_middle=="1" & !is.na(case_bmi_low_control_bmi_middle)])
#434
length(case_bmi_low_control_bmi_middle[case_bmi_low_control_bmi_middle=="0" & !is.na(case_bmi_low_control_bmi_middle)])
#3095
length(case_bmi_high_control_bmi_middle[case_bmi_high_control_bmi_middle=="1" & !is.na(case_bmi_high_control_bmi_middle)])
#257
length(case_bmi_high_control_bmi_middle[case_bmi_high_control_bmi_middle=="0" & !is.na(case_bmi_high_control_bmi_middle)])
#3364


length(case_bmi_high_control_susceptible[case_bmi_high_control_susceptible=="1" & !is.na(case_bmi_high_control_susceptible)])
#63
length(case_bmi_high_control_susceptible[case_bmi_high_control_susceptible=="0" & !is.na(case_bmi_high_control_susceptible)])
#194
length(case_bmi_low_control_resistance[case_bmi_low_control_resistance=="1" & !is.na(case_bmi_low_control_resistance)])
#26
length(case_bmi_low_control_resistance[case_bmi_low_control_resistance=="0" & !is.na(case_bmi_low_control_resistance)])
#408



detach(final_dataset_copy)

