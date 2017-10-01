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


#make a diet score 

#take only complete cases

# independent....44735

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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer")])
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi < 25]<-0
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 25 & VIP_data_independant_complete_cases$bmi < 30]<-1
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 30]<-2
VIP_data_independant_complete_cases$bmi_category<-as.factor(VIP_data_independant_complete_cases$bmi_category)


#visit1 & visit2....30380

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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer")])
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi < 25]<-0
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 25 & VIP_data_subset_visit1_complete_cases$bmi < 30]<-1
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit1_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit1_complete_cases$bmi_category)
VIP_data_subset_visit1_complete_cases$sm_status<-VIP_data_subset_visit1[VIP_data_subset_visit1$enummer %in% VIP_data_subset_visit1_complete_cases$enummer,"sm_status"]

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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer")])
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi < 25]<-0
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 25 & VIP_data_subset_visit2_complete_cases$bmi < 30]<-1
VIP_data_subset_visit2_complete_cases$bmi_category[VIP_data_subset_visit2_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit2_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit2_complete_cases$bmi_category)
VIP_data_subset_visit2_complete_cases$sm_status<-VIP_data_subset_visit2[VIP_data_subset_visit2$enummer %in% VIP_data_subset_visit2_complete_cases$enummer,"sm_status"]


#keep subjects that were in the complete cases in both visits
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]

#one guy ends up with very low environment score and goes from being normal weight in visit 1 to overweight in visit2, he is excluded
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id !=63335,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id !=63335,]

#check pairwise correlations
attach(VIP_data_independant_complete_cases)

#make the model
full_model<-lm(log(bmi)~age + agesq + gender_factor + year + ffq_factor + POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				fettsum1_TEI_adjusted_norm_sd+sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+MOSAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+kolesum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				karosum1_TEI_adjusted_norm_sd+TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+B2sum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				Dsum1_TEI_adjusted_norm_sd+tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd+g6)
summary(full_model)
vif(full_model)

#select the best, based on R²
all_predictors<-cbind(age,agesq,gender_factor,year,ffq_factor,POLYsum1_TEI_adjusted_norm_sd,MONOsum1_TEI_adjusted_norm_sd,mfetsum1_TEI_adjusted_norm_sd,
				fettsum1_TEI_adjusted_norm_sd,sacksum1_TEI_adjusted_norm_sd,kolhsum1_TEI_adjusted_norm_sd,FA_TEI_adjusted_norm_sd,
				protsum1_anim_TEI_adjusted_norm_sd,protsum1_veg_TEI_adjusted_norm_sd,
				fibesum1_TEI_adjusted_norm_sd,DISAsum1_TEI_adjusted_norm_sd,MOSAsum1_TEI_adjusted_norm_sd,TRANSsum1_TEI_adjusted_norm_sd,
				NATRsum1_TEI_adjusted_norm_sd,kolesum1_TEI_adjusted_norm_sd,ensum1_norm_sd, MAGNsum1_TEI_adjusted_norm_sd,
				FOSFsum1_TEI_adjusted_norm_sd,selesum1_TEI_adjusted_norm_sd,ZINCsum1_TEI_adjusted_norm_sd,retisum1_TEI_adjusted_norm_sd,
				karosum1_TEI_adjusted_norm_sd,TIAMsum1_TEI_adjusted_norm_sd,Folasum1_TEI_adjusted_norm_sd,B2sum1_TEI_adjusted_norm_sd,
				NIACsum1_TEI_adjusted_norm_sd,B6sum1_TEI_adjusted_norm_sd,B12sum1_TEI_adjusted_norm_sd,askosum1_TEI_adjusted_norm_sd,
				Dsum1_TEI_adjusted_norm_sd,tokosum1_TEI_adjusted_norm_sd,VITKsum1_TEI_adjusted_norm_sd,jernsum1_TEI_adjusted_norm_sd,
				JODIsum1_TEI_adjusted_norm_sd,kalcsum1_TEI_adjusted_norm_sd,KALIsum1_TEI_adjusted_norm_sd,g6)
		
model_selection<-leaps(y=bmi_norm_sd,x=all_predictors,names=colnames(all_predictors),method="adjr2",strictly.compatible=F)

model_selection$which[order(-model_selection$adjr2)[1],]
#exclude:
#MOSAsum1_TEI_adjusted_norm_sd
#kolesum1_TEI_adjusted_norm_sd
#karosum1_TEI_adjusted_norm_sd
#Dsum1_TEI_adjusted_norm_sd
#B2sum1_TEI_adjusted_norm_sd

selected_model<-lm(log(bmi)~age + agesq + gender_factor + year + ffq_factor + POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd+TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+jernsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd+g6)
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
#
#> mean(auc_independent_01,auc_independent_02,auc_independent_12)
#[1] 0.5480433
#> auc_independent_01
#Area under the curve: 0.5888
#> auc_independent_02
#Area under the curve: 0.5538
#> auc_independent_12
#Area under the curve: 0.5016



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
#
#> mean(auc_independent_01,auc_independent_02,auc_independent_12)
#[1] 0.5480885
#> auc_independent_01
#Area under the curve: 0.589
#> auc_independent_02
#Area under the curve: 0.5538
#> auc_independent_12
#Area under the curve: 0.5015
#> 
detach(VIP_data_independant_complete_cases)



#validate

selected_variables<-c("age","agesq","gender_factor","year","ffq_factor","POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
		"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd","protsum1_anim_TEI_adjusted_norm_sd",
		"protsum1_veg_TEI_adjusted_norm_sd","fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd","NATRsum1_TEI_adjusted_norm_sd",
		"ensum1_norm_sd","MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd","selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd",
		"retisum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd",
		"B12sum1_TEI_adjusted_norm_sd","askosum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd","VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd",
		"JODIsum1_TEI_adjusted_norm_sd","kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","g6")

#visit1
VIP_data_subset_visit1_complete_cases_selected<-VIP_data_subset_visit1_complete_cases[,selected_variables]

predicted_values_visit1<-predict(selected_model, VIP_data_subset_visit1_complete_cases_selected, se.fit = TRUE)

residuals_visit1<-log(VIP_data_subset_visit1_complete_cases$bmi)-predicted_values_visit1$fit

R_sq<-1-var(residuals_visit1)/var(log(VIP_data_subset_visit1_complete_cases$bmi))
		
N<-length(VIP_data_subset_visit1_complete_cases[,1])
k<-length(selected_variables)
R_sq_adj<-1-(1-R_sq)*(N-1)/(N-k-1)
#	
#> R_sq
#[1] 0.07438811
#> N<-length(VIP_data_subset_visit1_complete_cases[,1])
#> k<-length(selected_variables)
#> R_sq_adj<-1-(1-R_sq)*(N-1)/(N-k-1)
#> R_sq_adj
#[1] 0.07325939

		
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
#> mean(c(auc_visit1_01,auc_visit1_02,auc_visit1_12))
#[1] 0.5614187
#> auc_visit1_01
#Area under the curve: 0.6019
#> auc_visit1_02
#Area under the curve: 0.5821
#> auc_visit1_12
#Area under the curve: 0.5003


#visit2
VIP_data_subset_visit2_complete_cases_selected<-VIP_data_subset_visit2_complete_cases[,selected_variables]

predicted_values_visit2<-predict(selected_model, VIP_data_subset_visit2_complete_cases_selected, se.fit = TRUE)

residuals_visit2<-log(VIP_data_subset_visit2_complete_cases$bmi)-predicted_values_visit2$fit

R_sq<-1-var(residuals_visit2)/var(log(VIP_data_subset_visit2_complete_cases$bmi))

N<-length(VIP_data_subset_visit2_complete_cases[,1])
k<-length(selected_variables)
R_sq_adj<-1-(1-R_sq)*(N-1)/(N-k-1)
#
#> R_sq
#[1] 0.07776709
#> N<-length(VIP_data_subset_visit2_complete_cases[,1])
#> k<-length(selected_variables)
#> R_sq_adj<-1-(1-R_sq)*(N-1)/(N-k-1)
#> R_sq_adj
#[1] 0.07664249


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
#> mean(c(auc_visit2_01,auc_visit2_02,auc_visit2_12))
#[1] 0.5335895
#> 
#		> auc_visit2_01
#Area under the curve: 0.5628
#> auc_visit2_02
#Area under the curve: 0.537
#> auc_visit2_12
#Area under the curve: 0.5009
#>


#make the plots
boxplot(predicted_values_visit1$fit~as.numeric(VIP_data_subset_visit1_complete_cases$bmi_category),col=c("gray85","gray60","gray35"))
boxplot(predicted_values_visit2$fit~as.numeric(VIP_data_subset_visit2_complete_cases$bmi_category),col=c("gray85","gray60","gray35"))


#construct and save the continuous and discrete phenotypes, check pdf for heritability data for more info, note that compliant subjects will not have any persistently compliant 
# obese subjects, while susceptible subjects will only have overweight subjects, classified as normal, but no obese subjects classified as normal or overweight
# exclude smokers from resistant subjects and former smokers from susceptible subjects

#1. resistance_case_control_susceptible
#Control 0:
#		Susceptible subjects
#Case 1:
#		Resistant subjects

#2. resistance_continuous
#Continuous==mean of the residuals from both visits

#3.1. resistance_case_control_AB
#A:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Resistant subjects
#
#B:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Susceptible subjects

#3.2. resistance_case_control_ABCDE
#A:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Misclassified subjects 10
#
#B:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Misclassified subjects 21
#
#C:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Misclassified subjects 01
#
#D:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Misclassified subjects 02
#
#E:
#		Control 0:
#		Compliant subjects
#Case 1:
#		Misclassified subjects 12

#all classes:

#persistently correct(should have the least genetic variance explained)
#00
#11
#22....empty

#persistently higher bmi predicted for one class(should have some genetic variance explained)
#01
#12
#persistently higher bmi predicted for two class(should have the most genetic variance explained)
#02

#persistently lower bmi predicted for one class(should have some genetic variance explained)
#10
#21....empty
#persistently lower bmi predicted for two class(should have the most genetic variance explained)
#20....empty



#visit1
#-------------resistance_case_control_susceptible-------------------
empty_vector<-vector()
length(empty_vector)<-length(VIP_data_subset_visit1_complete_cases[,1])
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible<-empty_vector
#susceptible
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
	| (predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==2)) & VIP_data_subset_visit1_complete_cases$sm_status!=2
	& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-0
#resistant
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
	| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0)) & VIP_data_subset_visit1_complete_cases$sm_status!=1 
	& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1

#-------------resistance_case_control_susceptible-------------------


#-------------resistance_continuous-------------------
VIP_data_subset_visit1_complete_cases$resistance_continuous<-residuals_visit1
#-------------resistance_continuous-------------------


#-------------resistance_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#resistant
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant[((predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0)) & VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------resistance_case_control_compliant-------------------

#-------------susceptible_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$resistance_case_control_B<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$resistance_case_control_B[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#susceptible
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==2))  & VIP_data_subset_visit1_complete_cases$sm_status!=2 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------susceptible_case_control_compliant-------------------


#-------------01_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$case_01_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$case_01_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#01
VIP_data_subset_visit1_complete_cases$case_01_control_compliant[(predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0)& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------01_case_control_compliant-------------------

#-------------02_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$case_02_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$case_02_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#02
VIP_data_subset_visit1_complete_cases$case_02_control_compliant[(predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0)& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------01_case_control_compliant-------------------

#-------------10_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$case_10_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$case_10_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#10
VIP_data_subset_visit1_complete_cases$case_10_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==1)& VIP_data_subset_visit1_complete_cases$sm_status!=2 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------10_case_control_compliant-------------------

#-------------12_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$case_12_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$case_12_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#12
VIP_data_subset_visit1_complete_cases$case_12_control_compliant[(predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==2)& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------12_case_control_compliant-------------------




#visit2
#-------------resistance_case_control_susceptible-------------------
empty_vector<-vector()
length(empty_vector)<-length(VIP_data_subset_visit2_complete_cases[,1])
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible<-empty_vector
#susceptible
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
					| (predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==2)) & VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-0
#resistant
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
					| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0)) & VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1

#-------------resistance_case_control_susceptible-------------------


#-------------resistance_continuous-------------------
VIP_data_subset_visit2_complete_cases$resistance_continuous<-residuals_visit2
#-------------resistance_continuous-------------------


#-------------resistance_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#resistant
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant[((predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
					| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0)) & VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------resistance_case_control_compliant-------------------

#-------------susceptible_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$resistance_case_control_B<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$resistance_case_control_B[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#susceptible
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
					| (predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==2))  & VIP_data_subset_visit2_complete_cases$sm_status!=2 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------susceptible_case_control_compliant-------------------


#-------------01_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$case_01_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$case_01_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#01
VIP_data_subset_visit2_complete_cases$case_01_control_compliant[(predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0)& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------01_case_control_compliant-------------------

#-------------02_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$case_02_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$case_02_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#02
VIP_data_subset_visit2_complete_cases$case_02_control_compliant[(predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0)& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------01_case_control_compliant-------------------

#-------------10_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$case_10_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$case_10_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#10
VIP_data_subset_visit2_complete_cases$case_10_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==1)& VIP_data_subset_visit2_complete_cases$sm_status!=2 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------10_case_control_compliant-------------------

#-------------12_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$case_12_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$case_12_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#12
VIP_data_subset_visit2_complete_cases$case_12_control_compliant[(predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==2)& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------12_case_control_compliant-------------------

#save the datasets for further analysis in selection
write.csv(VIP_data_subset_visit1_complete_cases, "../VIP_data/VIP_data_subset_visit1_complete_cases_2908.csv", row.names=FALSE, na="")
write.csv(VIP_data_subset_visit2_complete_cases, "../VIP_data/VIP_data_subset_visit2_complete_cases_2908.csv", row.names=FALSE, na="")

link_data<- read.table("../Documents/femvfemk_qdates.txt", header = TRUE,row.names=NULL,sep="\t")

#save the data and enummers for the persistant ones

resistance_case_control_susceptible_enummers_0<-VIP_data_subset_visit1_complete_cases$enummers[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible) & VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible==0
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible==0]
resistance_case_control_susceptible_enummers_0<-VIP_data_subset_visit1_complete_cases$enummers[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible) & VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible==0
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible==1]
resistance_case_control_susceptible_enummers<-c(resistance_case_control_susceptible_enummers_0,resistance_case_control_susceptible_enummers_1)
resistance_case_control_susceptible_data<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$enummer %in% resistance_case_control_susceptible_enummers,"resistance_case_control_susceptible"]

