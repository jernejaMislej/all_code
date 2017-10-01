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



#take only complete cases

# independent....44735

VIP_data_independant_complete_cases <- VIP_data_independant[!is.na(VIP_data_independant$bmi),c("Subject_id","bmi","bmi_norm_sd","age","agesq","gender_factor","year","ffq_factor",
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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer","fibesum1_TEI_adjusted_norm_sd","alkosum1_TEI_adjusted_norm_sd","utbild")]
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi < 25]<-0
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 25 & VIP_data_independant_complete_cases$bmi < 30]<-1
VIP_data_independant_complete_cases$bmi_category[VIP_data_independant_complete_cases$bmi >= 30]<-2
VIP_data_independant_complete_cases$bmi_category<-as.factor(VIP_data_independant_complete_cases$bmi_category)
VIP_data_independant_complete_cases$utbild<-as.factor(VIP_data_independant_complete_cases$utbild)
VIP_data_independant_complete_cases$g6<-as.factor(VIP_data_independant_complete_cases$g6)

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
						"kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","enummer","fibesum1_TEI_adjusted_norm_sd","alkosum1_TEI_adjusted_norm_sd")])
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi < 25]<-0
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 25 & VIP_data_subset_visit1_complete_cases$bmi < 30]<-1
VIP_data_subset_visit1_complete_cases$bmi_category[VIP_data_subset_visit1_complete_cases$bmi >= 30]<-2
VIP_data_subset_visit1_complete_cases$bmi_category<-as.factor(VIP_data_subset_visit1_complete_cases$bmi_category)
VIP_data_subset_visit1_complete_cases$sm_status<-VIP_data_subset_visit1[VIP_data_subset_visit1$enummer %in% VIP_data_subset_visit1_complete_cases$enummer,"sm_status"]
VIP_data_subset_visit1_complete_cases$utbild<-VIP_data_subset_visit1[VIP_data_subset_visit1$enummer %in% VIP_data_subset_visit1_complete_cases$enummer,"utbild"]
VIP_data_subset_visit1_complete_cases$utbild<-as.factor(VIP_data_subset_visit1_complete_cases$utbild)
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
VIP_data_subset_visit2_complete_cases$utbild<-VIP_data_subset_visit2[VIP_data_subset_visit2$enummer %in% VIP_data_subset_visit2_complete_cases$enummer,"utbild"]
VIP_data_subset_visit2_complete_cases$utbild<-as.factor(VIP_data_subset_visit2_complete_cases$utbild)
VIP_data_subset_visit2_complete_cases$g6<-as.factor(VIP_data_subset_visit2_complete_cases$g6)


#keep subjects that were in the complete cases in both visits
VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id %in% VIP_data_subset_visit1_complete_cases$Subject_id,]
VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id %in% VIP_data_subset_visit2_complete_cases$Subject_id,]

#one guy ends up with very low environment score and goes from being normal weight in visit 1 to overweight in visit2, he is excluded
#VIP_data_subset_visit2_complete_cases<-VIP_data_subset_visit2_complete_cases[VIP_data_subset_visit2_complete_cases$Subject_id !=63335,]
#VIP_data_subset_visit1_complete_cases<-VIP_data_subset_visit1_complete_cases[VIP_data_subset_visit1_complete_cases$Subject_id !=63335,]

#check pairwise correlations

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
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd+g6, data = VIP_data_independant_complete_cases)
summary(full_model)
vif(full_model)

#select the best, based on R²
all_predictors<-VIP_data_independant_complete_cases(,c("age","agesq","gender_factor","year","ffq_factor","POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
				"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd",
				"protsum1_anim_TEI_adjusted_norm_sd","protsum1_veg_TEI_adjusted_norm_sd",
				"fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","MOSAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd",
				"NATRsum1_TEI_adjusted_norm_sd","kolesum1_TEI_adjusted_norm_sd","ensum1_norm_sd"," MAGNsum1_TEI_adjusted_norm_sd",
				"FOSFsum1_TEI_adjusted_norm_sd","selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd","retisum1_TEI_adjusted_norm_sd",
				"karosum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd","B2sum1_TEI_adjusted_norm_sd",
				"NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd","B12sum1_TEI_adjusted_norm_sd","askosum1_TEI_adjusted_norm_sd",
				"Dsum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd","VITKsum1_TEI_adjusted_norm_sd","jernsum1_TEI_adjusted_norm_sd",
				"JODIsum1_TEI_adjusted_norm_sd","kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","g6","fibesum1_TEI_adjusted_norm_sd",
				"alkosum1_TEI_adjusted_norm_sd","utbild"))

model_selection<-leaps(y=VIP_data_independant_complete_cases$bmi_norm_sd,x=all_predictors,names=colnames(all_predictors),method="adjr2",strictly.compatible=F)

model_selection$which[order(-model_selection$adjr2)[1],]
#exclude:
#kolesum1_TEI_adjusted_norm_sd
#fibesum1_TEI_adjusted_norm_sd
#karosum1_TEI_adjusted_norm_sd
#Dsum1_TEI_adjusted_norm_sd
#B2sum1_TEI_adjusted_norm_sd
#MOSAsum1_TEI_adjusted_norm_sd

selected_model<-lm(log(bmi)~age + agesq + gender_factor + year + ffq_factor + POLYsum1_TEI_adjusted_norm_sd+MONOsum1_TEI_adjusted_norm_sd+mfetsum1_TEI_adjusted_norm_sd+
				sacksum1_TEI_adjusted_norm_sd+kolhsum1_TEI_adjusted_norm_sd+FA_TEI_adjusted_norm_sd+
				protsum1_anim_TEI_adjusted_norm_sd+protsum1_veg_TEI_adjusted_norm_sd+
				fibesum1_TEI_adjusted_norm_sd+DISAsum1_TEI_adjusted_norm_sd + TRANSsum1_TEI_adjusted_norm_sd+
				NATRsum1_TEI_adjusted_norm_sd+ensum1_norm_sd+ MAGNsum1_TEI_adjusted_norm_sd+
				FOSFsum1_TEI_adjusted_norm_sd+selesum1_TEI_adjusted_norm_sd+ZINCsum1_TEI_adjusted_norm_sd+retisum1_TEI_adjusted_norm_sd+
				TIAMsum1_TEI_adjusted_norm_sd+Folasum1_TEI_adjusted_norm_sd+
				NIACsum1_TEI_adjusted_norm_sd+B6sum1_TEI_adjusted_norm_sd+B12sum1_TEI_adjusted_norm_sd+askosum1_TEI_adjusted_norm_sd+
				tokosum1_TEI_adjusted_norm_sd+VITKsum1_TEI_adjusted_norm_sd+
				JODIsum1_TEI_adjusted_norm_sd+kalcsum1_TEI_adjusted_norm_sd+KALIsum1_TEI_adjusted_norm_sd+g6+alkosum1_TEI_adjusted_norm_sd+utbild, data = VIP_data_independant_complete_cases)
summary(selected_model)
vif(selected_model)




#validate

selected_variables<-c("age","agesq","gender_factor","year","ffq_factor","POLYsum1_TEI_adjusted_norm_sd","MONOsum1_TEI_adjusted_norm_sd","mfetsum1_TEI_adjusted_norm_sd",
		"fettsum1_TEI_adjusted_norm_sd","sacksum1_TEI_adjusted_norm_sd","kolhsum1_TEI_adjusted_norm_sd","FA_TEI_adjusted_norm_sd","protsum1_anim_TEI_adjusted_norm_sd",
		"protsum1_veg_TEI_adjusted_norm_sd","fibesum1_TEI_adjusted_norm_sd","DISAsum1_TEI_adjusted_norm_sd","TRANSsum1_TEI_adjusted_norm_sd","NATRsum1_TEI_adjusted_norm_sd",
		"ensum1_norm_sd","MAGNsum1_TEI_adjusted_norm_sd","FOSFsum1_TEI_adjusted_norm_sd","selesum1_TEI_adjusted_norm_sd","ZINCsum1_TEI_adjusted_norm_sd",
		"retisum1_TEI_adjusted_norm_sd","TIAMsum1_TEI_adjusted_norm_sd","Folasum1_TEI_adjusted_norm_sd","NIACsum1_TEI_adjusted_norm_sd","B6sum1_TEI_adjusted_norm_sd",
		"B12sum1_TEI_adjusted_norm_sd","askosum1_TEI_adjusted_norm_sd","tokosum1_TEI_adjusted_norm_sd","VITKsum1_TEI_adjusted_norm_sd",
		"JODIsum1_TEI_adjusted_norm_sd","kalcsum1_TEI_adjusted_norm_sd","KALIsum1_TEI_adjusted_norm_sd","g6","alkosum1_TEI_adjusted_norm_sd","utbild")

#visit1
VIP_data_subset_visit1_selected<-VIP_data_subset_visit1_complete_cases[,selected_variables]

predicted_values_visit1<-predict(selected_model, VIP_data_subset_visit1_selected, se.fit = TRUE)
predicted_values_visit1_intervals<-predict(selected_model, VIP_data_subset_visit1_selected, interval = "prediction",level = 0.95)

residuals_visit1<-log(VIP_data_subset_visit1_complete_cases$bmi)-predicted_values_visit1$fit

#visit2
VIP_data_subset_visit2_selected<-VIP_data_subset_visit2_complete_cases[,selected_variables]

predicted_values_visit2<-predict(selected_model, VIP_data_subset_visit2_selected, se.fit = TRUE)
predicted_values_visit2_intervals<-predict(selected_model, VIP_data_subset_visit2_selected, interval = "prediction",level = 0.95)

residuals_visit2<-log(VIP_data_subset_visit2_complete_cases$bmi)-predicted_values_visit2$fit



#-------------resistance_continuous-------------------
VIP_data_subset_visit1_complete_cases$resistance_continuous<-residuals_visit1
#-------------resistance_continuous-------------------


#-------------resistance_continuous-------------------
VIP_data_subset_visit2_complete_cases$resistance_continuous<-residuals_visit2
#-------------resistance_continuous-------------------

#link with GLACIER and make a phenotype dataset for GCTA
link_data<- read.table("../Documents/femvfemk_qdates.txt", header = TRUE,row.names=NULL,sep="\t")

#get the intersect of subjects that have GWAS data in two visits
subjects_total1<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$enummer %in% link_data$enummer]

subjects_total2<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$enummer %in% link_data$enummer]

subject_total<-intersect(subjects_total1,subjects_total2)

#get the enummers for those subjects(from the first set)
enummers_total<-VIP_data_subset_visit1_complete_cases$enummer[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]


#glacier_id  persistant phenotype  identifierC1 identifierC2 ....
final_dataset<-data.frame(matrix(ncol=4,nrow=length(enummers_total)))
colnames(final_dataset)<-c("glacier_ID", "pat_code_dna", "resistance_phenotype","mean_bmi")

#id columns
final_dataset$glacier_ID<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% enummers_total]
final_dataset$pat_code_dna<-link_data$pat_code_dna[!is.na(link_data$pat_code_dna) & link_data$enummer %in% enummers_total]

#make a new continous phenotype, taking the mean of the two residuals at each visit
final_dataset$resistance_phenotype<-as.numeric((VIP_data_subset_visit1_complete_cases$resistance_continuous[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
					VIP_data_subset_visit2_complete_cases$resistance_continuous[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2)

final_dataset$mean_bmi<-(VIP_data_subset_visit1_complete_cases$bmi[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
			VIP_data_subset_visit2_complete_cases$bmi[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2


#set family id to 0 for GCTA
final_dataset$glacier_ID<-0

#remove column names for GCTA
colnames(final_dataset)<-NULL
write.table(final_dataset, "../Results/persistantly_lean_subjects/final_persistant_phenotype_dataset_test9.phen", row.names=FALSE,sep="\t",na="NA")


