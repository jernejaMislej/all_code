VIP_data_subset_visit1_complete_cases<- read.csv("../VIP_data/VIP_data_subset_visit1_complete_cases_2908.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)
VIP_data_subset_visit2_complete_cases<- read.csv("../VIP_data/VIP_data_subset_visit2_complete_cases_2908.csv", header = TRUE, sep = ",", row.names = NULL, fill=TRUE)

link_data<- read.table("../Documents/femvfemk_qdates.txt", header = TRUE,row.names=NULL,sep="\t")

persistent_subjects_biclass_strict_0<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_strict_0",row.names=NULL, header = FALSE))
persistent_subjects_biclass_strict_1_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_strict_1_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_biclass_strict_0_opposite<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_strict_0_opposite",row.names=NULL, header = FALSE))
persistent_subjects_biclass_strict_1_opposite_non_former_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_strict_1_opposite_non_former_smokers",row.names=NULL, header = FALSE))

persistent_subjects_biclass_0<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_0",row.names=NULL, header = FALSE))
persistent_subjects_biclass_1_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_1_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_biclass_0_opposite<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_0_opposite",row.names=NULL, header = FALSE))
persistent_subjects_biclass_1_opposite_non_former_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_biclass_1_opposite_non_former_smokers",row.names=NULL, header = FALSE))

persistent_subjects_multiclass_0<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_multiclass_0",row.names=NULL, header = FALSE))
persistent_subjects_multiclass_1_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_multiclass_1_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_multiclass_2_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_multiclass_2_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_multiclass_minus1_non_former_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_multiclass_minus1_non_former_smokers",row.names=NULL, header = FALSE))

persistent_subjects_allmulticlass_00<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_allmulticlass_00",row.names=NULL, header = FALSE))
persistent_subjects_allmulticlass_11<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_allmulticlass_11",row.names=NULL, header = FALSE))
persistent_subjects_allmulticlass_01_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_allmulticlass_01_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_allmulticlass_02_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_allmulticlass_02_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_allmulticlass_12_non_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_allmulticlass_12_non_smokers",row.names=NULL, header = FALSE))
persistent_subjects_allmulticlass_10_non_former_smokers<-unlist(read.table("../Results/persistantly_lean_subjects/enummers_allmulticlass_10_non_former_smokers",row.names=NULL, header = FALSE))


GLACIER_ids_biclass_strict_0<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_strict_0]
length(GLACIER_ids_biclass_strict_0)

GLACIER_ids_biclass_strict_1_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_strict_1_non_smokers]
length(GLACIER_ids_biclass_strict_1_non_smokers)

GLACIER_ids_biclass_strict_0_opposite<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_strict_0_opposite]
length(GLACIER_ids_biclass_strict_0_opposite)

GLACIER_ids_biclass_strict_1_opposite_non_former_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_strict_1_opposite_non_former_smokers]
length(GLACIER_ids_biclass_strict_1_opposite_non_former_smokers)



GLACIER_ids_biclass_0<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_0]
length(GLACIER_ids_biclass_0)

GLACIER_ids_biclass_1_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_1_non_smokers]
length(GLACIER_ids_biclass_1_non_smokers)

GLACIER_ids_biclass_0_opposite<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_0_opposite]
length(GLACIER_ids_biclass_0_opposite)

GLACIER_ids_biclass_1_opposite_non_former_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_biclass_1_opposite_non_former_smokers]
length(GLACIER_ids_biclass_1_opposite_non_former_smokers)



GLACIER_ids_multiclass_0<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_multiclass_0]
length(GLACIER_ids_multiclass_0)

GLACIER_ids_multiclass_1_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_multiclass_1_non_smokers]
length(GLACIER_ids_multiclass_1_non_smokers)

GLACIER_ids_multiclass_2_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_multiclass_2_non_smokers]
length(GLACIER_ids_multiclass_2_non_smokers)

GLACIER_ids_multiclass_minus1_non_former_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_multiclass_minus1_non_former_smokers]
length(GLACIER_ids_multiclass_minus1_non_former_smokers)



GLACIER_ids_allmulticlass_00<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_allmulticlass_00]
length(GLACIER_ids_allmulticlass_00)

GLACIER_ids_allmulticlass_11<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_allmulticlass_11]
length(GLACIER_ids_allmulticlass_11)

GLACIER_ids_allmulticlass_02_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_allmulticlass_02_non_smokers]
length(GLACIER_ids_allmulticlass_02_non_smokers)

GLACIER_ids_allmulticlass_01_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_allmulticlass_01_non_smokers]
length(GLACIER_ids_allmulticlass_01_non_smokers)

GLACIER_ids_allmulticlass_12_non_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_allmulticlass_12_non_smokers]
length(GLACIER_ids_allmulticlass_12_non_smokers)

GLACIER_ids_allmulticlass_10_non_former_smokers<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% persistent_subjects_allmulticlass_10_non_former_smokers]
length(GLACIER_ids_allmulticlass_10_non_former_smokers)

#construct new dataset with merged information from all

#first get all subject from vip subset that are in glacier 
subjects_total1<-VIP_data_subset_visit1_complete_cases$Subject_id[VIP_data_subset_visit1_complete_cases$enummer %in% link_data$enummer]

subjects_total2<-VIP_data_subset_visit2_complete_cases$Subject_id[VIP_data_subset_visit2_complete_cases$enummer %in% link_data$enummer]

subject_total<-intersect(subjects_total1,subjects_total2)

#get enummers also
enummers_total1<-VIP_data_subset_visit1_complete_cases$enummer[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]

enummers_total2<-VIP_data_subset_visit2_complete_cases$enummer[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total]

#final dataset:

#glacier_id  persistant phenotype  identifierC1 identifierC2 ....
final_dataset<-data.frame(matrix(ncol=10,nrow=length(enummers_total)))
colnames(final_dataset)<-c("glacier_ID", "pat_code_dna", "resistance_phenotype", "biclass", "biclass_opposite", "biclass_strict", "biclass_strict_opposite", "multiclass", "all_multiclass")

#id columns
final_dataset$glacier_ID<-link_data$glacier_ID[!is.na(link_data$glacier_ID) & link_data$enummer %in% enummers_total1]
final_dataset$pat_code_dna<-link_data$pat_code_dna[!is.na(link_data$pat_code_dna) & link_data$enummer %in% enummers_total1]

#make a new continous phenotype, taking the mean of the two residuals at each visit
final_dataset$resistance_phenotype<-(VIP_data_subset_visit1_complete_cases$compliance_continous[VIP_data_subset_visit1_complete_cases$Subject_id %in% subject_total]+
			VIP_data_subset_visit2_complete_cases$compliance_continous[VIP_data_subset_visit2_complete_cases$Subject_id %in% subject_total])/2

#construct the discrete phenotypes
final_dataset$biclass<-"NA"
final_dataset$biclass[final_dataset$glacier_ID %in% GLACIER_ids_biclass_1_non_smokers]<-1
final_dataset$biclass[final_dataset$glacier_ID %in% GLACIER_ids_biclass_0]<-0
final_dataset$biclass<-as.factor(final_dataset$biclass)

final_dataset$biclass_opposite<-"NA"
final_dataset$biclass_opposite[final_dataset$glacier_ID %in% GLACIER_ids_biclass_1_opposite_non_former_smokers]<-1
final_dataset$biclass_opposite[final_dataset$glacier_ID %in% GLACIER_ids_biclass_0_opposite]<-0
final_dataset$biclass_opposite<-as.factor(final_dataset$biclass_opposite)

final_dataset$biclass_strict<-"NA"
final_dataset$biclass_strict[final_dataset$glacier_ID %in% GLACIER_ids_biclass_strict_1_non_smokers]<-1
final_dataset$biclass_strict[final_dataset$glacier_ID %in% GLACIER_ids_biclass_strict_0]<-0
final_dataset$biclass_strict<-as.factor(final_dataset$biclass_strict)

final_dataset$biclass_strict_opposite<-"NA"
final_dataset$biclass_strict_opposite[final_dataset$glacier_ID %in% GLACIER_ids_biclass_strict_1_opposite_non_former_smokers]<-1
final_dataset$biclass_strict_opposite[final_dataset$glacier_ID %in% GLACIER_ids_biclass_strict_0_opposite]<-0
final_dataset$biclass_strict_opposite<-as.factor(final_dataset$biclass_strict_opposite)

final_dataset$multiclass<-"NA"
final_dataset$multiclass[final_dataset$glacier_ID %in% GLACIER_ids_multiclass_2_non_smokers]<-2
final_dataset$multiclass[final_dataset$glacier_ID %in% GLACIER_ids_multiclass_1_non_smokers]<-1
final_dataset$multiclass[final_dataset$glacier_ID %in% GLACIER_ids_multiclass_0]<-0
final_dataset$multiclass[final_dataset$glacier_ID %in% GLACIER_ids_multiclass_minus1_non_former_smokers]<--1
final_dataset$multiclass<-as.factor(final_dataset$multiclass)

final_dataset$all_multiclass<-"NA"
final_dataset$all_multiclass[final_dataset$glacier_ID %in% GLACIER_ids_allmulticlass_00]<-'00'
final_dataset$all_multiclass[final_dataset$glacier_ID %in% GLACIER_ids_allmulticlass_11]<-'11'
final_dataset$all_multiclass[final_dataset$glacier_ID %in% GLACIER_ids_allmulticlass_02_non_smokers]<-'02'
final_dataset$all_multiclass[final_dataset$glacier_ID %in% GLACIER_ids_allmulticlass_01_non_smokers]<-'01'
final_dataset$all_multiclass[final_dataset$glacier_ID %in% GLACIER_ids_allmulticlass_12_non_smokers]<-'12'
final_dataset$all_multiclass[final_dataset$glacier_ID %in% GLACIER_ids_allmulticlass_10_non_former_smokers]<-'10'
final_dataset$all_multiclass<-as.factor(final_dataset$all_multiclass)


#save the final dataset
write.csv(final_dataset, "../final_persistant_phenotype_dataset_2908.csv", row.names=FALSE, na="NA")

