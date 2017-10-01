library(randomForest)
library(AUC)
library(gtools)

dataset_full_Light<-read.table("Light_final_dataset_full.txt",header=TRUE)
dataset_full_Light$Group<-factor(dataset_full_Light$Group)


dataset_full_Dark<-read.table("Dark_final_dataset_full.txt",header=TRUE)
dataset_full_Dark$Group<-factor(dataset_full_Dark$Group)

common_var_Light_Dark_full<-intersect(colnames(dataset_full_Light),colnames(dataset_full_Dark))

control_Light_full<-dataset_full_Light[dataset_full_Light$Group=="Control",common_var_Light_Dark_full]
control_Light_full$Group<-rep("Control_Light",times=13)
control_Light_full$Group<-as.factor(control_Light_full$Group)

control_Dark_full<-dataset_full_Dark[dataset_full_Dark$Group=="Control",common_var_Light_Dark_full]
control_Dark_full$Group<-rep("Control_Dark",times=13)
control_Dark_full$Group<-as.factor(control_Dark_full$Group)

control_Light_Dark_full<-rbind(control_Light_full,control_Dark_full)

group_levels<-c("Control_Light","Control_Dark")
variables<-colnames(control_Light_Dark_full)[-c((1:length(colnames(control_Light_Dark_full)))[colnames(control_Light_Dark_full)=="Group"], (1:length(colnames(control_Light_Dark_full)))[colnames(control_Light_Dark_full)=="TimeFactor"])]

#scores for full model
#10 fold cross validation

ACC<-0
AUC_total<-0

PREC<-c()
RECALL<-c()
F1<-c()


for (fold in 1:10){
	
	random_indexes<-permute(1:13)
	
	dataset_learn<-control_Light_Dark_full[control_Light_Dark_full$TimeFactor %in% random_indexes[1:6],-((1:length(colnames(control_Light_Dark_full)))[colnames(control_Light_Dark_full)=="TimeFactor"])]
	dataset_test<-control_Light_Dark_full[control_Light_Dark_full$TimeFactor %in% random_indexes[7:13],-((1:length(colnames(control_Light_Dark_full)))[colnames(control_Light_Dark_full)=="TimeFactor"])]
	
	rf <- randomForest(Group ~., data=dataset_learn,
			ntree = 1000,scale=TRUE)
	
	predicted<-predict(rf,dataset_test)
	
	
	conf_table<-table(predicted,dataset_test$Group)
	n = sum(conf_table) # number of instances
	nc = nrow(conf_table) # number of classes
	diag = diag(conf_table) # number of correctly classified instances per class 
	rowsums = apply(conf_table, 1, sum) # number of instances per class
	colsums = apply(conf_table, 2, sum) # number of predictions per class
	p = rowsums / n # distribution of instances over the actual classes
	q = colsums / n # distribution of instances over the predicted classes
	
	accuracy = sum(diag) / n 
	
	message(accuracy) 
	
	ACC<-ACC+accuracy
	
	precision = diag / colsums 
	recall = diag / rowsums 
	f1 = 2 * precision * recall / (precision + recall) 
	
	if(fold ==1){
		PREC<-precision[group_levels]
		RECALL<-recall[group_levels]
		F1<-f1[group_levels]
	}else{
		PREC<-PREC+precision[group_levels]
		RECALL<-RECALL+recall[group_levels]
		F1<-F1+f1[group_levels]
		
	}
	
	classes_true<-matrix(0,ncol=1,nrow=length(dataset_test[,1]))
	classes_predicted<-matrix(0,ncol=1,nrow=length(dataset_test[,1]))
	

	classes_true[dataset_test$Group==group_levels[1],1]<-1
	classes_predicted[predicted==group_levels[1],1]<-1		

	
	message(auc(roc(classes_predicted[,1],as.factor(classes_true[,1]))))
	
	AUC_total<-AUC_total+auc(roc(classes_predicted[,1],as.factor(classes_true[,1])))
	
	
	if(fold ==1){
		
		variable_importance<-data.frame(importance(rf))
	}else{
		variable_importance<-variable_importance+as.data.frame(importance(rf))
		
	}
	
}


ACC<-ACC/10
ACC
AUC_total<-AUC_total/10
AUC_total

PREC<-PREC/10
RECALL<-RECALL/10
F1<-F1/10

data.frame(PREC, RECALL, F1) 

#get the mean decrease impurity and check several levels of cut offs

rownames_variable_importance<-variables[order(variable_importance)]
variable_importance<-as.data.frame(variable_importance[order(variable_importance),])
rownames(variable_importance)<-rownames_variable_importance
variable_importance<-variable_importance/10
variable_importance

list_nr<-1


ACC_selected_vector<-c()
AUC_total_selected_vector<-c()

other_scores_list<-list()


for (cutoff in seq(0.01,0.05,0.01)){
	
	ACC_selected<-0
	AUC_total_selected<-0
	
	PREC_selected<-c()
	RECALL_selected<-c()
	F1_selected<-c()
	
	selected_variables<-c(rownames(variable_importance)[variable_importance[,1]>=cutoff],"TimeFactor","Group")
	selected_dataset_full<-na.omit(control_Light_Dark_full[,selected_variables])
	for (fold in 1:10){
		
		random_indexes<-permute(1:13)
		
		selected_dataset_learn<-selected_dataset_full[selected_dataset_full$TimeFactor %in% random_indexes[1:6],-((1:length(colnames(selected_dataset_full)))[colnames(selected_dataset_full)=="TimeFactor"])]
		selected_dataset_test<-selected_dataset_full[selected_dataset_full$TimeFactor %in% random_indexes[7:13],-((1:length(colnames(selected_dataset_full)))[colnames(selected_dataset_full)=="TimeFactor"])]
		
		
		rf_selected <- randomForest(Group ~., data=selected_dataset_learn,ntree = 1000)
		predicted<-predict(rf_selected,selected_dataset_test)
		
		conf_table<-table(predicted,selected_dataset_test$Group)
		n = sum(conf_table) # number of instances
		nc = nrow(conf_table) # number of classes
		diag = diag(conf_table) # number of correctly classified instances per class 
		rowsums = apply(conf_table, 1, sum) # number of instances per class
		colsums = apply(conf_table, 2, sum) # number of predictions per class
		p = rowsums / n # distribution of instances over the actual classes
		q = colsums / n # distribution of instances over the predicted classes
		
		accuracy = sum(diag) / n 
		message(paste0("cutoff ",cutoff," , fold ",fold))
		message(accuracy) 
		
		
		ACC_selected<-ACC_selected+accuracy
		
		
		precision = diag / colsums 
		recall = diag / rowsums 
		f1 = 2 * precision * recall / (precision + recall) 
		
		if(fold ==1){
			PREC_selected<-precision[group_levels]
			RECALL_selected<-recall[group_levels]
			F1_selected<-f1[group_levels]
		}else{
			PREC_selected<-PREC_selected+precision[group_levels]
			RECALL_selected<-RECALL_selected+recall[group_levels]
			F1_selected<-F1_selected+f1[group_levels]
			
		}
		
		classes_true<-matrix(0,ncol=1,nrow=length(selected_dataset_test[,1]))
		classes_predicted<-matrix(0,ncol=1,nrow=length(selected_dataset_test[,1]))
		
		
		classes_true[selected_dataset_test$Group==group_levels[1],1]<-1
		classes_predicted[predicted==group_levels[1],1]<-1		
		
		
		message(auc(roc(classes_predicted[,1],as.factor(classes_true[,1]))))
		
		AUC_total_selected<-AUC_total_selected+auc(roc(classes_predicted[,1],as.factor(classes_true[,1])))
		
		
	}
	
	ACC_selected_vector<-c(ACC_selected_vector,ACC_selected/10)
	AUC_total_selected_vector<-c(AUC_total_selected_vector, AUC_total_selected/10)
	
	other_scores_list[[list_nr]]<-data.frame(PREC_selected/10, RECALL_selected/10, F1_selected/10) 
	list_nr<-list_nr+1
	
}

ACC_selected_vector
max(ACC_selected_vector)
AUC_total_selected_vector
max(AUC_total_selected_vector)

mean_all<-sapply(other_scores_list,function(x) apply(x,2,FUN=mean,na.rm=T))
max(mean_all[1,])#prec
max(mean_all[2,])#rec
max(mean_all[3,])#f1


#considering all together, take the best cut off 0.02
selected_variables<-c(rownames(variable_importance)[variable_importance[,1]>=0.02],"TimeFactor","Group")

write.table(control_Light_Dark_full[,selected_variables],file="~/git/zebrafish_action_sequence_project/processed_data/final_dataset/B1/Light_Dark_final_dataset_selected.txt",row.names=F,col.names=T)

write(selected_variables,file="~/git/zebrafish_action_sequence_project/processed_data/final_dataset/B1/Light_Dark_selected_variables.txt")

