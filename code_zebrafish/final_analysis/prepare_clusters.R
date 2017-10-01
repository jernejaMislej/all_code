library(reshape)

args<-commandArgs(trailingOnly = TRUE)
condition<-args[1]

state="hypoactive"

if(condition=="DarkApoHigh"){
	
	state="hyperactive"
	
	
}else if(condition=="DarkPTZ"){
	
	state="convulsant"
	
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

full_together<-full_together[,-grep("^Length.*Count$",colnames(full_together))]
full_together<-full_together[,apply(full_together,2,function(x) length(x[x!=0])>0)]


#reshape, start with healthy control
vars<-colnames(full_together)[-grep("TimeFactor|Group",colnames(full_together))][1]
reshaped_together<-cast(full_together[full_together$Group=="Control_Dark",c("TimeFactor","Group",vars)],Group~TimeFactor)

colnames(reshaped_together)[2:14]<-paste0(vars,1:13)
reshaped_together$Group<-"Control_healthy"

for (vars in colnames(full_together)[-grep("TimeFactor|Group",colnames(full_together))][-1]){
	
	reshaped_together<-cbind(reshaped_together, cast(full_together[full_together$Group=="Control_Dark",c("TimeFactor","Group",vars)],Group~TimeFactor)[-1])
	len1<-length(colnames(reshaped_together))-12
	len2<-length(colnames(reshaped_together))
	colnames(reshaped_together)[len1:len2]<-paste0(vars,1:13)
	
}

#add the rest
for (groupy in unique(full_together$Group)[-grep("Control_Dark$",unique(full_together$Group))]){
	
	vars<-colnames(full_together)[-grep("TimeFactor|Group",colnames(full_together))][1]
	reshaped_together_temp<-cast(full_together[full_together$Group==groupy,c("TimeFactor","Group",vars)],Group~TimeFactor)
	
	colnames(reshaped_together_temp)[2:14]<-paste0(vars,1:13)
	
	if(grepl("Dark$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-4),"healthy")
		
	}else if (grepl("DarkApoLow$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-10),"hypoactive")
		
	}else if (grepl("DarkApoHigh$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-11),"hyperactive")
		
	}else if (grepl("DarkPTZ$",groupy)){
		
		reshaped_together_temp$Group<-paste0(substr(groupy,1,nchar(groupy)-7),"convulsant")
		
	}
	
	
	for (vars in colnames(full_together)[-grep("TimeFactor|Group",colnames(full_together))][-1]){
		
		reshaped_together_temp<-cbind(reshaped_together_temp, cast(full_together[full_together$Group==groupy,c("TimeFactor","Group",vars)],Group~TimeFactor)[-1])
		len1<-length(colnames(reshaped_together_temp))-12
		len2<-length(colnames(reshaped_together_temp))
		colnames(reshaped_together_temp)[len1:len2]<-paste0(vars,1:13)
		
	}
	
	reshaped_together<-rbind(reshaped_together,reshaped_together_temp)
	
}

#save unscaled
write.table(reshaped_together,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/clusters/",condition,"_",args[2],"_cluster_unscaled.txt"),row.names=F,col.names=T)



reshaped_together<-reshaped_together[,apply(reshaped_together,2,function(x) length(x[x!=0])>0)]

#save scaled
reshaped_together<-cbind(reshaped_together[,1],scale(reshaped_together[,-1]))
colnames(reshaped_together)[1]<-"Group"
write.table(reshaped_together,file=paste0("~/git/zebrafish_action_sequence_project/processed_data/clusters/",condition,"_",args[2],"_cluster_scaled.txt"),row.names=F,col.names=T)





