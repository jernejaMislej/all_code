library(ctc)

args<-commandArgs(trailingOnly = TRUE)
condition<-args[1]
type<-args[2]

state="hypoactive"

if(condition=="DarkApoHigh"){
	
	state="hyperactive"
	
	
}else if(condition=="DarkPTZ"){
	
	state="convulsant"
	
}

files<-list.files()
files<-files[-grep("with_values",files)]
fil<-files[1]

effect_sizes<-read.table(fil,header=TRUE)
col_names<-c(colnames(effect_sizes),paste0("TimeFactor_",colnames(effect_sizes)))

drugs<-substr(rownames(effect_sizes)[2:74], 6, nchar(rownames(effect_sizes)[2:74]))
drugs<-gsub("Dark$", "healthy", drugs)
drugs<-gsub("Dark.+", state, drugs)

		
all_effect_sizes<-cbind(effect_sizes[2:74,1],effect_sizes[75:147,])
all_effect_sizes<-as.data.frame(all_effect_sizes)
rownames(all_effect_sizes)<-drugs

for (fil in files[-1]){
	
	
	effect_sizes<-read.table(fil,header=TRUE, row.names=NULL)
	
	all_effect_sizes<-cbind(all_effect_sizes, effect_sizes[2:74,2],effect_sizes[75:147,2])
	
	col_names<-c(col_names,c(colnames(effect_sizes)[2],paste0("TimeFactor_",colnames(effect_sizes)[2])))
	
}

all_effect_sizes<-rbind(all_effect_sizes,numeric(length(all_effect_sizes[1,])))
rownames(all_effect_sizes)[length(all_effect_sizes[,1])]<-"Control_healthy"
colnames(all_effect_sizes)<-col_names

#extra limitations, since the sparse variables can have some unrealistic and false effects
all_effect_sizes<-all_effect_sizes[,apply(all_effect_sizes,2,function(x) length(x[x!=0])>0)]
all_effect_sizes<-all_effect_sizes[,apply(all_effect_sizes,2,function(x) length(x[is.infinite(x)])==0)]
all_effect_sizes<-all_effect_sizes[,apply(all_effect_sizes,2,function(x) length(x[x>=5])==0)]


#ALL TOGETHER

d <- dist(all_effect_sizes)
hc<- hclust(d)
write(hc2Newick(hc),file=paste0('/home/jerneja/git/zebrafish_action_sequence_project/processed_data/clusters/',condition,"_",type,'_set1_effect_sizes_all.newick'))

#get the distances
disties<-as.matrix(d)

#healthy
disties_healthy<-disties[grep("healthy",names(disties[,74])),74]
disties_healthy<-disties_healthy[order(disties_healthy)][-1]
write.table(disties_healthy, file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/ranked_drugs_effects/healthy_",condition,"_",type,"_set1.txt"),row.names=T,col.names=F)

#disease
disties_disease<-c(disties[-grep("healthy",names(disties[,74])),74],disties[74,74])
names(disties_disease)[38]<-"Control_healthy"
disties_disease<-disties_disease[order(disties_disease)][-1]
write.table(disties_disease, file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/ranked_drugs_effects/disease_",condition,"_",type,"_set1.txt"),row.names=T,col.names=F)





#WITHOUT BOUT COUNT DATA
all_effect_sizes_subset<-all_effect_sizes[,-grep("Bout",colnames(all_effect_sizes))]
d <- dist(all_effect_sizes_subset)
hc<- hclust(d)
write(hc2Newick(hc),file=paste0('/home/jerneja/git/zebrafish_action_sequence_project/processed_data/clusters/',condition,"_",type,'_set2_effect_sizes_all.newick'))

#get the distances
disties<-as.matrix(d)

#healthy
disties_healthy<-disties[grep("healthy",names(disties[,74])),74]
disties_healthy<-disties_healthy[order(disties_healthy)][-1]
write.table(disties_healthy, file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/ranked_drugs_effects/healthy_",condition,"_",type,"_set2.txt"),row.names=T,col.names=F)

#disease
disties_disease<-c(disties[-grep("healthy",names(disties[,74])),74],disties[74,74])
names(disties_disease)[38]<-"Control_healthy"
disties_disease<-disties_disease[order(disties_disease)][-1]
write.table(disties_disease, file=paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/ranked_drugs_effects/disease_",condition,"_",type,"_set2.txt"),row.names=T,col.names=F)




