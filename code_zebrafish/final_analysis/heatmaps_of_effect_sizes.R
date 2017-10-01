args<-commandArgs(trailingOnly = TRUE)
condition<-args[1]
type<-args[2]
library(gplots)

state="hypoactive"

if(condition=="DarkApoHigh"){
	
	state="hyperactive type I"
	
	
}else if(condition=="DarkPTZ"){
	
	state="hyperactive type II"
	
}

files<-list.files()
#files_vals<-files[-grep("all.txt|with_values.txt|p_values.txt",files)]
files_vals<-files[grep("all.txt",files)]
files_p_vals<-files[grep("p_values.txt",files)]

all_files_vals<-read.table(files_vals[1],header=TRUE)

for(fil_val in files_vals[-1]){
	
	all_files_vals<-cbind(all_files_vals,read.table(fil_val,header=TRUE))
	
	
}

all_files_p_vals<-read.table(files_p_vals[1],header=TRUE)

for(fil_val in files_p_vals[-1]){
	
	all_files_p_vals<-cbind(all_files_p_vals,read.table(fil_val,header=TRUE))
	
	
}


all_files_vals<-all_files_vals[apply(all_files_vals,2,function(x) length(x[x>4])==0)]
sum_all_files_vals<-round(apply(all_files_vals,2,function(x) sum(abs(x-1))),2)

max_vars<-min(25,length(sum_all_files_vals))
all_files_vals<-all_files_vals[,names(sum_all_files_vals[order(-sum_all_files_vals)])[1:max_vars]]
all_files_p_vals<-all_files_p_vals[,names(sum_all_files_vals[order(-sum_all_files_vals)])[1:max_vars]]

row_names_correct<-gsub("Group", "", rownames(all_files_vals))
row_names_correct<-gsub("Dark$", "healthy", row_names_correct)
row_names_correct<-gsub("Dark..*", state, row_names_correct)
row_names_correct<-gsub("Factor", "", row_names_correct)
row_names_correct<-gsub("micro", "u", row_names_correct)
row_names_correct<-gsub("PCAP814", "PCAP3", row_names_correct)
row_names_correct<-gsub("PCAP931", "PCAP4", row_names_correct)

rownames(all_files_vals)<-row_names_correct
rownames(all_files_p_vals)<-row_names_correct

all_files_p_vals_sym<-all_files_p_vals
all_files_p_vals_sym[all_files_p_vals>0.05]<-"n.s"
all_files_p_vals_sym[all_files_p_vals<=0.05 & all_files_p_vals>0.01]<-"*"
all_files_p_vals_sym[all_files_p_vals<=0.01 & all_files_p_vals>0.001]<-"**"
all_files_p_vals_sym[all_files_p_vals<=0.001]<-"***"

right_margin<-18
if(condition!="DarkApoLow"){
	
	right_margin<-21
	
	
}
bottom_margin<-10
if(type=="B1"){
	
	bottom_margin<-13
	
}

#get the keys...stupid R

#1_microM
all_files_vals_1<-all_files_vals[grep("1uM|Control",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/with_time/",condition,"_heatmap_1_microM_",condition,"_",type,"KEY.png"),width=1500,height=750)

heatmap.2(as.matrix(all_files_vals_1), col=greenred(100), key=T, keysize=0.75,
		trace="none",dendrogram="none" ,labRow=row_names_correct[grep("1uM|Control",rownames(all_files_vals))],
		srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[grep("1uM|Control",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
		cexRow=1.2,cexCol=1.2, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key")
dev.off()


#3_microM
all_files_vals_3<-all_files_vals[grep("3uM|Control",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/with_time/",condition,"_heatmap_3_microM_",condition,"_",type,"KEY.png"),width=1500,height=750)

heatmap.2(as.matrix(all_files_vals_3), col=greenred(100), key=T, keysize=0.75,
		trace="none",dendrogram="none" ,labRow=row_names_correct[grep("3uM|Control",rownames(all_files_vals))],
		srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[grep("3uM|Control",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
		cexRow=1.2,cexCol=1.2, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key")
dev.off()


#10_microM
all_files_vals_10<-all_files_vals[grep("10uM|Control",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/with_time/",condition,"_heatmap_10_microM_",condition,"_",type,"KEY.png"),width=1500,height=750)

heatmap.2(as.matrix(all_files_vals_10), col=greenred(100), key=T, keysize=0.75,
		trace="none",dendrogram="none" ,labRow=row_names_correct[grep("10uM|Control",rownames(all_files_vals))],
		srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[grep("10uM|Control",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
		cexRow=1.2,cexCol=1.2, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key")
dev.off()


#without time, all together
all_files_vals_no_time<-all_files_vals[-grep("Time",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/no_time/",condition,"_heatmap_all_",condition,"_",type,"KEY.png"),width=1500,height=750)

heatmap.2(as.matrix(all_files_vals_no_time), col=greenred(100), key=T, keysize=0.75,
		trace="none",dendrogram="none" ,labRow=row_names_correct[-grep("Time",rownames(all_files_vals))],
		srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[-grep("Time",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
		cexRow=1,cexCol=1.2, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key")
dev.off()



#get a more streched out map....stupid R


lwid = c(0.4,4)
lhei = c(0.01,2,0.01)
lmat = rbind(c(4,0),c(3,1),c(2,0))

right_margin<-22
if(condition!="DarkApoLow"){
	
	right_margin<-25
	
	
}
bottom_margin<-12
if(type=="B1"){
	
	bottom_margin<-16
	
}

#with time

#1_microM
all_files_vals_1<-all_files_vals[grep("1uM|Control",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/with_time/",condition,"_heatmap_1_microM_",condition,"_",type,"MAP.png"),width=1500,height=1000)

tryCatch({
			heatmap.2(as.matrix(all_files_vals_1), col=greenred(100), key=T, keysize=0.75,lmat=lmat, lhei=lhei, lwid=lwid,
					trace="none",dendrogram="none" ,labRow=row_names_correct[grep("1uM|Control",rownames(all_files_vals))],
					srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[grep("1uM|Control",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
					cexRow=1.5,cexCol=1.5, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key", notecex=1.2)
		}, error=function(e){})
dev.off()


#3_microM
all_files_vals_3<-all_files_vals[grep("3uM|Control",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/with_time/",condition,"_heatmap_3_microM_",condition,"_",type,"MAP.png"),width=1500,height=1000)

tryCatch({
			heatmap.2(as.matrix(all_files_vals_3), col=greenred(100), key=T, keysize=0.75,lmat=lmat, lhei=lhei, lwid=lwid,
					trace="none",dendrogram="none" ,labRow=row_names_correct[grep("3uM|Control",rownames(all_files_vals))],
					srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[grep("3uM|Control",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
					cexRow=1.5,cexCol=1.5, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key", notecex=1.2)
		}, error=function(e){})
dev.off()


#10_microM
all_files_vals_10<-all_files_vals[grep("10uM|Control",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/with_time/",condition,"_heatmap_10_microM_",condition,"_",type,"MAP.png"),width=1500,height=1000)

tryCatch({
			heatmap.2(as.matrix(all_files_vals_10), col=greenred(100), key=T, keysize=0.75,lmat=lmat, lhei=lhei, lwid=lwid,
					trace="none",dendrogram="none" ,labRow=row_names_correct[grep("10uM|Control",rownames(all_files_vals))],
					srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[grep("10uM|Control",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
					cexRow=1.5,cexCol=1.5, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key", notecex=1.2)
		}, error=function(e){})
dev.off()


#without time, all together
all_files_vals_no_time<-all_files_vals[-grep("Time",rownames(all_files_vals)),]

png(paste0("/home/jerneja/git/zebrafish_action_sequence_project/results/output/heatmaps/no_time/",condition,"_heatmap_all_",condition,"_",type,"MAP.png"),width=1500,height=1000)

tryCatch({
			heatmap.2(as.matrix(all_files_vals_no_time), col=greenred(100), key=T, keysize=0.75,lmat=lmat, lhei=lhei, lwid=lwid,
					trace="none",dendrogram="none" ,labRow=row_names_correct[-grep("Time",rownames(all_files_vals))],
					srtCol=40, cellnote=as.matrix(all_files_p_vals_sym[-grep("Time",rownames(all_files_vals)),]),notecol="gray60",margins=c(bottom_margin,right_margin),
					cexRow=1.1,cexCol=1.5, density.info="density", breaks=seq(0,3.5,length.out = 101),key.title ="Color key", notecex=1.2)
		}, error=function(e){})

dev.off()
