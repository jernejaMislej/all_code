library(ctc)

args<-commandArgs(trailingOnly = TRUE)

#args<-c("DarkApoHigh_B1_cluster_scaled.txt","DarkApoHigh_B1")

cluster_data<-as.data.frame(read.table(args[1], header=TRUE))

rownames(cluster_data)<-cluster_data$Group

cluster_data<-cluster_data[,-1]
cluster_data<-cluster_data[,apply(cluster_data,2,function(x) length(x[is.na(x)])==0)]

d <- dist(cluster_data)

hc <- hclust(d) 

write(hc2Newick(hc),file=paste0(args[2],'.newick'))

