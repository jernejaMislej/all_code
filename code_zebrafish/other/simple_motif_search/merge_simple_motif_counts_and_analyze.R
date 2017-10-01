#Load packages
library(reshape2)
library(plotrix)

#args is the experiment condition and group
args <- commandArgs(trailingOnly = TRUE)

baseFile=paste('../../processed_data/simple_motif_search/',args[1],'/',args[2], sep="")
file_motif2=paste(baseFile,'_motif2', sep="")
file_motif3=paste(baseFile,'_motif3', sep="")
file_motif4=paste(baseFile,'_motif4', sep="")
file_motif5=paste(baseFile,'_motif5', sep="")



#read data
All_motif2 <- read.csv(file_motif2, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif3 <- read.csv(file_motif3, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif4 <- read.csv(file_motif4, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)
All_motif5 <- read.csv(file_motif5, header = FALSE, sep = ",", row.names = NULL, fill=TRUE)

#merge motifs and their counts
All_motif2_Merged <-aggregate(V2~V1,data=All_motif2,FUN=sum)
All_motif3_Merged <-aggregate(V2~V1,data=All_motif3,FUN=sum)
All_motif4_Merged <-aggregate(V2~V1,data=All_motif4,FUN=sum)
All_motif5_Merged <-aggregate(V2~V1,data=All_motif5,FUN=sum)

colnames(All_motif2_Merged)<-c("Motif","Count")
colnames(All_motif3_Merged)<-c("Motif","Count")
colnames(All_motif4_Merged)<-c("Motif","Count")
colnames(All_motif5_Merged)<-c("Motif","Count")

#add a column representing percentages
All_motif2_Merged$CountPrecentage<-round((All_motif2_Merged$Count/sum(All_motif2_Merged[,2]))*100,2)
All_motif3_Merged$CountPrecentage<-round((All_motif3_Merged$Count/sum(All_motif3_Merged[,2]))*100,2)
All_motif4_Merged$CountPrecentage<-round((All_motif4_Merged$Count/sum(All_motif4_Merged[,2]))*100,2)
All_motif5_Merged$CountPrecentage<-round((All_motif5_Merged$Count/sum(All_motif5_Merged[,2]))*100,2)

#write the results for the simple motif search
write.table(All_motif2_Merged, file=paste("../../results/output/simple_motif_search/", args[2],"_motif2",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif3_Merged, file=paste("../../results/output/simple_motif_search/", args[2],"_motif3",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif4_Merged, file=paste("../../results/output/simple_motif_search/", args[2],"_motif4",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")
write.table(All_motif5_Merged, file=paste("../../results/output/simple_motif_search/", args[2],"_motif5",sep=""), row.names=F,col.names=T, append = FALSE, sep=",")

#make boxplots for each motif length for the first 10 counts

#extract the first 10
All_motif2_Merged_first10<-na.omit(All_motif2_Merged[with(All_motif2_Merged,order(-Count)),][1:10,])
All_motif3_Merged_first10<-na.omit(All_motif3_Merged[with(All_motif3_Merged,order(-Count)),][1:10,])
All_motif4_Merged_first10<-na.omit(All_motif4_Merged[with(All_motif4_Merged,order(-Count)),][1:10,])
All_motif5_Merged_first10<-na.omit(All_motif5_Merged[with(All_motif5_Merged,order(-Count)),][1:10,])

#plot
png(paste('../../results/plots/simple_motif_search/first_10/',args[2],"_barplot_first10_motif2_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif2_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif2_Merged_first10$Motif, main=paste(args[2],", first 10 precentages of total ",sum(All_motif2_Merged[,2])," word counts of length 2",sep=""), xlab="Motifs of length 2", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif2_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif2_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif2_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif2_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif2_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_10/',args[2],"_barplot_first10_motif3_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif3_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif3_Merged_first10$Motif, main=paste(args[2],", first 10 precentages of total ",sum(All_motif3_Merged[,2])," word counts of length 3",sep=""), xlab="Motifs of length 3", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif3_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif3_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif3_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif3_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif3_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_10/',args[2],"_barplot_first10_motif4_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif4_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif4_Merged_first10$Motif, main=paste(args[2],", first 10 precentages of total ",sum(All_motif4_Merged[,2])," word counts of length 4",sep=""), xlab="Motifs of length 4", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif4_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif4_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif4_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif4_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif4_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_10/',args[2],"_barplot_first10_motif5_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif5_Merged_first10$CountPrecentage, yaxt='n',names.arg=All_motif5_Merged_first10$Motif, main=paste(args[2],", first 10 precentages of total ",sum(All_motif5_Merged[,2])," word counts of length 5",sep=""), xlab="Motifs of length 5", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif5_Merged_first10$CountPrecentage)))
boxed.labels(bpl,All_motif5_Merged_first10$CountPrecentage+3,sprintf('%d%s', All_motif5_Merged_first10$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif5_Merged_first10$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif5_Merged_first10$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

#make boxplots for each motif length for the first 20 counts

#extract the first 20
All_motif2_Merged_first20<-na.omit(All_motif2_Merged[with(All_motif2_Merged,order(-Count)),][1:20,])
All_motif3_Merged_first20<-na.omit(All_motif3_Merged[with(All_motif3_Merged,order(-Count)),][1:20,])
All_motif4_Merged_first20<-na.omit(All_motif4_Merged[with(All_motif4_Merged,order(-Count)),][1:20,])
All_motif5_Merged_first20<-na.omit(All_motif5_Merged[with(All_motif5_Merged,order(-Count)),][1:20,])

#plot
png(paste('../../results/plots/simple_motif_search/first_20/',args[2],"_barplot_first20_motif2_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif2_Merged_first20$CountPrecentage, yaxt='n',names.arg=All_motif2_Merged_first20$Motif, main=paste(args[2],", first 20 precentages of total ",sum(All_motif2_Merged[,2])," word counts of length 2",sep=""), xlab="Motifs of length 2", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif2_Merged_first20$CountPrecentage)))
boxed.labels(bpl,All_motif2_Merged_first20$CountPrecentage+3,sprintf('%d%s', All_motif2_Merged_first20$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif2_Merged_first20$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif2_Merged_first20$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_20/',args[2],"_barplot_first20_motif3_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif3_Merged_first20$CountPrecentage, yaxt='n',names.arg=All_motif3_Merged_first20$Motif, main=paste(args[2],", first 20 precentages of total ",sum(All_motif3_Merged[,2])," word counts of length 3",sep=""), xlab="Motifs of length 3", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif3_Merged_first20$CountPrecentage)))
boxed.labels(bpl,All_motif3_Merged_first20$CountPrecentage+3,sprintf('%d%s', All_motif3_Merged_first20$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif3_Merged_first20$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif3_Merged_first20$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_20/',args[2],"_barplot_first20_motif4_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif4_Merged_first20$CountPrecentage, yaxt='n',names.arg=All_motif4_Merged_first20$Motif, main=paste(args[2],", first 20 precentages of total ",sum(All_motif4_Merged[,2])," word counts of length 4",sep=""), xlab="Motifs of length 4", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif4_Merged_first20$CountPrecentage)))
boxed.labels(bpl,All_motif4_Merged_first20$CountPrecentage+3,sprintf('%d%s', All_motif4_Merged_first20$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif4_Merged_first20$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif4_Merged_first20$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_20/',args[2],"_barplot_first20_motif5_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif5_Merged_first20$CountPrecentage, yaxt='n',names.arg=All_motif5_Merged_first20$Motif, main=paste(args[2],", first 20 precentages of total ",sum(All_motif5_Merged[,2])," word counts of length 5",sep=""), xlab="Motifs of length 5", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif5_Merged_first20$CountPrecentage)))
boxed.labels(bpl,All_motif5_Merged_first20$CountPrecentage+3,sprintf('%d%s', All_motif5_Merged_first20$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif5_Merged_first20$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif5_Merged_first20$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()


#extract the first 30
All_motif2_Merged_first30<-na.omit(All_motif2_Merged[with(All_motif2_Merged,order(-Count)),][1:30,])
All_motif3_Merged_first30<-na.omit(All_motif3_Merged[with(All_motif3_Merged,order(-Count)),][1:30,])
All_motif4_Merged_first30<-na.omit(All_motif4_Merged[with(All_motif4_Merged,order(-Count)),][1:30,])
All_motif5_Merged_first30<-na.omit(All_motif5_Merged[with(All_motif5_Merged,order(-Count)),][1:30,])

#plot
png(paste('../../results/plots/simple_motif_search/first_30/',args[2],"_barplot_first30_motif2_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif2_Merged_first30$CountPrecentage, yaxt='n',names.arg=All_motif2_Merged_first30$Motif, main=paste(args[2],", first 30 precentages of total ",sum(All_motif2_Merged[,2])," word counts of length 2",sep=""), xlab="Motifs of length 2", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif2_Merged_first30$CountPrecentage)))
boxed.labels(bpl,All_motif2_Merged_first30$CountPrecentage+3,sprintf('%d%s', All_motif2_Merged_first30$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif2_Merged_first30$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif2_Merged_first30$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_30/',args[2],"_barplot_first30_motif3_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif3_Merged_first30$CountPrecentage, yaxt='n',names.arg=All_motif3_Merged_first30$Motif, main=paste(args[2],", first 30 precentages of total ",sum(All_motif3_Merged[,2])," word counts of length 3",sep=""), xlab="Motifs of length 3", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif3_Merged_first30$CountPrecentage)))
boxed.labels(bpl,All_motif3_Merged_first30$CountPrecentage+3,sprintf('%d%s', All_motif3_Merged_first30$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif3_Merged_first30$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif3_Merged_first30$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_30/',args[2],"_barplot_first30_motif4_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif4_Merged_first30$CountPrecentage, yaxt='n',names.arg=All_motif4_Merged_first30$Motif, main=paste(args[2],", first 30 precentages of total ",sum(All_motif4_Merged[,2])," word counts of length 4",sep=""), xlab="Motifs of length 4", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif4_Merged_first30$CountPrecentage)))
boxed.labels(bpl,All_motif4_Merged_first30$CountPrecentage+3,sprintf('%d%s', All_motif4_Merged_first30$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif4_Merged_first30$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif4_Merged_first30$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

png(paste('../../results/plots/simple_motif_search/first_30/',args[2],"_barplot_first30_motif5_count.png", sep=""),width=1500, height=750)
bpl<-barplot(All_motif5_Merged_first30$CountPrecentage, yaxt='n',names.arg=All_motif5_Merged_first30$Motif, main=paste(args[2],", first 30 precentages of total ",sum(All_motif5_Merged[,2])," word counts of length 5",sep=""), xlab="Motifs of length 5", ylab="Percentage of motif counts", ylim=c(0,10+max(All_motif5_Merged_first30$CountPrecentage)))
boxed.labels(bpl,All_motif5_Merged_first30$CountPrecentage+3,sprintf('%d%s', All_motif5_Merged_first30$Count, '\n counts'), bg='transparent', border=FALSE, cex=0.85)
axis(side = 2, at = seq(0,10+max(All_motif5_Merged_first30$CountPrecentage),by=10), labels = paste0(seq(0,10+max(All_motif5_Merged_first30$CountPrecentage),by=10), "%"), cex.axis = 1)
dev.off()

