library(reshape2)
library(rgl)
library(plotrix)
library(scatterplot3d)

args<-c("LightDark")
baseFile=paste('../../processed_data/bout_length_turn_proportion_time_factor_dataset/',args[1], sep="")
file_BoutLength=paste(baseFile,'/All',args[1],'_BoutLength', sep="")
file_TimeFactor=paste(baseFile,'/All',args[1],'_TimeFactor', sep="")
file_ScootsProportion=paste(baseFile,'/All',args[1],'_ScootsProportion', sep="")
file_JBendsProportion=paste(baseFile,'/All',args[1],'_JBendsProportion', sep="")
file_CBendsProportion=paste(baseFile,'/All',args[1],'_CBendsProportion', sep="")
file_OBendsProportion=paste(baseFile,'/All',args[1],'_OBendsProportion', sep="")
file_EBendsProportion=paste(baseFile,'/All',args[1],'_EBendsProportion', sep="")
file_GBendsProportion=paste(baseFile,'/All',args[1],'_GBendsProportion', sep="")
file_HBendsProportion=paste(baseFile,'/All',args[1],'_HBendsProportion', sep="")
file_IBendsProportion=paste(baseFile,'/All',args[1],'_IBendsProportion', sep="")


#read data for the given turn type
All_BoutLength <- read.csv(file_BoutLength, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_TimeFactor <- read.csv(file_TimeFactor, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_ScootsProportion <- read.csv(file_ScootsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_JBendsProportion <- read.csv(file_JBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_CBendsProportion <- read.csv(file_CBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_OBendsProportion <- read.csv(file_OBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_EBendsProportion <- read.csv(file_EBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_GBendsProportion <- read.csv(file_GBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_HBendsProportion <- read.csv(file_HBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))
All_IBendsProportion <- read.csv(file_IBendsProportion, header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:55000))


#melt the datasets
All_BoutLength_Melted<-na.omit(melt(t(All_BoutLength)))[,c(2,3)]
All_TimeFactor_Melted<-na.omit(melt(t(All_TimeFactor)))[,c(2,3)]
All_ScootsProportion_Melted<-na.omit(melt(t(All_ScootsProportion)))[,c(2,3)]
All_JBendsProportion_Melted<-na.omit(melt(t(All_JBendsProportion)))[,c(2,3)]
All_CBendsProportion_Melted<-na.omit(melt(t(All_CBendsProportion)))[,c(2,3)]
All_OBendsProportion_Melted<-na.omit(melt(t(All_OBendsProportion)))[,c(2,3)]
All_EBendsProportion_Melted<-na.omit(melt(t(All_EBendsProportion)))[,c(2,3)]
All_GBendsProportion_Melted<-na.omit(melt(t(All_GBendsProportion)))[,c(2,3)]
All_HBendsProportion_Melted<-na.omit(melt(t(All_HBendsProportion)))[,c(2,3)]
All_IBendsProportion_Melted<-na.omit(melt(t(All_IBendsProportion)))[,c(2,3)]





colnames(All_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(All_TimeFactor_Melted)<-c("Subject","TimeFactor")
colnames(All_ScootsProportion_Melted)<-c("Subject","ScootsProportion")
colnames(All_JBendsProportion_Melted)<-c("Subject","JBendsProportion")
colnames(All_CBendsProportion_Melted)<-c("Subject","CBendsProportion")
colnames(All_OBendsProportion_Melted)<-c("Subject","OBendsProportion")
colnames(All_EBendsProportion_Melted)<-c("Subject","EBendsProportion")
colnames(All_GBendsProportion_Melted)<-c("Subject","GBendsProportion")
colnames(All_HBendsProportion_Melted)<-c("Subject","HBendsProportion")
colnames(All_IBendsProportion_Melted)<-c("Subject","IBendsProportion")

All<-cbind(All_BoutLength_Melted$Subject, All_BoutLength_Melted$BoutLength, All_TimeFactor_Melted$TimeFactor, All_ScootsProportion_Melted$ScootsProportion, All_JBendsProportion_Melted$JBendsProportion, All_CBendsProportion_Melted$CBendsProportion, All_OBendsProportion_Melted$OBendsProportion, All_EBendsProportion_Melted$EBendsProportion, All_GBendsProportion_Melted$GBendsProportion, All_HBendsProportion_Melted$HBendsProportion, All_IBendsProportion_Melted$IBendsProportion)

All[,1]<-factor(All[,1])

colnames(All)<-c("Subject","BoutLength","TimeFactor","ScootsProportion","JBendsProportion","CBendsProportion","OBendsProportion","EBendsProportion","GBendsProportion","HBendsProportion","IBendsProportion")

All<-as.data.frame(All)

#REMOVE THOSE SUBJECTS THAT HAVE SO LITTLE BOUTS PER TIME FRAME

#NOT GOOD
min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==1,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==2,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==3,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==4,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==5,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==6,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==7,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==8,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==9,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==10,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==11,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==12,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==13,],FUN=length)[,2])




#A BIT BETTER
mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==1,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==2,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==3,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==4,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==5,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==6,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==7,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==8,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==9,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==10,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==11,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==12,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==13,],FUN=length)[,2])



#remove those action sequences where there are less then 50 bouts
boutCounts_TimeFactor1<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==1,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor1$Subject[boutCounts_TimeFactor1$BoutLength<50]) & All$TimeFactor==1),]
boutCounts_TimeFactor2<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==2,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor2$Subject[boutCounts_TimeFactor2$BoutLength<50]) & All$TimeFactor==2),]
boutCounts_TimeFactor3<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==3,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor3$Subject[boutCounts_TimeFactor3$BoutLength<50]) & All$TimeFactor==3),]
boutCounts_TimeFactor4<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==4,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor4$Subject[boutCounts_TimeFactor4$BoutLength<50]) & All$TimeFactor==4),]
boutCounts_TimeFactor5<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==5,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor5$Subject[boutCounts_TimeFactor5$BoutLength<50]) & All$TimeFactor==5),]
boutCounts_TimeFactor6<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==6,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor6$Subject[boutCounts_TimeFactor6$BoutLength<50]) & All$TimeFactor==6),]
boutCounts_TimeFactor7<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==7,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor7$Subject[boutCounts_TimeFactor7$BoutLength<50]) & All$TimeFactor==7),]
boutCounts_TimeFactor8<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==8,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor8$Subject[boutCounts_TimeFactor8$BoutLength<50]) & All$TimeFactor==8),]
boutCounts_TimeFactor9<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==9,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor9$Subject[boutCounts_TimeFactor9$BoutLength<50]) & All$TimeFactor==9),]
boutCounts_TimeFactor10<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==10,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor10$Subject[boutCounts_TimeFactor10$BoutLength<50]) & All$TimeFactor==10),]
boutCounts_TimeFactor11<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==11,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor11$Subject[boutCounts_TimeFactor11$BoutLength<50]) & All$TimeFactor==11),]
boutCounts_TimeFactor12<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==12,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor12$Subject[boutCounts_TimeFactor12$BoutLength<50]) & All$TimeFactor==12),]
boutCounts_TimeFactor13<-aggregate(BoutLength~Subject,data=All[All$TimeFactor==13,],FUN=length)
All<-All[!((All$Subject %in% boutCounts_TimeFactor13$Subject[boutCounts_TimeFactor13$BoutLength<50]) & All$TimeFactor==13),]

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==1,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==2,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==3,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==4,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==5,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==6,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==7,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==8,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==9,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==10,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==11,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==12,],FUN=length)[,2])

min(aggregate(BoutLength~Subject,data=All[All$TimeFactor==13,],FUN=length)[,2])




#A BIT BETTER
mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==1,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==2,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==3,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==4,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==5,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==6,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==7,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==8,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==9,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==10,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==11,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==12,],FUN=length)[,2])

mean(aggregate(BoutLength~Subject,data=All[All$TimeFactor==13,],FUN=length)[,2])


#create subsets for Light, Dark, LD tranzition and DL tranzition, for now the sizes of the tranzition subset will depend on the preset 
#number of bouts to be taken into consideration. This number might be able to be predicted, based on the characteristics
#of the pure Lig0ht and pure Dark action sequences

tranzition_bouts=2
tranzition_bouts_1=12

#create Light

#first time frame
Light_subset<-aggregate(All[All$TimeFactor==1,],by=list(All[All$TimeFactor==1,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
			return(mean(x[c(tranzition_bouts_1:x_length)]))})[,c(-1,-4)]
#second time frame	
Light_subset<-rbind(Light_subset,aggregate(All[All$TimeFactor==2,],by=list(All[All$TimeFactor==2,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:x_length_tranzition_bouts)]))})[,c(-1,-4)])
#fifth time frame(similar as first)
Light_subset<-rbind(Light_subset,aggregate(All[All$TimeFactor==5,],by=list(All[All$TimeFactor==5,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(tranzition_bouts_1:x_length)]))})[,c(-1,-4)])
#sixth time frame(similar as second)
Light_subset<-rbind(Light_subset,aggregate(All[All$TimeFactor==6,],by=list(All[All$TimeFactor==6,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:x_length_tranzition_bouts)]))})[,c(-1,-4)])
#ninth time frame(similar as first)
Light_subset<-rbind(Light_subset,aggregate(All[All$TimeFactor==9,],by=list(All[All$TimeFactor==9,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(tranzition_bouts_1:x_length)]))})[,c(-1,-4)])
#tenth time frame(similar as second)
Light_subset<-rbind(Light_subset,aggregate(All[All$TimeFactor==10,],by=list(All[All$TimeFactor==10,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:x_length_tranzition_bouts)]))})[,c(-1,-4)])


#create Dark

#third time frame
Dark_subset<-aggregate(All[All$TimeFactor==3,],by=list(All[All$TimeFactor==3,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
			return(mean(x[c(tranzition_bouts_1:x_length)]))})[,c(-1,-4)]
#fourth time frame	
Dark_subset<-rbind(Dark_subset,aggregate(All[All$TimeFactor==4,],by=list(All[All$TimeFactor==4,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:x_length_tranzition_bouts)]))})[,c(-1,-4)])
#seventh time frame(similar as first)
Dark_subset<-rbind(Dark_subset,aggregate(All[All$TimeFactor==7,],by=list(All[All$TimeFactor==7,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(tranzition_bouts_1:x_length)]))})[,c(-1,-4)])
#eight time frame(similar as second)
Dark_subset<-rbind(Dark_subset,aggregate(All[All$TimeFactor==8,],by=list(All[All$TimeFactor==8,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:x_length_tranzition_bouts)]))})[,c(-1,-4)])
#eleventh time frame(similar as first)
Dark_subset<-rbind(Dark_subset,aggregate(All[All$TimeFactor==11,],by=list(All[All$TimeFactor==11,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(tranzition_bouts_1:x_length)]))})[,c(-1,-4)])
#twelveth time frame(similar as second)
Dark_subset<-rbind(Dark_subset,aggregate(All[All$TimeFactor==12,],by=list(All[All$TimeFactor==12,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:x_length_tranzition_bouts)]))})[,c(-1,-4)])


#create LD tranzition

#third time frame	
LD_subset<-aggregate(All[All$TimeFactor==3,],by=list(All[All$TimeFactor==3,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:tranzition_bouts_1)]))})[,c(-1,-4)]

#seveth time frame(similar as second)
LD_subset<-rbind(LD_subset,aggregate(All[All$TimeFactor==7,],by=list(All[All$TimeFactor==7,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:tranzition_bouts_1)]))})[,c(-1,-4)])

#eleventh time frame(similar as second)
LD_subset<-rbind(LD_subset,aggregate(All[All$TimeFactor==11,],by=list(All[All$TimeFactor==11,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:tranzition_bouts_1)]))})[,c(-1,-4)])

#create DL tranzition

#fifth time frame	
DL_subset<-aggregate(All[All$TimeFactor==5,],by=list(All[All$TimeFactor==5,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:tranzition_bouts_1)]))})[,c(-1,-4)]
		
#nineth time frame(similar as second)
DL_subset<-rbind(DL_subset,aggregate(All[All$TimeFactor==9,],by=list(All[All$TimeFactor==9,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:tranzition_bouts_1)]))})[,c(-1,-4)])

#thirteenth time frame(similar as second)
DL_subset<-rbind(DL_subset,aggregate(All[All$TimeFactor==13,],by=list(All[All$TimeFactor==13,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(mean(x[c(1:tranzition_bouts_1)]))})[,c(-1,-4)])



plot(Light_subset$BoutLength,Light_subset$ScootsProportion,pch=16,col="gray")
points(Dark_subset$BoutLength,Dark_subset$ScootsProportion,pch=16,col="black")
points(LD_subset$BoutLength,LD_subset$ScootsProportion,pch=16,col="blue")
points(DL_subset$BoutLength,DL_subset$ScootsProportion,pch=16,col="green")


plot(Light_subset$BoutLength,Light_subset$CBendsProportion,pch=16,col="gray")
points(Dark_subset$BoutLength,Dark_subset$CBendsProportion,pch=16,col="black")
points(LD_subset$BoutLength,LD_subset$CBendsProportion,pch=16,col="blue")
points(DL_subset$BoutLength,DL_subset$CBendsProportion,pch=16,col="green")


plot(Light_subset$BoutLength,Light_subset$OBendsProportion,pch=16,col="gray")
points(Dark_subset$BoutLength,Dark_subset$OBendsProportion,pch=16,col="black")
points(LD_subset$BoutLength,LD_subset$OBendsProportion,pch=16,col="blue")
points(DL_subset$BoutLength,DL_subset$OBendsProportion,pch=16,col="green")


plot(Light_subset$BoutLength,Light_subset$GBendsProportion,pch=16,col="gray")
points(Dark_subset$BoutLength,Dark_subset$GBendsProportion,pch=16,col="black")
points(LD_subset$BoutLength,LD_subset$GBendsProportion,pch=16,col="blue")
points(DL_subset$BoutLength,DL_subset$GBendsProportion,pch=16,col="green")


plot(Light_subset$BoutLength,Light_subset$HBendsProportion,pch=16,col="gray")
points(Dark_subset$BoutLength,Dark_subset$HBendsProportion,pch=16,col="black")
points(LD_subset$BoutLength,LD_subset$HBendsProportion,pch=16,col="blue")
points(DL_subset$BoutLength,DL_subset$HBendsProportion,pch=16,col="green")

#----------------------data when looking per time frame---------------------------
#aggregate(BoutLength~TimeFactor, data=All, FUN=length)[,2]


#				   L	 L	   D  	 D	   L	 L	   D  	 D	   L	 L	   D  	 D	   L

# Bout Count 	58656 59121 67186 49221 28346 40776 59278 39211 24868 36493 55209 37550 25094

# Bout Mean  	1.33  1.37  1.37  1.38  1.34  1.38  1.33  1.35  1.34  1.36  1.31  1.36  1.36

# Scoots Mean 	0.36  0.37  0.40  0.37  0.40  0.41  0.39  0.37  0.48  0.46  0.40  0.39  0.51

# JBends Mean 	0.05  0.05  0.04  0.04  0.06  0.05  0.05  0.05  0.05  0.05  0.05  0.05  0.05

# CBends Mean   0.06  0.05  0.03  0.05  0.06  0.05  0.04  0.05  0.05  0.04  0.04  0.05  0.05	

# OBends Mean	0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01

# EBends Mean   0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01

# GBends Mean	0.30  0.31  0.36  0.31  0.23  0.28  0.33  0.29  0.22  0.26  0.33  0.28  0.21

# HBends Mean   0.04  0.03  0.02  0.03  0.04  0.03  0.02  0.03  0.03  0.03  0.02  0.03  0.03

# IBends Mean	0.18  0.17  0.13  0.17  0.18  0.16  0.14  0.18  0.16  0.15  0.15  0.18  0.14

# Bout Max		 25    22    54    23    14    31    38    10    18    40    18    16    26


#round(aggregate(BoutLength~TimeFactor, data=All, FUN=max)[,2],2)




#----------------------trying some other stuff, not finished-----------------------

#instead of the mean, take the raw data from the subset and create different subsets for Light, Dark, LD tranzition and DL tranzition, for now the sizes of the tranzition subset will depend on the preset 
#number of bouts to be taken into consideration. This number might be able to be predicted, based on the characteristics
#of the pure Lig0ht and pure Dark action sequences

tranzition_bouts=5
tranzition_bouts_1=6


#create Light

#first time frame
x_length=length(All[All$TimeFactor==1,1])
x_length_tranzition_bouts=x_length-tranzition_bouts_1
Light_subset<-c()
for (subject in All[All$TimeFactor==1,]$Subject){
	Light_subset<-rbind(Light_subset,All[(All$Subject==subject & All$TimeFactor==1),][c(tranzition_bouts_1:x_length),])
}


#second time frame	
Light_subset<-c(Light_subset,aggregate(All[All$TimeFactor==2,2],by=list(All[All$TimeFactor==2,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(1:x_length_tranzition_bouts)]))})[,2])
#fifth time frame(similar as first)
Light_subset<-c(Light_subset,aggregate(All[All$TimeFactor==5,2],by=list(All[All$TimeFactor==5,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(tranzition_bouts_1:x_length)]))})[,2])
#sixth time frame(similar as second)
Light_subset<-c(Light_subset,aggregate(All[All$TimeFactor==6,2],by=list(All[All$TimeFactor==6,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(1:x_length_tranzition_bouts)]))})[,2])
#ninth time frame(similar as first)
Light_subset<-c(Light_subset,aggregate(All[All$TimeFactor==9,2],by=list(All[All$TimeFactor==9,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(tranzition_bouts_1:x_length)]))})[,2])
#tenth time frame(similar as second)
Light_subset<-c(Light_subset,aggregate(All[All$TimeFactor==10,2],by=list(All[All$TimeFactor==10,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(1:x_length_tranzition_bouts)]))})[,2])



#create Dark

#third time frame
Dark_subset<-aggregate(All[All$TimeFactor==3,2],by=list(All[All$TimeFactor==3,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
			return(length(x[c(tranzition_bouts_1:x_length)]))})[,2]
#fourth time frame	
Dark_subset<-c(Dark_subset,aggregate(All[All$TimeFactor==4,2],by=list(All[All$TimeFactor==4,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(1:x_length_tranzition_bouts)]))})[,2])
#seventh time frame(similar as first)
Dark_subset<-c(Dark_subset,aggregate(All[All$TimeFactor==7,2],by=list(All[All$TimeFactor==7,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(tranzition_bouts_1:x_length)]))})[,2])
#eight time frame(similar as second)
Dark_subset<-c(Dark_subset,aggregate(All[All$TimeFactor==8,2],by=list(All[All$TimeFactor==8,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(1:x_length_tranzition_bouts)]))})[,2])
#eleventh time frame(similar as first)
Dark_subset<-c(Dark_subset,aggregate(All[All$TimeFactor==11,2],by=list(All[All$TimeFactor==11,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(tranzition_bouts_1:x_length)]))})[,2])
#twelveth time frame(similar as second)
Dark_subset<-c(Dark_subset,aggregate(All[All$TimeFactor==12,2],by=list(All[All$TimeFactor==12,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts_1;
					return(length(x[c(1:x_length_tranzition_bouts)]))})[,2])


#create LD tranzition

#second time frame
LD_subset<-aggregate(All[All$TimeFactor==2,2],by=list(All[All$TimeFactor==2,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
			return(length(x[c(x_length_tranzition_bouts:x_length)]))})[,2]
#third time frame	
LD_subset<-c(LD_subset,aggregate(All[All$TimeFactor==3,2],by=list(All[All$TimeFactor==3,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(length(x[c(1:tranzition_bouts)]))})[,2])
#sixth time frame(similar as first)
LD_subset<-aggregate(All[All$TimeFactor==6,2],by=list(All[All$TimeFactor==6,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
			return(length(x[c(x_length_tranzition_bouts:x_length)]))})[,2]

#seveth time frame(similar as second)
LD_subset<-c(LD_subset,aggregate(All[All$TimeFactor==7,2],by=list(All[All$TimeFactor==7,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(length(x[c(1:tranzition_bouts)]))})[,2])
#tenth time frame(similar as first)
LD_subset<-aggregate(All[All$TimeFactor==10,2],by=list(All[All$TimeFactor==10,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
			return(length(x[c(x_length_tranzition_bouts:x_length)]))})[,2]

#eleventh time frame(similar as second)
LD_subset<-c(LD_subset,aggregate(All[All$TimeFactor==11,2],by=list(All[All$TimeFactor==11,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(length(x[c(1:tranzition_bouts)]))})[,2])

#create DL tranzition

#fourth time frame
DL_subset<-aggregate(All[All$TimeFactor==4,2],by=list(All[All$TimeFactor==4,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
			return(length(x[c(x_length_tranzition_bouts:x_length)]))})[,2]
#fifth time frame	
DL_subset<-c(DL_subset,aggregate(All[All$TimeFactor==5,2],by=list(All[All$TimeFactor==5,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(length(x[c(1:tranzition_bouts)]))})[,2])
#eighth time frame(similar as first)
DL_subset<-aggregate(All[All$TimeFactor==8,2],by=list(All[All$TimeFactor==8,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
			return(length(x[c(x_length_tranzition_bouts:x_length)]))})[,2]

#nineth time frame(similar as second)
DL_subset<-c(DL_subset,aggregate(All[All$TimeFactor==9,2],by=list(All[All$TimeFactor==9,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(length(x[c(1:tranzition_bouts)]))})[,2])
#twelveth time frame(similar as first)
DL_subset<-aggregate(All[All$TimeFactor==12,2],by=list(All[All$TimeFactor==12,1]), 
		function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
			return(length(x[c(x_length_tranzition_bouts:x_length)]))})[,2]

#thirteenth time frame(similar as second)
DL_subset<-c(DL_subset,aggregate(All[All$TimeFactor==13,2],by=list(All[All$TimeFactor==13,1]), 
				function(x){x_length=length(x);x_length_tranzition_bouts=x_length-tranzition_bouts;
					return(length(x[c(1:tranzition_bouts)]))})[,2])


