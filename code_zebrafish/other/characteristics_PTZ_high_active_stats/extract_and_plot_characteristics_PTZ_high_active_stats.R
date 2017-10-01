
#Load packages
library(reshape2)
library(car)
library(pracma)


#read data for the given turn type
AllDarkPTZ_BoutLength <- read.csv('Disease_Control_DarkPTZ_BoutLength', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:2361))
AllDarkPTZ_TimeFactor <- read.csv('Disease_Control_DarkPTZ_TimeFactor', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:2361))
AllDarkPTZ_CBendsProportion <- read.csv('Disease_Control_DarkPTZ_CBendsProportion', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:2361))
AllDarkPTZ_OBendsProportion <- read.csv('Disease_Control_DarkPTZ_OBendsProportion', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:2361))

#melt the datasets
AllDarkPTZ_BoutLength_Melted<-na.omit(melt(t(AllDarkPTZ_BoutLength)))[,c(2,3)]
AllDarkPTZ_TimeFactor_Melted<-na.omit(melt(t(AllDarkPTZ_TimeFactor)))[,c(2,3)]
AllDarkPTZ_CBendsProportion_Melted<-na.omit(melt(t(AllDarkPTZ_CBendsProportion)))[,c(2,3)]
AllDarkPTZ_OBendsProportion_Melted<-na.omit(melt(t(AllDarkPTZ_OBendsProportion)))[,c(2,3)]


colnames(AllDarkPTZ_BoutLength_Melted)<-c("Subject","BoutLength")
colnames(AllDarkPTZ_TimeFactor_Melted)<-c("Subject","TimeFactor")
colnames(AllDarkPTZ_CBendsProportion_Melted)<-c("Subject","CBendsProportion")
colnames(AllDarkPTZ_OBendsProportion_Melted)<-c("Subject","OBendsProportion")



AllPTZ<-cbind(AllDarkPTZ_BoutLength_Melted$Subject, AllDarkPTZ_BoutLength_Melted$BoutLength, AllDarkPTZ_TimeFactor_Melted$TimeFactor, AllDarkPTZ_CBendsProportion_Melted$CBendsProportion, AllDarkPTZ_OBendsProportion_Melted$OBendsProportion)

AllPTZ[,1]<-factor(AllPTZ[,1])

colnames(AllPTZ)<-c("Subject","BoutLength","TimeFactor", "CBendsProportion", "OBendsProportion")

AllPTZ<-as.data.frame(AllPTZ)

#check all subjects have all time frames
unique(aggregate(TimeFactor~Subject,data=AllPTZ,function(x){return(length(unique(x)))})[,2])

#since we are interested in high active states only, we only keep those bouts that have at least one C or at least one O turn
AllPTZ<-AllPTZ[(AllPTZ$CBendsProportion>0 | AllPTZ$OBendsProportion>0), ]

#get the max bout length for each subject (min length is 1 for all)
max_bouts_PTZ<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, FUN = max), AllPTZ)

max_bouts_PTZ<-max_bouts_PTZ[order(max_bouts_PTZ[,1]),]

max_bouts_PTZ<-max_bouts_PTZ[!duplicated(max_bouts_PTZ[,1]),]


max_bouts_PTZ

#get the min, max, mean and sd of the max lengths of bouts
min(max_bouts_PTZ$BoutLength)

max(max_bouts_PTZ$BoutLength)

mean(max_bouts_PTZ$BoutLength)

sd(max_bouts_PTZ$BoutLength)


#get the distribution
hist(max_bouts_PTZ$BoutLength)$counts

hist(max_bouts_PTZ$BoutLength)$breaks


#get the proportions
length(max_bouts_PTZ$BoutLength[max_bouts_PTZ$BoutLength>10])/144

length(max_bouts_PTZ$BoutLength[max_bouts_PTZ$BoutLength>15])/144

length(max_bouts_PTZ$BoutLength[max_bouts_PTZ$BoutLength>20])/144


#get the distribution for the time factor
hist(max_bouts_PTZ$TimeFactor)$counts

hist(max_bouts_PTZ$TimeFactor)$breaks

#get the distribution for the C Bends
hist(max_bouts_PTZ$CBendsProportion)$counts

hist(max_bouts_PTZ$CBendsProportion)$breaks

#get the distribution for the O Bends
hist(max_bouts_PTZ$OBendsProportion)$counts

hist(max_bouts_PTZ$OBendsProportion)$breaks


#check the first five max bout lengths
max_5_bouts_PTZ<-aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[1:5])})

max_5_bouts_PTZ

#calculate the mean of the first five max bout lengths
mean_max_5_bouts_PTZ<-as.data.frame(cbind(max_5_bouts_PTZ$Subject,apply(max_5_bouts_PTZ[,-1],1,mean)))

colnames(mean_max_5_bouts_PTZ)<-c("Subject","MeanBoutLength_Max_5")


#get the min, max, mean and sd for the means of the first five max bout lengths
min(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5)

max(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5)

mean(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5)

sd(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5)


#distribution of the means of the first five max bout lengths
hist(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5)$counts

hist(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5)$breaks


#proportions
length(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5[mean_max_5_bouts_PTZ$MeanBoutLength_Max_5>10])/144

length(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5[mean_max_5_bouts_PTZ$MeanBoutLength_Max_5>15])/144

length(mean_max_5_bouts_PTZ$MeanBoutLength_Max_5[mean_max_5_bouts_PTZ$MeanBoutLength_Max_5>20])/144


#number of bouts longer than the minimum maximum bout length, which is now 6
count_high_active_PTZ<-aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(length(x[x>=6]))})

colnames(count_high_active_PTZ)<-c("Subject","CountBoutsOver_6")

#min, max, mean, sd
min(count_high_active_PTZ$CountBoutsOver_6)

max(count_high_active_PTZ$CountBoutsOver_6)

mean(count_high_active_PTZ$CountBoutsOver_6)

sd(count_high_active_PTZ$CountBoutsOver_6)


#distribution
hist(count_high_active_PTZ$CountBoutsOver_6)$counts

hist(count_high_active_PTZ$CountBoutsOver_6)$breaks


#proportions
length(count_high_active_PTZ$CountBoutsOver_6[count_high_active_PTZ$CountBoutsOver_6>5])/144

length(count_high_active_PTZ$CountBoutsOver_6[count_high_active_PTZ$CountBoutsOver_6>10])/144

length(count_high_active_PTZ$CountBoutsOver_6[count_high_active_PTZ$CountBoutsOver_6>15])/144




#check the distribution of the time frames for the first three max bout lengths

max_3_bouts_time_frames<- max_bouts_PTZ$TimeFactor

get_time_frames<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[2])}),AllPTZ)
get_time_frames<-get_time_frames[order(get_time_frames[,1]),]
get_time_frames<-get_time_frames[!duplicated(get_time_frames[,1]),3]

max_3_bouts_time_frames<-c(max_3_bouts_time_frames,get_time_frames)


get_time_frames<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[3])}),AllPTZ)
get_time_frames<-get_time_frames[order(get_time_frames[,1]),]
get_time_frames<-get_time_frames[!duplicated(get_time_frames[,1]),3]

max_3_bouts_time_frames<-c(max_3_bouts_time_frames,get_time_frames)


hist(max_3_bouts_time_frames)$counts

hist(max_3_bouts_time_frames)$breaks


max_3_bouts_CBends<- max_bouts_PTZ$CBendsProportion

get_CBends<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[2])}),AllPTZ)
get_CBends<-get_CBends[order(get_CBends[,1]),]
get_CBends<-get_CBends[!duplicated(get_CBends[,1]),4]

max_3_bouts_CBends<-c(max_3_bouts_CBends,get_CBends)


get_CBends<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[3])}),AllPTZ)
get_CBends<-get_CBends[order(get_CBends[,1]),]
get_CBends<-get_CBends[!duplicated(get_CBends[,1]),4]

max_3_bouts_CBends<-c(max_3_bouts_CBends,get_CBends)


hist(max_3_bouts_CBends)$counts

hist(max_3_bouts_CBends)$breaks



max_3_bouts_OBends<- max_bouts_PTZ$OBendsProportion

get_OBends<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[2])}),AllPTZ)
get_OBends<-get_OBends[order(get_OBends[,1]),]
get_OBends<-get_OBends[!duplicated(get_OBends[,1]),5]

max_3_bouts_OBends<-c(max_3_bouts_OBends,get_OBends)


get_OBends<-merge(aggregate(BoutLength ~ Subject, data = AllPTZ, function(x){return(-sort(-x)[3])}),AllPTZ)
get_OBends<-get_OBends[order(get_OBends[,1]),]
get_OBends<-get_OBends[!duplicated(get_OBends[,1]),5]

max_3_bouts_OBends<-c(max_3_bouts_OBends,get_OBends)


hist(max_3_bouts_OBends)$counts

hist(max_3_bouts_OBends)$breaks




#mean number of bouts longer than 4, 6, 8, 10, over all subjects

mean_count_long_bouts_10_PTZ<-aggregate(BoutLength ~ TimeFactor, data = AllPTZ, function(x){return(length(x[x>=10])/144)})
colnames(mean_count_long_bouts_10_PTZ)<-c("TimeFactor","MeanCountLongBouts")
mean_count_long_bouts_10_PTZ
plot(mean_count_long_bouts_10_PTZ$TimeFactor,mean_count_long_bouts_10_PTZ$MeanCountLongBouts)

mean_count_long_bouts_8_PTZ<-aggregate(BoutLength ~ TimeFactor, data = AllPTZ, function(x){return(length(x[x>=8])/144)})
colnames(mean_count_long_bouts_8_PTZ)<-c("TimeFactor","MeanCountLongBouts")
mean_count_long_bouts_8_PTZ
plot(mean_count_long_bouts_8_PTZ$TimeFactor,mean_count_long_bouts_8_PTZ$MeanCountLongBouts)

mean_count_long_bouts_6_PTZ<-aggregate(BoutLength ~ TimeFactor, data = AllPTZ, function(x){return(length(x[x>=6])/144)})
colnames(mean_count_long_bouts_6_PTZ)<-c("TimeFactor","MeanCountLongBouts")
mean_count_long_bouts_6_PTZ
plot(mean_count_long_bouts_6_PTZ$TimeFactor,mean_count_long_bouts_6_PTZ$MeanCountLongBouts)

mean_count_long_bouts_4_PTZ<-aggregate(BoutLength ~ TimeFactor, data = AllPTZ, function(x){return(length(x[x>=4])/144)})
colnames(mean_count_long_bouts_4_PTZ)<-c("TimeFactor","MeanCountLongBouts")
mean_count_long_bouts_4_PTZ
plot(mean_count_long_bouts_4_PTZ$TimeFactor,mean_count_long_bouts_4_PTZ$MeanCountLongBouts)


#box plots of bouts of length from 8 to 16 and over 16 per time frame 
dev.new()
PTZ_long_bouts_8_16<-aggregate(BoutLength ~ TimeFactor, data = AllPTZ, function(x){return(x[x>=8 & x<16])})
boxplot(sapply(PTZ_long_bouts_8_16[,2], '[', seq(max(sapply(PTZ_long_bouts_8_16[,2],length)))))

dev.new()
PTZ_long_bouts_16<-aggregate(BoutLength ~ TimeFactor, data = AllPTZ, function(x){return(x[x>=16])})
boxplot(sapply(PTZ_long_bouts_16[,2], '[', seq(max(sapply(PTZ_long_bouts_16[,2],length)))))



#boxplots of counts of bouts longer then 4,6,8 per time frame 

count_longer_8_bouts_PTZ<-aggregate(BoutLength ~ Subject, data = AllPTZ[AllPTZ$TimeFactor==1,], function(x){return(length(x[x>=8]))})

for(time_frame in 2:13){
	#extract the current time frame data and add zeros for the missing subjects who did not have any bouts with at least one C turn or at least one O turn 
	time_frame_data<-aggregate(BoutLength ~ Subject, data = AllPTZ[AllPTZ$TimeFactor==time_frame,], function(x){return(length(x[x>=8]))})[,2]
	time_frame_data<-c(time_frame_data,rep(0,times=144-length(time_frame_data)))

	count_longer_8_bouts_PTZ<-cbind(count_longer_8_bouts_PTZ, time_frame_data)
}

colnames(count_longer_8_bouts_PTZ)<-c("Subject","TimeFrame1","TimeFrame2","TimeFrame3","TimeFrame4","TimeFrame5","TimeFrame6","TimeFrame7","TimeFrame8","TimeFrame9","TimeFrame10","TimeFrame11","TimeFrame12","TimeFrame13")

boxplot(count_longer_8_bouts_PTZ[,-1])




count_longer_4_bouts_PTZ<-aggregate(BoutLength ~ Subject, data = AllPTZ[AllPTZ$TimeFactor==1,], function(x){return(length(x[x>=4]))})

for(time_frame in 2:13){
	#extract the current time frame data and add zeros for the missing subjects who did not have any bouts with at least one C turn or at least one O turn 
	time_frame_data<-aggregate(BoutLength ~ Subject, data = AllPTZ[AllPTZ$TimeFactor==time_frame,], function(x){return(length(x[x>=4]))})[,2]
	time_frame_data<-c(time_frame_data,rep(0,times=144-length(time_frame_data)))
	
	count_longer_4_bouts_PTZ<-cbind(count_longer_4_bouts_PTZ, time_frame_data)
}

colnames(count_longer_4_bouts_PTZ)<-c("Subject","TimeFrame1","TimeFrame2","TimeFrame3","TimeFrame4","TimeFrame5","TimeFrame6","TimeFrame7","TimeFrame8","TimeFrame9","TimeFrame10","TimeFrame11","TimeFrame12","TimeFrame13")

boxplot(count_longer_4_bouts_PTZ[,-1])






count_longer_6_bouts_PTZ<-aggregate(BoutLength ~ Subject, data = AllPTZ[AllPTZ$TimeFactor==1,], function(x){return(length(x[x>=6]))})

for(time_frame in 2:13){
	#extract the current time frame data and add zeros for the missing subjects who did not have any bouts with at least one C turn or at least one O turn 
	time_frame_data<-aggregate(BoutLength ~ Subject, data = AllPTZ[AllPTZ$TimeFactor==time_frame,], function(x){return(length(x[x>=6]))})[,2]
	time_frame_data<-c(time_frame_data,rep(0,times=144-length(time_frame_data)))
	
	count_longer_6_bouts_PTZ<-cbind(count_longer_6_bouts_PTZ, time_frame_data)
}

colnames(count_longer_6_bouts_PTZ)<-c("Subject","TimeFrame1","TimeFrame2","TimeFrame3","TimeFrame4","TimeFrame5","TimeFrame6","TimeFrame7","TimeFrame8","TimeFrame9","TimeFrame10","TimeFrame11","TimeFrame12","TimeFrame13")

boxplot(count_longer_6_bouts_PTZ[,-1])


