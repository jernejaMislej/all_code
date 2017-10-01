
############################################### 10 microM ######################################################
#Light

#Light


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_10microM_Light<-read.table("Natural_Aripiprazole_10microM_Light.txt")
Natural_Cariprazine_10microM_Light<-read.table("Natural_Cariprazine_10microM_Light.txt")
Natural_Clozapine_10microM_Light<-read.table("Natural_Clozapine_10microM_Light.txt")
Natural_CNO_10microM_Light<-read.table("Natural_CNO_10microM_Light.txt")
Natural_Control_Light<-read.table("Natural_Control_Light.txt")
Natural_Haloperidol_10microM_Light<-read.table("Natural_Haloperidol_10microM_Light.txt")
Natural_NDMC_10microM_Light<-read.table("Natural_NDMC_10microM_Light.txt")
Natural_NDMCHigh_100microM_Light<-read.table("Natural_NDMCHigh_100microM_Light.txt")
Natural_OSU6162_10microM_Light<-read.table("Natural_OSU6162_10microM_Light.txt")
Natural_PCAP1_10microM_Light<-read.table("Natural_PCAP1_10microM_Light.txt")
Natural_PCAP2_10microM_Light<-read.table("Natural_PCAP2_10microM_Light.txt")
Natural_PCAP814_10microM_Light<-read.table("Natural_PCAP814_10microM_Light.txt")
Natural_PCAP931_10microM_Light<-read.table("Natural_PCAP931_10microM_Light.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_Light<-Natural_Control_Light[,c(1,3,2,4:11)]
Natural_Aripiprazole_10microM_Light<-Natural_Aripiprazole_10microM_Light[,c(1,3,2,4:11)]
Natural_Cariprazine_10microM_Light<-Natural_Cariprazine_10microM_Light[,c(1,3,2,4:11)]
Natural_Clozapine_10microM_Light<-Natural_Clozapine_10microM_Light[,c(1,3,2,4:11)]
Natural_CNO_10microM_Light<-Natural_CNO_10microM_Light[,c(1,3,2,4:11)]
Natural_Haloperidol_10microM_Light<-Natural_Haloperidol_10microM_Light[,c(1,3,2,4:11)]
Natural_NDMC_10microM_Light<-Natural_NDMC_10microM_Light[,c(1,3,2,4:11)]
Natural_NDMCHigh_100microM_Light<-Natural_NDMCHigh_100microM_Light[,c(1,3,2,4:11)]
Natural_OSU6162_10microM_Light<-Natural_OSU6162_10microM_Light[,c(1,3,2,4:11)]
Natural_PCAP1_10microM_Light<-Natural_PCAP1_10microM_Light[,c(1,3,2,4:11)]
Natural_PCAP2_10microM_Light<-Natural_PCAP2_10microM_Light[,c(1,3,2,4:11)]
Natural_PCAP814_10microM_Light<-Natural_PCAP814_10microM_Light[,c(1,3,2,4:11)]
Natural_PCAP931_10microM_Light<-Natural_PCAP931_10microM_Light[,c(1,3,2,4:11)]



Natural_Control_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_Light_averaged<-rbind(Natural_Control_Light_averaged,
			cbind(aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,3],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,],
#		by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_Light_averaged)[2:10],"_SD"))


colnames(Natural_Control_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_Light_averaged<-aggregate(Natural_Control_Light_averaged,by=list(Natural_Control_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_10microM_Light_averaged<-rbind(Natural_Aripiprazole_10microM_Light_averaged,
			cbind(aggregate(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_10microM_Light[Natural_Aripiprazole_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_10microM_Light_averaged<-aggregate(Natural_Aripiprazole_10microM_Light_averaged,by=list(Natural_Aripiprazole_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_10microM_Light_averaged<-rbind(Natural_Cariprazine_10microM_Light_averaged,
			cbind(aggregate(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_10microM_Light[Natural_Cariprazine_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_10microM_Light_averaged<-aggregate(Natural_Cariprazine_10microM_Light_averaged,by=list(Natural_Cariprazine_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_10microM_Light_averaged<-rbind(Natural_Clozapine_10microM_Light_averaged,
			cbind(aggregate(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_10microM_Light[Natural_Clozapine_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_10microM_Light_averaged<-aggregate(Natural_Clozapine_10microM_Light_averaged,by=list(Natural_Clozapine_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_10microM_Light_averaged<-rbind(Natural_CNO_10microM_Light_averaged,
			cbind(aggregate(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_CNO_10microM_Light[Natural_CNO_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_CNO_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_10microM_Light_averaged<-aggregate(Natural_CNO_10microM_Light_averaged,by=list(Natural_CNO_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_10microM_Light_averaged<-rbind(Natural_Haloperidol_10microM_Light_averaged,
			cbind(aggregate(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_10microM_Light[Natural_Haloperidol_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_10microM_Light_averaged<-aggregate(Natural_Haloperidol_10microM_Light_averaged,by=list(Natural_Haloperidol_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_10microM_Light_averaged<-rbind(Natural_NDMC_10microM_Light_averaged,
			cbind(aggregate(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_10microM_Light[Natural_NDMC_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_10microM_Light_averaged<-aggregate(Natural_NDMC_10microM_Light_averaged,by=list(Natural_NDMC_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_100microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_100microM_Light_averaged<-rbind(Natural_NDMCHigh_100microM_Light_averaged,
			cbind(aggregate(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_100microM_Light[Natural_NDMCHigh_100microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_100microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_100microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_100microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_100microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_100microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_100microM_Light_averaged<-aggregate(Natural_NDMCHigh_100microM_Light_averaged,by=list(Natural_NDMCHigh_100microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_10microM_Light_averaged<-rbind(Natural_OSU6162_10microM_Light_averaged,
			cbind(aggregate(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_10microM_Light[Natural_OSU6162_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_10microM_Light_averaged<-aggregate(Natural_OSU6162_10microM_Light_averaged,by=list(Natural_OSU6162_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_10microM_Light_averaged<-rbind(Natural_PCAP1_10microM_Light_averaged,
			cbind(aggregate(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_10microM_Light[Natural_PCAP1_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_10microM_Light_averaged<-aggregate(Natural_PCAP1_10microM_Light_averaged,by=list(Natural_PCAP1_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_10microM_Light_averaged<-rbind(Natural_PCAP2_10microM_Light_averaged,
			cbind(aggregate(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_10microM_Light[Natural_PCAP2_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_10microM_Light_averaged<-aggregate(Natural_PCAP2_10microM_Light_averaged,by=list(Natural_PCAP2_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_10microM_Light_averaged<-rbind(Natural_PCAP814_10microM_Light_averaged,
			cbind(aggregate(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_10microM_Light[Natural_PCAP814_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_10microM_Light_averaged<-aggregate(Natural_PCAP814_10microM_Light_averaged,by=list(Natural_PCAP814_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_10microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_10microM_Light_averaged<-rbind(Natural_PCAP931_10microM_Light_averaged,
			cbind(aggregate(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_10microM_Light[Natural_PCAP931_10microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_10microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_10microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_10microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_10microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_10microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_10microM_Light_averaged<-aggregate(Natural_PCAP931_10microM_Light_averaged,by=list(Natural_PCAP931_10microM_Light_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_Light_averaged_flat<-c(Natural_Control_Light_averaged_flat,Natural_Control_Light_averaged[variable,-1])
}

Natural_Aripiprazole_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_10microM_Light_averaged_flat<-c(Natural_Aripiprazole_10microM_Light_averaged_flat,Natural_Aripiprazole_10microM_Light_averaged[variable,-1])
}

Natural_Cariprazine_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_10microM_Light_averaged_flat<-c(Natural_Cariprazine_10microM_Light_averaged_flat,Natural_Cariprazine_10microM_Light_averaged[variable,-1])
}


Natural_Clozapine_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_10microM_Light_averaged_flat<-c(Natural_Clozapine_10microM_Light_averaged_flat,Natural_Clozapine_10microM_Light_averaged[variable,-1])
}

Natural_CNO_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_10microM_Light_averaged_flat<-c(Natural_CNO_10microM_Light_averaged_flat,Natural_CNO_10microM_Light_averaged[variable,-1])
}

Natural_Haloperidol_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_10microM_Light_averaged_flat<-c(Natural_Haloperidol_10microM_Light_averaged_flat,Natural_Haloperidol_10microM_Light_averaged[variable,-1])
}

Natural_NDMC_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_10microM_Light_averaged_flat<-c(Natural_NDMC_10microM_Light_averaged_flat,Natural_NDMC_10microM_Light_averaged[variable,-1])
}

Natural_NDMCHigh_100microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_100microM_Light_averaged_flat<-c(Natural_NDMCHigh_100microM_Light_averaged_flat,Natural_NDMCHigh_100microM_Light_averaged[variable,-1])
}

Natural_OSU6162_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_10microM_Light_averaged_flat<-c(Natural_OSU6162_10microM_Light_averaged_flat,Natural_OSU6162_10microM_Light_averaged[variable,-1])
}

Natural_PCAP1_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_10microM_Light_averaged_flat<-c(Natural_PCAP1_10microM_Light_averaged_flat,Natural_PCAP1_10microM_Light_averaged[variable,-1])
}

Natural_PCAP2_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_10microM_Light_averaged_flat<-c(Natural_PCAP2_10microM_Light_averaged_flat,Natural_PCAP2_10microM_Light_averaged[variable,-1])
}


Natural_PCAP814_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_10microM_Light_averaged_flat<-c(Natural_PCAP814_10microM_Light_averaged_flat,Natural_PCAP814_10microM_Light_averaged[variable,-1])
}


Natural_PCAP931_10microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_10microM_Light_averaged_flat<-c(Natural_PCAP931_10microM_Light_averaged_flat,Natural_PCAP931_10microM_Light_averaged[variable,-1])
}


Natural_10microM_Light_all<-rbind(Natural_Control_Light_averaged_flat,Natural_Aripiprazole_10microM_Light_averaged_flat, 
		Natural_Cariprazine_10microM_Light_averaged_flat, Natural_Clozapine_10microM_Light_averaged_flat, Natural_CNO_10microM_Light_averaged_flat,
		Natural_Haloperidol_10microM_Light_averaged_flat, Natural_NDMC_10microM_Light_averaged_flat, Natural_NDMCHigh_100microM_Light_averaged_flat,
		Natural_OSU6162_10microM_Light_averaged_flat, Natural_PCAP1_10microM_Light_averaged_flat, Natural_PCAP2_10microM_Light_averaged_flat,
		Natural_PCAP814_10microM_Light_averaged_flat, Natural_PCAP931_10microM_Light_averaged_flat)

#as numeric
Natural_10microM_Light_all<-apply(Natural_10microM_Light_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_10microM_Light_all<-apply(Natural_10microM_Light_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_10microM_Light_all)<-c("Control","Aripiprazole_10microM","Cariprazine_10microM","Clozapine_10microM",
		"CNO_10microM","Haloperidol_10microM","NDMC_10microM","NDMCHigh_100microM",
		"OSU6162_10microM","PCAP1_10microM","PCAP2_10microM","PCAP814_10microM",
		"PCAP931_10microM")


#euclidean distance
d <- dist(Natural_10microM_Light_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_Light.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_10microM_Light_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_Light.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_10microM_Light_all)[drug],"Light.png"),width=1500,height=750)
	plot(Natural_10microM_Light_all[c("Control"),122:143],Natural_10microM_Light_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_10microM_Light_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_10microM_Light_all[c("Control"),100:121],Natural_10microM_Light_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_10microM_Light_all[c("Control"),78:99],Natural_10microM_Light_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_10microM_Light_all[c("Control"),56:77],Natural_10microM_Light_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_10microM_Light_all[c("Control"),34:55],Natural_10microM_Light_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_10microM_Light_all[c("Control"),12:33],Natural_10microM_Light_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_10microM_Light_all[c("Control"),1:11],Natural_10microM_Light_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}



#LightDark


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_10microM_LightDark<-read.table("Natural_Aripiprazole_10microM_LightDark.txt")
Natural_Cariprazine_10microM_LightDark<-read.table("Natural_Cariprazine_10microM_LightDark.txt")
Natural_Clozapine_10microM_LightDark<-read.table("Natural_Clozapine_10microM_LightDark.txt")
Natural_CNO_10microM_LightDark<-read.table("Natural_CNO_10microM_LightDark.txt")
Natural_Control_LightDark<-read.table("Natural_Control_LightDark.txt")
Natural_Haloperidol_10microM_LightDark<-read.table("Natural_Haloperidol_10microM_LightDark.txt")
Natural_NDMC_10microM_LightDark<-read.table("Natural_NDMC_10microM_LightDark.txt")
Natural_NDMCHigh_100microM_LightDark<-read.table("Natural_NDMCHigh_100microM_LightDark.txt")
Natural_OSU6162_10microM_LightDark<-read.table("Natural_OSU6162_10microM_LightDark.txt")
Natural_PCAP1_10microM_LightDark<-read.table("Natural_PCAP1_10microM_LightDark.txt")
Natural_PCAP2_10microM_LightDark<-read.table("Natural_PCAP2_10microM_LightDark.txt")
Natural_PCAP814_10microM_LightDark<-read.table("Natural_PCAP814_10microM_LightDark.txt")
Natural_PCAP931_10microM_LightDark<-read.table("Natural_PCAP931_10microM_LightDark.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_LightDark<-Natural_Control_LightDark[,c(1,3,2,4:11)]
Natural_Aripiprazole_10microM_LightDark<-Natural_Aripiprazole_10microM_LightDark[,c(1,3,2,4:11)]
Natural_Cariprazine_10microM_LightDark<-Natural_Cariprazine_10microM_LightDark[,c(1,3,2,4:11)]
Natural_Clozapine_10microM_LightDark<-Natural_Clozapine_10microM_LightDark[,c(1,3,2,4:11)]
Natural_CNO_10microM_LightDark<-Natural_CNO_10microM_LightDark[,c(1,3,2,4:11)]
Natural_Haloperidol_10microM_LightDark<-Natural_Haloperidol_10microM_LightDark[,c(1,3,2,4:11)]
Natural_NDMC_10microM_LightDark<-Natural_NDMC_10microM_LightDark[,c(1,3,2,4:11)]
Natural_NDMCHigh_100microM_LightDark<-Natural_NDMCHigh_100microM_LightDark[,c(1,3,2,4:11)]
Natural_OSU6162_10microM_LightDark<-Natural_OSU6162_10microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP1_10microM_LightDark<-Natural_PCAP1_10microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP2_10microM_LightDark<-Natural_PCAP2_10microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP814_10microM_LightDark<-Natural_PCAP814_10microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP931_10microM_LightDark<-Natural_PCAP931_10microM_LightDark[,c(1,3,2,4:11)]



Natural_Control_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_LightDark_averaged<-rbind(Natural_Control_LightDark_averaged,
			cbind(aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Control_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_LightDark_averaged<-aggregate(Natural_Control_LightDark_averaged,by=list(Natural_Control_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_10microM_LightDark_averaged<-rbind(Natural_Aripiprazole_10microM_LightDark_averaged,
			cbind(aggregate(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_10microM_LightDark[Natural_Aripiprazole_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_10microM_LightDark_averaged<-aggregate(Natural_Aripiprazole_10microM_LightDark_averaged,by=list(Natural_Aripiprazole_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_10microM_LightDark_averaged<-rbind(Natural_Cariprazine_10microM_LightDark_averaged,
			cbind(aggregate(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_10microM_LightDark[Natural_Cariprazine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_10microM_LightDark_averaged<-aggregate(Natural_Cariprazine_10microM_LightDark_averaged,by=list(Natural_Cariprazine_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_10microM_LightDark_averaged<-rbind(Natural_Clozapine_10microM_LightDark_averaged,
			cbind(aggregate(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_10microM_LightDark[Natural_Clozapine_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_10microM_LightDark_averaged<-aggregate(Natural_Clozapine_10microM_LightDark_averaged,by=list(Natural_Clozapine_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_10microM_LightDark_averaged<-rbind(Natural_CNO_10microM_LightDark_averaged,
			cbind(aggregate(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_CNO_10microM_LightDark[Natural_CNO_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_CNO_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_10microM_LightDark_averaged<-aggregate(Natural_CNO_10microM_LightDark_averaged,by=list(Natural_CNO_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_10microM_LightDark_averaged<-rbind(Natural_Haloperidol_10microM_LightDark_averaged,
			cbind(aggregate(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_10microM_LightDark[Natural_Haloperidol_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_10microM_LightDark_averaged<-aggregate(Natural_Haloperidol_10microM_LightDark_averaged,by=list(Natural_Haloperidol_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_10microM_LightDark_averaged<-rbind(Natural_NDMC_10microM_LightDark_averaged,
			cbind(aggregate(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_10microM_LightDark[Natural_NDMC_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_10microM_LightDark_averaged<-aggregate(Natural_NDMC_10microM_LightDark_averaged,by=list(Natural_NDMC_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_100microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_100microM_LightDark_averaged<-rbind(Natural_NDMCHigh_100microM_LightDark_averaged,
			cbind(aggregate(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_100microM_LightDark[Natural_NDMCHigh_100microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_100microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_100microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_100microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_100microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_100microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_100microM_LightDark_averaged<-aggregate(Natural_NDMCHigh_100microM_LightDark_averaged,by=list(Natural_NDMCHigh_100microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_10microM_LightDark_averaged<-rbind(Natural_OSU6162_10microM_LightDark_averaged,
			cbind(aggregate(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_10microM_LightDark[Natural_OSU6162_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_10microM_LightDark_averaged<-aggregate(Natural_OSU6162_10microM_LightDark_averaged,by=list(Natural_OSU6162_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_10microM_LightDark_averaged<-rbind(Natural_PCAP1_10microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_10microM_LightDark[Natural_PCAP1_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_10microM_LightDark_averaged<-aggregate(Natural_PCAP1_10microM_LightDark_averaged,by=list(Natural_PCAP1_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_10microM_LightDark_averaged<-rbind(Natural_PCAP2_10microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_10microM_LightDark[Natural_PCAP2_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_10microM_LightDark_averaged<-aggregate(Natural_PCAP2_10microM_LightDark_averaged,by=list(Natural_PCAP2_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_10microM_LightDark_averaged<-rbind(Natural_PCAP814_10microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_10microM_LightDark[Natural_PCAP814_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_10microM_LightDark_averaged<-aggregate(Natural_PCAP814_10microM_LightDark_averaged,by=list(Natural_PCAP814_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_10microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_10microM_LightDark_averaged<-rbind(Natural_PCAP931_10microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_10microM_LightDark[Natural_PCAP931_10microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_10microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_10microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_10microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_10microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_10microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_10microM_LightDark_averaged<-aggregate(Natural_PCAP931_10microM_LightDark_averaged,by=list(Natural_PCAP931_10microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_LightDark_averaged_flat<-c(Natural_Control_LightDark_averaged_flat,Natural_Control_LightDark_averaged[variable,-1])
}

Natural_Aripiprazole_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_10microM_LightDark_averaged_flat<-c(Natural_Aripiprazole_10microM_LightDark_averaged_flat,Natural_Aripiprazole_10microM_LightDark_averaged[variable,-1])
}

Natural_Cariprazine_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_10microM_LightDark_averaged_flat<-c(Natural_Cariprazine_10microM_LightDark_averaged_flat,Natural_Cariprazine_10microM_LightDark_averaged[variable,-1])
}


Natural_Clozapine_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_10microM_LightDark_averaged_flat<-c(Natural_Clozapine_10microM_LightDark_averaged_flat,Natural_Clozapine_10microM_LightDark_averaged[variable,-1])
}

Natural_CNO_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_10microM_LightDark_averaged_flat<-c(Natural_CNO_10microM_LightDark_averaged_flat,Natural_CNO_10microM_LightDark_averaged[variable,-1])
}

Natural_Haloperidol_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_10microM_LightDark_averaged_flat<-c(Natural_Haloperidol_10microM_LightDark_averaged_flat,Natural_Haloperidol_10microM_LightDark_averaged[variable,-1])
}

Natural_NDMC_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_10microM_LightDark_averaged_flat<-c(Natural_NDMC_10microM_LightDark_averaged_flat,Natural_NDMC_10microM_LightDark_averaged[variable,-1])
}

Natural_NDMCHigh_100microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_100microM_LightDark_averaged_flat<-c(Natural_NDMCHigh_100microM_LightDark_averaged_flat,Natural_NDMCHigh_100microM_LightDark_averaged[variable,-1])
}

Natural_OSU6162_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_10microM_LightDark_averaged_flat<-c(Natural_OSU6162_10microM_LightDark_averaged_flat,Natural_OSU6162_10microM_LightDark_averaged[variable,-1])
}

Natural_PCAP1_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_10microM_LightDark_averaged_flat<-c(Natural_PCAP1_10microM_LightDark_averaged_flat,Natural_PCAP1_10microM_LightDark_averaged[variable,-1])
}

Natural_PCAP2_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_10microM_LightDark_averaged_flat<-c(Natural_PCAP2_10microM_LightDark_averaged_flat,Natural_PCAP2_10microM_LightDark_averaged[variable,-1])
}


Natural_PCAP814_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_10microM_LightDark_averaged_flat<-c(Natural_PCAP814_10microM_LightDark_averaged_flat,Natural_PCAP814_10microM_LightDark_averaged[variable,-1])
}


Natural_PCAP931_10microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_10microM_LightDark_averaged_flat<-c(Natural_PCAP931_10microM_LightDark_averaged_flat,Natural_PCAP931_10microM_LightDark_averaged[variable,-1])
}


Natural_10microM_LightDark_all<-rbind(Natural_Control_LightDark_averaged_flat,Natural_Aripiprazole_10microM_LightDark_averaged_flat, 
		Natural_Cariprazine_10microM_LightDark_averaged_flat, Natural_Clozapine_10microM_LightDark_averaged_flat, Natural_CNO_10microM_LightDark_averaged_flat,
		Natural_Haloperidol_10microM_LightDark_averaged_flat, Natural_NDMC_10microM_LightDark_averaged_flat, Natural_NDMCHigh_100microM_LightDark_averaged_flat,
		Natural_OSU6162_10microM_LightDark_averaged_flat, Natural_PCAP1_10microM_LightDark_averaged_flat, Natural_PCAP2_10microM_LightDark_averaged_flat,
		Natural_PCAP814_10microM_LightDark_averaged_flat, Natural_PCAP931_10microM_LightDark_averaged_flat)

#as numeric
Natural_10microM_LightDark_all<-apply(Natural_10microM_LightDark_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_10microM_LightDark_all<-apply(Natural_10microM_LightDark_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_10microM_LightDark_all)<-c("Control","Aripiprazole_10microM","Cariprazine_10microM","Clozapine_10microM",
		"CNO_10microM","Haloperidol_10microM","NDMC_10microM","NDMCHigh_100microM",
		"OSU6162_10microM","PCAP1_10microM","PCAP2_10microM","PCAP814_10microM",
		"PCAP931_10microM")


#euclidean distance
d <- dist(Natural_10microM_LightDark_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_LightDark.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_10microM_LightDark_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_LightDark.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_10microM_LightDark_all)[drug],"LightDark.png"),width=1500,height=750)
	plot(Natural_10microM_LightDark_all[c("Control"),122:143],Natural_10microM_LightDark_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_10microM_LightDark_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_10microM_LightDark_all[c("Control"),100:121],Natural_10microM_LightDark_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_10microM_LightDark_all[c("Control"),78:99],Natural_10microM_LightDark_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_10microM_LightDark_all[c("Control"),56:77],Natural_10microM_LightDark_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_10microM_LightDark_all[c("Control"),34:55],Natural_10microM_LightDark_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_10microM_LightDark_all[c("Control"),12:33],Natural_10microM_LightDark_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_10microM_LightDark_all[c("Control"),1:11],Natural_10microM_LightDark_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}





#Dark


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_10microM_Dark<-read.table("Natural_Aripiprazole_10microM_Dark.txt")
Natural_Cariprazine_10microM_Dark<-read.table("Natural_Cariprazine_10microM_Dark.txt")
Natural_Clozapine_10microM_Dark<-read.table("Natural_Clozapine_10microM_Dark.txt")
Natural_CNO_10microM_Dark<-read.table("Natural_CNO_10microM_Dark.txt")
Natural_Control_Dark<-read.table("Natural_Control_Dark.txt")
Natural_Haloperidol_10microM_Dark<-read.table("Natural_Haloperidol_10microM_Dark.txt")
Natural_NDMC_10microM_Dark<-read.table("Natural_NDMC_10microM_Dark.txt")
Natural_NDMCHigh_100microM_Dark<-read.table("Natural_NDMCHigh_100microM_Dark.txt")
Natural_OSU6162_10microM_Dark<-read.table("Natural_OSU6162_10microM_Dark.txt")
Natural_PCAP1_10microM_Dark<-read.table("Natural_PCAP1_10microM_Dark.txt")
Natural_PCAP2_10microM_Dark<-read.table("Natural_PCAP2_10microM_Dark.txt")
Natural_PCAP814_10microM_Dark<-read.table("Natural_PCAP814_10microM_Dark.txt")
Natural_PCAP931_10microM_Dark<-read.table("Natural_PCAP931_10microM_Dark.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_Dark<-Natural_Control_Dark[,c(1,3,2,4:11)]
Natural_Aripiprazole_10microM_Dark<-Natural_Aripiprazole_10microM_Dark[,c(1,3,2,4:11)]
Natural_Cariprazine_10microM_Dark<-Natural_Cariprazine_10microM_Dark[,c(1,3,2,4:11)]
Natural_Clozapine_10microM_Dark<-Natural_Clozapine_10microM_Dark[,c(1,3,2,4:11)]
Natural_CNO_10microM_Dark<-Natural_CNO_10microM_Dark[,c(1,3,2,4:11)]
Natural_Haloperidol_10microM_Dark<-Natural_Haloperidol_10microM_Dark[,c(1,3,2,4:11)]
Natural_NDMC_10microM_Dark<-Natural_NDMC_10microM_Dark[,c(1,3,2,4:11)]
Natural_NDMCHigh_100microM_Dark<-Natural_NDMCHigh_100microM_Dark[,c(1,3,2,4:11)]
Natural_OSU6162_10microM_Dark<-Natural_OSU6162_10microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP1_10microM_Dark<-Natural_PCAP1_10microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP2_10microM_Dark<-Natural_PCAP2_10microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP814_10microM_Dark<-Natural_PCAP814_10microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP931_10microM_Dark<-Natural_PCAP931_10microM_Dark[,c(1,3,2,4:11)]



Natural_Control_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_Dark_averaged<-rbind(Natural_Control_Dark_averaged,
			cbind(aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Control_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_Dark_averaged<-aggregate(Natural_Control_Dark_averaged,by=list(Natural_Control_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_10microM_Dark_averaged<-rbind(Natural_Aripiprazole_10microM_Dark_averaged,
			cbind(aggregate(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_10microM_Dark[Natural_Aripiprazole_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_10microM_Dark_averaged<-aggregate(Natural_Aripiprazole_10microM_Dark_averaged,by=list(Natural_Aripiprazole_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_10microM_Dark_averaged<-rbind(Natural_Cariprazine_10microM_Dark_averaged,
			cbind(aggregate(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_10microM_Dark[Natural_Cariprazine_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_10microM_Dark_averaged<-aggregate(Natural_Cariprazine_10microM_Dark_averaged,by=list(Natural_Cariprazine_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_10microM_Dark_averaged<-rbind(Natural_Clozapine_10microM_Dark_averaged,
			cbind(aggregate(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_10microM_Dark[Natural_Clozapine_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_10microM_Dark_averaged<-aggregate(Natural_Clozapine_10microM_Dark_averaged,by=list(Natural_Clozapine_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_10microM_Dark_averaged<-rbind(Natural_CNO_10microM_Dark_averaged,
			cbind(aggregate(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_CNO_10microM_Dark[Natural_CNO_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_CNO_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_10microM_Dark_averaged<-aggregate(Natural_CNO_10microM_Dark_averaged,by=list(Natural_CNO_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_10microM_Dark_averaged<-rbind(Natural_Haloperidol_10microM_Dark_averaged,
			cbind(aggregate(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_10microM_Dark[Natural_Haloperidol_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_10microM_Dark_averaged<-aggregate(Natural_Haloperidol_10microM_Dark_averaged,by=list(Natural_Haloperidol_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_10microM_Dark_averaged<-rbind(Natural_NDMC_10microM_Dark_averaged,
			cbind(aggregate(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_10microM_Dark[Natural_NDMC_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_10microM_Dark_averaged<-aggregate(Natural_NDMC_10microM_Dark_averaged,by=list(Natural_NDMC_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_100microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_100microM_Dark_averaged<-rbind(Natural_NDMCHigh_100microM_Dark_averaged,
			cbind(aggregate(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_100microM_Dark[Natural_NDMCHigh_100microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_100microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_100microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_100microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_100microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_100microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_100microM_Dark_averaged<-aggregate(Natural_NDMCHigh_100microM_Dark_averaged,by=list(Natural_NDMCHigh_100microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_10microM_Dark_averaged<-rbind(Natural_OSU6162_10microM_Dark_averaged,
			cbind(aggregate(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_10microM_Dark[Natural_OSU6162_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_10microM_Dark_averaged<-aggregate(Natural_OSU6162_10microM_Dark_averaged,by=list(Natural_OSU6162_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_10microM_Dark_averaged<-rbind(Natural_PCAP1_10microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_10microM_Dark[Natural_PCAP1_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_10microM_Dark_averaged<-aggregate(Natural_PCAP1_10microM_Dark_averaged,by=list(Natural_PCAP1_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_10microM_Dark_averaged<-rbind(Natural_PCAP2_10microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_10microM_Dark[Natural_PCAP2_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_10microM_Dark_averaged<-aggregate(Natural_PCAP2_10microM_Dark_averaged,by=list(Natural_PCAP2_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_10microM_Dark_averaged<-rbind(Natural_PCAP814_10microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_10microM_Dark[Natural_PCAP814_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_10microM_Dark_averaged<-aggregate(Natural_PCAP814_10microM_Dark_averaged,by=list(Natural_PCAP814_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_10microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_10microM_Dark_averaged<-rbind(Natural_PCAP931_10microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_10microM_Dark[Natural_PCAP931_10microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_10microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_10microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_10microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_10microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_10microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_10microM_Dark_averaged<-aggregate(Natural_PCAP931_10microM_Dark_averaged,by=list(Natural_PCAP931_10microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_Dark_averaged_flat<-c(Natural_Control_Dark_averaged_flat,Natural_Control_Dark_averaged[variable,-1])
}

Natural_Aripiprazole_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_10microM_Dark_averaged_flat<-c(Natural_Aripiprazole_10microM_Dark_averaged_flat,Natural_Aripiprazole_10microM_Dark_averaged[variable,-1])
}

Natural_Cariprazine_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_10microM_Dark_averaged_flat<-c(Natural_Cariprazine_10microM_Dark_averaged_flat,Natural_Cariprazine_10microM_Dark_averaged[variable,-1])
}


Natural_Clozapine_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_10microM_Dark_averaged_flat<-c(Natural_Clozapine_10microM_Dark_averaged_flat,Natural_Clozapine_10microM_Dark_averaged[variable,-1])
}

Natural_CNO_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_10microM_Dark_averaged_flat<-c(Natural_CNO_10microM_Dark_averaged_flat,Natural_CNO_10microM_Dark_averaged[variable,-1])
}

Natural_Haloperidol_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_10microM_Dark_averaged_flat<-c(Natural_Haloperidol_10microM_Dark_averaged_flat,Natural_Haloperidol_10microM_Dark_averaged[variable,-1])
}

Natural_NDMC_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_10microM_Dark_averaged_flat<-c(Natural_NDMC_10microM_Dark_averaged_flat,Natural_NDMC_10microM_Dark_averaged[variable,-1])
}

Natural_NDMCHigh_100microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_100microM_Dark_averaged_flat<-c(Natural_NDMCHigh_100microM_Dark_averaged_flat,Natural_NDMCHigh_100microM_Dark_averaged[variable,-1])
}

Natural_OSU6162_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_10microM_Dark_averaged_flat<-c(Natural_OSU6162_10microM_Dark_averaged_flat,Natural_OSU6162_10microM_Dark_averaged[variable,-1])
}

Natural_PCAP1_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_10microM_Dark_averaged_flat<-c(Natural_PCAP1_10microM_Dark_averaged_flat,Natural_PCAP1_10microM_Dark_averaged[variable,-1])
}

Natural_PCAP2_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_10microM_Dark_averaged_flat<-c(Natural_PCAP2_10microM_Dark_averaged_flat,Natural_PCAP2_10microM_Dark_averaged[variable,-1])
}


Natural_PCAP814_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_10microM_Dark_averaged_flat<-c(Natural_PCAP814_10microM_Dark_averaged_flat,Natural_PCAP814_10microM_Dark_averaged[variable,-1])
}


Natural_PCAP931_10microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_10microM_Dark_averaged_flat<-c(Natural_PCAP931_10microM_Dark_averaged_flat,Natural_PCAP931_10microM_Dark_averaged[variable,-1])
}


Natural_10microM_Dark_all<-rbind(Natural_Control_Dark_averaged_flat,Natural_Aripiprazole_10microM_Dark_averaged_flat, 
		Natural_Cariprazine_10microM_Dark_averaged_flat, Natural_Clozapine_10microM_Dark_averaged_flat, Natural_CNO_10microM_Dark_averaged_flat,
		Natural_Haloperidol_10microM_Dark_averaged_flat, Natural_NDMC_10microM_Dark_averaged_flat, Natural_NDMCHigh_100microM_Dark_averaged_flat,
		Natural_OSU6162_10microM_Dark_averaged_flat, Natural_PCAP1_10microM_Dark_averaged_flat, Natural_PCAP2_10microM_Dark_averaged_flat,
		Natural_PCAP814_10microM_Dark_averaged_flat, Natural_PCAP931_10microM_Dark_averaged_flat)

#as numeric
Natural_10microM_Dark_all<-apply(Natural_10microM_Dark_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_10microM_Dark_all<-apply(Natural_10microM_Dark_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_10microM_Dark_all)<-c("Control","Aripiprazole_10microM","Cariprazine_10microM","Clozapine_10microM",
		"CNO_10microM","Haloperidol_10microM","NDMC_10microM","NDMCHigh_100microM",
		"OSU6162_10microM","PCAP1_10microM","PCAP2_10microM","PCAP814_10microM",
		"PCAP931_10microM")


#euclidean distance
d <- dist(Natural_10microM_Dark_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_Dark.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_10microM_Dark_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_Dark.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_10microM_Dark_all)[drug],"Dark.png"),width=1500,height=750)
	plot(Natural_10microM_Dark_all[c("Control"),122:143],Natural_10microM_Dark_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_10microM_Dark_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_10microM_Dark_all[c("Control"),100:121],Natural_10microM_Dark_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_10microM_Dark_all[c("Control"),78:99],Natural_10microM_Dark_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_10microM_Dark_all[c("Control"),56:77],Natural_10microM_Dark_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_10microM_Dark_all[c("Control"),34:55],Natural_10microM_Dark_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_10microM_Dark_all[c("Control"),12:33],Natural_10microM_Dark_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_10microM_Dark_all[c("Control"),1:11],Natural_10microM_Dark_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}









#DarkApoLow


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_10microM_DarkApoLow<-read.table("Disease_Aripiprazole_10microM_DarkApoLow.txt")
Disease_Cariprazine_10microM_DarkApoLow<-read.table("Disease_Cariprazine_10microM_DarkApoLow.txt")
Disease_Clozapine_10microM_DarkApoLow<-read.table("Disease_Clozapine_10microM_DarkApoLow.txt")
Disease_CNO_10microM_DarkApoLow<-read.table("Disease_CNO_10microM_DarkApoLow.txt")
Disease_Control_DarkApoLow<-read.table("Disease_Control_DarkApoLow.txt")
Disease_Haloperidol_10microM_DarkApoLow<-read.table("Disease_Haloperidol_10microM_DarkApoLow.txt")
Disease_NDMC_10microM_DarkApoLow<-read.table("Disease_NDMC_10microM_DarkApoLow.txt")
Disease_NDMCHigh_100microM_DarkApoLow<-read.table("Disease_NDMCHigh_100microM_DarkApoLow.txt")
Disease_OSU6162_10microM_DarkApoLow<-read.table("Disease_OSU6162_10microM_DarkApoLow.txt")
Disease_PCAP1_10microM_DarkApoLow<-read.table("Disease_PCAP1_10microM_DarkApoLow.txt")
Disease_PCAP2_10microM_DarkApoLow<-read.table("Disease_PCAP2_10microM_DarkApoLow.txt")
Disease_PCAP814_10microM_DarkApoLow<-read.table("Disease_PCAP814_10microM_DarkApoLow.txt")
Disease_PCAP931_10microM_DarkApoLow<-read.table("Disease_PCAP931_10microM_DarkApoLow.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkApoLow<-Disease_Control_DarkApoLow[,c(1,3,2,4:11)]
Disease_Aripiprazole_10microM_DarkApoLow<-Disease_Aripiprazole_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Cariprazine_10microM_DarkApoLow<-Disease_Cariprazine_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Clozapine_10microM_DarkApoLow<-Disease_Clozapine_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_CNO_10microM_DarkApoLow<-Disease_CNO_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Haloperidol_10microM_DarkApoLow<-Disease_Haloperidol_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_NDMC_10microM_DarkApoLow<-Disease_NDMC_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_NDMCHigh_100microM_DarkApoLow<-Disease_NDMCHigh_100microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_OSU6162_10microM_DarkApoLow<-Disease_OSU6162_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP1_10microM_DarkApoLow<-Disease_PCAP1_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP2_10microM_DarkApoLow<-Disease_PCAP2_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP814_10microM_DarkApoLow<-Disease_PCAP814_10microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP931_10microM_DarkApoLow<-Disease_PCAP931_10microM_DarkApoLow[,c(1,3,2,4:11)]



Disease_Control_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkApoLow_averaged<-rbind(Disease_Control_DarkApoLow_averaged,
			cbind(aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkApoLow_averaged<-aggregate(Disease_Control_DarkApoLow_averaged,by=list(Disease_Control_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_10microM_DarkApoLow_averaged<-rbind(Disease_Aripiprazole_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_10microM_DarkApoLow[Disease_Aripiprazole_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_10microM_DarkApoLow_averaged<-aggregate(Disease_Aripiprazole_10microM_DarkApoLow_averaged,by=list(Disease_Aripiprazole_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_10microM_DarkApoLow_averaged<-rbind(Disease_Cariprazine_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_10microM_DarkApoLow[Disease_Cariprazine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_10microM_DarkApoLow_averaged<-aggregate(Disease_Cariprazine_10microM_DarkApoLow_averaged,by=list(Disease_Cariprazine_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_10microM_DarkApoLow_averaged<-rbind(Disease_Clozapine_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_10microM_DarkApoLow[Disease_Clozapine_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_10microM_DarkApoLow_averaged<-aggregate(Disease_Clozapine_10microM_DarkApoLow_averaged,by=list(Disease_Clozapine_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_10microM_DarkApoLow_averaged<-rbind(Disease_CNO_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_CNO_10microM_DarkApoLow[Disease_CNO_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_CNO_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_10microM_DarkApoLow_averaged<-aggregate(Disease_CNO_10microM_DarkApoLow_averaged,by=list(Disease_CNO_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_10microM_DarkApoLow_averaged<-rbind(Disease_Haloperidol_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_10microM_DarkApoLow[Disease_Haloperidol_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_10microM_DarkApoLow_averaged<-aggregate(Disease_Haloperidol_10microM_DarkApoLow_averaged,by=list(Disease_Haloperidol_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_10microM_DarkApoLow_averaged<-rbind(Disease_NDMC_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_10microM_DarkApoLow[Disease_NDMC_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_10microM_DarkApoLow_averaged<-aggregate(Disease_NDMC_10microM_DarkApoLow_averaged,by=list(Disease_NDMC_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_100microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_100microM_DarkApoLow_averaged<-rbind(Disease_NDMCHigh_100microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_100microM_DarkApoLow[Disease_NDMCHigh_100microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_100microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_100microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_100microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_100microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_100microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_100microM_DarkApoLow_averaged<-aggregate(Disease_NDMCHigh_100microM_DarkApoLow_averaged,by=list(Disease_NDMCHigh_100microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_10microM_DarkApoLow_averaged<-rbind(Disease_OSU6162_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_10microM_DarkApoLow[Disease_OSU6162_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_10microM_DarkApoLow_averaged<-aggregate(Disease_OSU6162_10microM_DarkApoLow_averaged,by=list(Disease_OSU6162_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_10microM_DarkApoLow_averaged<-rbind(Disease_PCAP1_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_10microM_DarkApoLow[Disease_PCAP1_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_10microM_DarkApoLow_averaged<-aggregate(Disease_PCAP1_10microM_DarkApoLow_averaged,by=list(Disease_PCAP1_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_10microM_DarkApoLow_averaged<-rbind(Disease_PCAP2_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_10microM_DarkApoLow[Disease_PCAP2_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_10microM_DarkApoLow_averaged<-aggregate(Disease_PCAP2_10microM_DarkApoLow_averaged,by=list(Disease_PCAP2_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_10microM_DarkApoLow_averaged<-rbind(Disease_PCAP814_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_10microM_DarkApoLow[Disease_PCAP814_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_10microM_DarkApoLow_averaged<-aggregate(Disease_PCAP814_10microM_DarkApoLow_averaged,by=list(Disease_PCAP814_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_10microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_10microM_DarkApoLow_averaged<-rbind(Disease_PCAP931_10microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_10microM_DarkApoLow[Disease_PCAP931_10microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_10microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_10microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_10microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_10microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_10microM_DarkApoLow_averaged<-aggregate(Disease_PCAP931_10microM_DarkApoLow_averaged,by=list(Disease_PCAP931_10microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkApoLow_averaged_flat<-c(Disease_Control_DarkApoLow_averaged_flat,Disease_Control_DarkApoLow_averaged[variable,-1])
}

Disease_Aripiprazole_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_10microM_DarkApoLow_averaged_flat<-c(Disease_Aripiprazole_10microM_DarkApoLow_averaged_flat,Disease_Aripiprazole_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_Cariprazine_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_10microM_DarkApoLow_averaged_flat<-c(Disease_Cariprazine_10microM_DarkApoLow_averaged_flat,Disease_Cariprazine_10microM_DarkApoLow_averaged[variable,-1])
}


Disease_Clozapine_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_10microM_DarkApoLow_averaged_flat<-c(Disease_Clozapine_10microM_DarkApoLow_averaged_flat,Disease_Clozapine_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_CNO_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_10microM_DarkApoLow_averaged_flat<-c(Disease_CNO_10microM_DarkApoLow_averaged_flat,Disease_CNO_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_Haloperidol_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_10microM_DarkApoLow_averaged_flat<-c(Disease_Haloperidol_10microM_DarkApoLow_averaged_flat,Disease_Haloperidol_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_NDMC_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_10microM_DarkApoLow_averaged_flat<-c(Disease_NDMC_10microM_DarkApoLow_averaged_flat,Disease_NDMC_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_NDMCHigh_100microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_100microM_DarkApoLow_averaged_flat<-c(Disease_NDMCHigh_100microM_DarkApoLow_averaged_flat,Disease_NDMCHigh_100microM_DarkApoLow_averaged[variable,-1])
}

Disease_OSU6162_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_10microM_DarkApoLow_averaged_flat<-c(Disease_OSU6162_10microM_DarkApoLow_averaged_flat,Disease_OSU6162_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_PCAP1_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_10microM_DarkApoLow_averaged_flat<-c(Disease_PCAP1_10microM_DarkApoLow_averaged_flat,Disease_PCAP1_10microM_DarkApoLow_averaged[variable,-1])
}

Disease_PCAP2_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_10microM_DarkApoLow_averaged_flat<-c(Disease_PCAP2_10microM_DarkApoLow_averaged_flat,Disease_PCAP2_10microM_DarkApoLow_averaged[variable,-1])
}


Disease_PCAP814_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_10microM_DarkApoLow_averaged_flat<-c(Disease_PCAP814_10microM_DarkApoLow_averaged_flat,Disease_PCAP814_10microM_DarkApoLow_averaged[variable,-1])
}


Disease_PCAP931_10microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_10microM_DarkApoLow_averaged_flat<-c(Disease_PCAP931_10microM_DarkApoLow_averaged_flat,Disease_PCAP931_10microM_DarkApoLow_averaged[variable,-1])
}


Disease_10microM_DarkApoLow_all<-rbind(Disease_Control_DarkApoLow_averaged_flat,Disease_Aripiprazole_10microM_DarkApoLow_averaged_flat, 
		Disease_Cariprazine_10microM_DarkApoLow_averaged_flat, Disease_Clozapine_10microM_DarkApoLow_averaged_flat, Disease_CNO_10microM_DarkApoLow_averaged_flat,
		Disease_Haloperidol_10microM_DarkApoLow_averaged_flat, Disease_NDMC_10microM_DarkApoLow_averaged_flat, Disease_NDMCHigh_100microM_DarkApoLow_averaged_flat,
		Disease_OSU6162_10microM_DarkApoLow_averaged_flat, Disease_PCAP1_10microM_DarkApoLow_averaged_flat, Disease_PCAP2_10microM_DarkApoLow_averaged_flat,
		Disease_PCAP814_10microM_DarkApoLow_averaged_flat, Disease_PCAP931_10microM_DarkApoLow_averaged_flat)

#as numeric
Disease_10microM_DarkApoLow_all<-apply(Disease_10microM_DarkApoLow_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_10microM_DarkApoLow_all<-apply(Disease_10microM_DarkApoLow_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_10microM_DarkApoLow_all)<-c("Control","Aripiprazole_10microM","Cariprazine_10microM","Clozapine_10microM",
		"CNO_10microM","Haloperidol_10microM","NDMC_10microM","NDMCHigh_100microM",
		"OSU6162_10microM","PCAP1_10microM","PCAP2_10microM","PCAP814_10microM",
		"PCAP931_10microM")


#euclidean distance
d <- dist(Disease_10microM_DarkApoLow_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkApoLow.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_10microM_DarkApoLow_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkApoLow.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_10microM_DarkApoLow_all)[drug],"DarkApoLow.png"),width=1500,height=750)
	plot(Disease_10microM_DarkApoLow_all[c("Control"),122:143],Disease_10microM_DarkApoLow_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_10microM_DarkApoLow_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_10microM_DarkApoLow_all[c("Control"),100:121],Disease_10microM_DarkApoLow_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_10microM_DarkApoLow_all[c("Control"),78:99],Disease_10microM_DarkApoLow_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_10microM_DarkApoLow_all[c("Control"),56:77],Disease_10microM_DarkApoLow_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_10microM_DarkApoLow_all[c("Control"),34:55],Disease_10microM_DarkApoLow_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_10microM_DarkApoLow_all[c("Control"),12:33],Disease_10microM_DarkApoLow_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_10microM_DarkApoLow_all[c("Control"),1:11],Disease_10microM_DarkApoLow_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}



#DarkApoHigh


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_10microM_DarkApoHigh<-read.table("Disease_Aripiprazole_10microM_DarkApoHigh.txt")
Disease_Cariprazine_10microM_DarkApoHigh<-read.table("Disease_Cariprazine_10microM_DarkApoHigh.txt")
Disease_Clozapine_10microM_DarkApoHigh<-read.table("Disease_Clozapine_10microM_DarkApoHigh.txt")
Disease_CNO_10microM_DarkApoHigh<-read.table("Disease_CNO_10microM_DarkApoHigh.txt")
Disease_Control_DarkApoHigh<-read.table("Disease_Control_DarkApoHigh.txt")
Disease_Haloperidol_10microM_DarkApoHigh<-read.table("Disease_Haloperidol_10microM_DarkApoHigh.txt")
Disease_NDMC_10microM_DarkApoHigh<-read.table("Disease_NDMC_10microM_DarkApoHigh.txt")
Disease_NDMCHigh_100microM_DarkApoHigh<-read.table("Disease_NDMCHigh_100microM_DarkApoHigh.txt")
Disease_OSU6162_10microM_DarkApoHigh<-read.table("Disease_OSU6162_10microM_DarkApoHigh.txt")
Disease_PCAP1_10microM_DarkApoHigh<-read.table("Disease_PCAP1_10microM_DarkApoHigh.txt")
Disease_PCAP2_10microM_DarkApoHigh<-read.table("Disease_PCAP2_10microM_DarkApoHigh.txt")
Disease_PCAP814_10microM_DarkApoHigh<-read.table("Disease_PCAP814_10microM_DarkApoHigh.txt")
Disease_PCAP931_10microM_DarkApoHigh<-read.table("Disease_PCAP931_10microM_DarkApoHigh.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkApoHigh<-Disease_Control_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Aripiprazole_10microM_DarkApoHigh<-Disease_Aripiprazole_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Cariprazine_10microM_DarkApoHigh<-Disease_Cariprazine_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Clozapine_10microM_DarkApoHigh<-Disease_Clozapine_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_CNO_10microM_DarkApoHigh<-Disease_CNO_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Haloperidol_10microM_DarkApoHigh<-Disease_Haloperidol_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_NDMC_10microM_DarkApoHigh<-Disease_NDMC_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_NDMCHigh_100microM_DarkApoHigh<-Disease_NDMCHigh_100microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_OSU6162_10microM_DarkApoHigh<-Disease_OSU6162_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP1_10microM_DarkApoHigh<-Disease_PCAP1_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP2_10microM_DarkApoHigh<-Disease_PCAP2_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP814_10microM_DarkApoHigh<-Disease_PCAP814_10microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP931_10microM_DarkApoHigh<-Disease_PCAP931_10microM_DarkApoHigh[,c(1,3,2,4:11)]



Disease_Control_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkApoHigh_averaged<-rbind(Disease_Control_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkApoHigh_averaged<-aggregate(Disease_Control_DarkApoHigh_averaged,by=list(Disease_Control_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_10microM_DarkApoHigh_averaged<-rbind(Disease_Aripiprazole_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_10microM_DarkApoHigh[Disease_Aripiprazole_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_10microM_DarkApoHigh_averaged<-aggregate(Disease_Aripiprazole_10microM_DarkApoHigh_averaged,by=list(Disease_Aripiprazole_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_10microM_DarkApoHigh_averaged<-rbind(Disease_Cariprazine_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_10microM_DarkApoHigh[Disease_Cariprazine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_10microM_DarkApoHigh_averaged<-aggregate(Disease_Cariprazine_10microM_DarkApoHigh_averaged,by=list(Disease_Cariprazine_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_10microM_DarkApoHigh_averaged<-rbind(Disease_Clozapine_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_10microM_DarkApoHigh[Disease_Clozapine_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_10microM_DarkApoHigh_averaged<-aggregate(Disease_Clozapine_10microM_DarkApoHigh_averaged,by=list(Disease_Clozapine_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_10microM_DarkApoHigh_averaged<-rbind(Disease_CNO_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_CNO_10microM_DarkApoHigh[Disease_CNO_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_CNO_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_10microM_DarkApoHigh_averaged<-aggregate(Disease_CNO_10microM_DarkApoHigh_averaged,by=list(Disease_CNO_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_10microM_DarkApoHigh_averaged<-rbind(Disease_Haloperidol_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_10microM_DarkApoHigh[Disease_Haloperidol_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_10microM_DarkApoHigh_averaged<-aggregate(Disease_Haloperidol_10microM_DarkApoHigh_averaged,by=list(Disease_Haloperidol_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_10microM_DarkApoHigh_averaged<-rbind(Disease_NDMC_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_10microM_DarkApoHigh[Disease_NDMC_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_10microM_DarkApoHigh_averaged<-aggregate(Disease_NDMC_10microM_DarkApoHigh_averaged,by=list(Disease_NDMC_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_100microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_100microM_DarkApoHigh_averaged<-rbind(Disease_NDMCHigh_100microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_100microM_DarkApoHigh[Disease_NDMCHigh_100microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_100microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_100microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_100microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_100microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_100microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_100microM_DarkApoHigh_averaged<-aggregate(Disease_NDMCHigh_100microM_DarkApoHigh_averaged,by=list(Disease_NDMCHigh_100microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_10microM_DarkApoHigh_averaged<-rbind(Disease_OSU6162_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_10microM_DarkApoHigh[Disease_OSU6162_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_10microM_DarkApoHigh_averaged<-aggregate(Disease_OSU6162_10microM_DarkApoHigh_averaged,by=list(Disease_OSU6162_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_10microM_DarkApoHigh_averaged<-rbind(Disease_PCAP1_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_10microM_DarkApoHigh[Disease_PCAP1_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_10microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP1_10microM_DarkApoHigh_averaged,by=list(Disease_PCAP1_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_10microM_DarkApoHigh_averaged<-rbind(Disease_PCAP2_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_10microM_DarkApoHigh[Disease_PCAP2_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_10microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP2_10microM_DarkApoHigh_averaged,by=list(Disease_PCAP2_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_10microM_DarkApoHigh_averaged<-rbind(Disease_PCAP814_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_10microM_DarkApoHigh[Disease_PCAP814_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_10microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP814_10microM_DarkApoHigh_averaged,by=list(Disease_PCAP814_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_10microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_10microM_DarkApoHigh_averaged<-rbind(Disease_PCAP931_10microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_10microM_DarkApoHigh[Disease_PCAP931_10microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_10microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_10microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_10microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_10microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_10microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP931_10microM_DarkApoHigh_averaged,by=list(Disease_PCAP931_10microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkApoHigh_averaged_flat<-c(Disease_Control_DarkApoHigh_averaged_flat,Disease_Control_DarkApoHigh_averaged[variable,-1])
}

Disease_Aripiprazole_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_10microM_DarkApoHigh_averaged_flat<-c(Disease_Aripiprazole_10microM_DarkApoHigh_averaged_flat,Disease_Aripiprazole_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_Cariprazine_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_10microM_DarkApoHigh_averaged_flat<-c(Disease_Cariprazine_10microM_DarkApoHigh_averaged_flat,Disease_Cariprazine_10microM_DarkApoHigh_averaged[variable,-1])
}


Disease_Clozapine_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_10microM_DarkApoHigh_averaged_flat<-c(Disease_Clozapine_10microM_DarkApoHigh_averaged_flat,Disease_Clozapine_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_CNO_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_10microM_DarkApoHigh_averaged_flat<-c(Disease_CNO_10microM_DarkApoHigh_averaged_flat,Disease_CNO_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_Haloperidol_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_10microM_DarkApoHigh_averaged_flat<-c(Disease_Haloperidol_10microM_DarkApoHigh_averaged_flat,Disease_Haloperidol_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_NDMC_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_10microM_DarkApoHigh_averaged_flat<-c(Disease_NDMC_10microM_DarkApoHigh_averaged_flat,Disease_NDMC_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_NDMCHigh_100microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_100microM_DarkApoHigh_averaged_flat<-c(Disease_NDMCHigh_100microM_DarkApoHigh_averaged_flat,Disease_NDMCHigh_100microM_DarkApoHigh_averaged[variable,-1])
}

Disease_OSU6162_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_10microM_DarkApoHigh_averaged_flat<-c(Disease_OSU6162_10microM_DarkApoHigh_averaged_flat,Disease_OSU6162_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_PCAP1_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_10microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP1_10microM_DarkApoHigh_averaged_flat,Disease_PCAP1_10microM_DarkApoHigh_averaged[variable,-1])
}

Disease_PCAP2_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_10microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP2_10microM_DarkApoHigh_averaged_flat,Disease_PCAP2_10microM_DarkApoHigh_averaged[variable,-1])
}


Disease_PCAP814_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_10microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP814_10microM_DarkApoHigh_averaged_flat,Disease_PCAP814_10microM_DarkApoHigh_averaged[variable,-1])
}


Disease_PCAP931_10microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_10microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP931_10microM_DarkApoHigh_averaged_flat,Disease_PCAP931_10microM_DarkApoHigh_averaged[variable,-1])
}


Disease_10microM_DarkApoHigh_all<-rbind(Disease_Control_DarkApoHigh_averaged_flat,Disease_Aripiprazole_10microM_DarkApoHigh_averaged_flat, 
		Disease_Cariprazine_10microM_DarkApoHigh_averaged_flat, Disease_Clozapine_10microM_DarkApoHigh_averaged_flat, Disease_CNO_10microM_DarkApoHigh_averaged_flat,
		Disease_Haloperidol_10microM_DarkApoHigh_averaged_flat, Disease_NDMC_10microM_DarkApoHigh_averaged_flat, Disease_NDMCHigh_100microM_DarkApoHigh_averaged_flat,
		Disease_OSU6162_10microM_DarkApoHigh_averaged_flat, Disease_PCAP1_10microM_DarkApoHigh_averaged_flat, Disease_PCAP2_10microM_DarkApoHigh_averaged_flat,
		Disease_PCAP814_10microM_DarkApoHigh_averaged_flat, Disease_PCAP931_10microM_DarkApoHigh_averaged_flat)

#as numeric
Disease_10microM_DarkApoHigh_all<-apply(Disease_10microM_DarkApoHigh_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_10microM_DarkApoHigh_all<-apply(Disease_10microM_DarkApoHigh_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_10microM_DarkApoHigh_all)<-c("Control","Aripiprazole_10microM","Cariprazine_10microM","Clozapine_10microM",
		"CNO_10microM","Haloperidol_10microM","NDMC_10microM","NDMCHigh_100microM",
		"OSU6162_10microM","PCAP1_10microM","PCAP2_10microM","PCAP814_10microM",
		"PCAP931_10microM")


#euclidean distance
d <- dist(Disease_10microM_DarkApoHigh_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkApoHigh.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_10microM_DarkApoHigh_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkApoHigh.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_10microM_DarkApoHigh_all)[drug],"DarkApoHigh.png"),width=1500,height=750)
	plot(Disease_10microM_DarkApoHigh_all[c("Control"),122:143],Disease_10microM_DarkApoHigh_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_10microM_DarkApoHigh_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_10microM_DarkApoHigh_all[c("Control"),100:121],Disease_10microM_DarkApoHigh_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_10microM_DarkApoHigh_all[c("Control"),78:99],Disease_10microM_DarkApoHigh_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_10microM_DarkApoHigh_all[c("Control"),56:77],Disease_10microM_DarkApoHigh_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_10microM_DarkApoHigh_all[c("Control"),34:55],Disease_10microM_DarkApoHigh_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_10microM_DarkApoHigh_all[c("Control"),12:33],Disease_10microM_DarkApoHigh_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_10microM_DarkApoHigh_all[c("Control"),1:11],Disease_10microM_DarkApoHigh_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}





#DarkPTZ


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_10microM_DarkPTZ<-read.table("Disease_Aripiprazole_10microM_DarkPTZ.txt")
Disease_Cariprazine_10microM_DarkPTZ<-read.table("Disease_Cariprazine_10microM_DarkPTZ.txt")
Disease_Clozapine_10microM_DarkPTZ<-read.table("Disease_Clozapine_10microM_DarkPTZ.txt")
Disease_CNO_10microM_DarkPTZ<-read.table("Disease_CNO_10microM_DarkPTZ.txt")
Disease_Control_DarkPTZ<-read.table("Disease_Control_DarkPTZ.txt")
Disease_Haloperidol_10microM_DarkPTZ<-read.table("Disease_Haloperidol_10microM_DarkPTZ.txt")
Disease_NDMC_10microM_DarkPTZ<-read.table("Disease_NDMC_10microM_DarkPTZ.txt")
Disease_NDMCHigh_100microM_DarkPTZ<-read.table("Disease_NDMCHigh_100microM_DarkPTZ.txt")
Disease_OSU6162_10microM_DarkPTZ<-read.table("Disease_OSU6162_10microM_DarkPTZ.txt")
Disease_PCAP1_10microM_DarkPTZ<-read.table("Disease_PCAP1_10microM_DarkPTZ.txt")
Disease_PCAP2_10microM_DarkPTZ<-read.table("Disease_PCAP2_10microM_DarkPTZ.txt")
Disease_PCAP814_10microM_DarkPTZ<-read.table("Disease_PCAP814_10microM_DarkPTZ.txt")
Disease_PCAP931_10microM_DarkPTZ<-read.table("Disease_PCAP931_10microM_DarkPTZ.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkPTZ<-Disease_Control_DarkPTZ[,c(1,3,2,4:11)]
Disease_Aripiprazole_10microM_DarkPTZ<-Disease_Aripiprazole_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Cariprazine_10microM_DarkPTZ<-Disease_Cariprazine_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Clozapine_10microM_DarkPTZ<-Disease_Clozapine_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_CNO_10microM_DarkPTZ<-Disease_CNO_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Haloperidol_10microM_DarkPTZ<-Disease_Haloperidol_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_NDMC_10microM_DarkPTZ<-Disease_NDMC_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_NDMCHigh_100microM_DarkPTZ<-Disease_NDMCHigh_100microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_OSU6162_10microM_DarkPTZ<-Disease_OSU6162_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP1_10microM_DarkPTZ<-Disease_PCAP1_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP2_10microM_DarkPTZ<-Disease_PCAP2_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP814_10microM_DarkPTZ<-Disease_PCAP814_10microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP931_10microM_DarkPTZ<-Disease_PCAP931_10microM_DarkPTZ[,c(1,3,2,4:11)]



Disease_Control_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkPTZ_averaged<-rbind(Disease_Control_DarkPTZ_averaged,
			cbind(aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,],
			by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
			aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1],
			by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
			aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,3],
			by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")
		
Disease_Control_DarkPTZ_averaged<-aggregate(Disease_Control_DarkPTZ_averaged,by=list(Disease_Control_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_10microM_DarkPTZ_averaged<-rbind(Disease_Aripiprazole_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_10microM_DarkPTZ[Disease_Aripiprazole_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_10microM_DarkPTZ_averaged<-aggregate(Disease_Aripiprazole_10microM_DarkPTZ_averaged,by=list(Disease_Aripiprazole_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_10microM_DarkPTZ_averaged<-rbind(Disease_Cariprazine_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_10microM_DarkPTZ[Disease_Cariprazine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_10microM_DarkPTZ_averaged<-aggregate(Disease_Cariprazine_10microM_DarkPTZ_averaged,by=list(Disease_Cariprazine_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_10microM_DarkPTZ_averaged<-rbind(Disease_Clozapine_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_10microM_DarkPTZ[Disease_Clozapine_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_10microM_DarkPTZ_averaged<-aggregate(Disease_Clozapine_10microM_DarkPTZ_averaged,by=list(Disease_Clozapine_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_10microM_DarkPTZ_averaged<-rbind(Disease_CNO_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_CNO_10microM_DarkPTZ[Disease_CNO_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_CNO_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_10microM_DarkPTZ_averaged<-aggregate(Disease_CNO_10microM_DarkPTZ_averaged,by=list(Disease_CNO_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_10microM_DarkPTZ_averaged<-rbind(Disease_Haloperidol_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_10microM_DarkPTZ[Disease_Haloperidol_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_10microM_DarkPTZ_averaged<-aggregate(Disease_Haloperidol_10microM_DarkPTZ_averaged,by=list(Disease_Haloperidol_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_10microM_DarkPTZ_averaged<-rbind(Disease_NDMC_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_10microM_DarkPTZ[Disease_NDMC_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_10microM_DarkPTZ_averaged<-aggregate(Disease_NDMC_10microM_DarkPTZ_averaged,by=list(Disease_NDMC_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_100microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_100microM_DarkPTZ_averaged<-rbind(Disease_NDMCHigh_100microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_100microM_DarkPTZ[Disease_NDMCHigh_100microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_100microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_100microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_100microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_100microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_100microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_100microM_DarkPTZ_averaged<-aggregate(Disease_NDMCHigh_100microM_DarkPTZ_averaged,by=list(Disease_NDMCHigh_100microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_10microM_DarkPTZ_averaged<-rbind(Disease_OSU6162_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_10microM_DarkPTZ[Disease_OSU6162_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_10microM_DarkPTZ_averaged<-aggregate(Disease_OSU6162_10microM_DarkPTZ_averaged,by=list(Disease_OSU6162_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_10microM_DarkPTZ_averaged<-rbind(Disease_PCAP1_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_10microM_DarkPTZ[Disease_PCAP1_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_10microM_DarkPTZ_averaged<-aggregate(Disease_PCAP1_10microM_DarkPTZ_averaged,by=list(Disease_PCAP1_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_10microM_DarkPTZ_averaged<-rbind(Disease_PCAP2_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_10microM_DarkPTZ[Disease_PCAP2_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_10microM_DarkPTZ_averaged<-aggregate(Disease_PCAP2_10microM_DarkPTZ_averaged,by=list(Disease_PCAP2_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_10microM_DarkPTZ_averaged<-rbind(Disease_PCAP814_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_10microM_DarkPTZ[Disease_PCAP814_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_10microM_DarkPTZ_averaged<-aggregate(Disease_PCAP814_10microM_DarkPTZ_averaged,by=list(Disease_PCAP814_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_10microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_10microM_DarkPTZ_averaged<-rbind(Disease_PCAP931_10microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_10microM_DarkPTZ[Disease_PCAP931_10microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_10microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_10microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_10microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_10microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_10microM_DarkPTZ_averaged<-aggregate(Disease_PCAP931_10microM_DarkPTZ_averaged,by=list(Disease_PCAP931_10microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkPTZ_averaged_flat<-c(Disease_Control_DarkPTZ_averaged_flat,Disease_Control_DarkPTZ_averaged[variable,-1])
}

Disease_Aripiprazole_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_10microM_DarkPTZ_averaged_flat<-c(Disease_Aripiprazole_10microM_DarkPTZ_averaged_flat,Disease_Aripiprazole_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_Cariprazine_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_10microM_DarkPTZ_averaged_flat<-c(Disease_Cariprazine_10microM_DarkPTZ_averaged_flat,Disease_Cariprazine_10microM_DarkPTZ_averaged[variable,-1])
}


Disease_Clozapine_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_10microM_DarkPTZ_averaged_flat<-c(Disease_Clozapine_10microM_DarkPTZ_averaged_flat,Disease_Clozapine_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_CNO_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_10microM_DarkPTZ_averaged_flat<-c(Disease_CNO_10microM_DarkPTZ_averaged_flat,Disease_CNO_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_Haloperidol_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_10microM_DarkPTZ_averaged_flat<-c(Disease_Haloperidol_10microM_DarkPTZ_averaged_flat,Disease_Haloperidol_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_NDMC_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_10microM_DarkPTZ_averaged_flat<-c(Disease_NDMC_10microM_DarkPTZ_averaged_flat,Disease_NDMC_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_NDMCHigh_100microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_100microM_DarkPTZ_averaged_flat<-c(Disease_NDMCHigh_100microM_DarkPTZ_averaged_flat,Disease_NDMCHigh_100microM_DarkPTZ_averaged[variable,-1])
}

Disease_OSU6162_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_10microM_DarkPTZ_averaged_flat<-c(Disease_OSU6162_10microM_DarkPTZ_averaged_flat,Disease_OSU6162_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_PCAP1_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_10microM_DarkPTZ_averaged_flat<-c(Disease_PCAP1_10microM_DarkPTZ_averaged_flat,Disease_PCAP1_10microM_DarkPTZ_averaged[variable,-1])
}

Disease_PCAP2_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_10microM_DarkPTZ_averaged_flat<-c(Disease_PCAP2_10microM_DarkPTZ_averaged_flat,Disease_PCAP2_10microM_DarkPTZ_averaged[variable,-1])
}


Disease_PCAP814_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_10microM_DarkPTZ_averaged_flat<-c(Disease_PCAP814_10microM_DarkPTZ_averaged_flat,Disease_PCAP814_10microM_DarkPTZ_averaged[variable,-1])
}


Disease_PCAP931_10microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_10microM_DarkPTZ_averaged_flat<-c(Disease_PCAP931_10microM_DarkPTZ_averaged_flat,Disease_PCAP931_10microM_DarkPTZ_averaged[variable,-1])
}


Disease_10microM_DarkPTZ_all<-rbind(Disease_Control_DarkPTZ_averaged_flat,Disease_Aripiprazole_10microM_DarkPTZ_averaged_flat, 
		Disease_Cariprazine_10microM_DarkPTZ_averaged_flat, Disease_Clozapine_10microM_DarkPTZ_averaged_flat, Disease_CNO_10microM_DarkPTZ_averaged_flat,
		Disease_Haloperidol_10microM_DarkPTZ_averaged_flat, Disease_NDMC_10microM_DarkPTZ_averaged_flat, Disease_NDMCHigh_100microM_DarkPTZ_averaged_flat,
		Disease_OSU6162_10microM_DarkPTZ_averaged_flat, Disease_PCAP1_10microM_DarkPTZ_averaged_flat, Disease_PCAP2_10microM_DarkPTZ_averaged_flat,
		Disease_PCAP814_10microM_DarkPTZ_averaged_flat, Disease_PCAP931_10microM_DarkPTZ_averaged_flat)

#as numeric
Disease_10microM_DarkPTZ_all<-apply(Disease_10microM_DarkPTZ_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_10microM_DarkPTZ_all<-apply(Disease_10microM_DarkPTZ_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_10microM_DarkPTZ_all)<-c("Control","Aripiprazole_10microM","Cariprazine_10microM","Clozapine_10microM",
		"CNO_10microM","Haloperidol_10microM","NDMC_10microM","NDMCHigh_100microM",
		"OSU6162_10microM","PCAP1_10microM","PCAP2_10microM","PCAP814_10microM",
		"PCAP931_10microM")


#euclidean distance
d <- dist(Disease_10microM_DarkPTZ_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkPTZ.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_10microM_DarkPTZ_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkPTZ.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_10microM_DarkPTZ_all)[drug],"DarkPTZ.png"),width=1500,height=750)
	plot(Disease_10microM_DarkPTZ_all[c("Control"),122:143],Disease_10microM_DarkPTZ_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_10microM_DarkPTZ_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_10microM_DarkPTZ_all[c("Control"),100:121],Disease_10microM_DarkPTZ_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_10microM_DarkPTZ_all[c("Control"),78:99],Disease_10microM_DarkPTZ_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_10microM_DarkPTZ_all[c("Control"),56:77],Disease_10microM_DarkPTZ_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_10microM_DarkPTZ_all[c("Control"),34:55],Disease_10microM_DarkPTZ_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_10microM_DarkPTZ_all[c("Control"),12:33],Disease_10microM_DarkPTZ_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_10microM_DarkPTZ_all[c("Control"),1:11],Disease_10microM_DarkPTZ_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}
############################################### 10 microM ######################################################



############################################### 3 microM ######################################################



#Light

#Light


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_3microM_Light<-read.table("Natural_Aripiprazole_3microM_Light.txt")
Natural_Cariprazine_3microM_Light<-read.table("Natural_Cariprazine_3microM_Light.txt")
Natural_Clozapine_3microM_Light<-read.table("Natural_Clozapine_3microM_Light.txt")
Natural_CNO_3microM_Light<-read.table("Natural_CNO_3microM_Light.txt")
Natural_Control_Light<-read.table("Natural_Control_Light.txt")
Natural_Haloperidol_3microM_Light<-read.table("Natural_Haloperidol_3microM_Light.txt")
Natural_NDMC_3microM_Light<-read.table("Natural_NDMC_3microM_Light.txt")
Natural_NDMCHigh_50microM_Light<-read.table("Natural_NDMCHigh_50microM_Light.txt")
Natural_OSU6162_3microM_Light<-read.table("Natural_OSU6162_3microM_Light.txt")
Natural_PCAP1_3microM_Light<-read.table("Natural_PCAP1_3microM_Light.txt")
Natural_PCAP2_3microM_Light<-read.table("Natural_PCAP2_3microM_Light.txt")
Natural_PCAP814_3microM_Light<-read.table("Natural_PCAP814_3microM_Light.txt")
Natural_PCAP931_3microM_Light<-read.table("Natural_PCAP931_3microM_Light.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_Light<-Natural_Control_Light[,c(1,3,2,4:11)]
Natural_Aripiprazole_3microM_Light<-Natural_Aripiprazole_3microM_Light[,c(1,3,2,4:11)]
Natural_Cariprazine_3microM_Light<-Natural_Cariprazine_3microM_Light[,c(1,3,2,4:11)]
Natural_Clozapine_3microM_Light<-Natural_Clozapine_3microM_Light[,c(1,3,2,4:11)]
Natural_CNO_3microM_Light<-Natural_CNO_3microM_Light[,c(1,3,2,4:11)]
Natural_Haloperidol_3microM_Light<-Natural_Haloperidol_3microM_Light[,c(1,3,2,4:11)]
Natural_NDMC_3microM_Light<-Natural_NDMC_3microM_Light[,c(1,3,2,4:11)]
Natural_NDMCHigh_50microM_Light<-Natural_NDMCHigh_50microM_Light[,c(1,3,2,4:11)]
Natural_OSU6162_3microM_Light<-Natural_OSU6162_3microM_Light[,c(1,3,2,4:11)]
Natural_PCAP1_3microM_Light<-Natural_PCAP1_3microM_Light[,c(1,3,2,4:11)]
Natural_PCAP2_3microM_Light<-Natural_PCAP2_3microM_Light[,c(1,3,2,4:11)]
Natural_PCAP814_3microM_Light<-Natural_PCAP814_3microM_Light[,c(1,3,2,4:11)]
Natural_PCAP931_3microM_Light<-Natural_PCAP931_3microM_Light[,c(1,3,2,4:11)]



Natural_Control_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_Light_averaged<-rbind(Natural_Control_Light_averaged,
			cbind(aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,3],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,],
#		by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_Light_averaged)[2:10],"_SD"))


colnames(Natural_Control_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_Light_averaged<-aggregate(Natural_Control_Light_averaged,by=list(Natural_Control_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_3microM_Light_averaged<-rbind(Natural_Aripiprazole_3microM_Light_averaged,
			cbind(aggregate(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_3microM_Light[Natural_Aripiprazole_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_3microM_Light_averaged<-aggregate(Natural_Aripiprazole_3microM_Light_averaged,by=list(Natural_Aripiprazole_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_3microM_Light_averaged<-rbind(Natural_Cariprazine_3microM_Light_averaged,
			cbind(aggregate(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_3microM_Light[Natural_Cariprazine_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_3microM_Light_averaged<-aggregate(Natural_Cariprazine_3microM_Light_averaged,by=list(Natural_Cariprazine_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_3microM_Light_averaged<-rbind(Natural_Clozapine_3microM_Light_averaged,
			cbind(aggregate(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_3microM_Light[Natural_Clozapine_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_3microM_Light_averaged<-aggregate(Natural_Clozapine_3microM_Light_averaged,by=list(Natural_Clozapine_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_3microM_Light_averaged<-rbind(Natural_CNO_3microM_Light_averaged,
			cbind(aggregate(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_CNO_3microM_Light[Natural_CNO_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_CNO_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_3microM_Light_averaged<-aggregate(Natural_CNO_3microM_Light_averaged,by=list(Natural_CNO_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_3microM_Light_averaged<-rbind(Natural_Haloperidol_3microM_Light_averaged,
			cbind(aggregate(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_3microM_Light[Natural_Haloperidol_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_3microM_Light_averaged<-aggregate(Natural_Haloperidol_3microM_Light_averaged,by=list(Natural_Haloperidol_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_3microM_Light_averaged<-rbind(Natural_NDMC_3microM_Light_averaged,
			cbind(aggregate(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_3microM_Light[Natural_NDMC_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_3microM_Light_averaged<-aggregate(Natural_NDMC_3microM_Light_averaged,by=list(Natural_NDMC_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_50microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_50microM_Light_averaged<-rbind(Natural_NDMCHigh_50microM_Light_averaged,
			cbind(aggregate(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_50microM_Light[Natural_NDMCHigh_50microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_50microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_50microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_50microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_50microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_50microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_50microM_Light_averaged<-aggregate(Natural_NDMCHigh_50microM_Light_averaged,by=list(Natural_NDMCHigh_50microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_3microM_Light_averaged<-rbind(Natural_OSU6162_3microM_Light_averaged,
			cbind(aggregate(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_3microM_Light[Natural_OSU6162_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_3microM_Light_averaged<-aggregate(Natural_OSU6162_3microM_Light_averaged,by=list(Natural_OSU6162_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_3microM_Light_averaged<-rbind(Natural_PCAP1_3microM_Light_averaged,
			cbind(aggregate(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_3microM_Light[Natural_PCAP1_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_3microM_Light_averaged<-aggregate(Natural_PCAP1_3microM_Light_averaged,by=list(Natural_PCAP1_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_3microM_Light_averaged<-rbind(Natural_PCAP2_3microM_Light_averaged,
			cbind(aggregate(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_3microM_Light[Natural_PCAP2_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_3microM_Light_averaged<-aggregate(Natural_PCAP2_3microM_Light_averaged,by=list(Natural_PCAP2_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_3microM_Light_averaged<-rbind(Natural_PCAP814_3microM_Light_averaged,
			cbind(aggregate(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_3microM_Light[Natural_PCAP814_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_3microM_Light_averaged<-aggregate(Natural_PCAP814_3microM_Light_averaged,by=list(Natural_PCAP814_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_3microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_3microM_Light_averaged<-rbind(Natural_PCAP931_3microM_Light_averaged,
			cbind(aggregate(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_3microM_Light[Natural_PCAP931_3microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_3microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_3microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_3microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_3microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_3microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_3microM_Light_averaged<-aggregate(Natural_PCAP931_3microM_Light_averaged,by=list(Natural_PCAP931_3microM_Light_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_Light_averaged_flat<-c(Natural_Control_Light_averaged_flat,Natural_Control_Light_averaged[variable,-1])
}

Natural_Aripiprazole_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_3microM_Light_averaged_flat<-c(Natural_Aripiprazole_3microM_Light_averaged_flat,Natural_Aripiprazole_3microM_Light_averaged[variable,-1])
}

Natural_Cariprazine_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_3microM_Light_averaged_flat<-c(Natural_Cariprazine_3microM_Light_averaged_flat,Natural_Cariprazine_3microM_Light_averaged[variable,-1])
}


Natural_Clozapine_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_3microM_Light_averaged_flat<-c(Natural_Clozapine_3microM_Light_averaged_flat,Natural_Clozapine_3microM_Light_averaged[variable,-1])
}

Natural_CNO_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_3microM_Light_averaged_flat<-c(Natural_CNO_3microM_Light_averaged_flat,Natural_CNO_3microM_Light_averaged[variable,-1])
}

Natural_Haloperidol_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_3microM_Light_averaged_flat<-c(Natural_Haloperidol_3microM_Light_averaged_flat,Natural_Haloperidol_3microM_Light_averaged[variable,-1])
}

Natural_NDMC_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_3microM_Light_averaged_flat<-c(Natural_NDMC_3microM_Light_averaged_flat,Natural_NDMC_3microM_Light_averaged[variable,-1])
}

Natural_NDMCHigh_50microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_50microM_Light_averaged_flat<-c(Natural_NDMCHigh_50microM_Light_averaged_flat,Natural_NDMCHigh_50microM_Light_averaged[variable,-1])
}

Natural_OSU6162_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_3microM_Light_averaged_flat<-c(Natural_OSU6162_3microM_Light_averaged_flat,Natural_OSU6162_3microM_Light_averaged[variable,-1])
}

Natural_PCAP1_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_3microM_Light_averaged_flat<-c(Natural_PCAP1_3microM_Light_averaged_flat,Natural_PCAP1_3microM_Light_averaged[variable,-1])
}

Natural_PCAP2_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_3microM_Light_averaged_flat<-c(Natural_PCAP2_3microM_Light_averaged_flat,Natural_PCAP2_3microM_Light_averaged[variable,-1])
}


Natural_PCAP814_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_3microM_Light_averaged_flat<-c(Natural_PCAP814_3microM_Light_averaged_flat,Natural_PCAP814_3microM_Light_averaged[variable,-1])
}


Natural_PCAP931_3microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_3microM_Light_averaged_flat<-c(Natural_PCAP931_3microM_Light_averaged_flat,Natural_PCAP931_3microM_Light_averaged[variable,-1])
}


Natural_3microM_Light_all<-rbind(Natural_Control_Light_averaged_flat,Natural_Aripiprazole_3microM_Light_averaged_flat, 
		Natural_Cariprazine_3microM_Light_averaged_flat, Natural_Clozapine_3microM_Light_averaged_flat, Natural_CNO_3microM_Light_averaged_flat,
		Natural_Haloperidol_3microM_Light_averaged_flat, Natural_NDMC_3microM_Light_averaged_flat, Natural_NDMCHigh_50microM_Light_averaged_flat,
		Natural_OSU6162_3microM_Light_averaged_flat, Natural_PCAP1_3microM_Light_averaged_flat, Natural_PCAP2_3microM_Light_averaged_flat,
		Natural_PCAP814_3microM_Light_averaged_flat, Natural_PCAP931_3microM_Light_averaged_flat)

#as numeric
Natural_3microM_Light_all<-apply(Natural_3microM_Light_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_3microM_Light_all<-apply(Natural_3microM_Light_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_3microM_Light_all)<-c("Control","Aripiprazole_3microM","Cariprazine_3microM","Clozapine_3microM",
		"CNO_3microM","Haloperidol_3microM","NDMC_3microM","NDMCHigh_50microM",
		"OSU6162_3microM","PCAP1_3microM","PCAP2_3microM","PCAP814_3microM",
		"PCAP931_3microM")


#euclidean distance
d <- dist(Natural_3microM_Light_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_Light.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_3microM_Light_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_Light.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_3microM_Light_all)[drug],"Light.png"),width=1500,height=750)
	plot(Natural_3microM_Light_all[c("Control"),122:143],Natural_3microM_Light_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_3microM_Light_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_3microM_Light_all[c("Control"),100:121],Natural_3microM_Light_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_3microM_Light_all[c("Control"),78:99],Natural_3microM_Light_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_3microM_Light_all[c("Control"),56:77],Natural_3microM_Light_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_3microM_Light_all[c("Control"),34:55],Natural_3microM_Light_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_3microM_Light_all[c("Control"),12:33],Natural_3microM_Light_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_3microM_Light_all[c("Control"),1:11],Natural_3microM_Light_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}



#LightDark


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_3microM_LightDark<-read.table("Natural_Aripiprazole_3microM_LightDark.txt")
Natural_Cariprazine_3microM_LightDark<-read.table("Natural_Cariprazine_3microM_LightDark.txt")
Natural_Clozapine_3microM_LightDark<-read.table("Natural_Clozapine_3microM_LightDark.txt")
Natural_CNO_3microM_LightDark<-read.table("Natural_CNO_3microM_LightDark.txt")
Natural_Control_LightDark<-read.table("Natural_Control_LightDark.txt")
Natural_Haloperidol_3microM_LightDark<-read.table("Natural_Haloperidol_3microM_LightDark.txt")
Natural_NDMC_3microM_LightDark<-read.table("Natural_NDMC_3microM_LightDark.txt")
Natural_NDMCHigh_50microM_LightDark<-read.table("Natural_NDMCHigh_50microM_LightDark.txt")
Natural_OSU6162_3microM_LightDark<-read.table("Natural_OSU6162_3microM_LightDark.txt")
Natural_PCAP1_3microM_LightDark<-read.table("Natural_PCAP1_3microM_LightDark.txt")
Natural_PCAP2_3microM_LightDark<-read.table("Natural_PCAP2_3microM_LightDark.txt")
Natural_PCAP814_3microM_LightDark<-read.table("Natural_PCAP814_3microM_LightDark.txt")
Natural_PCAP931_3microM_LightDark<-read.table("Natural_PCAP931_3microM_LightDark.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_LightDark<-Natural_Control_LightDark[,c(1,3,2,4:11)]
Natural_Aripiprazole_3microM_LightDark<-Natural_Aripiprazole_3microM_LightDark[,c(1,3,2,4:11)]
Natural_Cariprazine_3microM_LightDark<-Natural_Cariprazine_3microM_LightDark[,c(1,3,2,4:11)]
Natural_Clozapine_3microM_LightDark<-Natural_Clozapine_3microM_LightDark[,c(1,3,2,4:11)]
Natural_CNO_3microM_LightDark<-Natural_CNO_3microM_LightDark[,c(1,3,2,4:11)]
Natural_Haloperidol_3microM_LightDark<-Natural_Haloperidol_3microM_LightDark[,c(1,3,2,4:11)]
Natural_NDMC_3microM_LightDark<-Natural_NDMC_3microM_LightDark[,c(1,3,2,4:11)]
Natural_NDMCHigh_50microM_LightDark<-Natural_NDMCHigh_50microM_LightDark[,c(1,3,2,4:11)]
Natural_OSU6162_3microM_LightDark<-Natural_OSU6162_3microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP1_3microM_LightDark<-Natural_PCAP1_3microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP2_3microM_LightDark<-Natural_PCAP2_3microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP814_3microM_LightDark<-Natural_PCAP814_3microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP931_3microM_LightDark<-Natural_PCAP931_3microM_LightDark[,c(1,3,2,4:11)]



Natural_Control_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_LightDark_averaged<-rbind(Natural_Control_LightDark_averaged,
			cbind(aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Control_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_LightDark_averaged<-aggregate(Natural_Control_LightDark_averaged,by=list(Natural_Control_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_3microM_LightDark_averaged<-rbind(Natural_Aripiprazole_3microM_LightDark_averaged,
			cbind(aggregate(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_3microM_LightDark[Natural_Aripiprazole_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_3microM_LightDark_averaged<-aggregate(Natural_Aripiprazole_3microM_LightDark_averaged,by=list(Natural_Aripiprazole_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_3microM_LightDark_averaged<-rbind(Natural_Cariprazine_3microM_LightDark_averaged,
			cbind(aggregate(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_3microM_LightDark[Natural_Cariprazine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_3microM_LightDark_averaged<-aggregate(Natural_Cariprazine_3microM_LightDark_averaged,by=list(Natural_Cariprazine_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_3microM_LightDark_averaged<-rbind(Natural_Clozapine_3microM_LightDark_averaged,
			cbind(aggregate(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_3microM_LightDark[Natural_Clozapine_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_3microM_LightDark_averaged<-aggregate(Natural_Clozapine_3microM_LightDark_averaged,by=list(Natural_Clozapine_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_3microM_LightDark_averaged<-rbind(Natural_CNO_3microM_LightDark_averaged,
			cbind(aggregate(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_CNO_3microM_LightDark[Natural_CNO_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_CNO_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_3microM_LightDark_averaged<-aggregate(Natural_CNO_3microM_LightDark_averaged,by=list(Natural_CNO_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_3microM_LightDark_averaged<-rbind(Natural_Haloperidol_3microM_LightDark_averaged,
			cbind(aggregate(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_3microM_LightDark[Natural_Haloperidol_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_3microM_LightDark_averaged<-aggregate(Natural_Haloperidol_3microM_LightDark_averaged,by=list(Natural_Haloperidol_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_3microM_LightDark_averaged<-rbind(Natural_NDMC_3microM_LightDark_averaged,
			cbind(aggregate(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_3microM_LightDark[Natural_NDMC_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_3microM_LightDark_averaged<-aggregate(Natural_NDMC_3microM_LightDark_averaged,by=list(Natural_NDMC_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_50microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_50microM_LightDark_averaged<-rbind(Natural_NDMCHigh_50microM_LightDark_averaged,
			cbind(aggregate(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_50microM_LightDark[Natural_NDMCHigh_50microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_50microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_50microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_50microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_50microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_50microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_50microM_LightDark_averaged<-aggregate(Natural_NDMCHigh_50microM_LightDark_averaged,by=list(Natural_NDMCHigh_50microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_3microM_LightDark_averaged<-rbind(Natural_OSU6162_3microM_LightDark_averaged,
			cbind(aggregate(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_3microM_LightDark[Natural_OSU6162_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_3microM_LightDark_averaged<-aggregate(Natural_OSU6162_3microM_LightDark_averaged,by=list(Natural_OSU6162_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_3microM_LightDark_averaged<-rbind(Natural_PCAP1_3microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_3microM_LightDark[Natural_PCAP1_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_3microM_LightDark_averaged<-aggregate(Natural_PCAP1_3microM_LightDark_averaged,by=list(Natural_PCAP1_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_3microM_LightDark_averaged<-rbind(Natural_PCAP2_3microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_3microM_LightDark[Natural_PCAP2_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_3microM_LightDark_averaged<-aggregate(Natural_PCAP2_3microM_LightDark_averaged,by=list(Natural_PCAP2_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_3microM_LightDark_averaged<-rbind(Natural_PCAP814_3microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_3microM_LightDark[Natural_PCAP814_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_3microM_LightDark_averaged<-aggregate(Natural_PCAP814_3microM_LightDark_averaged,by=list(Natural_PCAP814_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_3microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_3microM_LightDark_averaged<-rbind(Natural_PCAP931_3microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_3microM_LightDark[Natural_PCAP931_3microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_3microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_3microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_3microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_3microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_3microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_3microM_LightDark_averaged<-aggregate(Natural_PCAP931_3microM_LightDark_averaged,by=list(Natural_PCAP931_3microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_LightDark_averaged_flat<-c(Natural_Control_LightDark_averaged_flat,Natural_Control_LightDark_averaged[variable,-1])
}

Natural_Aripiprazole_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_3microM_LightDark_averaged_flat<-c(Natural_Aripiprazole_3microM_LightDark_averaged_flat,Natural_Aripiprazole_3microM_LightDark_averaged[variable,-1])
}

Natural_Cariprazine_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_3microM_LightDark_averaged_flat<-c(Natural_Cariprazine_3microM_LightDark_averaged_flat,Natural_Cariprazine_3microM_LightDark_averaged[variable,-1])
}


Natural_Clozapine_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_3microM_LightDark_averaged_flat<-c(Natural_Clozapine_3microM_LightDark_averaged_flat,Natural_Clozapine_3microM_LightDark_averaged[variable,-1])
}

Natural_CNO_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_3microM_LightDark_averaged_flat<-c(Natural_CNO_3microM_LightDark_averaged_flat,Natural_CNO_3microM_LightDark_averaged[variable,-1])
}

Natural_Haloperidol_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_3microM_LightDark_averaged_flat<-c(Natural_Haloperidol_3microM_LightDark_averaged_flat,Natural_Haloperidol_3microM_LightDark_averaged[variable,-1])
}

Natural_NDMC_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_3microM_LightDark_averaged_flat<-c(Natural_NDMC_3microM_LightDark_averaged_flat,Natural_NDMC_3microM_LightDark_averaged[variable,-1])
}

Natural_NDMCHigh_50microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_50microM_LightDark_averaged_flat<-c(Natural_NDMCHigh_50microM_LightDark_averaged_flat,Natural_NDMCHigh_50microM_LightDark_averaged[variable,-1])
}

Natural_OSU6162_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_3microM_LightDark_averaged_flat<-c(Natural_OSU6162_3microM_LightDark_averaged_flat,Natural_OSU6162_3microM_LightDark_averaged[variable,-1])
}

Natural_PCAP1_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_3microM_LightDark_averaged_flat<-c(Natural_PCAP1_3microM_LightDark_averaged_flat,Natural_PCAP1_3microM_LightDark_averaged[variable,-1])
}

Natural_PCAP2_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_3microM_LightDark_averaged_flat<-c(Natural_PCAP2_3microM_LightDark_averaged_flat,Natural_PCAP2_3microM_LightDark_averaged[variable,-1])
}


Natural_PCAP814_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_3microM_LightDark_averaged_flat<-c(Natural_PCAP814_3microM_LightDark_averaged_flat,Natural_PCAP814_3microM_LightDark_averaged[variable,-1])
}


Natural_PCAP931_3microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_3microM_LightDark_averaged_flat<-c(Natural_PCAP931_3microM_LightDark_averaged_flat,Natural_PCAP931_3microM_LightDark_averaged[variable,-1])
}


Natural_3microM_LightDark_all<-rbind(Natural_Control_LightDark_averaged_flat,Natural_Aripiprazole_3microM_LightDark_averaged_flat, 
		Natural_Cariprazine_3microM_LightDark_averaged_flat, Natural_Clozapine_3microM_LightDark_averaged_flat, Natural_CNO_3microM_LightDark_averaged_flat,
		Natural_Haloperidol_3microM_LightDark_averaged_flat, Natural_NDMC_3microM_LightDark_averaged_flat, Natural_NDMCHigh_50microM_LightDark_averaged_flat,
		Natural_OSU6162_3microM_LightDark_averaged_flat, Natural_PCAP1_3microM_LightDark_averaged_flat, Natural_PCAP2_3microM_LightDark_averaged_flat,
		Natural_PCAP814_3microM_LightDark_averaged_flat, Natural_PCAP931_3microM_LightDark_averaged_flat)

#as numeric
Natural_3microM_LightDark_all<-apply(Natural_3microM_LightDark_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_3microM_LightDark_all<-apply(Natural_3microM_LightDark_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_3microM_LightDark_all)<-c("Control","Aripiprazole_3microM","Cariprazine_3microM","Clozapine_3microM",
		"CNO_3microM","Haloperidol_3microM","NDMC_3microM","NDMCHigh_50microM",
		"OSU6162_3microM","PCAP1_3microM","PCAP2_3microM","PCAP814_3microM",
		"PCAP931_3microM")


#euclidean distance
d <- dist(Natural_3microM_LightDark_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_LightDark.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_3microM_LightDark_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_LightDark.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_3microM_LightDark_all)[drug],"LightDark.png"),width=1500,height=750)
	plot(Natural_3microM_LightDark_all[c("Control"),122:143],Natural_3microM_LightDark_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_3microM_LightDark_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_3microM_LightDark_all[c("Control"),100:121],Natural_3microM_LightDark_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_3microM_LightDark_all[c("Control"),78:99],Natural_3microM_LightDark_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_3microM_LightDark_all[c("Control"),56:77],Natural_3microM_LightDark_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_3microM_LightDark_all[c("Control"),34:55],Natural_3microM_LightDark_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_3microM_LightDark_all[c("Control"),12:33],Natural_3microM_LightDark_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_3microM_LightDark_all[c("Control"),1:11],Natural_3microM_LightDark_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}





#Dark


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_3microM_Dark<-read.table("Natural_Aripiprazole_3microM_Dark.txt")
Natural_Cariprazine_3microM_Dark<-read.table("Natural_Cariprazine_3microM_Dark.txt")
Natural_Clozapine_3microM_Dark<-read.table("Natural_Clozapine_3microM_Dark.txt")
Natural_CNO_3microM_Dark<-read.table("Natural_CNO_3microM_Dark.txt")
Natural_Control_Dark<-read.table("Natural_Control_Dark.txt")
Natural_Haloperidol_3microM_Dark<-read.table("Natural_Haloperidol_3microM_Dark.txt")
Natural_NDMC_3microM_Dark<-read.table("Natural_NDMC_3microM_Dark.txt")
Natural_NDMCHigh_50microM_Dark<-read.table("Natural_NDMCHigh_50microM_Dark.txt")
Natural_OSU6162_3microM_Dark<-read.table("Natural_OSU6162_3microM_Dark.txt")
Natural_PCAP1_3microM_Dark<-read.table("Natural_PCAP1_3microM_Dark.txt")
Natural_PCAP2_3microM_Dark<-read.table("Natural_PCAP2_3microM_Dark.txt")
Natural_PCAP814_3microM_Dark<-read.table("Natural_PCAP814_3microM_Dark.txt")
Natural_PCAP931_3microM_Dark<-read.table("Natural_PCAP931_3microM_Dark.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_Dark<-Natural_Control_Dark[,c(1,3,2,4:11)]
Natural_Aripiprazole_3microM_Dark<-Natural_Aripiprazole_3microM_Dark[,c(1,3,2,4:11)]
Natural_Cariprazine_3microM_Dark<-Natural_Cariprazine_3microM_Dark[,c(1,3,2,4:11)]
Natural_Clozapine_3microM_Dark<-Natural_Clozapine_3microM_Dark[,c(1,3,2,4:11)]
Natural_CNO_3microM_Dark<-Natural_CNO_3microM_Dark[,c(1,3,2,4:11)]
Natural_Haloperidol_3microM_Dark<-Natural_Haloperidol_3microM_Dark[,c(1,3,2,4:11)]
Natural_NDMC_3microM_Dark<-Natural_NDMC_3microM_Dark[,c(1,3,2,4:11)]
Natural_NDMCHigh_50microM_Dark<-Natural_NDMCHigh_50microM_Dark[,c(1,3,2,4:11)]
Natural_OSU6162_3microM_Dark<-Natural_OSU6162_3microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP1_3microM_Dark<-Natural_PCAP1_3microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP2_3microM_Dark<-Natural_PCAP2_3microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP814_3microM_Dark<-Natural_PCAP814_3microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP931_3microM_Dark<-Natural_PCAP931_3microM_Dark[,c(1,3,2,4:11)]



Natural_Control_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_Dark_averaged<-rbind(Natural_Control_Dark_averaged,
			cbind(aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Control_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_Dark_averaged<-aggregate(Natural_Control_Dark_averaged,by=list(Natural_Control_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_3microM_Dark_averaged<-rbind(Natural_Aripiprazole_3microM_Dark_averaged,
			cbind(aggregate(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_3microM_Dark[Natural_Aripiprazole_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_3microM_Dark_averaged<-aggregate(Natural_Aripiprazole_3microM_Dark_averaged,by=list(Natural_Aripiprazole_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_3microM_Dark_averaged<-rbind(Natural_Cariprazine_3microM_Dark_averaged,
			cbind(aggregate(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_3microM_Dark[Natural_Cariprazine_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_3microM_Dark_averaged<-aggregate(Natural_Cariprazine_3microM_Dark_averaged,by=list(Natural_Cariprazine_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_3microM_Dark_averaged<-rbind(Natural_Clozapine_3microM_Dark_averaged,
			cbind(aggregate(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_3microM_Dark[Natural_Clozapine_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_3microM_Dark_averaged<-aggregate(Natural_Clozapine_3microM_Dark_averaged,by=list(Natural_Clozapine_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_3microM_Dark_averaged<-rbind(Natural_CNO_3microM_Dark_averaged,
			cbind(aggregate(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_CNO_3microM_Dark[Natural_CNO_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_CNO_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_3microM_Dark_averaged<-aggregate(Natural_CNO_3microM_Dark_averaged,by=list(Natural_CNO_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_3microM_Dark_averaged<-rbind(Natural_Haloperidol_3microM_Dark_averaged,
			cbind(aggregate(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_3microM_Dark[Natural_Haloperidol_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_3microM_Dark_averaged<-aggregate(Natural_Haloperidol_3microM_Dark_averaged,by=list(Natural_Haloperidol_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_3microM_Dark_averaged<-rbind(Natural_NDMC_3microM_Dark_averaged,
			cbind(aggregate(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_3microM_Dark[Natural_NDMC_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_3microM_Dark_averaged<-aggregate(Natural_NDMC_3microM_Dark_averaged,by=list(Natural_NDMC_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_50microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_50microM_Dark_averaged<-rbind(Natural_NDMCHigh_50microM_Dark_averaged,
			cbind(aggregate(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_50microM_Dark[Natural_NDMCHigh_50microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_50microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_50microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_50microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_50microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_50microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_50microM_Dark_averaged<-aggregate(Natural_NDMCHigh_50microM_Dark_averaged,by=list(Natural_NDMCHigh_50microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_3microM_Dark_averaged<-rbind(Natural_OSU6162_3microM_Dark_averaged,
			cbind(aggregate(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_3microM_Dark[Natural_OSU6162_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_3microM_Dark_averaged<-aggregate(Natural_OSU6162_3microM_Dark_averaged,by=list(Natural_OSU6162_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_3microM_Dark_averaged<-rbind(Natural_PCAP1_3microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_3microM_Dark[Natural_PCAP1_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_3microM_Dark_averaged<-aggregate(Natural_PCAP1_3microM_Dark_averaged,by=list(Natural_PCAP1_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_3microM_Dark_averaged<-rbind(Natural_PCAP2_3microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_3microM_Dark[Natural_PCAP2_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_3microM_Dark_averaged<-aggregate(Natural_PCAP2_3microM_Dark_averaged,by=list(Natural_PCAP2_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_3microM_Dark_averaged<-rbind(Natural_PCAP814_3microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_3microM_Dark[Natural_PCAP814_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_3microM_Dark_averaged<-aggregate(Natural_PCAP814_3microM_Dark_averaged,by=list(Natural_PCAP814_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_3microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_3microM_Dark_averaged<-rbind(Natural_PCAP931_3microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_3microM_Dark[Natural_PCAP931_3microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_3microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_3microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_3microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_3microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_3microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_3microM_Dark_averaged<-aggregate(Natural_PCAP931_3microM_Dark_averaged,by=list(Natural_PCAP931_3microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_Dark_averaged_flat<-c(Natural_Control_Dark_averaged_flat,Natural_Control_Dark_averaged[variable,-1])
}

Natural_Aripiprazole_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_3microM_Dark_averaged_flat<-c(Natural_Aripiprazole_3microM_Dark_averaged_flat,Natural_Aripiprazole_3microM_Dark_averaged[variable,-1])
}

Natural_Cariprazine_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_3microM_Dark_averaged_flat<-c(Natural_Cariprazine_3microM_Dark_averaged_flat,Natural_Cariprazine_3microM_Dark_averaged[variable,-1])
}


Natural_Clozapine_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_3microM_Dark_averaged_flat<-c(Natural_Clozapine_3microM_Dark_averaged_flat,Natural_Clozapine_3microM_Dark_averaged[variable,-1])
}

Natural_CNO_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_3microM_Dark_averaged_flat<-c(Natural_CNO_3microM_Dark_averaged_flat,Natural_CNO_3microM_Dark_averaged[variable,-1])
}

Natural_Haloperidol_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_3microM_Dark_averaged_flat<-c(Natural_Haloperidol_3microM_Dark_averaged_flat,Natural_Haloperidol_3microM_Dark_averaged[variable,-1])
}

Natural_NDMC_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_3microM_Dark_averaged_flat<-c(Natural_NDMC_3microM_Dark_averaged_flat,Natural_NDMC_3microM_Dark_averaged[variable,-1])
}

Natural_NDMCHigh_50microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_50microM_Dark_averaged_flat<-c(Natural_NDMCHigh_50microM_Dark_averaged_flat,Natural_NDMCHigh_50microM_Dark_averaged[variable,-1])
}

Natural_OSU6162_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_3microM_Dark_averaged_flat<-c(Natural_OSU6162_3microM_Dark_averaged_flat,Natural_OSU6162_3microM_Dark_averaged[variable,-1])
}

Natural_PCAP1_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_3microM_Dark_averaged_flat<-c(Natural_PCAP1_3microM_Dark_averaged_flat,Natural_PCAP1_3microM_Dark_averaged[variable,-1])
}

Natural_PCAP2_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_3microM_Dark_averaged_flat<-c(Natural_PCAP2_3microM_Dark_averaged_flat,Natural_PCAP2_3microM_Dark_averaged[variable,-1])
}


Natural_PCAP814_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_3microM_Dark_averaged_flat<-c(Natural_PCAP814_3microM_Dark_averaged_flat,Natural_PCAP814_3microM_Dark_averaged[variable,-1])
}


Natural_PCAP931_3microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_3microM_Dark_averaged_flat<-c(Natural_PCAP931_3microM_Dark_averaged_flat,Natural_PCAP931_3microM_Dark_averaged[variable,-1])
}


Natural_3microM_Dark_all<-rbind(Natural_Control_Dark_averaged_flat,Natural_Aripiprazole_3microM_Dark_averaged_flat, 
		Natural_Cariprazine_3microM_Dark_averaged_flat, Natural_Clozapine_3microM_Dark_averaged_flat, Natural_CNO_3microM_Dark_averaged_flat,
		Natural_Haloperidol_3microM_Dark_averaged_flat, Natural_NDMC_3microM_Dark_averaged_flat, Natural_NDMCHigh_50microM_Dark_averaged_flat,
		Natural_OSU6162_3microM_Dark_averaged_flat, Natural_PCAP1_3microM_Dark_averaged_flat, Natural_PCAP2_3microM_Dark_averaged_flat,
		Natural_PCAP814_3microM_Dark_averaged_flat, Natural_PCAP931_3microM_Dark_averaged_flat)

#as numeric
Natural_3microM_Dark_all<-apply(Natural_3microM_Dark_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_3microM_Dark_all<-apply(Natural_3microM_Dark_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_3microM_Dark_all)<-c("Control","Aripiprazole_3microM","Cariprazine_3microM","Clozapine_3microM",
		"CNO_3microM","Haloperidol_3microM","NDMC_3microM","NDMCHigh_50microM",
		"OSU6162_3microM","PCAP1_3microM","PCAP2_3microM","PCAP814_3microM",
		"PCAP931_3microM")


#euclidean distance
d <- dist(Natural_3microM_Dark_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_Dark.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_3microM_Dark_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_Dark.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_3microM_Dark_all)[drug],"Dark.png"),width=1500,height=750)
	plot(Natural_3microM_Dark_all[c("Control"),122:143],Natural_3microM_Dark_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_3microM_Dark_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_3microM_Dark_all[c("Control"),100:121],Natural_3microM_Dark_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_3microM_Dark_all[c("Control"),78:99],Natural_3microM_Dark_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_3microM_Dark_all[c("Control"),56:77],Natural_3microM_Dark_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_3microM_Dark_all[c("Control"),34:55],Natural_3microM_Dark_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_3microM_Dark_all[c("Control"),12:33],Natural_3microM_Dark_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_3microM_Dark_all[c("Control"),1:11],Natural_3microM_Dark_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}









#DarkApoLow


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_3microM_DarkApoLow<-read.table("Disease_Aripiprazole_3microM_DarkApoLow.txt")
Disease_Cariprazine_3microM_DarkApoLow<-read.table("Disease_Cariprazine_3microM_DarkApoLow.txt")
Disease_Clozapine_3microM_DarkApoLow<-read.table("Disease_Clozapine_3microM_DarkApoLow.txt")
Disease_CNO_3microM_DarkApoLow<-read.table("Disease_CNO_3microM_DarkApoLow.txt")
Disease_Control_DarkApoLow<-read.table("Disease_Control_DarkApoLow.txt")
Disease_Haloperidol_3microM_DarkApoLow<-read.table("Disease_Haloperidol_3microM_DarkApoLow.txt")
Disease_NDMC_3microM_DarkApoLow<-read.table("Disease_NDMC_3microM_DarkApoLow.txt")
Disease_NDMCHigh_50microM_DarkApoLow<-read.table("Disease_NDMCHigh_50microM_DarkApoLow.txt")
Disease_OSU6162_3microM_DarkApoLow<-read.table("Disease_OSU6162_3microM_DarkApoLow.txt")
Disease_PCAP1_3microM_DarkApoLow<-read.table("Disease_PCAP1_3microM_DarkApoLow.txt")
Disease_PCAP2_3microM_DarkApoLow<-read.table("Disease_PCAP2_3microM_DarkApoLow.txt")
Disease_PCAP814_3microM_DarkApoLow<-read.table("Disease_PCAP814_3microM_DarkApoLow.txt")
Disease_PCAP931_3microM_DarkApoLow<-read.table("Disease_PCAP931_3microM_DarkApoLow.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkApoLow<-Disease_Control_DarkApoLow[,c(1,3,2,4:11)]
Disease_Aripiprazole_3microM_DarkApoLow<-Disease_Aripiprazole_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Cariprazine_3microM_DarkApoLow<-Disease_Cariprazine_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Clozapine_3microM_DarkApoLow<-Disease_Clozapine_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_CNO_3microM_DarkApoLow<-Disease_CNO_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Haloperidol_3microM_DarkApoLow<-Disease_Haloperidol_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_NDMC_3microM_DarkApoLow<-Disease_NDMC_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_NDMCHigh_50microM_DarkApoLow<-Disease_NDMCHigh_50microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_OSU6162_3microM_DarkApoLow<-Disease_OSU6162_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP1_3microM_DarkApoLow<-Disease_PCAP1_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP2_3microM_DarkApoLow<-Disease_PCAP2_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP814_3microM_DarkApoLow<-Disease_PCAP814_3microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP931_3microM_DarkApoLow<-Disease_PCAP931_3microM_DarkApoLow[,c(1,3,2,4:11)]



Disease_Control_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkApoLow_averaged<-rbind(Disease_Control_DarkApoLow_averaged,
			cbind(aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkApoLow_averaged<-aggregate(Disease_Control_DarkApoLow_averaged,by=list(Disease_Control_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_3microM_DarkApoLow_averaged<-rbind(Disease_Aripiprazole_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_3microM_DarkApoLow[Disease_Aripiprazole_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_3microM_DarkApoLow_averaged<-aggregate(Disease_Aripiprazole_3microM_DarkApoLow_averaged,by=list(Disease_Aripiprazole_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_3microM_DarkApoLow_averaged<-rbind(Disease_Cariprazine_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_3microM_DarkApoLow[Disease_Cariprazine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_3microM_DarkApoLow_averaged<-aggregate(Disease_Cariprazine_3microM_DarkApoLow_averaged,by=list(Disease_Cariprazine_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_3microM_DarkApoLow_averaged<-rbind(Disease_Clozapine_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_3microM_DarkApoLow[Disease_Clozapine_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_3microM_DarkApoLow_averaged<-aggregate(Disease_Clozapine_3microM_DarkApoLow_averaged,by=list(Disease_Clozapine_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_3microM_DarkApoLow_averaged<-rbind(Disease_CNO_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_CNO_3microM_DarkApoLow[Disease_CNO_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_CNO_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_3microM_DarkApoLow_averaged<-aggregate(Disease_CNO_3microM_DarkApoLow_averaged,by=list(Disease_CNO_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_3microM_DarkApoLow_averaged<-rbind(Disease_Haloperidol_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_3microM_DarkApoLow[Disease_Haloperidol_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_3microM_DarkApoLow_averaged<-aggregate(Disease_Haloperidol_3microM_DarkApoLow_averaged,by=list(Disease_Haloperidol_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_3microM_DarkApoLow_averaged<-rbind(Disease_NDMC_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_3microM_DarkApoLow[Disease_NDMC_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_3microM_DarkApoLow_averaged<-aggregate(Disease_NDMC_3microM_DarkApoLow_averaged,by=list(Disease_NDMC_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_50microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_50microM_DarkApoLow_averaged<-rbind(Disease_NDMCHigh_50microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_50microM_DarkApoLow[Disease_NDMCHigh_50microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_50microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_50microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_50microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_50microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_50microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_50microM_DarkApoLow_averaged<-aggregate(Disease_NDMCHigh_50microM_DarkApoLow_averaged,by=list(Disease_NDMCHigh_50microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_3microM_DarkApoLow_averaged<-rbind(Disease_OSU6162_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_3microM_DarkApoLow[Disease_OSU6162_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_3microM_DarkApoLow_averaged<-aggregate(Disease_OSU6162_3microM_DarkApoLow_averaged,by=list(Disease_OSU6162_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_3microM_DarkApoLow_averaged<-rbind(Disease_PCAP1_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_3microM_DarkApoLow[Disease_PCAP1_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_3microM_DarkApoLow_averaged<-aggregate(Disease_PCAP1_3microM_DarkApoLow_averaged,by=list(Disease_PCAP1_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_3microM_DarkApoLow_averaged<-rbind(Disease_PCAP2_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_3microM_DarkApoLow[Disease_PCAP2_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_3microM_DarkApoLow_averaged<-aggregate(Disease_PCAP2_3microM_DarkApoLow_averaged,by=list(Disease_PCAP2_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_3microM_DarkApoLow_averaged<-rbind(Disease_PCAP814_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_3microM_DarkApoLow[Disease_PCAP814_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_3microM_DarkApoLow_averaged<-aggregate(Disease_PCAP814_3microM_DarkApoLow_averaged,by=list(Disease_PCAP814_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_3microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_3microM_DarkApoLow_averaged<-rbind(Disease_PCAP931_3microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_3microM_DarkApoLow[Disease_PCAP931_3microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_3microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_3microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_3microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_3microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_3microM_DarkApoLow_averaged<-aggregate(Disease_PCAP931_3microM_DarkApoLow_averaged,by=list(Disease_PCAP931_3microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkApoLow_averaged_flat<-c(Disease_Control_DarkApoLow_averaged_flat,Disease_Control_DarkApoLow_averaged[variable,-1])
}

Disease_Aripiprazole_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_3microM_DarkApoLow_averaged_flat<-c(Disease_Aripiprazole_3microM_DarkApoLow_averaged_flat,Disease_Aripiprazole_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_Cariprazine_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_3microM_DarkApoLow_averaged_flat<-c(Disease_Cariprazine_3microM_DarkApoLow_averaged_flat,Disease_Cariprazine_3microM_DarkApoLow_averaged[variable,-1])
}


Disease_Clozapine_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_3microM_DarkApoLow_averaged_flat<-c(Disease_Clozapine_3microM_DarkApoLow_averaged_flat,Disease_Clozapine_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_CNO_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_3microM_DarkApoLow_averaged_flat<-c(Disease_CNO_3microM_DarkApoLow_averaged_flat,Disease_CNO_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_Haloperidol_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_3microM_DarkApoLow_averaged_flat<-c(Disease_Haloperidol_3microM_DarkApoLow_averaged_flat,Disease_Haloperidol_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_NDMC_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_3microM_DarkApoLow_averaged_flat<-c(Disease_NDMC_3microM_DarkApoLow_averaged_flat,Disease_NDMC_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_NDMCHigh_50microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_50microM_DarkApoLow_averaged_flat<-c(Disease_NDMCHigh_50microM_DarkApoLow_averaged_flat,Disease_NDMCHigh_50microM_DarkApoLow_averaged[variable,-1])
}

Disease_OSU6162_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_3microM_DarkApoLow_averaged_flat<-c(Disease_OSU6162_3microM_DarkApoLow_averaged_flat,Disease_OSU6162_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_PCAP1_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_3microM_DarkApoLow_averaged_flat<-c(Disease_PCAP1_3microM_DarkApoLow_averaged_flat,Disease_PCAP1_3microM_DarkApoLow_averaged[variable,-1])
}

Disease_PCAP2_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_3microM_DarkApoLow_averaged_flat<-c(Disease_PCAP2_3microM_DarkApoLow_averaged_flat,Disease_PCAP2_3microM_DarkApoLow_averaged[variable,-1])
}


Disease_PCAP814_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_3microM_DarkApoLow_averaged_flat<-c(Disease_PCAP814_3microM_DarkApoLow_averaged_flat,Disease_PCAP814_3microM_DarkApoLow_averaged[variable,-1])
}


Disease_PCAP931_3microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_3microM_DarkApoLow_averaged_flat<-c(Disease_PCAP931_3microM_DarkApoLow_averaged_flat,Disease_PCAP931_3microM_DarkApoLow_averaged[variable,-1])
}


Disease_3microM_DarkApoLow_all<-rbind(Disease_Control_DarkApoLow_averaged_flat,Disease_Aripiprazole_3microM_DarkApoLow_averaged_flat, 
		Disease_Cariprazine_3microM_DarkApoLow_averaged_flat, Disease_Clozapine_3microM_DarkApoLow_averaged_flat, Disease_CNO_3microM_DarkApoLow_averaged_flat,
		Disease_Haloperidol_3microM_DarkApoLow_averaged_flat, Disease_NDMC_3microM_DarkApoLow_averaged_flat, Disease_NDMCHigh_50microM_DarkApoLow_averaged_flat,
		Disease_OSU6162_3microM_DarkApoLow_averaged_flat, Disease_PCAP1_3microM_DarkApoLow_averaged_flat, Disease_PCAP2_3microM_DarkApoLow_averaged_flat,
		Disease_PCAP814_3microM_DarkApoLow_averaged_flat, Disease_PCAP931_3microM_DarkApoLow_averaged_flat)

#as numeric
Disease_3microM_DarkApoLow_all<-apply(Disease_3microM_DarkApoLow_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_3microM_DarkApoLow_all<-apply(Disease_3microM_DarkApoLow_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_3microM_DarkApoLow_all)<-c("Control","Aripiprazole_3microM","Cariprazine_3microM","Clozapine_3microM",
		"CNO_3microM","Haloperidol_3microM","NDMC_3microM","NDMCHigh_50microM",
		"OSU6162_3microM","PCAP1_3microM","PCAP2_3microM","PCAP814_3microM",
		"PCAP931_3microM")


#euclidean distance
d <- dist(Disease_3microM_DarkApoLow_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkApoLow.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_3microM_DarkApoLow_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkApoLow.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_3microM_DarkApoLow_all)[drug],"DarkApoLow.png"),width=1500,height=750)
	plot(Disease_3microM_DarkApoLow_all[c("Control"),122:143],Disease_3microM_DarkApoLow_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_3microM_DarkApoLow_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_3microM_DarkApoLow_all[c("Control"),100:121],Disease_3microM_DarkApoLow_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_3microM_DarkApoLow_all[c("Control"),78:99],Disease_3microM_DarkApoLow_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_3microM_DarkApoLow_all[c("Control"),56:77],Disease_3microM_DarkApoLow_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_3microM_DarkApoLow_all[c("Control"),34:55],Disease_3microM_DarkApoLow_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_3microM_DarkApoLow_all[c("Control"),12:33],Disease_3microM_DarkApoLow_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_3microM_DarkApoLow_all[c("Control"),1:11],Disease_3microM_DarkApoLow_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}



#DarkApoHigh


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_3microM_DarkApoHigh<-read.table("Disease_Aripiprazole_3microM_DarkApoHigh.txt")
Disease_Cariprazine_3microM_DarkApoHigh<-read.table("Disease_Cariprazine_3microM_DarkApoHigh.txt")
Disease_Clozapine_3microM_DarkApoHigh<-read.table("Disease_Clozapine_3microM_DarkApoHigh.txt")
Disease_CNO_3microM_DarkApoHigh<-read.table("Disease_CNO_3microM_DarkApoHigh.txt")
Disease_Control_DarkApoHigh<-read.table("Disease_Control_DarkApoHigh.txt")
Disease_Haloperidol_3microM_DarkApoHigh<-read.table("Disease_Haloperidol_3microM_DarkApoHigh.txt")
Disease_NDMC_3microM_DarkApoHigh<-read.table("Disease_NDMC_3microM_DarkApoHigh.txt")
Disease_NDMCHigh_50microM_DarkApoHigh<-read.table("Disease_NDMCHigh_50microM_DarkApoHigh.txt")
Disease_OSU6162_3microM_DarkApoHigh<-read.table("Disease_OSU6162_3microM_DarkApoHigh.txt")
Disease_PCAP1_3microM_DarkApoHigh<-read.table("Disease_PCAP1_3microM_DarkApoHigh.txt")
Disease_PCAP2_3microM_DarkApoHigh<-read.table("Disease_PCAP2_3microM_DarkApoHigh.txt")
Disease_PCAP814_3microM_DarkApoHigh<-read.table("Disease_PCAP814_3microM_DarkApoHigh.txt")
Disease_PCAP931_3microM_DarkApoHigh<-read.table("Disease_PCAP931_3microM_DarkApoHigh.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkApoHigh<-Disease_Control_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Aripiprazole_3microM_DarkApoHigh<-Disease_Aripiprazole_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Cariprazine_3microM_DarkApoHigh<-Disease_Cariprazine_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Clozapine_3microM_DarkApoHigh<-Disease_Clozapine_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_CNO_3microM_DarkApoHigh<-Disease_CNO_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Haloperidol_3microM_DarkApoHigh<-Disease_Haloperidol_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_NDMC_3microM_DarkApoHigh<-Disease_NDMC_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_NDMCHigh_50microM_DarkApoHigh<-Disease_NDMCHigh_50microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_OSU6162_3microM_DarkApoHigh<-Disease_OSU6162_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP1_3microM_DarkApoHigh<-Disease_PCAP1_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP2_3microM_DarkApoHigh<-Disease_PCAP2_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP814_3microM_DarkApoHigh<-Disease_PCAP814_3microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP931_3microM_DarkApoHigh<-Disease_PCAP931_3microM_DarkApoHigh[,c(1,3,2,4:11)]



Disease_Control_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkApoHigh_averaged<-rbind(Disease_Control_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkApoHigh_averaged<-aggregate(Disease_Control_DarkApoHigh_averaged,by=list(Disease_Control_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_3microM_DarkApoHigh_averaged<-rbind(Disease_Aripiprazole_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_3microM_DarkApoHigh[Disease_Aripiprazole_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_3microM_DarkApoHigh_averaged<-aggregate(Disease_Aripiprazole_3microM_DarkApoHigh_averaged,by=list(Disease_Aripiprazole_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_3microM_DarkApoHigh_averaged<-rbind(Disease_Cariprazine_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_3microM_DarkApoHigh[Disease_Cariprazine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_3microM_DarkApoHigh_averaged<-aggregate(Disease_Cariprazine_3microM_DarkApoHigh_averaged,by=list(Disease_Cariprazine_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_3microM_DarkApoHigh_averaged<-rbind(Disease_Clozapine_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_3microM_DarkApoHigh[Disease_Clozapine_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_3microM_DarkApoHigh_averaged<-aggregate(Disease_Clozapine_3microM_DarkApoHigh_averaged,by=list(Disease_Clozapine_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_3microM_DarkApoHigh_averaged<-rbind(Disease_CNO_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_CNO_3microM_DarkApoHigh[Disease_CNO_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_CNO_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_3microM_DarkApoHigh_averaged<-aggregate(Disease_CNO_3microM_DarkApoHigh_averaged,by=list(Disease_CNO_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_3microM_DarkApoHigh_averaged<-rbind(Disease_Haloperidol_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_3microM_DarkApoHigh[Disease_Haloperidol_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_3microM_DarkApoHigh_averaged<-aggregate(Disease_Haloperidol_3microM_DarkApoHigh_averaged,by=list(Disease_Haloperidol_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_3microM_DarkApoHigh_averaged<-rbind(Disease_NDMC_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_3microM_DarkApoHigh[Disease_NDMC_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_3microM_DarkApoHigh_averaged<-aggregate(Disease_NDMC_3microM_DarkApoHigh_averaged,by=list(Disease_NDMC_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_50microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_50microM_DarkApoHigh_averaged<-rbind(Disease_NDMCHigh_50microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_50microM_DarkApoHigh[Disease_NDMCHigh_50microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_50microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_50microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_50microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_50microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_50microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_50microM_DarkApoHigh_averaged<-aggregate(Disease_NDMCHigh_50microM_DarkApoHigh_averaged,by=list(Disease_NDMCHigh_50microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_3microM_DarkApoHigh_averaged<-rbind(Disease_OSU6162_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_3microM_DarkApoHigh[Disease_OSU6162_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_3microM_DarkApoHigh_averaged<-aggregate(Disease_OSU6162_3microM_DarkApoHigh_averaged,by=list(Disease_OSU6162_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_3microM_DarkApoHigh_averaged<-rbind(Disease_PCAP1_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_3microM_DarkApoHigh[Disease_PCAP1_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_3microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP1_3microM_DarkApoHigh_averaged,by=list(Disease_PCAP1_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_3microM_DarkApoHigh_averaged<-rbind(Disease_PCAP2_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_3microM_DarkApoHigh[Disease_PCAP2_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_3microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP2_3microM_DarkApoHigh_averaged,by=list(Disease_PCAP2_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_3microM_DarkApoHigh_averaged<-rbind(Disease_PCAP814_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_3microM_DarkApoHigh[Disease_PCAP814_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_3microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP814_3microM_DarkApoHigh_averaged,by=list(Disease_PCAP814_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_3microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_3microM_DarkApoHigh_averaged<-rbind(Disease_PCAP931_3microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_3microM_DarkApoHigh[Disease_PCAP931_3microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_3microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_3microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_3microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_3microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_3microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP931_3microM_DarkApoHigh_averaged,by=list(Disease_PCAP931_3microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkApoHigh_averaged_flat<-c(Disease_Control_DarkApoHigh_averaged_flat,Disease_Control_DarkApoHigh_averaged[variable,-1])
}

Disease_Aripiprazole_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_3microM_DarkApoHigh_averaged_flat<-c(Disease_Aripiprazole_3microM_DarkApoHigh_averaged_flat,Disease_Aripiprazole_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_Cariprazine_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_3microM_DarkApoHigh_averaged_flat<-c(Disease_Cariprazine_3microM_DarkApoHigh_averaged_flat,Disease_Cariprazine_3microM_DarkApoHigh_averaged[variable,-1])
}


Disease_Clozapine_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_3microM_DarkApoHigh_averaged_flat<-c(Disease_Clozapine_3microM_DarkApoHigh_averaged_flat,Disease_Clozapine_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_CNO_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_3microM_DarkApoHigh_averaged_flat<-c(Disease_CNO_3microM_DarkApoHigh_averaged_flat,Disease_CNO_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_Haloperidol_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_3microM_DarkApoHigh_averaged_flat<-c(Disease_Haloperidol_3microM_DarkApoHigh_averaged_flat,Disease_Haloperidol_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_NDMC_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_3microM_DarkApoHigh_averaged_flat<-c(Disease_NDMC_3microM_DarkApoHigh_averaged_flat,Disease_NDMC_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_NDMCHigh_50microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_50microM_DarkApoHigh_averaged_flat<-c(Disease_NDMCHigh_50microM_DarkApoHigh_averaged_flat,Disease_NDMCHigh_50microM_DarkApoHigh_averaged[variable,-1])
}

Disease_OSU6162_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_3microM_DarkApoHigh_averaged_flat<-c(Disease_OSU6162_3microM_DarkApoHigh_averaged_flat,Disease_OSU6162_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_PCAP1_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_3microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP1_3microM_DarkApoHigh_averaged_flat,Disease_PCAP1_3microM_DarkApoHigh_averaged[variable,-1])
}

Disease_PCAP2_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_3microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP2_3microM_DarkApoHigh_averaged_flat,Disease_PCAP2_3microM_DarkApoHigh_averaged[variable,-1])
}


Disease_PCAP814_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_3microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP814_3microM_DarkApoHigh_averaged_flat,Disease_PCAP814_3microM_DarkApoHigh_averaged[variable,-1])
}


Disease_PCAP931_3microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_3microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP931_3microM_DarkApoHigh_averaged_flat,Disease_PCAP931_3microM_DarkApoHigh_averaged[variable,-1])
}


Disease_3microM_DarkApoHigh_all<-rbind(Disease_Control_DarkApoHigh_averaged_flat,Disease_Aripiprazole_3microM_DarkApoHigh_averaged_flat, 
		Disease_Cariprazine_3microM_DarkApoHigh_averaged_flat, Disease_Clozapine_3microM_DarkApoHigh_averaged_flat, Disease_CNO_3microM_DarkApoHigh_averaged_flat,
		Disease_Haloperidol_3microM_DarkApoHigh_averaged_flat, Disease_NDMC_3microM_DarkApoHigh_averaged_flat, Disease_NDMCHigh_50microM_DarkApoHigh_averaged_flat,
		Disease_OSU6162_3microM_DarkApoHigh_averaged_flat, Disease_PCAP1_3microM_DarkApoHigh_averaged_flat, Disease_PCAP2_3microM_DarkApoHigh_averaged_flat,
		Disease_PCAP814_3microM_DarkApoHigh_averaged_flat, Disease_PCAP931_3microM_DarkApoHigh_averaged_flat)

#as numeric
Disease_3microM_DarkApoHigh_all<-apply(Disease_3microM_DarkApoHigh_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_3microM_DarkApoHigh_all<-apply(Disease_3microM_DarkApoHigh_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_3microM_DarkApoHigh_all)<-c("Control","Aripiprazole_3microM","Cariprazine_3microM","Clozapine_3microM",
		"CNO_3microM","Haloperidol_3microM","NDMC_3microM","NDMCHigh_50microM",
		"OSU6162_3microM","PCAP1_3microM","PCAP2_3microM","PCAP814_3microM",
		"PCAP931_3microM")


#euclidean distance
d <- dist(Disease_3microM_DarkApoHigh_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkApoHigh.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_3microM_DarkApoHigh_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkApoHigh.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_3microM_DarkApoHigh_all)[drug],"DarkApoHigh.png"),width=1500,height=750)
	plot(Disease_3microM_DarkApoHigh_all[c("Control"),122:143],Disease_3microM_DarkApoHigh_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_3microM_DarkApoHigh_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_3microM_DarkApoHigh_all[c("Control"),100:121],Disease_3microM_DarkApoHigh_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_3microM_DarkApoHigh_all[c("Control"),78:99],Disease_3microM_DarkApoHigh_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_3microM_DarkApoHigh_all[c("Control"),56:77],Disease_3microM_DarkApoHigh_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_3microM_DarkApoHigh_all[c("Control"),34:55],Disease_3microM_DarkApoHigh_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_3microM_DarkApoHigh_all[c("Control"),12:33],Disease_3microM_DarkApoHigh_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_3microM_DarkApoHigh_all[c("Control"),1:11],Disease_3microM_DarkApoHigh_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}





#DarkPTZ


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_3microM_DarkPTZ<-read.table("Disease_Aripiprazole_3microM_DarkPTZ.txt")
Disease_Cariprazine_3microM_DarkPTZ<-read.table("Disease_Cariprazine_3microM_DarkPTZ.txt")
Disease_Clozapine_3microM_DarkPTZ<-read.table("Disease_Clozapine_3microM_DarkPTZ.txt")
Disease_CNO_3microM_DarkPTZ<-read.table("Disease_CNO_3microM_DarkPTZ.txt")
Disease_Control_DarkPTZ<-read.table("Disease_Control_DarkPTZ.txt")
Disease_Haloperidol_3microM_DarkPTZ<-read.table("Disease_Haloperidol_3microM_DarkPTZ.txt")
Disease_NDMC_3microM_DarkPTZ<-read.table("Disease_NDMC_3microM_DarkPTZ.txt")
Disease_NDMCHigh_50microM_DarkPTZ<-read.table("Disease_NDMCHigh_50microM_DarkPTZ.txt")
Disease_OSU6162_3microM_DarkPTZ<-read.table("Disease_OSU6162_3microM_DarkPTZ.txt")
Disease_PCAP1_3microM_DarkPTZ<-read.table("Disease_PCAP1_3microM_DarkPTZ.txt")
Disease_PCAP2_3microM_DarkPTZ<-read.table("Disease_PCAP2_3microM_DarkPTZ.txt")
Disease_PCAP814_3microM_DarkPTZ<-read.table("Disease_PCAP814_3microM_DarkPTZ.txt")
Disease_PCAP931_3microM_DarkPTZ<-read.table("Disease_PCAP931_3microM_DarkPTZ.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkPTZ<-Disease_Control_DarkPTZ[,c(1,3,2,4:11)]
Disease_Aripiprazole_3microM_DarkPTZ<-Disease_Aripiprazole_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Cariprazine_3microM_DarkPTZ<-Disease_Cariprazine_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Clozapine_3microM_DarkPTZ<-Disease_Clozapine_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_CNO_3microM_DarkPTZ<-Disease_CNO_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Haloperidol_3microM_DarkPTZ<-Disease_Haloperidol_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_NDMC_3microM_DarkPTZ<-Disease_NDMC_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_NDMCHigh_50microM_DarkPTZ<-Disease_NDMCHigh_50microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_OSU6162_3microM_DarkPTZ<-Disease_OSU6162_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP1_3microM_DarkPTZ<-Disease_PCAP1_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP2_3microM_DarkPTZ<-Disease_PCAP2_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP814_3microM_DarkPTZ<-Disease_PCAP814_3microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP931_3microM_DarkPTZ<-Disease_PCAP931_3microM_DarkPTZ[,c(1,3,2,4:11)]



Disease_Control_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkPTZ_averaged<-rbind(Disease_Control_DarkPTZ_averaged,
			cbind(aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkPTZ_averaged<-aggregate(Disease_Control_DarkPTZ_averaged,by=list(Disease_Control_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_3microM_DarkPTZ_averaged<-rbind(Disease_Aripiprazole_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_3microM_DarkPTZ[Disease_Aripiprazole_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_3microM_DarkPTZ_averaged<-aggregate(Disease_Aripiprazole_3microM_DarkPTZ_averaged,by=list(Disease_Aripiprazole_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_3microM_DarkPTZ_averaged<-rbind(Disease_Cariprazine_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_3microM_DarkPTZ[Disease_Cariprazine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_3microM_DarkPTZ_averaged<-aggregate(Disease_Cariprazine_3microM_DarkPTZ_averaged,by=list(Disease_Cariprazine_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_3microM_DarkPTZ_averaged<-rbind(Disease_Clozapine_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_3microM_DarkPTZ[Disease_Clozapine_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_3microM_DarkPTZ_averaged<-aggregate(Disease_Clozapine_3microM_DarkPTZ_averaged,by=list(Disease_Clozapine_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_3microM_DarkPTZ_averaged<-rbind(Disease_CNO_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_CNO_3microM_DarkPTZ[Disease_CNO_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_CNO_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_3microM_DarkPTZ_averaged<-aggregate(Disease_CNO_3microM_DarkPTZ_averaged,by=list(Disease_CNO_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_3microM_DarkPTZ_averaged<-rbind(Disease_Haloperidol_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_3microM_DarkPTZ[Disease_Haloperidol_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_3microM_DarkPTZ_averaged<-aggregate(Disease_Haloperidol_3microM_DarkPTZ_averaged,by=list(Disease_Haloperidol_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_3microM_DarkPTZ_averaged<-rbind(Disease_NDMC_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_3microM_DarkPTZ[Disease_NDMC_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_3microM_DarkPTZ_averaged<-aggregate(Disease_NDMC_3microM_DarkPTZ_averaged,by=list(Disease_NDMC_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_50microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_50microM_DarkPTZ_averaged<-rbind(Disease_NDMCHigh_50microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_50microM_DarkPTZ[Disease_NDMCHigh_50microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_50microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_50microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_50microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_50microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_50microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_50microM_DarkPTZ_averaged<-aggregate(Disease_NDMCHigh_50microM_DarkPTZ_averaged,by=list(Disease_NDMCHigh_50microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_3microM_DarkPTZ_averaged<-rbind(Disease_OSU6162_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_3microM_DarkPTZ[Disease_OSU6162_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_3microM_DarkPTZ_averaged<-aggregate(Disease_OSU6162_3microM_DarkPTZ_averaged,by=list(Disease_OSU6162_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_3microM_DarkPTZ_averaged<-rbind(Disease_PCAP1_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_3microM_DarkPTZ[Disease_PCAP1_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_3microM_DarkPTZ_averaged<-aggregate(Disease_PCAP1_3microM_DarkPTZ_averaged,by=list(Disease_PCAP1_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_3microM_DarkPTZ_averaged<-rbind(Disease_PCAP2_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_3microM_DarkPTZ[Disease_PCAP2_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_3microM_DarkPTZ_averaged<-aggregate(Disease_PCAP2_3microM_DarkPTZ_averaged,by=list(Disease_PCAP2_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_3microM_DarkPTZ_averaged<-rbind(Disease_PCAP814_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_3microM_DarkPTZ[Disease_PCAP814_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_3microM_DarkPTZ_averaged<-aggregate(Disease_PCAP814_3microM_DarkPTZ_averaged,by=list(Disease_PCAP814_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_3microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_3microM_DarkPTZ_averaged<-rbind(Disease_PCAP931_3microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_3microM_DarkPTZ[Disease_PCAP931_3microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_3microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_3microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_3microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_3microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_3microM_DarkPTZ_averaged<-aggregate(Disease_PCAP931_3microM_DarkPTZ_averaged,by=list(Disease_PCAP931_3microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkPTZ_averaged_flat<-c(Disease_Control_DarkPTZ_averaged_flat,Disease_Control_DarkPTZ_averaged[variable,-1])
}

Disease_Aripiprazole_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_3microM_DarkPTZ_averaged_flat<-c(Disease_Aripiprazole_3microM_DarkPTZ_averaged_flat,Disease_Aripiprazole_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_Cariprazine_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_3microM_DarkPTZ_averaged_flat<-c(Disease_Cariprazine_3microM_DarkPTZ_averaged_flat,Disease_Cariprazine_3microM_DarkPTZ_averaged[variable,-1])
}


Disease_Clozapine_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_3microM_DarkPTZ_averaged_flat<-c(Disease_Clozapine_3microM_DarkPTZ_averaged_flat,Disease_Clozapine_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_CNO_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_3microM_DarkPTZ_averaged_flat<-c(Disease_CNO_3microM_DarkPTZ_averaged_flat,Disease_CNO_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_Haloperidol_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_3microM_DarkPTZ_averaged_flat<-c(Disease_Haloperidol_3microM_DarkPTZ_averaged_flat,Disease_Haloperidol_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_NDMC_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_3microM_DarkPTZ_averaged_flat<-c(Disease_NDMC_3microM_DarkPTZ_averaged_flat,Disease_NDMC_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_NDMCHigh_50microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_50microM_DarkPTZ_averaged_flat<-c(Disease_NDMCHigh_50microM_DarkPTZ_averaged_flat,Disease_NDMCHigh_50microM_DarkPTZ_averaged[variable,-1])
}

Disease_OSU6162_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_3microM_DarkPTZ_averaged_flat<-c(Disease_OSU6162_3microM_DarkPTZ_averaged_flat,Disease_OSU6162_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_PCAP1_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_3microM_DarkPTZ_averaged_flat<-c(Disease_PCAP1_3microM_DarkPTZ_averaged_flat,Disease_PCAP1_3microM_DarkPTZ_averaged[variable,-1])
}

Disease_PCAP2_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_3microM_DarkPTZ_averaged_flat<-c(Disease_PCAP2_3microM_DarkPTZ_averaged_flat,Disease_PCAP2_3microM_DarkPTZ_averaged[variable,-1])
}


Disease_PCAP814_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_3microM_DarkPTZ_averaged_flat<-c(Disease_PCAP814_3microM_DarkPTZ_averaged_flat,Disease_PCAP814_3microM_DarkPTZ_averaged[variable,-1])
}


Disease_PCAP931_3microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_3microM_DarkPTZ_averaged_flat<-c(Disease_PCAP931_3microM_DarkPTZ_averaged_flat,Disease_PCAP931_3microM_DarkPTZ_averaged[variable,-1])
}


Disease_3microM_DarkPTZ_all<-rbind(Disease_Control_DarkPTZ_averaged_flat,Disease_Aripiprazole_3microM_DarkPTZ_averaged_flat, 
		Disease_Cariprazine_3microM_DarkPTZ_averaged_flat, Disease_Clozapine_3microM_DarkPTZ_averaged_flat, Disease_CNO_3microM_DarkPTZ_averaged_flat,
		Disease_Haloperidol_3microM_DarkPTZ_averaged_flat, Disease_NDMC_3microM_DarkPTZ_averaged_flat, Disease_NDMCHigh_50microM_DarkPTZ_averaged_flat,
		Disease_OSU6162_3microM_DarkPTZ_averaged_flat, Disease_PCAP1_3microM_DarkPTZ_averaged_flat, Disease_PCAP2_3microM_DarkPTZ_averaged_flat,
		Disease_PCAP814_3microM_DarkPTZ_averaged_flat, Disease_PCAP931_3microM_DarkPTZ_averaged_flat)

#as numeric
Disease_3microM_DarkPTZ_all<-apply(Disease_3microM_DarkPTZ_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_3microM_DarkPTZ_all<-apply(Disease_3microM_DarkPTZ_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_3microM_DarkPTZ_all)<-c("Control","Aripiprazole_3microM","Cariprazine_3microM","Clozapine_3microM",
		"CNO_3microM","Haloperidol_3microM","NDMC_3microM","NDMCHigh_50microM",
		"OSU6162_3microM","PCAP1_3microM","PCAP2_3microM","PCAP814_3microM",
		"PCAP931_3microM")


#euclidean distance
d <- dist(Disease_3microM_DarkPTZ_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkPTZ.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_3microM_DarkPTZ_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkPTZ.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_3microM_DarkPTZ_all)[drug],"DarkPTZ.png"),width=1500,height=750)
	plot(Disease_3microM_DarkPTZ_all[c("Control"),122:143],Disease_3microM_DarkPTZ_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_3microM_DarkPTZ_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_3microM_DarkPTZ_all[c("Control"),100:121],Disease_3microM_DarkPTZ_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_3microM_DarkPTZ_all[c("Control"),78:99],Disease_3microM_DarkPTZ_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_3microM_DarkPTZ_all[c("Control"),56:77],Disease_3microM_DarkPTZ_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_3microM_DarkPTZ_all[c("Control"),34:55],Disease_3microM_DarkPTZ_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_3microM_DarkPTZ_all[c("Control"),12:33],Disease_3microM_DarkPTZ_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_3microM_DarkPTZ_all[c("Control"),1:11],Disease_3microM_DarkPTZ_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}

############################################### 3 microM ######################################################



############################################### 1 microM ######################################################



#Light

#Light


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_1microM_Light<-read.table("Natural_Aripiprazole_1microM_Light.txt")
Natural_Cariprazine_1microM_Light<-read.table("Natural_Cariprazine_1microM_Light.txt")
Natural_Clozapine_1microM_Light<-read.table("Natural_Clozapine_1microM_Light.txt")
Natural_CNO_1microM_Light<-read.table("Natural_CNO_1microM_Light.txt")
Natural_Control_Light<-read.table("Natural_Control_Light.txt")
Natural_Haloperidol_1microM_Light<-read.table("Natural_Haloperidol_1microM_Light.txt")
Natural_NDMC_1microM_Light<-read.table("Natural_NDMC_1microM_Light.txt")
Natural_NDMCHigh_25microM_Light<-read.table("Natural_NDMCHigh_25microM_Light.txt")
Natural_OSU6162_1microM_Light<-read.table("Natural_OSU6162_1microM_Light.txt")
Natural_PCAP1_1microM_Light<-read.table("Natural_PCAP1_1microM_Light.txt")
Natural_PCAP2_1microM_Light<-read.table("Natural_PCAP2_1microM_Light.txt")
Natural_PCAP814_1microM_Light<-read.table("Natural_PCAP814_1microM_Light.txt")
Natural_PCAP931_1microM_Light<-read.table("Natural_PCAP931_1microM_Light.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_Light<-Natural_Control_Light[,c(1,3,2,4:11)]
Natural_Aripiprazole_1microM_Light<-Natural_Aripiprazole_1microM_Light[,c(1,3,2,4:11)]
Natural_Cariprazine_1microM_Light<-Natural_Cariprazine_1microM_Light[,c(1,3,2,4:11)]
Natural_Clozapine_1microM_Light<-Natural_Clozapine_1microM_Light[,c(1,3,2,4:11)]
Natural_CNO_1microM_Light<-Natural_CNO_1microM_Light[,c(1,3,2,4:11)]
Natural_Haloperidol_1microM_Light<-Natural_Haloperidol_1microM_Light[,c(1,3,2,4:11)]
Natural_NDMC_1microM_Light<-Natural_NDMC_1microM_Light[,c(1,3,2,4:11)]
Natural_NDMCHigh_25microM_Light<-Natural_NDMCHigh_25microM_Light[,c(1,3,2,4:11)]
Natural_OSU6162_1microM_Light<-Natural_OSU6162_1microM_Light[,c(1,3,2,4:11)]
Natural_PCAP1_1microM_Light<-Natural_PCAP1_1microM_Light[,c(1,3,2,4:11)]
Natural_PCAP2_1microM_Light<-Natural_PCAP2_1microM_Light[,c(1,3,2,4:11)]
Natural_PCAP814_1microM_Light<-Natural_PCAP814_1microM_Light[,c(1,3,2,4:11)]
Natural_PCAP931_1microM_Light<-Natural_PCAP931_1microM_Light[,c(1,3,2,4:11)]



Natural_Control_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_Light_averaged<-rbind(Natural_Control_Light_averaged,
			cbind(aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,3],
							by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,],
#		by=list(Natural_Control_Light[Natural_Control_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_Light_averaged)[2:10],"_SD"))


colnames(Natural_Control_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_Light_averaged<-aggregate(Natural_Control_Light_averaged,by=list(Natural_Control_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_1microM_Light_averaged<-rbind(Natural_Aripiprazole_1microM_Light_averaged,
			cbind(aggregate(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_1microM_Light[Natural_Aripiprazole_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_1microM_Light_averaged<-aggregate(Natural_Aripiprazole_1microM_Light_averaged,by=list(Natural_Aripiprazole_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_1microM_Light_averaged<-rbind(Natural_Cariprazine_1microM_Light_averaged,
			cbind(aggregate(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_1microM_Light[Natural_Cariprazine_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_1microM_Light_averaged<-aggregate(Natural_Cariprazine_1microM_Light_averaged,by=list(Natural_Cariprazine_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_1microM_Light_averaged<-rbind(Natural_Clozapine_1microM_Light_averaged,
			cbind(aggregate(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_1microM_Light[Natural_Clozapine_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_1microM_Light_averaged<-aggregate(Natural_Clozapine_1microM_Light_averaged,by=list(Natural_Clozapine_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_1microM_Light_averaged<-rbind(Natural_CNO_1microM_Light_averaged,
			cbind(aggregate(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_CNO_1microM_Light[Natural_CNO_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_CNO_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_1microM_Light_averaged<-aggregate(Natural_CNO_1microM_Light_averaged,by=list(Natural_CNO_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_1microM_Light_averaged<-rbind(Natural_Haloperidol_1microM_Light_averaged,
			cbind(aggregate(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_1microM_Light[Natural_Haloperidol_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_1microM_Light_averaged<-aggregate(Natural_Haloperidol_1microM_Light_averaged,by=list(Natural_Haloperidol_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_1microM_Light_averaged<-rbind(Natural_NDMC_1microM_Light_averaged,
			cbind(aggregate(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_1microM_Light[Natural_NDMC_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_1microM_Light_averaged<-aggregate(Natural_NDMC_1microM_Light_averaged,by=list(Natural_NDMC_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_25microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_25microM_Light_averaged<-rbind(Natural_NDMCHigh_25microM_Light_averaged,
			cbind(aggregate(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_25microM_Light[Natural_NDMCHigh_25microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_25microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_25microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_25microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_25microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_25microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_25microM_Light_averaged<-aggregate(Natural_NDMCHigh_25microM_Light_averaged,by=list(Natural_NDMCHigh_25microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_1microM_Light_averaged<-rbind(Natural_OSU6162_1microM_Light_averaged,
			cbind(aggregate(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_1microM_Light[Natural_OSU6162_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_1microM_Light_averaged<-aggregate(Natural_OSU6162_1microM_Light_averaged,by=list(Natural_OSU6162_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_1microM_Light_averaged<-rbind(Natural_PCAP1_1microM_Light_averaged,
			cbind(aggregate(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_1microM_Light[Natural_PCAP1_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_1microM_Light_averaged<-aggregate(Natural_PCAP1_1microM_Light_averaged,by=list(Natural_PCAP1_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_1microM_Light_averaged<-rbind(Natural_PCAP2_1microM_Light_averaged,
			cbind(aggregate(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_1microM_Light[Natural_PCAP2_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_1microM_Light_averaged<-aggregate(Natural_PCAP2_1microM_Light_averaged,by=list(Natural_PCAP2_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_1microM_Light_averaged<-rbind(Natural_PCAP814_1microM_Light_averaged,
			cbind(aggregate(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_1microM_Light[Natural_PCAP814_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_1microM_Light_averaged<-aggregate(Natural_PCAP814_1microM_Light_averaged,by=list(Natural_PCAP814_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_1microM_Light_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_1microM_Light_averaged<-rbind(Natural_PCAP931_1microM_Light_averaged,
			cbind(aggregate(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_1microM_Light[Natural_PCAP931_1microM_Light$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_1microM_Light_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_1microM_Light_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_1microM_Light_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_1microM_Light_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_1microM_Light_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_1microM_Light_averaged<-aggregate(Natural_PCAP931_1microM_Light_averaged,by=list(Natural_PCAP931_1microM_Light_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_Light_averaged_flat<-c(Natural_Control_Light_averaged_flat,Natural_Control_Light_averaged[variable,-1])
}

Natural_Aripiprazole_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_1microM_Light_averaged_flat<-c(Natural_Aripiprazole_1microM_Light_averaged_flat,Natural_Aripiprazole_1microM_Light_averaged[variable,-1])
}

Natural_Cariprazine_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_1microM_Light_averaged_flat<-c(Natural_Cariprazine_1microM_Light_averaged_flat,Natural_Cariprazine_1microM_Light_averaged[variable,-1])
}


Natural_Clozapine_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_1microM_Light_averaged_flat<-c(Natural_Clozapine_1microM_Light_averaged_flat,Natural_Clozapine_1microM_Light_averaged[variable,-1])
}

Natural_CNO_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_1microM_Light_averaged_flat<-c(Natural_CNO_1microM_Light_averaged_flat,Natural_CNO_1microM_Light_averaged[variable,-1])
}

Natural_Haloperidol_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_1microM_Light_averaged_flat<-c(Natural_Haloperidol_1microM_Light_averaged_flat,Natural_Haloperidol_1microM_Light_averaged[variable,-1])
}

Natural_NDMC_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_1microM_Light_averaged_flat<-c(Natural_NDMC_1microM_Light_averaged_flat,Natural_NDMC_1microM_Light_averaged[variable,-1])
}

Natural_NDMCHigh_25microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_25microM_Light_averaged_flat<-c(Natural_NDMCHigh_25microM_Light_averaged_flat,Natural_NDMCHigh_25microM_Light_averaged[variable,-1])
}

Natural_OSU6162_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_1microM_Light_averaged_flat<-c(Natural_OSU6162_1microM_Light_averaged_flat,Natural_OSU6162_1microM_Light_averaged[variable,-1])
}

Natural_PCAP1_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_1microM_Light_averaged_flat<-c(Natural_PCAP1_1microM_Light_averaged_flat,Natural_PCAP1_1microM_Light_averaged[variable,-1])
}

Natural_PCAP2_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_1microM_Light_averaged_flat<-c(Natural_PCAP2_1microM_Light_averaged_flat,Natural_PCAP2_1microM_Light_averaged[variable,-1])
}


Natural_PCAP814_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_1microM_Light_averaged_flat<-c(Natural_PCAP814_1microM_Light_averaged_flat,Natural_PCAP814_1microM_Light_averaged[variable,-1])
}


Natural_PCAP931_1microM_Light_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_1microM_Light_averaged_flat<-c(Natural_PCAP931_1microM_Light_averaged_flat,Natural_PCAP931_1microM_Light_averaged[variable,-1])
}


Natural_1microM_Light_all<-rbind(Natural_Control_Light_averaged_flat,Natural_Aripiprazole_1microM_Light_averaged_flat, 
		Natural_Cariprazine_1microM_Light_averaged_flat, Natural_Clozapine_1microM_Light_averaged_flat, Natural_CNO_1microM_Light_averaged_flat,
		Natural_Haloperidol_1microM_Light_averaged_flat, Natural_NDMC_1microM_Light_averaged_flat, Natural_NDMCHigh_25microM_Light_averaged_flat,
		Natural_OSU6162_1microM_Light_averaged_flat, Natural_PCAP1_1microM_Light_averaged_flat, Natural_PCAP2_1microM_Light_averaged_flat,
		Natural_PCAP814_1microM_Light_averaged_flat, Natural_PCAP931_1microM_Light_averaged_flat)

#as numeric
Natural_1microM_Light_all<-apply(Natural_1microM_Light_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_1microM_Light_all<-apply(Natural_1microM_Light_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_1microM_Light_all)<-c("Control","Aripiprazole_1microM","Cariprazine_1microM","Clozapine_1microM",
		"CNO_1microM","Haloperidol_1microM","NDMC_1microM","NDMCHigh_25microM",
		"OSU6162_1microM","PCAP1_1microM","PCAP2_1microM","PCAP814_1microM",
		"PCAP931_1microM")


#euclidean distance
d <- dist(Natural_1microM_Light_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_Light.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_1microM_Light_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_Light.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_1microM_Light_all)[drug],"Light.png"),width=1500,height=750)
	plot(Natural_1microM_Light_all[c("Control"),122:143],Natural_1microM_Light_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_1microM_Light_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_1microM_Light_all[c("Control"),100:121],Natural_1microM_Light_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_1microM_Light_all[c("Control"),78:99],Natural_1microM_Light_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_1microM_Light_all[c("Control"),56:77],Natural_1microM_Light_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_1microM_Light_all[c("Control"),34:55],Natural_1microM_Light_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_1microM_Light_all[c("Control"),12:33],Natural_1microM_Light_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_1microM_Light_all[c("Control"),1:11],Natural_1microM_Light_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}



#LightDark


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_1microM_LightDark<-read.table("Natural_Aripiprazole_1microM_LightDark.txt")
Natural_Cariprazine_1microM_LightDark<-read.table("Natural_Cariprazine_1microM_LightDark.txt")
Natural_Clozapine_1microM_LightDark<-read.table("Natural_Clozapine_1microM_LightDark.txt")
Natural_CNO_1microM_LightDark<-read.table("Natural_CNO_1microM_LightDark.txt")
Natural_Control_LightDark<-read.table("Natural_Control_LightDark.txt")
Natural_Haloperidol_1microM_LightDark<-read.table("Natural_Haloperidol_1microM_LightDark.txt")
Natural_NDMC_1microM_LightDark<-read.table("Natural_NDMC_1microM_LightDark.txt")
Natural_NDMCHigh_25microM_LightDark<-read.table("Natural_NDMCHigh_25microM_LightDark.txt")
Natural_OSU6162_1microM_LightDark<-read.table("Natural_OSU6162_1microM_LightDark.txt")
Natural_PCAP1_1microM_LightDark<-read.table("Natural_PCAP1_1microM_LightDark.txt")
Natural_PCAP2_1microM_LightDark<-read.table("Natural_PCAP2_1microM_LightDark.txt")
Natural_PCAP814_1microM_LightDark<-read.table("Natural_PCAP814_1microM_LightDark.txt")
Natural_PCAP931_1microM_LightDark<-read.table("Natural_PCAP931_1microM_LightDark.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_LightDark<-Natural_Control_LightDark[,c(1,3,2,4:11)]
Natural_Aripiprazole_1microM_LightDark<-Natural_Aripiprazole_1microM_LightDark[,c(1,3,2,4:11)]
Natural_Cariprazine_1microM_LightDark<-Natural_Cariprazine_1microM_LightDark[,c(1,3,2,4:11)]
Natural_Clozapine_1microM_LightDark<-Natural_Clozapine_1microM_LightDark[,c(1,3,2,4:11)]
Natural_CNO_1microM_LightDark<-Natural_CNO_1microM_LightDark[,c(1,3,2,4:11)]
Natural_Haloperidol_1microM_LightDark<-Natural_Haloperidol_1microM_LightDark[,c(1,3,2,4:11)]
Natural_NDMC_1microM_LightDark<-Natural_NDMC_1microM_LightDark[,c(1,3,2,4:11)]
Natural_NDMCHigh_25microM_LightDark<-Natural_NDMCHigh_25microM_LightDark[,c(1,3,2,4:11)]
Natural_OSU6162_1microM_LightDark<-Natural_OSU6162_1microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP1_1microM_LightDark<-Natural_PCAP1_1microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP2_1microM_LightDark<-Natural_PCAP2_1microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP814_1microM_LightDark<-Natural_PCAP814_1microM_LightDark[,c(1,3,2,4:11)]
Natural_PCAP931_1microM_LightDark<-Natural_PCAP931_1microM_LightDark[,c(1,3,2,4:11)]



Natural_Control_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_LightDark_averaged<-rbind(Natural_Control_LightDark_averaged,
			cbind(aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Control_LightDark[Natural_Control_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Control_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_LightDark_averaged<-aggregate(Natural_Control_LightDark_averaged,by=list(Natural_Control_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_1microM_LightDark_averaged<-rbind(Natural_Aripiprazole_1microM_LightDark_averaged,
			cbind(aggregate(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_1microM_LightDark[Natural_Aripiprazole_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_1microM_LightDark_averaged<-aggregate(Natural_Aripiprazole_1microM_LightDark_averaged,by=list(Natural_Aripiprazole_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_1microM_LightDark_averaged<-rbind(Natural_Cariprazine_1microM_LightDark_averaged,
			cbind(aggregate(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_1microM_LightDark[Natural_Cariprazine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_1microM_LightDark_averaged<-aggregate(Natural_Cariprazine_1microM_LightDark_averaged,by=list(Natural_Cariprazine_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_1microM_LightDark_averaged<-rbind(Natural_Clozapine_1microM_LightDark_averaged,
			cbind(aggregate(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_1microM_LightDark[Natural_Clozapine_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_1microM_LightDark_averaged<-aggregate(Natural_Clozapine_1microM_LightDark_averaged,by=list(Natural_Clozapine_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_1microM_LightDark_averaged<-rbind(Natural_CNO_1microM_LightDark_averaged,
			cbind(aggregate(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_CNO_1microM_LightDark[Natural_CNO_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_CNO_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_1microM_LightDark_averaged<-aggregate(Natural_CNO_1microM_LightDark_averaged,by=list(Natural_CNO_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_1microM_LightDark_averaged<-rbind(Natural_Haloperidol_1microM_LightDark_averaged,
			cbind(aggregate(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_1microM_LightDark[Natural_Haloperidol_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_1microM_LightDark_averaged<-aggregate(Natural_Haloperidol_1microM_LightDark_averaged,by=list(Natural_Haloperidol_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_1microM_LightDark_averaged<-rbind(Natural_NDMC_1microM_LightDark_averaged,
			cbind(aggregate(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_1microM_LightDark[Natural_NDMC_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_1microM_LightDark_averaged<-aggregate(Natural_NDMC_1microM_LightDark_averaged,by=list(Natural_NDMC_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_25microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_25microM_LightDark_averaged<-rbind(Natural_NDMCHigh_25microM_LightDark_averaged,
			cbind(aggregate(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_25microM_LightDark[Natural_NDMCHigh_25microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_25microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_25microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_25microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_25microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_25microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_25microM_LightDark_averaged<-aggregate(Natural_NDMCHigh_25microM_LightDark_averaged,by=list(Natural_NDMCHigh_25microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_1microM_LightDark_averaged<-rbind(Natural_OSU6162_1microM_LightDark_averaged,
			cbind(aggregate(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_1microM_LightDark[Natural_OSU6162_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_1microM_LightDark_averaged<-aggregate(Natural_OSU6162_1microM_LightDark_averaged,by=list(Natural_OSU6162_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_1microM_LightDark_averaged<-rbind(Natural_PCAP1_1microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_1microM_LightDark[Natural_PCAP1_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_1microM_LightDark_averaged<-aggregate(Natural_PCAP1_1microM_LightDark_averaged,by=list(Natural_PCAP1_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_1microM_LightDark_averaged<-rbind(Natural_PCAP2_1microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_1microM_LightDark[Natural_PCAP2_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_1microM_LightDark_averaged<-aggregate(Natural_PCAP2_1microM_LightDark_averaged,by=list(Natural_PCAP2_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_1microM_LightDark_averaged<-rbind(Natural_PCAP814_1microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_1microM_LightDark[Natural_PCAP814_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_1microM_LightDark_averaged<-aggregate(Natural_PCAP814_1microM_LightDark_averaged,by=list(Natural_PCAP814_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_1microM_LightDark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_1microM_LightDark_averaged<-rbind(Natural_PCAP931_1microM_LightDark_averaged,
			cbind(aggregate(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_1microM_LightDark[Natural_PCAP931_1microM_LightDark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_1microM_LightDark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_1microM_LightDark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_1microM_LightDark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_1microM_LightDark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_1microM_LightDark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_1microM_LightDark_averaged<-aggregate(Natural_PCAP931_1microM_LightDark_averaged,by=list(Natural_PCAP931_1microM_LightDark_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_LightDark_averaged_flat<-c(Natural_Control_LightDark_averaged_flat,Natural_Control_LightDark_averaged[variable,-1])
}

Natural_Aripiprazole_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_1microM_LightDark_averaged_flat<-c(Natural_Aripiprazole_1microM_LightDark_averaged_flat,Natural_Aripiprazole_1microM_LightDark_averaged[variable,-1])
}

Natural_Cariprazine_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_1microM_LightDark_averaged_flat<-c(Natural_Cariprazine_1microM_LightDark_averaged_flat,Natural_Cariprazine_1microM_LightDark_averaged[variable,-1])
}


Natural_Clozapine_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_1microM_LightDark_averaged_flat<-c(Natural_Clozapine_1microM_LightDark_averaged_flat,Natural_Clozapine_1microM_LightDark_averaged[variable,-1])
}

Natural_CNO_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_1microM_LightDark_averaged_flat<-c(Natural_CNO_1microM_LightDark_averaged_flat,Natural_CNO_1microM_LightDark_averaged[variable,-1])
}

Natural_Haloperidol_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_1microM_LightDark_averaged_flat<-c(Natural_Haloperidol_1microM_LightDark_averaged_flat,Natural_Haloperidol_1microM_LightDark_averaged[variable,-1])
}

Natural_NDMC_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_1microM_LightDark_averaged_flat<-c(Natural_NDMC_1microM_LightDark_averaged_flat,Natural_NDMC_1microM_LightDark_averaged[variable,-1])
}

Natural_NDMCHigh_25microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_25microM_LightDark_averaged_flat<-c(Natural_NDMCHigh_25microM_LightDark_averaged_flat,Natural_NDMCHigh_25microM_LightDark_averaged[variable,-1])
}

Natural_OSU6162_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_1microM_LightDark_averaged_flat<-c(Natural_OSU6162_1microM_LightDark_averaged_flat,Natural_OSU6162_1microM_LightDark_averaged[variable,-1])
}

Natural_PCAP1_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_1microM_LightDark_averaged_flat<-c(Natural_PCAP1_1microM_LightDark_averaged_flat,Natural_PCAP1_1microM_LightDark_averaged[variable,-1])
}

Natural_PCAP2_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_1microM_LightDark_averaged_flat<-c(Natural_PCAP2_1microM_LightDark_averaged_flat,Natural_PCAP2_1microM_LightDark_averaged[variable,-1])
}


Natural_PCAP814_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_1microM_LightDark_averaged_flat<-c(Natural_PCAP814_1microM_LightDark_averaged_flat,Natural_PCAP814_1microM_LightDark_averaged[variable,-1])
}


Natural_PCAP931_1microM_LightDark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_1microM_LightDark_averaged_flat<-c(Natural_PCAP931_1microM_LightDark_averaged_flat,Natural_PCAP931_1microM_LightDark_averaged[variable,-1])
}


Natural_1microM_LightDark_all<-rbind(Natural_Control_LightDark_averaged_flat,Natural_Aripiprazole_1microM_LightDark_averaged_flat, 
		Natural_Cariprazine_1microM_LightDark_averaged_flat, Natural_Clozapine_1microM_LightDark_averaged_flat, Natural_CNO_1microM_LightDark_averaged_flat,
		Natural_Haloperidol_1microM_LightDark_averaged_flat, Natural_NDMC_1microM_LightDark_averaged_flat, Natural_NDMCHigh_25microM_LightDark_averaged_flat,
		Natural_OSU6162_1microM_LightDark_averaged_flat, Natural_PCAP1_1microM_LightDark_averaged_flat, Natural_PCAP2_1microM_LightDark_averaged_flat,
		Natural_PCAP814_1microM_LightDark_averaged_flat, Natural_PCAP931_1microM_LightDark_averaged_flat)

#as numeric
Natural_1microM_LightDark_all<-apply(Natural_1microM_LightDark_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_1microM_LightDark_all<-apply(Natural_1microM_LightDark_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_1microM_LightDark_all)<-c("Control","Aripiprazole_1microM","Cariprazine_1microM","Clozapine_1microM",
		"CNO_1microM","Haloperidol_1microM","NDMC_1microM","NDMCHigh_25microM",
		"OSU6162_1microM","PCAP1_1microM","PCAP2_1microM","PCAP814_1microM",
		"PCAP931_1microM")


#euclidean distance
d <- dist(Natural_1microM_LightDark_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_LightDark.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_1microM_LightDark_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_LightDark.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_1microM_LightDark_all)[drug],"LightDark.png"),width=1500,height=750)
	plot(Natural_1microM_LightDark_all[c("Control"),122:143],Natural_1microM_LightDark_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_1microM_LightDark_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_1microM_LightDark_all[c("Control"),100:121],Natural_1microM_LightDark_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_1microM_LightDark_all[c("Control"),78:99],Natural_1microM_LightDark_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_1microM_LightDark_all[c("Control"),56:77],Natural_1microM_LightDark_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_1microM_LightDark_all[c("Control"),34:55],Natural_1microM_LightDark_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_1microM_LightDark_all[c("Control"),12:33],Natural_1microM_LightDark_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_1microM_LightDark_all[c("Control"),1:11],Natural_1microM_LightDark_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}





#Dark


#load files for Control and all 12 drugs for 10 microM dosage
Natural_Aripiprazole_1microM_Dark<-read.table("Natural_Aripiprazole_1microM_Dark.txt")
Natural_Cariprazine_1microM_Dark<-read.table("Natural_Cariprazine_1microM_Dark.txt")
Natural_Clozapine_1microM_Dark<-read.table("Natural_Clozapine_1microM_Dark.txt")
Natural_CNO_1microM_Dark<-read.table("Natural_CNO_1microM_Dark.txt")
Natural_Control_Dark<-read.table("Natural_Control_Dark.txt")
Natural_Haloperidol_1microM_Dark<-read.table("Natural_Haloperidol_1microM_Dark.txt")
Natural_NDMC_1microM_Dark<-read.table("Natural_NDMC_1microM_Dark.txt")
Natural_NDMCHigh_25microM_Dark<-read.table("Natural_NDMCHigh_25microM_Dark.txt")
Natural_OSU6162_1microM_Dark<-read.table("Natural_OSU6162_1microM_Dark.txt")
Natural_PCAP1_1microM_Dark<-read.table("Natural_PCAP1_1microM_Dark.txt")
Natural_PCAP2_1microM_Dark<-read.table("Natural_PCAP2_1microM_Dark.txt")
Natural_PCAP814_1microM_Dark<-read.table("Natural_PCAP814_1microM_Dark.txt")
Natural_PCAP931_1microM_Dark<-read.table("Natural_PCAP931_1microM_Dark.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Natural_Control_Dark<-Natural_Control_Dark[,c(1,3,2,4:11)]
Natural_Aripiprazole_1microM_Dark<-Natural_Aripiprazole_1microM_Dark[,c(1,3,2,4:11)]
Natural_Cariprazine_1microM_Dark<-Natural_Cariprazine_1microM_Dark[,c(1,3,2,4:11)]
Natural_Clozapine_1microM_Dark<-Natural_Clozapine_1microM_Dark[,c(1,3,2,4:11)]
Natural_CNO_1microM_Dark<-Natural_CNO_1microM_Dark[,c(1,3,2,4:11)]
Natural_Haloperidol_1microM_Dark<-Natural_Haloperidol_1microM_Dark[,c(1,3,2,4:11)]
Natural_NDMC_1microM_Dark<-Natural_NDMC_1microM_Dark[,c(1,3,2,4:11)]
Natural_NDMCHigh_25microM_Dark<-Natural_NDMCHigh_25microM_Dark[,c(1,3,2,4:11)]
Natural_OSU6162_1microM_Dark<-Natural_OSU6162_1microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP1_1microM_Dark<-Natural_PCAP1_1microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP2_1microM_Dark<-Natural_PCAP2_1microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP814_1microM_Dark<-Natural_PCAP814_1microM_Dark[,c(1,3,2,4:11)]
Natural_PCAP931_1microM_Dark<-Natural_PCAP931_1microM_Dark[,c(1,3,2,4:11)]



Natural_Control_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Control_Dark_averaged<-rbind(Natural_Control_Dark_averaged,
			cbind(aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Control_Dark[Natural_Control_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Control_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Control_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Control_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Control_Dark_averaged<-aggregate(Natural_Control_Dark_averaged,by=list(Natural_Control_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Aripiprazole_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Aripiprazole_1microM_Dark_averaged<-rbind(Natural_Aripiprazole_1microM_Dark_averaged,
			cbind(aggregate(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Aripiprazole_1microM_Dark[Natural_Aripiprazole_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Aripiprazole_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Aripiprazole_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Aripiprazole_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Aripiprazole_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Aripiprazole_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Aripiprazole_1microM_Dark_averaged<-aggregate(Natural_Aripiprazole_1microM_Dark_averaged,by=list(Natural_Aripiprazole_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Cariprazine_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Cariprazine_1microM_Dark_averaged<-rbind(Natural_Cariprazine_1microM_Dark_averaged,
			cbind(aggregate(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Cariprazine_1microM_Dark[Natural_Cariprazine_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Cariprazine_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Cariprazine_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Cariprazine_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Cariprazine_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Cariprazine_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Cariprazine_1microM_Dark_averaged<-aggregate(Natural_Cariprazine_1microM_Dark_averaged,by=list(Natural_Cariprazine_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_Clozapine_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Clozapine_1microM_Dark_averaged<-rbind(Natural_Clozapine_1microM_Dark_averaged,
			cbind(aggregate(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Clozapine_1microM_Dark[Natural_Clozapine_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Clozapine_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Clozapine_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Clozapine_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Clozapine_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Clozapine_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Clozapine_1microM_Dark_averaged<-aggregate(Natural_Clozapine_1microM_Dark_averaged,by=list(Natural_Clozapine_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_CNO_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_CNO_1microM_Dark_averaged<-rbind(Natural_CNO_1microM_Dark_averaged,
			cbind(aggregate(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_CNO_1microM_Dark[Natural_CNO_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_CNO_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_CNO_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_CNO_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_CNO_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_CNO_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_CNO_1microM_Dark_averaged<-aggregate(Natural_CNO_1microM_Dark_averaged,by=list(Natural_CNO_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_Haloperidol_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_Haloperidol_1microM_Dark_averaged<-rbind(Natural_Haloperidol_1microM_Dark_averaged,
			cbind(aggregate(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_Haloperidol_1microM_Dark[Natural_Haloperidol_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_Haloperidol_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_Haloperidol_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_Haloperidol_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_Haloperidol_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_Haloperidol_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_Haloperidol_1microM_Dark_averaged<-aggregate(Natural_Haloperidol_1microM_Dark_averaged,by=list(Natural_Haloperidol_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]


Natural_NDMC_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMC_1microM_Dark_averaged<-rbind(Natural_NDMC_1microM_Dark_averaged,
			cbind(aggregate(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_NDMC_1microM_Dark[Natural_NDMC_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMC_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMC_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMC_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_NDMC_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMC_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMC_1microM_Dark_averaged<-aggregate(Natural_NDMC_1microM_Dark_averaged,by=list(Natural_NDMC_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_NDMCHigh_25microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_NDMCHigh_25microM_Dark_averaged<-rbind(Natural_NDMCHigh_25microM_Dark_averaged,
			cbind(aggregate(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_NDMCHigh_25microM_Dark[Natural_NDMCHigh_25microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_NDMCHigh_25microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_NDMCHigh_25microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_NDMCHigh_25microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_NDMCHigh_25microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_NDMCHigh_25microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_NDMCHigh_25microM_Dark_averaged<-aggregate(Natural_NDMCHigh_25microM_Dark_averaged,by=list(Natural_NDMCHigh_25microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_OSU6162_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_OSU6162_1microM_Dark_averaged<-rbind(Natural_OSU6162_1microM_Dark_averaged,
			cbind(aggregate(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_OSU6162_1microM_Dark[Natural_OSU6162_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_OSU6162_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_OSU6162_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_OSU6162_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_OSU6162_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_OSU6162_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_OSU6162_1microM_Dark_averaged<-aggregate(Natural_OSU6162_1microM_Dark_averaged,by=list(Natural_OSU6162_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP1_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP1_1microM_Dark_averaged<-rbind(Natural_PCAP1_1microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP1_1microM_Dark[Natural_PCAP1_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP1_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP1_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP1_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP1_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP1_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP1_1microM_Dark_averaged<-aggregate(Natural_PCAP1_1microM_Dark_averaged,by=list(Natural_PCAP1_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP2_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP2_1microM_Dark_averaged<-rbind(Natural_PCAP2_1microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP2_1microM_Dark[Natural_PCAP2_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP2_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP2_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP2_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP2_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP2_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP2_1microM_Dark_averaged<-aggregate(Natural_PCAP2_1microM_Dark_averaged,by=list(Natural_PCAP2_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP814_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP814_1microM_Dark_averaged<-rbind(Natural_PCAP814_1microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP814_1microM_Dark[Natural_PCAP814_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP814_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP814_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP814_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP814_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP814_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP814_1microM_Dark_averaged<-aggregate(Natural_PCAP814_1microM_Dark_averaged,by=list(Natural_PCAP814_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]



Natural_PCAP931_1microM_Dark_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Natural_PCAP931_1microM_Dark_averaged<-rbind(Natural_PCAP931_1microM_Dark_averaged,
			cbind(aggregate(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,],
							by=list(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,1],
							by=list(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,3],
							by=list(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,],
#		by=list(Natural_PCAP931_1microM_Dark[Natural_PCAP931_1microM_Dark$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Natural_PCAP931_1microM_Dark_averaged)[c(2:21)]<-c(paste0(colnames(Natural_PCAP931_1microM_Dark_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Natural_PCAP931_1microM_Dark_averaged)[2:10],"_SD"))


colnames(Natural_PCAP931_1microM_Dark_averaged)[c(2:12)]<-c(paste0(colnames(Natural_PCAP931_1microM_Dark_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Natural_PCAP931_1microM_Dark_averaged<-aggregate(Natural_PCAP931_1microM_Dark_averaged,by=list(Natural_PCAP931_1microM_Dark_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Natural_Control_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Control_Dark_averaged_flat<-c(Natural_Control_Dark_averaged_flat,Natural_Control_Dark_averaged[variable,-1])
}

Natural_Aripiprazole_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Aripiprazole_1microM_Dark_averaged_flat<-c(Natural_Aripiprazole_1microM_Dark_averaged_flat,Natural_Aripiprazole_1microM_Dark_averaged[variable,-1])
}

Natural_Cariprazine_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Cariprazine_1microM_Dark_averaged_flat<-c(Natural_Cariprazine_1microM_Dark_averaged_flat,Natural_Cariprazine_1microM_Dark_averaged[variable,-1])
}


Natural_Clozapine_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Clozapine_1microM_Dark_averaged_flat<-c(Natural_Clozapine_1microM_Dark_averaged_flat,Natural_Clozapine_1microM_Dark_averaged[variable,-1])
}

Natural_CNO_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_CNO_1microM_Dark_averaged_flat<-c(Natural_CNO_1microM_Dark_averaged_flat,Natural_CNO_1microM_Dark_averaged[variable,-1])
}

Natural_Haloperidol_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_Haloperidol_1microM_Dark_averaged_flat<-c(Natural_Haloperidol_1microM_Dark_averaged_flat,Natural_Haloperidol_1microM_Dark_averaged[variable,-1])
}

Natural_NDMC_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMC_1microM_Dark_averaged_flat<-c(Natural_NDMC_1microM_Dark_averaged_flat,Natural_NDMC_1microM_Dark_averaged[variable,-1])
}

Natural_NDMCHigh_25microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_NDMCHigh_25microM_Dark_averaged_flat<-c(Natural_NDMCHigh_25microM_Dark_averaged_flat,Natural_NDMCHigh_25microM_Dark_averaged[variable,-1])
}

Natural_OSU6162_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_OSU6162_1microM_Dark_averaged_flat<-c(Natural_OSU6162_1microM_Dark_averaged_flat,Natural_OSU6162_1microM_Dark_averaged[variable,-1])
}

Natural_PCAP1_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP1_1microM_Dark_averaged_flat<-c(Natural_PCAP1_1microM_Dark_averaged_flat,Natural_PCAP1_1microM_Dark_averaged[variable,-1])
}

Natural_PCAP2_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP2_1microM_Dark_averaged_flat<-c(Natural_PCAP2_1microM_Dark_averaged_flat,Natural_PCAP2_1microM_Dark_averaged[variable,-1])
}


Natural_PCAP814_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP814_1microM_Dark_averaged_flat<-c(Natural_PCAP814_1microM_Dark_averaged_flat,Natural_PCAP814_1microM_Dark_averaged[variable,-1])
}


Natural_PCAP931_1microM_Dark_averaged_flat<-c()
for(variable in 1:13){
	
	Natural_PCAP931_1microM_Dark_averaged_flat<-c(Natural_PCAP931_1microM_Dark_averaged_flat,Natural_PCAP931_1microM_Dark_averaged[variable,-1])
}


Natural_1microM_Dark_all<-rbind(Natural_Control_Dark_averaged_flat,Natural_Aripiprazole_1microM_Dark_averaged_flat, 
		Natural_Cariprazine_1microM_Dark_averaged_flat, Natural_Clozapine_1microM_Dark_averaged_flat, Natural_CNO_1microM_Dark_averaged_flat,
		Natural_Haloperidol_1microM_Dark_averaged_flat, Natural_NDMC_1microM_Dark_averaged_flat, Natural_NDMCHigh_25microM_Dark_averaged_flat,
		Natural_OSU6162_1microM_Dark_averaged_flat, Natural_PCAP1_1microM_Dark_averaged_flat, Natural_PCAP2_1microM_Dark_averaged_flat,
		Natural_PCAP814_1microM_Dark_averaged_flat, Natural_PCAP931_1microM_Dark_averaged_flat)

#as numeric
Natural_1microM_Dark_all<-apply(Natural_1microM_Dark_all,2,function(x){return(as.numeric(x))})

#log and standardize
Natural_1microM_Dark_all<-apply(Natural_1microM_Dark_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Natural_1microM_Dark_all)<-c("Control","Aripiprazole_1microM","Cariprazine_1microM","Clozapine_1microM",
		"CNO_1microM","Haloperidol_1microM","NDMC_1microM","NDMCHigh_25microM",
		"OSU6162_1microM","PCAP1_1microM","PCAP2_1microM","PCAP814_1microM",
		"PCAP931_1microM")


#euclidean distance
d <- dist(Natural_1microM_Dark_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_Dark.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Natural_1microM_Dark_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_Dark.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Natural_1microM_Dark_all)[drug],"Dark.png"),width=1500,height=750)
	plot(Natural_1microM_Dark_all[c("Control"),122:143],Natural_1microM_Dark_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Natural_1microM_Dark_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Natural_1microM_Dark_all[c("Control"),100:121],Natural_1microM_Dark_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Natural_1microM_Dark_all[c("Control"),78:99],Natural_1microM_Dark_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Natural_1microM_Dark_all[c("Control"),56:77],Natural_1microM_Dark_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Natural_1microM_Dark_all[c("Control"),34:55],Natural_1microM_Dark_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Natural_1microM_Dark_all[c("Control"),12:33],Natural_1microM_Dark_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Natural_1microM_Dark_all[c("Control"),1:11],Natural_1microM_Dark_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}









#DarkApoLow


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_1microM_DarkApoLow<-read.table("Disease_Aripiprazole_1microM_DarkApoLow.txt")
Disease_Cariprazine_1microM_DarkApoLow<-read.table("Disease_Cariprazine_1microM_DarkApoLow.txt")
Disease_Clozapine_1microM_DarkApoLow<-read.table("Disease_Clozapine_1microM_DarkApoLow.txt")
Disease_CNO_1microM_DarkApoLow<-read.table("Disease_CNO_1microM_DarkApoLow.txt")
Disease_Control_DarkApoLow<-read.table("Disease_Control_DarkApoLow.txt")
Disease_Haloperidol_1microM_DarkApoLow<-read.table("Disease_Haloperidol_1microM_DarkApoLow.txt")
Disease_NDMC_1microM_DarkApoLow<-read.table("Disease_NDMC_1microM_DarkApoLow.txt")
Disease_NDMCHigh_25microM_DarkApoLow<-read.table("Disease_NDMCHigh_25microM_DarkApoLow.txt")
Disease_OSU6162_1microM_DarkApoLow<-read.table("Disease_OSU6162_1microM_DarkApoLow.txt")
Disease_PCAP1_1microM_DarkApoLow<-read.table("Disease_PCAP1_1microM_DarkApoLow.txt")
Disease_PCAP2_1microM_DarkApoLow<-read.table("Disease_PCAP2_1microM_DarkApoLow.txt")
Disease_PCAP814_1microM_DarkApoLow<-read.table("Disease_PCAP814_1microM_DarkApoLow.txt")
Disease_PCAP931_1microM_DarkApoLow<-read.table("Disease_PCAP931_1microM_DarkApoLow.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkApoLow<-Disease_Control_DarkApoLow[,c(1,3,2,4:11)]
Disease_Aripiprazole_1microM_DarkApoLow<-Disease_Aripiprazole_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Cariprazine_1microM_DarkApoLow<-Disease_Cariprazine_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Clozapine_1microM_DarkApoLow<-Disease_Clozapine_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_CNO_1microM_DarkApoLow<-Disease_CNO_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_Haloperidol_1microM_DarkApoLow<-Disease_Haloperidol_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_NDMC_1microM_DarkApoLow<-Disease_NDMC_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_NDMCHigh_25microM_DarkApoLow<-Disease_NDMCHigh_25microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_OSU6162_1microM_DarkApoLow<-Disease_OSU6162_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP1_1microM_DarkApoLow<-Disease_PCAP1_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP2_1microM_DarkApoLow<-Disease_PCAP2_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP814_1microM_DarkApoLow<-Disease_PCAP814_1microM_DarkApoLow[,c(1,3,2,4:11)]
Disease_PCAP931_1microM_DarkApoLow<-Disease_PCAP931_1microM_DarkApoLow[,c(1,3,2,4:11)]



Disease_Control_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkApoLow_averaged<-rbind(Disease_Control_DarkApoLow_averaged,
			cbind(aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkApoLow[Disease_Control_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkApoLow_averaged<-aggregate(Disease_Control_DarkApoLow_averaged,by=list(Disease_Control_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_1microM_DarkApoLow_averaged<-rbind(Disease_Aripiprazole_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_1microM_DarkApoLow[Disease_Aripiprazole_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_1microM_DarkApoLow_averaged<-aggregate(Disease_Aripiprazole_1microM_DarkApoLow_averaged,by=list(Disease_Aripiprazole_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_1microM_DarkApoLow_averaged<-rbind(Disease_Cariprazine_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_1microM_DarkApoLow[Disease_Cariprazine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_1microM_DarkApoLow_averaged<-aggregate(Disease_Cariprazine_1microM_DarkApoLow_averaged,by=list(Disease_Cariprazine_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_1microM_DarkApoLow_averaged<-rbind(Disease_Clozapine_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_1microM_DarkApoLow[Disease_Clozapine_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_1microM_DarkApoLow_averaged<-aggregate(Disease_Clozapine_1microM_DarkApoLow_averaged,by=list(Disease_Clozapine_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_1microM_DarkApoLow_averaged<-rbind(Disease_CNO_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_CNO_1microM_DarkApoLow[Disease_CNO_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_CNO_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_1microM_DarkApoLow_averaged<-aggregate(Disease_CNO_1microM_DarkApoLow_averaged,by=list(Disease_CNO_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_1microM_DarkApoLow_averaged<-rbind(Disease_Haloperidol_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_1microM_DarkApoLow[Disease_Haloperidol_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_1microM_DarkApoLow_averaged<-aggregate(Disease_Haloperidol_1microM_DarkApoLow_averaged,by=list(Disease_Haloperidol_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_1microM_DarkApoLow_averaged<-rbind(Disease_NDMC_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_1microM_DarkApoLow[Disease_NDMC_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_1microM_DarkApoLow_averaged<-aggregate(Disease_NDMC_1microM_DarkApoLow_averaged,by=list(Disease_NDMC_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_25microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_25microM_DarkApoLow_averaged<-rbind(Disease_NDMCHigh_25microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_25microM_DarkApoLow[Disease_NDMCHigh_25microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_25microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_25microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_25microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_25microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_25microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_25microM_DarkApoLow_averaged<-aggregate(Disease_NDMCHigh_25microM_DarkApoLow_averaged,by=list(Disease_NDMCHigh_25microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_1microM_DarkApoLow_averaged<-rbind(Disease_OSU6162_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_1microM_DarkApoLow[Disease_OSU6162_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_1microM_DarkApoLow_averaged<-aggregate(Disease_OSU6162_1microM_DarkApoLow_averaged,by=list(Disease_OSU6162_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_1microM_DarkApoLow_averaged<-rbind(Disease_PCAP1_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_1microM_DarkApoLow[Disease_PCAP1_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_1microM_DarkApoLow_averaged<-aggregate(Disease_PCAP1_1microM_DarkApoLow_averaged,by=list(Disease_PCAP1_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_1microM_DarkApoLow_averaged<-rbind(Disease_PCAP2_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_1microM_DarkApoLow[Disease_PCAP2_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_1microM_DarkApoLow_averaged<-aggregate(Disease_PCAP2_1microM_DarkApoLow_averaged,by=list(Disease_PCAP2_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_1microM_DarkApoLow_averaged<-rbind(Disease_PCAP814_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_1microM_DarkApoLow[Disease_PCAP814_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_1microM_DarkApoLow_averaged<-aggregate(Disease_PCAP814_1microM_DarkApoLow_averaged,by=list(Disease_PCAP814_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_1microM_DarkApoLow_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_1microM_DarkApoLow_averaged<-rbind(Disease_PCAP931_1microM_DarkApoLow_averaged,
			cbind(aggregate(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_1microM_DarkApoLow[Disease_PCAP931_1microM_DarkApoLow$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_1microM_DarkApoLow_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_1microM_DarkApoLow_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_1microM_DarkApoLow_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_1microM_DarkApoLow_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_1microM_DarkApoLow_averaged<-aggregate(Disease_PCAP931_1microM_DarkApoLow_averaged,by=list(Disease_PCAP931_1microM_DarkApoLow_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkApoLow_averaged_flat<-c(Disease_Control_DarkApoLow_averaged_flat,Disease_Control_DarkApoLow_averaged[variable,-1])
}

Disease_Aripiprazole_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_1microM_DarkApoLow_averaged_flat<-c(Disease_Aripiprazole_1microM_DarkApoLow_averaged_flat,Disease_Aripiprazole_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_Cariprazine_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_1microM_DarkApoLow_averaged_flat<-c(Disease_Cariprazine_1microM_DarkApoLow_averaged_flat,Disease_Cariprazine_1microM_DarkApoLow_averaged[variable,-1])
}


Disease_Clozapine_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_1microM_DarkApoLow_averaged_flat<-c(Disease_Clozapine_1microM_DarkApoLow_averaged_flat,Disease_Clozapine_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_CNO_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_1microM_DarkApoLow_averaged_flat<-c(Disease_CNO_1microM_DarkApoLow_averaged_flat,Disease_CNO_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_Haloperidol_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_1microM_DarkApoLow_averaged_flat<-c(Disease_Haloperidol_1microM_DarkApoLow_averaged_flat,Disease_Haloperidol_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_NDMC_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_1microM_DarkApoLow_averaged_flat<-c(Disease_NDMC_1microM_DarkApoLow_averaged_flat,Disease_NDMC_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_NDMCHigh_25microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_25microM_DarkApoLow_averaged_flat<-c(Disease_NDMCHigh_25microM_DarkApoLow_averaged_flat,Disease_NDMCHigh_25microM_DarkApoLow_averaged[variable,-1])
}

Disease_OSU6162_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_1microM_DarkApoLow_averaged_flat<-c(Disease_OSU6162_1microM_DarkApoLow_averaged_flat,Disease_OSU6162_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_PCAP1_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_1microM_DarkApoLow_averaged_flat<-c(Disease_PCAP1_1microM_DarkApoLow_averaged_flat,Disease_PCAP1_1microM_DarkApoLow_averaged[variable,-1])
}

Disease_PCAP2_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_1microM_DarkApoLow_averaged_flat<-c(Disease_PCAP2_1microM_DarkApoLow_averaged_flat,Disease_PCAP2_1microM_DarkApoLow_averaged[variable,-1])
}


Disease_PCAP814_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_1microM_DarkApoLow_averaged_flat<-c(Disease_PCAP814_1microM_DarkApoLow_averaged_flat,Disease_PCAP814_1microM_DarkApoLow_averaged[variable,-1])
}


Disease_PCAP931_1microM_DarkApoLow_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_1microM_DarkApoLow_averaged_flat<-c(Disease_PCAP931_1microM_DarkApoLow_averaged_flat,Disease_PCAP931_1microM_DarkApoLow_averaged[variable,-1])
}


Disease_1microM_DarkApoLow_all<-rbind(Disease_Control_DarkApoLow_averaged_flat,Disease_Aripiprazole_1microM_DarkApoLow_averaged_flat, 
		Disease_Cariprazine_1microM_DarkApoLow_averaged_flat, Disease_Clozapine_1microM_DarkApoLow_averaged_flat, Disease_CNO_1microM_DarkApoLow_averaged_flat,
		Disease_Haloperidol_1microM_DarkApoLow_averaged_flat, Disease_NDMC_1microM_DarkApoLow_averaged_flat, Disease_NDMCHigh_25microM_DarkApoLow_averaged_flat,
		Disease_OSU6162_1microM_DarkApoLow_averaged_flat, Disease_PCAP1_1microM_DarkApoLow_averaged_flat, Disease_PCAP2_1microM_DarkApoLow_averaged_flat,
		Disease_PCAP814_1microM_DarkApoLow_averaged_flat, Disease_PCAP931_1microM_DarkApoLow_averaged_flat)

#as numeric
Disease_1microM_DarkApoLow_all<-apply(Disease_1microM_DarkApoLow_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_1microM_DarkApoLow_all<-apply(Disease_1microM_DarkApoLow_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_1microM_DarkApoLow_all)<-c("Control","Aripiprazole_1microM","Cariprazine_1microM","Clozapine_1microM",
		"CNO_1microM","Haloperidol_1microM","NDMC_1microM","NDMCHigh_25microM",
		"OSU6162_1microM","PCAP1_1microM","PCAP2_1microM","PCAP814_1microM",
		"PCAP931_1microM")


#euclidean distance
d <- dist(Disease_1microM_DarkApoLow_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkApoLow.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_1microM_DarkApoLow_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkApoLow.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_1microM_DarkApoLow_all)[drug],"DarkApoLow.png"),width=1500,height=750)
	plot(Disease_1microM_DarkApoLow_all[c("Control"),122:143],Disease_1microM_DarkApoLow_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_1microM_DarkApoLow_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_1microM_DarkApoLow_all[c("Control"),100:121],Disease_1microM_DarkApoLow_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_1microM_DarkApoLow_all[c("Control"),78:99],Disease_1microM_DarkApoLow_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_1microM_DarkApoLow_all[c("Control"),56:77],Disease_1microM_DarkApoLow_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_1microM_DarkApoLow_all[c("Control"),34:55],Disease_1microM_DarkApoLow_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_1microM_DarkApoLow_all[c("Control"),12:33],Disease_1microM_DarkApoLow_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_1microM_DarkApoLow_all[c("Control"),1:11],Disease_1microM_DarkApoLow_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}



#DarkApoHigh


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_1microM_DarkApoHigh<-read.table("Disease_Aripiprazole_1microM_DarkApoHigh.txt")
Disease_Cariprazine_1microM_DarkApoHigh<-read.table("Disease_Cariprazine_1microM_DarkApoHigh.txt")
Disease_Clozapine_1microM_DarkApoHigh<-read.table("Disease_Clozapine_1microM_DarkApoHigh.txt")
Disease_CNO_1microM_DarkApoHigh<-read.table("Disease_CNO_1microM_DarkApoHigh.txt")
Disease_Control_DarkApoHigh<-read.table("Disease_Control_DarkApoHigh.txt")
Disease_Haloperidol_1microM_DarkApoHigh<-read.table("Disease_Haloperidol_1microM_DarkApoHigh.txt")
Disease_NDMC_1microM_DarkApoHigh<-read.table("Disease_NDMC_1microM_DarkApoHigh.txt")
Disease_NDMCHigh_25microM_DarkApoHigh<-read.table("Disease_NDMCHigh_25microM_DarkApoHigh.txt")
Disease_OSU6162_1microM_DarkApoHigh<-read.table("Disease_OSU6162_1microM_DarkApoHigh.txt")
Disease_PCAP1_1microM_DarkApoHigh<-read.table("Disease_PCAP1_1microM_DarkApoHigh.txt")
Disease_PCAP2_1microM_DarkApoHigh<-read.table("Disease_PCAP2_1microM_DarkApoHigh.txt")
Disease_PCAP814_1microM_DarkApoHigh<-read.table("Disease_PCAP814_1microM_DarkApoHigh.txt")
Disease_PCAP931_1microM_DarkApoHigh<-read.table("Disease_PCAP931_1microM_DarkApoHigh.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkApoHigh<-Disease_Control_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Aripiprazole_1microM_DarkApoHigh<-Disease_Aripiprazole_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Cariprazine_1microM_DarkApoHigh<-Disease_Cariprazine_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Clozapine_1microM_DarkApoHigh<-Disease_Clozapine_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_CNO_1microM_DarkApoHigh<-Disease_CNO_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_Haloperidol_1microM_DarkApoHigh<-Disease_Haloperidol_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_NDMC_1microM_DarkApoHigh<-Disease_NDMC_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_NDMCHigh_25microM_DarkApoHigh<-Disease_NDMCHigh_25microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_OSU6162_1microM_DarkApoHigh<-Disease_OSU6162_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP1_1microM_DarkApoHigh<-Disease_PCAP1_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP2_1microM_DarkApoHigh<-Disease_PCAP2_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP814_1microM_DarkApoHigh<-Disease_PCAP814_1microM_DarkApoHigh[,c(1,3,2,4:11)]
Disease_PCAP931_1microM_DarkApoHigh<-Disease_PCAP931_1microM_DarkApoHigh[,c(1,3,2,4:11)]



Disease_Control_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkApoHigh_averaged<-rbind(Disease_Control_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkApoHigh[Disease_Control_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkApoHigh_averaged<-aggregate(Disease_Control_DarkApoHigh_averaged,by=list(Disease_Control_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_1microM_DarkApoHigh_averaged<-rbind(Disease_Aripiprazole_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_1microM_DarkApoHigh[Disease_Aripiprazole_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_1microM_DarkApoHigh_averaged<-aggregate(Disease_Aripiprazole_1microM_DarkApoHigh_averaged,by=list(Disease_Aripiprazole_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_1microM_DarkApoHigh_averaged<-rbind(Disease_Cariprazine_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_1microM_DarkApoHigh[Disease_Cariprazine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_1microM_DarkApoHigh_averaged<-aggregate(Disease_Cariprazine_1microM_DarkApoHigh_averaged,by=list(Disease_Cariprazine_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_1microM_DarkApoHigh_averaged<-rbind(Disease_Clozapine_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_1microM_DarkApoHigh[Disease_Clozapine_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_1microM_DarkApoHigh_averaged<-aggregate(Disease_Clozapine_1microM_DarkApoHigh_averaged,by=list(Disease_Clozapine_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_1microM_DarkApoHigh_averaged<-rbind(Disease_CNO_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_CNO_1microM_DarkApoHigh[Disease_CNO_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_CNO_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_1microM_DarkApoHigh_averaged<-aggregate(Disease_CNO_1microM_DarkApoHigh_averaged,by=list(Disease_CNO_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_1microM_DarkApoHigh_averaged<-rbind(Disease_Haloperidol_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_1microM_DarkApoHigh[Disease_Haloperidol_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_1microM_DarkApoHigh_averaged<-aggregate(Disease_Haloperidol_1microM_DarkApoHigh_averaged,by=list(Disease_Haloperidol_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_1microM_DarkApoHigh_averaged<-rbind(Disease_NDMC_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_1microM_DarkApoHigh[Disease_NDMC_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_1microM_DarkApoHigh_averaged<-aggregate(Disease_NDMC_1microM_DarkApoHigh_averaged,by=list(Disease_NDMC_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_25microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_25microM_DarkApoHigh_averaged<-rbind(Disease_NDMCHigh_25microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_25microM_DarkApoHigh[Disease_NDMCHigh_25microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_25microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_25microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_25microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_25microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_25microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_25microM_DarkApoHigh_averaged<-aggregate(Disease_NDMCHigh_25microM_DarkApoHigh_averaged,by=list(Disease_NDMCHigh_25microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_1microM_DarkApoHigh_averaged<-rbind(Disease_OSU6162_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_1microM_DarkApoHigh[Disease_OSU6162_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_1microM_DarkApoHigh_averaged<-aggregate(Disease_OSU6162_1microM_DarkApoHigh_averaged,by=list(Disease_OSU6162_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_1microM_DarkApoHigh_averaged<-rbind(Disease_PCAP1_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_1microM_DarkApoHigh[Disease_PCAP1_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_1microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP1_1microM_DarkApoHigh_averaged,by=list(Disease_PCAP1_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_1microM_DarkApoHigh_averaged<-rbind(Disease_PCAP2_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_1microM_DarkApoHigh[Disease_PCAP2_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_1microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP2_1microM_DarkApoHigh_averaged,by=list(Disease_PCAP2_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_1microM_DarkApoHigh_averaged<-rbind(Disease_PCAP814_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_1microM_DarkApoHigh[Disease_PCAP814_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_1microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP814_1microM_DarkApoHigh_averaged,by=list(Disease_PCAP814_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_1microM_DarkApoHigh_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_1microM_DarkApoHigh_averaged<-rbind(Disease_PCAP931_1microM_DarkApoHigh_averaged,
			cbind(aggregate(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_1microM_DarkApoHigh[Disease_PCAP931_1microM_DarkApoHigh$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_1microM_DarkApoHigh_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_1microM_DarkApoHigh_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_1microM_DarkApoHigh_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_1microM_DarkApoHigh_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_1microM_DarkApoHigh_averaged<-aggregate(Disease_PCAP931_1microM_DarkApoHigh_averaged,by=list(Disease_PCAP931_1microM_DarkApoHigh_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkApoHigh_averaged_flat<-c(Disease_Control_DarkApoHigh_averaged_flat,Disease_Control_DarkApoHigh_averaged[variable,-1])
}

Disease_Aripiprazole_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_1microM_DarkApoHigh_averaged_flat<-c(Disease_Aripiprazole_1microM_DarkApoHigh_averaged_flat,Disease_Aripiprazole_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_Cariprazine_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_1microM_DarkApoHigh_averaged_flat<-c(Disease_Cariprazine_1microM_DarkApoHigh_averaged_flat,Disease_Cariprazine_1microM_DarkApoHigh_averaged[variable,-1])
}


Disease_Clozapine_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_1microM_DarkApoHigh_averaged_flat<-c(Disease_Clozapine_1microM_DarkApoHigh_averaged_flat,Disease_Clozapine_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_CNO_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_1microM_DarkApoHigh_averaged_flat<-c(Disease_CNO_1microM_DarkApoHigh_averaged_flat,Disease_CNO_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_Haloperidol_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_1microM_DarkApoHigh_averaged_flat<-c(Disease_Haloperidol_1microM_DarkApoHigh_averaged_flat,Disease_Haloperidol_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_NDMC_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_1microM_DarkApoHigh_averaged_flat<-c(Disease_NDMC_1microM_DarkApoHigh_averaged_flat,Disease_NDMC_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_NDMCHigh_25microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_25microM_DarkApoHigh_averaged_flat<-c(Disease_NDMCHigh_25microM_DarkApoHigh_averaged_flat,Disease_NDMCHigh_25microM_DarkApoHigh_averaged[variable,-1])
}

Disease_OSU6162_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_1microM_DarkApoHigh_averaged_flat<-c(Disease_OSU6162_1microM_DarkApoHigh_averaged_flat,Disease_OSU6162_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_PCAP1_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_1microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP1_1microM_DarkApoHigh_averaged_flat,Disease_PCAP1_1microM_DarkApoHigh_averaged[variable,-1])
}

Disease_PCAP2_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_1microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP2_1microM_DarkApoHigh_averaged_flat,Disease_PCAP2_1microM_DarkApoHigh_averaged[variable,-1])
}


Disease_PCAP814_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_1microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP814_1microM_DarkApoHigh_averaged_flat,Disease_PCAP814_1microM_DarkApoHigh_averaged[variable,-1])
}


Disease_PCAP931_1microM_DarkApoHigh_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_1microM_DarkApoHigh_averaged_flat<-c(Disease_PCAP931_1microM_DarkApoHigh_averaged_flat,Disease_PCAP931_1microM_DarkApoHigh_averaged[variable,-1])
}


Disease_1microM_DarkApoHigh_all<-rbind(Disease_Control_DarkApoHigh_averaged_flat,Disease_Aripiprazole_1microM_DarkApoHigh_averaged_flat, 
		Disease_Cariprazine_1microM_DarkApoHigh_averaged_flat, Disease_Clozapine_1microM_DarkApoHigh_averaged_flat, Disease_CNO_1microM_DarkApoHigh_averaged_flat,
		Disease_Haloperidol_1microM_DarkApoHigh_averaged_flat, Disease_NDMC_1microM_DarkApoHigh_averaged_flat, Disease_NDMCHigh_25microM_DarkApoHigh_averaged_flat,
		Disease_OSU6162_1microM_DarkApoHigh_averaged_flat, Disease_PCAP1_1microM_DarkApoHigh_averaged_flat, Disease_PCAP2_1microM_DarkApoHigh_averaged_flat,
		Disease_PCAP814_1microM_DarkApoHigh_averaged_flat, Disease_PCAP931_1microM_DarkApoHigh_averaged_flat)

#as numeric
Disease_1microM_DarkApoHigh_all<-apply(Disease_1microM_DarkApoHigh_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_1microM_DarkApoHigh_all<-apply(Disease_1microM_DarkApoHigh_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_1microM_DarkApoHigh_all)<-c("Control","Aripiprazole_1microM","Cariprazine_1microM","Clozapine_1microM",
		"CNO_1microM","Haloperidol_1microM","NDMC_1microM","NDMCHigh_25microM",
		"OSU6162_1microM","PCAP1_1microM","PCAP2_1microM","PCAP814_1microM",
		"PCAP931_1microM")


#euclidean distance
d <- dist(Disease_1microM_DarkApoHigh_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkApoHigh.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_1microM_DarkApoHigh_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkApoHigh.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_1microM_DarkApoHigh_all)[drug],"DarkApoHigh.png"),width=1500,height=750)
	plot(Disease_1microM_DarkApoHigh_all[c("Control"),122:143],Disease_1microM_DarkApoHigh_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_1microM_DarkApoHigh_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_1microM_DarkApoHigh_all[c("Control"),100:121],Disease_1microM_DarkApoHigh_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_1microM_DarkApoHigh_all[c("Control"),78:99],Disease_1microM_DarkApoHigh_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_1microM_DarkApoHigh_all[c("Control"),56:77],Disease_1microM_DarkApoHigh_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_1microM_DarkApoHigh_all[c("Control"),34:55],Disease_1microM_DarkApoHigh_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_1microM_DarkApoHigh_all[c("Control"),12:33],Disease_1microM_DarkApoHigh_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_1microM_DarkApoHigh_all[c("Control"),1:11],Disease_1microM_DarkApoHigh_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}





#DarkPTZ


#load files for Control and all 12 drugs for 10 microM dosage
Disease_Aripiprazole_1microM_DarkPTZ<-read.table("Disease_Aripiprazole_1microM_DarkPTZ.txt")
Disease_Cariprazine_1microM_DarkPTZ<-read.table("Disease_Cariprazine_1microM_DarkPTZ.txt")
Disease_Clozapine_1microM_DarkPTZ<-read.table("Disease_Clozapine_1microM_DarkPTZ.txt")
Disease_CNO_1microM_DarkPTZ<-read.table("Disease_CNO_1microM_DarkPTZ.txt")
Disease_Control_DarkPTZ<-read.table("Disease_Control_DarkPTZ.txt")
Disease_Haloperidol_1microM_DarkPTZ<-read.table("Disease_Haloperidol_1microM_DarkPTZ.txt")
Disease_NDMC_1microM_DarkPTZ<-read.table("Disease_NDMC_1microM_DarkPTZ.txt")
Disease_NDMCHigh_25microM_DarkPTZ<-read.table("Disease_NDMCHigh_25microM_DarkPTZ.txt")
Disease_OSU6162_1microM_DarkPTZ<-read.table("Disease_OSU6162_1microM_DarkPTZ.txt")
Disease_PCAP1_1microM_DarkPTZ<-read.table("Disease_PCAP1_1microM_DarkPTZ.txt")
Disease_PCAP2_1microM_DarkPTZ<-read.table("Disease_PCAP2_1microM_DarkPTZ.txt")
Disease_PCAP814_1microM_DarkPTZ<-read.table("Disease_PCAP814_1microM_DarkPTZ.txt")
Disease_PCAP931_1microM_DarkPTZ<-read.table("Disease_PCAP931_1microM_DarkPTZ.txt")

#temporal simple solution, taking averages per action sequence and subjects, removing the timepoint variable
Disease_Control_DarkPTZ<-Disease_Control_DarkPTZ[,c(1,3,2,4:11)]
Disease_Aripiprazole_1microM_DarkPTZ<-Disease_Aripiprazole_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Cariprazine_1microM_DarkPTZ<-Disease_Cariprazine_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Clozapine_1microM_DarkPTZ<-Disease_Clozapine_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_CNO_1microM_DarkPTZ<-Disease_CNO_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_Haloperidol_1microM_DarkPTZ<-Disease_Haloperidol_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_NDMC_1microM_DarkPTZ<-Disease_NDMC_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_NDMCHigh_25microM_DarkPTZ<-Disease_NDMCHigh_25microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_OSU6162_1microM_DarkPTZ<-Disease_OSU6162_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP1_1microM_DarkPTZ<-Disease_PCAP1_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP2_1microM_DarkPTZ<-Disease_PCAP2_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP814_1microM_DarkPTZ<-Disease_PCAP814_1microM_DarkPTZ[,c(1,3,2,4:11)]
Disease_PCAP931_1microM_DarkPTZ<-Disease_PCAP931_1microM_DarkPTZ[,c(1,3,2,4:11)]



Disease_Control_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Control_DarkPTZ_averaged<-rbind(Disease_Control_DarkPTZ_averaged,
			cbind(aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Control_DarkPTZ[Disease_Control_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Control_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Control_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Control_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Control_DarkPTZ_averaged<-aggregate(Disease_Control_DarkPTZ_averaged,by=list(Disease_Control_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Aripiprazole_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Aripiprazole_1microM_DarkPTZ_averaged<-rbind(Disease_Aripiprazole_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Aripiprazole_1microM_DarkPTZ[Disease_Aripiprazole_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Aripiprazole_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Aripiprazole_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Aripiprazole_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Aripiprazole_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Aripiprazole_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Aripiprazole_1microM_DarkPTZ_averaged<-aggregate(Disease_Aripiprazole_1microM_DarkPTZ_averaged,by=list(Disease_Aripiprazole_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Cariprazine_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Cariprazine_1microM_DarkPTZ_averaged<-rbind(Disease_Cariprazine_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Cariprazine_1microM_DarkPTZ[Disease_Cariprazine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Cariprazine_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Cariprazine_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Cariprazine_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Cariprazine_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Cariprazine_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Cariprazine_1microM_DarkPTZ_averaged<-aggregate(Disease_Cariprazine_1microM_DarkPTZ_averaged,by=list(Disease_Cariprazine_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_Clozapine_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Clozapine_1microM_DarkPTZ_averaged<-rbind(Disease_Clozapine_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Clozapine_1microM_DarkPTZ[Disease_Clozapine_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Clozapine_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Clozapine_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Clozapine_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Clozapine_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Clozapine_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Clozapine_1microM_DarkPTZ_averaged<-aggregate(Disease_Clozapine_1microM_DarkPTZ_averaged,by=list(Disease_Clozapine_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_CNO_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_CNO_1microM_DarkPTZ_averaged<-rbind(Disease_CNO_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_CNO_1microM_DarkPTZ[Disease_CNO_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_CNO_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_CNO_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_CNO_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_CNO_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_CNO_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_CNO_1microM_DarkPTZ_averaged<-aggregate(Disease_CNO_1microM_DarkPTZ_averaged,by=list(Disease_CNO_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_Haloperidol_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_Haloperidol_1microM_DarkPTZ_averaged<-rbind(Disease_Haloperidol_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_Haloperidol_1microM_DarkPTZ[Disease_Haloperidol_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_Haloperidol_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_Haloperidol_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_Haloperidol_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_Haloperidol_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_Haloperidol_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_Haloperidol_1microM_DarkPTZ_averaged<-aggregate(Disease_Haloperidol_1microM_DarkPTZ_averaged,by=list(Disease_Haloperidol_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]


Disease_NDMC_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMC_1microM_DarkPTZ_averaged<-rbind(Disease_NDMC_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_NDMC_1microM_DarkPTZ[Disease_NDMC_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMC_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMC_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMC_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_NDMC_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMC_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMC_1microM_DarkPTZ_averaged<-aggregate(Disease_NDMC_1microM_DarkPTZ_averaged,by=list(Disease_NDMC_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_NDMCHigh_25microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_NDMCHigh_25microM_DarkPTZ_averaged<-rbind(Disease_NDMCHigh_25microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_NDMCHigh_25microM_DarkPTZ[Disease_NDMCHigh_25microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_NDMCHigh_25microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_NDMCHigh_25microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_NDMCHigh_25microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_NDMCHigh_25microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_NDMCHigh_25microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_NDMCHigh_25microM_DarkPTZ_averaged<-aggregate(Disease_NDMCHigh_25microM_DarkPTZ_averaged,by=list(Disease_NDMCHigh_25microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_OSU6162_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_OSU6162_1microM_DarkPTZ_averaged<-rbind(Disease_OSU6162_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_OSU6162_1microM_DarkPTZ[Disease_OSU6162_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_OSU6162_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_OSU6162_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_OSU6162_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_OSU6162_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_OSU6162_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_OSU6162_1microM_DarkPTZ_averaged<-aggregate(Disease_OSU6162_1microM_DarkPTZ_averaged,by=list(Disease_OSU6162_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP1_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP1_1microM_DarkPTZ_averaged<-rbind(Disease_PCAP1_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP1_1microM_DarkPTZ[Disease_PCAP1_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP1_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP1_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP1_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP1_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP1_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP1_1microM_DarkPTZ_averaged<-aggregate(Disease_PCAP1_1microM_DarkPTZ_averaged,by=list(Disease_PCAP1_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP2_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP2_1microM_DarkPTZ_averaged<-rbind(Disease_PCAP2_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP2_1microM_DarkPTZ[Disease_PCAP2_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP2_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP2_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP2_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP2_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP2_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP2_1microM_DarkPTZ_averaged<-aggregate(Disease_PCAP2_1microM_DarkPTZ_averaged,by=list(Disease_PCAP2_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP814_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP814_1microM_DarkPTZ_averaged<-rbind(Disease_PCAP814_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP814_1microM_DarkPTZ[Disease_PCAP814_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP814_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP814_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP814_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP814_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP814_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP814_1microM_DarkPTZ_averaged<-aggregate(Disease_PCAP814_1microM_DarkPTZ_averaged,by=list(Disease_PCAP814_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]



Disease_PCAP931_1microM_DarkPTZ_averaged<-c()
#first take average for action sequence for each subject for each time frame
for(time_frame in 1:13){
	
	Disease_PCAP931_1microM_DarkPTZ_averaged<-rbind(Disease_PCAP931_1microM_DarkPTZ_averaged,
			cbind(aggregate(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,],
							by=list(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = mean)[,c(-1,-2)],
					aggregate(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,1],
							by=list(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = length)[,2],
					aggregate(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,3],
							by=list(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sum)[,2]))
	
}


#,
#aggregate(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,],
#		by=list(Disease_PCAP931_1microM_DarkPTZ[Disease_PCAP931_1microM_DarkPTZ$TimeFactor==time_frame,1]), FUN = sd)[,c(-1,-2,-3)]

#colnames(Disease_PCAP931_1microM_DarkPTZ_averaged)[c(2:21)]<-c(paste0(colnames(Disease_PCAP931_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
#		"BoutCount","SequenceLength",paste0(colnames(Disease_PCAP931_1microM_DarkPTZ_averaged)[2:10],"_SD"))


colnames(Disease_PCAP931_1microM_DarkPTZ_averaged)[c(2:12)]<-c(paste0(colnames(Disease_PCAP931_1microM_DarkPTZ_averaged)[2:10],"_Mean"),
		"BoutCount","SequenceLength")

Disease_PCAP931_1microM_DarkPTZ_averaged<-aggregate(Disease_PCAP931_1microM_DarkPTZ_averaged,by=list(Disease_PCAP931_1microM_DarkPTZ_averaged$TimeFactor),FUN=mean)[,-1]






#flatten each
Disease_Control_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Control_DarkPTZ_averaged_flat<-c(Disease_Control_DarkPTZ_averaged_flat,Disease_Control_DarkPTZ_averaged[variable,-1])
}

Disease_Aripiprazole_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Aripiprazole_1microM_DarkPTZ_averaged_flat<-c(Disease_Aripiprazole_1microM_DarkPTZ_averaged_flat,Disease_Aripiprazole_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_Cariprazine_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Cariprazine_1microM_DarkPTZ_averaged_flat<-c(Disease_Cariprazine_1microM_DarkPTZ_averaged_flat,Disease_Cariprazine_1microM_DarkPTZ_averaged[variable,-1])
}


Disease_Clozapine_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Clozapine_1microM_DarkPTZ_averaged_flat<-c(Disease_Clozapine_1microM_DarkPTZ_averaged_flat,Disease_Clozapine_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_CNO_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_CNO_1microM_DarkPTZ_averaged_flat<-c(Disease_CNO_1microM_DarkPTZ_averaged_flat,Disease_CNO_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_Haloperidol_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_Haloperidol_1microM_DarkPTZ_averaged_flat<-c(Disease_Haloperidol_1microM_DarkPTZ_averaged_flat,Disease_Haloperidol_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_NDMC_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMC_1microM_DarkPTZ_averaged_flat<-c(Disease_NDMC_1microM_DarkPTZ_averaged_flat,Disease_NDMC_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_NDMCHigh_25microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_NDMCHigh_25microM_DarkPTZ_averaged_flat<-c(Disease_NDMCHigh_25microM_DarkPTZ_averaged_flat,Disease_NDMCHigh_25microM_DarkPTZ_averaged[variable,-1])
}

Disease_OSU6162_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_OSU6162_1microM_DarkPTZ_averaged_flat<-c(Disease_OSU6162_1microM_DarkPTZ_averaged_flat,Disease_OSU6162_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_PCAP1_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP1_1microM_DarkPTZ_averaged_flat<-c(Disease_PCAP1_1microM_DarkPTZ_averaged_flat,Disease_PCAP1_1microM_DarkPTZ_averaged[variable,-1])
}

Disease_PCAP2_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP2_1microM_DarkPTZ_averaged_flat<-c(Disease_PCAP2_1microM_DarkPTZ_averaged_flat,Disease_PCAP2_1microM_DarkPTZ_averaged[variable,-1])
}


Disease_PCAP814_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP814_1microM_DarkPTZ_averaged_flat<-c(Disease_PCAP814_1microM_DarkPTZ_averaged_flat,Disease_PCAP814_1microM_DarkPTZ_averaged[variable,-1])
}


Disease_PCAP931_1microM_DarkPTZ_averaged_flat<-c()
for(variable in 1:13){
	
	Disease_PCAP931_1microM_DarkPTZ_averaged_flat<-c(Disease_PCAP931_1microM_DarkPTZ_averaged_flat,Disease_PCAP931_1microM_DarkPTZ_averaged[variable,-1])
}


Disease_1microM_DarkPTZ_all<-rbind(Disease_Control_DarkPTZ_averaged_flat,Disease_Aripiprazole_1microM_DarkPTZ_averaged_flat, 
		Disease_Cariprazine_1microM_DarkPTZ_averaged_flat, Disease_Clozapine_1microM_DarkPTZ_averaged_flat, Disease_CNO_1microM_DarkPTZ_averaged_flat,
		Disease_Haloperidol_1microM_DarkPTZ_averaged_flat, Disease_NDMC_1microM_DarkPTZ_averaged_flat, Disease_NDMCHigh_25microM_DarkPTZ_averaged_flat,
		Disease_OSU6162_1microM_DarkPTZ_averaged_flat, Disease_PCAP1_1microM_DarkPTZ_averaged_flat, Disease_PCAP2_1microM_DarkPTZ_averaged_flat,
		Disease_PCAP814_1microM_DarkPTZ_averaged_flat, Disease_PCAP931_1microM_DarkPTZ_averaged_flat)

#as numeric
Disease_1microM_DarkPTZ_all<-apply(Disease_1microM_DarkPTZ_all,2,function(x){return(as.numeric(x))})

#log and standardize
Disease_1microM_DarkPTZ_all<-apply(Disease_1microM_DarkPTZ_all,2,function(x){return((x-mean(x))/(sd(x)))})


row.names(Disease_1microM_DarkPTZ_all)<-c("Control","Aripiprazole_1microM","Cariprazine_1microM","Clozapine_1microM",
		"CNO_1microM","Haloperidol_1microM","NDMC_1microM","NDMCHigh_25microM",
		"OSU6162_1microM","PCAP1_1microM","PCAP2_1microM","PCAP814_1microM",
		"PCAP931_1microM")


#euclidean distance
d <- dist(Disease_1microM_DarkPTZ_all)
hc <- hclust(d) 
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Euclidiean_distance_dendogram_sd_DarkPTZ.png",width=1500,height=750)
plot(hc, main="Euclidiean distance dendogram with standardized parameters")
dev.off()

#correlation based distance
res.cor <- cor(t(Disease_1microM_DarkPTZ_all), method = "pearson")
d.cor <- as.dist(1 - res.cor)
png("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Correlation_distance_dendogram_sd_DarkPTZ.png",width=1500,height=750)
plot(hclust(d.cor), main="Correlation based distance dendogram with standardized parameters")
dev.off()

for (drug in 2:13){
	
	png(paste0("~/git/zebrafish_action_sequence_project/results/plots/cluster_analysis/Scatterplot_Control_vs_",row.names(Disease_1microM_DarkPTZ_all)[drug],"DarkPTZ.png"),width=1500,height=750)
	plot(Disease_1microM_DarkPTZ_all[c("Control"),122:143],Disease_1microM_DarkPTZ_all[drug,122:143], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="blue",xlim=c(-6,6),
			ylim=c(-6,6),ylab="Control",xlab=row.names(Disease_1microM_DarkPTZ_all)[drug],main="Scatter plot of all parameters flattened in time")
	points(Disease_1microM_DarkPTZ_all[c("Control"),100:121],Disease_1microM_DarkPTZ_all[drug,100:121], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="orange")
	points(Disease_1microM_DarkPTZ_all[c("Control"),78:99],Disease_1microM_DarkPTZ_all[drug,78:99], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="green")
	points(Disease_1microM_DarkPTZ_all[c("Control"),56:77],Disease_1microM_DarkPTZ_all[drug,56:77], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="red")
	points(Disease_1microM_DarkPTZ_all[c("Control"),34:55],Disease_1microM_DarkPTZ_all[drug,34:55], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="pink")
	points(Disease_1microM_DarkPTZ_all[c("Control"),12:33],Disease_1microM_DarkPTZ_all[drug,12:33], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="cyan")
	points(Disease_1microM_DarkPTZ_all[c("Control"),1:11],Disease_1microM_DarkPTZ_all[drug,1:11], pch = c(0,2,10,3,12,6,6,6,6,1,8),cex=1.6,col="gray")
	lines(-6:6,-6:6,type="l")
	legend(-6, 6, c("Time 55-65 minutes","Time 45-55 minutes","Time 35-45 minutes","Time 25-35 minutes","Time 15-25 minutes",
					"Time 5-15 minutes","Time 0-5 minutes","identity line","Mean bout length","Mean bout count", "Mean sequence length","Scoots proportion", "JBends proportion","CBends proportion","OBends proportion","Routine turn proportion"), cex=1.3, 
			col=c("blue","orange","green","red","pink","cyan", "gray", "black", "black", "black", "black", "black", "black", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,NA,0,1,8,2,10,3,12,6),lty=c(NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA))
	dev.off()
}
