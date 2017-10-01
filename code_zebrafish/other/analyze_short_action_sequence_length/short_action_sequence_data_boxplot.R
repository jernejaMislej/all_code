#read data
#HEALTHY CONTROL

#dark
AllDarkProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllDarkProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllDarkScootProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllDarkScootProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllDarkOBendProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllDarkOBendProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)

#light
AllLightProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightScootProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightScootProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightOBendProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightOBendProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)

#light-dark
AllLightDarkProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightDarkProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkScootProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightDarkScootProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkOBendProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightDarkOBendProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)

#DISEASE CONTROL

#APO low
AllLightApoLowProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightApoLowProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowScootProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightApoLowScootProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowOBendProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightApoLowOBendProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)

#APO high
AllDarkApoHighProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllDarkApoHighProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighScootProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllDarkApoHighScootProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighOBendProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllDarkApoHighOBendProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)

#PTZ
AllLightDarkPTZProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightDarkPTZProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZScootProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightDarkPTZScootProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZOBendProportionOfShort <- read.csv('../../processed_data/processed_data/analyze_short_action_sequence_length/AllLightDarkPTZOBendProportionOfShortData', header = FALSE, sep = ",", row.names = NULL)

ProportionOfShort<-cbind(AllDarkProportionOfShort[,1],AllLightProportionOfShort[,1],AllLightDarkProportionOfShort[,1],AllLightApoLowProportionOfShort[,1],AllDarkApoHighProportionOfShort[,1],AllLightDarkPTZProportionOfShort[,1],AllDarkProportionOfShort[,2],AllLightProportionOfShort[,2],AllLightDarkProportionOfShort[,2],AllLightApoLowProportionOfShort[,2],AllDarkApoHighProportionOfShort[,2],AllLightDarkPTZProportionOfShort[,2],AllDarkProportionOfShort[,3],AllLightProportionOfShort[,3],AllLightDarkProportionOfShort[,3],AllLightApoLowProportionOfShort[,3],AllDarkApoHighProportionOfShort[,3],AllLightDarkPTZProportionOfShort[,3],AllDarkProportionOfShort[,4],AllLightProportionOfShort[,4],AllLightDarkProportionOfShort[,4],AllLightApoLowProportionOfShort[,4],AllDarkApoHighProportionOfShort[,4],AllLightDarkPTZProportionOfShort[,4],AllDarkProportionOfShort[,5],AllLightProportionOfShort[,5],AllLightDarkProportionOfShort[,5],AllLightApoLowProportionOfShort[,5],AllDarkApoHighProportionOfShort[,5],AllLightDarkPTZProportionOfShort[,5])

ScootProportionOfShort<-cbind(AllDarkScootProportionOfShort[,1],AllLightScootProportionOfShort[,1],AllLightDarkScootProportionOfShort[,1],AllLightApoLowScootProportionOfShort[,1],AllDarkApoHighScootProportionOfShort[,1],AllLightDarkPTZScootProportionOfShort[,1],AllDarkScootProportionOfShort[,2],AllLightScootProportionOfShort[,2],AllLightDarkScootProportionOfShort[,2],AllLightApoLowScootProportionOfShort[,2],AllDarkApoHighScootProportionOfShort[,2],AllLightDarkPTZScootProportionOfShort[,2],AllDarkScootProportionOfShort[,3],AllLightScootProportionOfShort[,3],AllLightDarkScootProportionOfShort[,3],AllLightApoLowScootProportionOfShort[,3],AllDarkApoHighScootProportionOfShort[,3],AllLightDarkPTZScootProportionOfShort[,3],AllDarkScootProportionOfShort[,4],AllLightScootProportionOfShort[,4],AllLightDarkScootProportionOfShort[,4],AllLightApoLowScootProportionOfShort[,4],AllDarkApoHighScootProportionOfShort[,4],AllLightDarkPTZScootProportionOfShort[,4])

OBendProportionOfShort<-cbind(AllDarkOBendProportionOfShort[,1],AllLightOBendProportionOfShort[,1],AllLightDarkOBendProportionOfShort[,1],AllLightApoLowOBendProportionOfShort[,1],AllDarkApoHighOBendProportionOfShort[,1],AllLightDarkPTZOBendProportionOfShort[,1],AllDarkOBendProportionOfShort[,2],AllLightOBendProportionOfShort[,2],AllLightDarkOBendProportionOfShort[,2],AllLightApoLowOBendProportionOfShort[,2],AllDarkApoHighOBendProportionOfShort[,2],AllLightDarkPTZOBendProportionOfShort[,2],AllDarkOBendProportionOfShort[,3],AllLightOBendProportionOfShort[,3],AllLightDarkOBendProportionOfShort[,3],AllLightApoLowOBendProportionOfShort[,3],AllDarkApoHighOBendProportionOfShort[,3],AllLightDarkPTZOBendProportionOfShort[,3],AllDarkOBendProportionOfShort[,4],AllLightOBendProportionOfShort[,4],AllLightDarkOBendProportionOfShort[,4],AllLightApoLowOBendProportionOfShort[,4],AllDarkApoHighOBendProportionOfShort[,4],AllLightDarkPTZOBendProportionOfShort[,4])

ControlGroups<-c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ')

ControlGroups2<-c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ','Dark','Light','LightDark','ApoLow','ApoHigh','PTZ')

SeqLengths<-c('=0','=0','=0','=0','=0','=0','<=4','<=4','<=4','<=4','<=4','<=4','<=10','<=10','<=10','<=10','<=10','<=10','<=20','<=20','<=20','<=20','<=20','<=20','<=100','<=100','<=100','<=100','<=100','<=100')

Scoots<-c('<=4','<=4','<=4','<=4','<=4','<=4','<=10','<=10','<=10','<=10','<=10','<=10','<=20','<=20','<=20','<=20','<=20','<=20','<=100','<=100','<=100','<=100','<=100','<=100')

seqLengthsDF<-data.frame(t(ProportionOfShort))
seqLengthsDF$Groups<-ControlGroups
seqLengthsDF$Lengths<-SeqLengths
seqLengthsDF$Groups<-factor(seqLengthsDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
seqLengthsDF$Lengths<-factor(seqLengthsDF$Lengths,c('=0','<=4','<=10','<=20','<=100'))

ScootsDF<-data.frame(t(ScootProportionOfShort))
ScootsDF$Groups<-ControlGroups2
ScootsDF$Lengths<-Scoots
ScootsDF$Groups<-factor(ScootsDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
ScootsDF$Lengths<-factor(ScootsDF$Lengths,c('<=4','<=10','<=20','<=100'))

OBendDF<-data.frame(t(OBendProportionOfShort))
OBendDF$Groups<-ControlGroups2
OBendDF$Lengths<-Scoots
OBendDF$Groups<-factor(OBendDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
OBendDF$Lengths<-factor(OBendDF$Lengths,c('<=4','<=10','<=20','<=100'))

library(reshape2)
stacked.data = melt(seqLengthsDF, id = c('Groups', 'Lengths'))
stacked.data = stacked.data[, -3]

stacked.data2 = melt(ScootsDF, id = c('Groups', 'Lengths'))
stacked.data2 = stacked.data2[, -3]

stacked.data3 = melt(OBendDF, id = c('Groups', 'Lengths'))
stacked.data3 = stacked.data3[, -3]

png('../../results/plots/analyze_short_action_sequence_length/shortSequenceLengthControll.png',width=2500,height=1600)
boxplots = boxplot(value~Groups + Lengths, data = stacked.data, at = c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0,   25, 25.8, 26.6, 27.4, 28.2, 29), xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), ylab="% of sequences")
axis(side=1, at=c(3, 9, 15, 21, 27), labels=c('=0', '<=4', '<=10', '<=20', '<=100'),cex=2.5, line=0.5, lwd=0)
title('Short action sequence lengths for all control groups')
text(c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0,   25, 25.8, 26.6, 27.4, 28.2, 29), c(-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05), c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.5,srt=90)
dev.off()

png('../../results/plots/analyze_short_action_sequence_length/shortSequenceLengthControllScoot.png',width=2500,height=1600)
boxplots = boxplot(value~Groups + Lengths, data = stacked.data2, at = c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0), xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21), labels=c('<=4', '<=10', '<=20', '<=100'),cex=2.5, line=0.5, lwd=0)
title('Proportion of scoots in short action sequences for all control groups')
text(c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0), c(-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05), c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.5,srt=90)
dev.off()

png('../../results/plots/analyze_short_action_sequence_length/shortSequenceLengthControllOBend.png',width=2500,height=1600)
boxplots = boxplot(value~Groups + Lengths, data = stacked.data3, at = c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0), xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21), labels=c('<=4', '<=10', '<=20', '<=100'),cex=2.5, line=0.5, lwd=0)
title('Proportion of O bends in short action sequences for all control groups')
text(c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0), c(-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05), c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.5,srt=90)
dev.off()
