#read data
#HEALTHY CONTROL

#dark
AllDarkLengths <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllDarkLengthsData', header = FALSE, sep = ",", row.names = NULL)
AllDarkMeanLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllDarkMeanLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllDarkNumberOfBouts <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllDarkNumberOfBoutsData', header = FALSE, sep = ",", row.names = NULL)
AllDarkSdLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllDarkSdLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllDarkScootProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkScootProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkOBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkOBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkJBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkJBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkCBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkCBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkEBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkEBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkGBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkGBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkHBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkHBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkIBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkIBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkLeftProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkLeftProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkRightProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkRightProportionData', header = FALSE, sep = ",", row.names = NULL)

#light
AllLightLengths <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllLightLengthsData', header = FALSE, sep = ",", row.names = NULL)
AllLightMeanLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllLightMeanLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightNumberOfBouts <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllLightNumberOfBoutsData', header = FALSE, sep = ",", row.names = NULL)
AllLightSdLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllLightSdLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightScootProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightScootProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightOBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightOBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightJBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightJBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightCBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightCBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightEBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightEBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightGBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightGBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightHBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightHBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightIBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightIBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightLeftProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightLeftProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightRightProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightRightProportionData', header = FALSE, sep = ",", row.names = NULL)

#light-dark
AllLightDarkLengths <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllLightDarkLengthsData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkMeanLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllLightDarkMeanLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkNumberOfBouts <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllLightDarkNumberOfBoutsData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkSdLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllLightDarkSdLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkScootProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkScootProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkOBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkOBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkJBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkJBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkCBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkCBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkEBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkEBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkGBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkGBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkHBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkHBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkIBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkIBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkLeftProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkLeftProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkRightProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkRightProportionData', header = FALSE, sep = ",", row.names = NULL)

#DISEASE CONTROL

#APO low
AllLightApoLowLengths <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllLightApoLowLengthsData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowMeanLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllLightApoLowMeanLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowNumberOfBouts <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllLightApoLowNumberOfBoutsData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowSdLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllLightApoLowSdLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowScootProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowScootProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowOBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowOBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowJBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowJBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowCBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowCBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowEBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowEBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowGBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowGBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowHBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowHBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowIBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowIBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowLeftProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowLeftProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightApoLowRightProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightApoLowRightProportionData', header = FALSE, sep = ",", row.names = NULL)

#APO high
AllDarkApoHighLengths <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllDarkApoHighLengthsData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighMeanLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllDarkApoHighMeanLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighNumberOfBouts <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllDarkApoHighNumberOfBoutsData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighSdLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllDarkApoHighSdLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighScootProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighScootProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighOBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighOBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighJBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighJBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighCBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighCBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighEBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighEBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighGBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighGBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighHBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighHBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighIBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighIBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighLeftProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighLeftProportionData', header = FALSE, sep = ",", row.names = NULL)
AllDarkApoHighRightProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllDarkApoHighRightProportionData', header = FALSE, sep = ",", row.names = NULL)

#PTZ
AllLightDarkPTZLengths <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/action_sequence_length/grouped/AllLightDarkPTZLengthsData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZMeanLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/mean_bout_length_per_timeframe/grouped/AllLightDarkPTZMeanLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZNumberOfBouts <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/number_of_bouts_per_timeframe/grouped/AllLightDarkPTZNumberOfBoutsData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZSdLengthOfBout <- read.csv('../../processed_data/simple_descriptive_statistics/all_conditions/sd_bout_length_per_timeframe/grouped/AllLightDarkPTZSdLengthOfBoutData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZScootProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZScootProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZOBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZOBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZJBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZJBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZCBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZCBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZEBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZEBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZGBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZGBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZHBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZHBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZIBendProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZIBendProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZLeftProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZLeftProportionData', header = FALSE, sep = ",", row.names = NULL)
AllLightDarkPTZRightProportion <- read.csv('../../processed_data/first_simple_analysis_turn_proportion/AllLightDarkPTZRightProportionData', header = FALSE, sep = ",", row.names = NULL)

#combine into dataframe over 5 minute grouping
stat<-0
AllLengths<-matrix(, nrow = 144, ncol = 78)
AllMeanLengthOfBout<-matrix(, nrow = 144, ncol = 78)
AllNumberOfBouts<-matrix(, nrow = 144, ncol = 78)
AllSdLengthOfBout<-matrix(, nrow = 144, ncol = 78)
AllScootProportion<-matrix(, nrow = 144, ncol = 78)
AllOBendProportion<-matrix(, nrow = 144, ncol = 78)
AllJBendProportion<-matrix(, nrow = 144, ncol = 78)
AllCBendProportion<-matrix(, nrow = 144, ncol = 78)
AllEBendProportion<-matrix(, nrow = 144, ncol = 78)
AllGBendProportion<-matrix(, nrow = 144, ncol = 78)
AllHBendProportion<-matrix(, nrow = 144, ncol = 78)
AllIBendProportion<-matrix(, nrow = 144, ncol = 78)
AllLeftProportion<-matrix(, nrow = 144, ncol = 78)
AllRightProportion<-matrix(, nrow = 144, ncol = 78)

AllDarkTurns<-matrix(, nrow = 144, ncol = 104)
AllLightTurns<-matrix(, nrow = 144, ncol = 104)
AllLightDarkTurns<-matrix(, nrow = 144, ncol = 104)
AllDarkApoHighTurns<-matrix(, nrow = 144, ncol = 104)
AllLightApoLowTurns<-matrix(, nrow = 144, ncol = 104)
AllLightDarkPTZTurns<-matrix(, nrow = 144, ncol = 104)


for(recording in 1:13){

	AllLengths[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkLengths[,recording],AllLightLengths[,recording],AllLightDarkLengths[,recording],AllLightApoLowLengths[,recording],AllDarkApoHighLengths[,recording],AllLightDarkPTZLengths[,recording])
	AllMeanLengthOfBout[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkMeanLengthOfBout[,recording],AllLightMeanLengthOfBout[,recording],AllLightDarkMeanLengthOfBout[,recording],AllLightApoLowMeanLengthOfBout[,recording],AllDarkApoHighMeanLengthOfBout[,recording],AllLightDarkPTZMeanLengthOfBout[,recording])
	AllNumberOfBouts[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkNumberOfBouts[,recording],AllLightNumberOfBouts[,recording],AllLightDarkNumberOfBouts[,recording],AllLightApoLowNumberOfBouts[,recording],AllDarkApoHighNumberOfBouts[,recording],AllLightDarkPTZNumberOfBouts[,recording])
	AllSdLengthOfBout[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkSdLengthOfBout[,recording],AllLightSdLengthOfBout[,recording],AllLightDarkSdLengthOfBout[,recording],AllLightApoLowSdLengthOfBout[,recording],AllDarkApoHighSdLengthOfBout[,recording],AllLightDarkPTZSdLengthOfBout[,recording])
	AllScootProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkScootProportion[,recording],AllLightScootProportion[,recording],AllLightDarkScootProportion[,recording],AllLightApoLowScootProportion[,recording],AllDarkApoHighScootProportion[,recording],AllLightDarkPTZScootProportion[,recording])
	AllOBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkOBendProportion[,recording],AllLightOBendProportion[,recording],AllLightDarkOBendProportion[,recording],AllLightApoLowOBendProportion[,recording],AllDarkApoHighOBendProportion[,recording],AllLightDarkPTZOBendProportion[,recording])
	AllJBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkJBendProportion[,recording],AllLightJBendProportion[,recording],AllLightDarkJBendProportion[,recording],AllLightApoLowJBendProportion[,recording],AllDarkApoHighJBendProportion[,recording],AllLightDarkPTZJBendProportion[,recording])
	AllCBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkCBendProportion[,recording],AllLightCBendProportion[,recording],AllLightDarkCBendProportion[,recording],AllLightApoLowCBendProportion[,recording],AllDarkApoHighCBendProportion[,recording],AllLightDarkPTZCBendProportion[,recording])
	AllEBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkEBendProportion[,recording],AllLightEBendProportion[,recording],AllLightDarkEBendProportion[,recording],AllLightApoLowEBendProportion[,recording],AllDarkApoHighEBendProportion[,recording],AllLightDarkPTZEBendProportion[,recording])
	AllGBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkGBendProportion[,recording],AllLightGBendProportion[,recording],AllLightDarkGBendProportion[,recording],AllLightApoLowGBendProportion[,recording],AllDarkApoHighGBendProportion[,recording],AllLightDarkPTZGBendProportion[,recording])
	AllHBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkHBendProportion[,recording],AllLightHBendProportion[,recording],AllLightDarkHBendProportion[,recording],AllLightApoLowHBendProportion[,recording],AllDarkApoHighHBendProportion[,recording],AllLightDarkPTZHBendProportion[,recording])
	AllIBendProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkIBendProportion[,recording],AllLightIBendProportion[,recording],AllLightDarkIBendProportion[,recording],AllLightApoLowIBendProportion[,recording],AllDarkApoHighIBendProportion[,recording],AllLightDarkPTZIBendProportion[,recording])
	AllLeftProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkLeftProportion[,recording],AllLightLeftProportion[,recording],AllLightDarkLeftProportion[,recording],AllLightApoLowLeftProportion[,recording],AllDarkApoHighLeftProportion[,recording],AllLightDarkPTZLeftProportion[,recording])
	AllRightProportion[,(recording+stat*5):(recording+5+stat*5)]<-cbind(AllDarkRightProportion[,recording],AllLightRightProportion[,recording],AllLightDarkRightProportion[,recording],AllLightApoLowRightProportion[,recording],AllDarkApoHighRightProportion[,recording],AllLightDarkPTZRightProportion[,recording])

	AllDarkTurns[,(recording+stat*7):(recording+7+stat*7)]<-cbind(AllDarkScootProportion[,recording], AllDarkOBendProportion[,recording], AllDarkJBendProportion[,recording], AllDarkCBendProportion[,recording], AllDarkEBendProportion[,recording], AllDarkGBendProportion[,recording], AllDarkHBendProportion[,recording], AllDarkIBendProportion[,recording])
	AllLightTurns[,(recording+stat*7):(recording+7+stat*7)]<-cbind(AllLightScootProportion[,recording], AllLightOBendProportion[,recording], AllLightJBendProportion[,recording], AllLightCBendProportion[,recording], AllLightEBendProportion[,recording], AllLightGBendProportion[,recording], AllLightHBendProportion[,recording], AllLightIBendProportion[,recording])
	AllLightDarkTurns[,(recording+stat*7):(recording+7+stat*7)]<-cbind(AllLightDarkScootProportion[,recording], AllLightDarkOBendProportion[,recording], AllLightDarkJBendProportion[,recording], AllLightDarkCBendProportion[,recording], AllLightDarkEBendProportion[,recording], AllLightDarkGBendProportion[,recording], AllLightDarkHBendProportion[,recording], AllLightDarkIBendProportion[,recording])
	AllLightApoLowTurns[,(recording+stat*7):(recording+7+stat*7)]<-cbind(AllLightApoLowScootProportion[,recording], AllLightApoLowOBendProportion[,recording], AllLightApoLowJBendProportion[,recording], AllLightApoLowCBendProportion[,recording], AllLightApoLowEBendProportion[,recording], AllLightApoLowGBendProportion[,recording], AllLightApoLowHBendProportion[,recording], AllLightApoLowIBendProportion[,recording])
	AllDarkApoHighTurns[,(recording+stat*7):(recording+7+stat*7)]<-cbind(AllDarkApoHighScootProportion[,recording], AllDarkApoHighOBendProportion[,recording], AllDarkApoHighJBendProportion[,recording], AllDarkApoHighCBendProportion[,recording], AllDarkApoHighEBendProportion[,recording], AllDarkApoHighGBendProportion[,recording], AllDarkApoHighHBendProportion[,recording], AllDarkApoHighIBendProportion[,recording])
	AllLightDarkPTZTurns[,(recording+stat*7):(recording+7+stat*7)]<-cbind(AllLightDarkPTZScootProportion[,recording], AllLightDarkPTZOBendProportion[,recording], AllLightDarkPTZJBendProportion[,recording], AllLightDarkPTZCBendProportion[,recording], AllLightDarkPTZEBendProportion[,recording], AllLightDarkPTZGBendProportion[,recording], AllLightDarkPTZHBendProportion[,recording], AllLightDarkPTZIBendProportion[,recording])

	stat<-stat+1
}

ControlGroups<-rep(c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),times=13)

TurnGroups<-rep(c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),times=13)

TimeGroups<-rep(c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),each=6)

TimeGroups2<-rep(c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),each=8)

AllLengths[is.na(AllLengths)] <- 0
AllMeanLengthOfBout[is.na(AllMeanLengthOfBout)] <- 0
AllNumberOfBouts[is.na(AllNumberOfBouts)] <- 0
AllSdLengthOfBout[is.na(AllSdLengthOfBout)] <- 0
AllScootProportion[is.na(AllScootProportion)] <- 0
AllOBendProportion[is.na(AllOBendProportion)] <- 0
AllJBendProportion[is.na(AllJBendProportion)] <- 0
AllCBendProportion[is.na(AllCBendProportion)] <- 0
AllEBendProportion[is.na(AllEBendProportion)] <- 0
AllGBendProportion[is.na(AllGBendProportion)] <- 0
AllHBendProportion[is.na(AllHBendProportion)] <- 0
AllIBendProportion[is.na(AllIBendProportion)] <- 0
AllLeftProportion[is.na(AllLeftProportion)] <- 0
AllRightProportion[is.na(AllRightProportion)] <- 0


AllLengthsDF<-data.frame(t(AllLengths))
AllLengthsDF$Groups<-ControlGroups
AllLengthsDF$Time<-TimeGroups
AllLengthsDF$Groups<-factor(AllLengthsDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllLengthsDF$Time<-factor(AllLengthsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllMeanLengthOfBoutDF<-data.frame(t(AllMeanLengthOfBout))
AllMeanLengthOfBoutDF$Groups<-ControlGroups
AllMeanLengthOfBoutDF$Time<-TimeGroups
AllMeanLengthOfBoutDF$Groups<-factor(AllMeanLengthOfBoutDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllMeanLengthOfBoutDF$Time<-factor(AllMeanLengthOfBoutDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllNumberOfBoutsDF<-data.frame(t(AllNumberOfBouts))
AllNumberOfBoutsDF$Groups<-ControlGroups
AllNumberOfBoutsDF$Time<-TimeGroups
AllNumberOfBoutsDF$Groups<-factor(AllNumberOfBoutsDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllNumberOfBoutsDF$Time<-factor(AllNumberOfBoutsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllSdLengthOfBoutDF<-data.frame(t(AllSdLengthOfBout))
AllSdLengthOfBoutDF$Groups<-ControlGroups
AllSdLengthOfBoutDF$Time<-TimeGroups
AllSdLengthOfBoutDF$Groups<-factor(AllSdLengthOfBoutDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllSdLengthOfBoutDF$Time<-factor(AllSdLengthOfBoutDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllScootProportionDF<-data.frame(t(AllScootProportion))
AllScootProportionDF$Groups<-ControlGroups
AllScootProportionDF$Time<-TimeGroups
AllScootProportionDF$Groups<-factor(AllScootProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllScootProportionDF$Time<-factor(AllScootProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllOBendProportionDF<-data.frame(t(AllOBendProportion))
AllOBendProportionDF$Groups<-ControlGroups
AllOBendProportionDF$Time<-TimeGroups
AllOBendProportionDF$Groups<-factor(AllOBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllOBendProportionDF$Time<-factor(AllOBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllJBendProportionDF<-data.frame(t(AllJBendProportion))
AllJBendProportionDF$Groups<-ControlGroups
AllJBendProportionDF$Time<-TimeGroups
AllJBendProportionDF$Groups<-factor(AllJBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllJBendProportionDF$Time<-factor(AllJBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllCBendProportionDF<-data.frame(t(AllCBendProportion))
AllCBendProportionDF$Groups<-ControlGroups
AllCBendProportionDF$Time<-TimeGroups
AllCBendProportionDF$Groups<-factor(AllCBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllCBendProportionDF$Time<-factor(AllCBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllEBendProportionDF<-data.frame(t(AllEBendProportion))
AllEBendProportionDF$Groups<-ControlGroups
AllEBendProportionDF$Time<-TimeGroups
AllEBendProportionDF$Groups<-factor(AllEBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllEBendProportionDF$Time<-factor(AllEBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllGBendProportionDF<-data.frame(t(AllGBendProportion))
AllGBendProportionDF$Groups<-ControlGroups
AllGBendProportionDF$Time<-TimeGroups
AllGBendProportionDF$Groups<-factor(AllGBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllGBendProportionDF$Time<-factor(AllGBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllHBendProportionDF<-data.frame(t(AllHBendProportion))
AllHBendProportionDF$Groups<-ControlGroups
AllHBendProportionDF$Time<-TimeGroups
AllHBendProportionDF$Groups<-factor(AllHBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllHBendProportionDF$Time<-factor(AllHBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllIBendProportionDF<-data.frame(t(AllIBendProportion))
AllIBendProportionDF$Groups<-ControlGroups
AllIBendProportionDF$Time<-TimeGroups
AllIBendProportionDF$Groups<-factor(AllIBendProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllIBendProportionDF$Time<-factor(AllIBendProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllLeftProportionDF<-data.frame(t(AllLeftProportion))
AllLeftProportionDF$Groups<-ControlGroups
AllLeftProportionDF$Time<-TimeGroups
AllLeftProportionDF$Groups<-factor(AllLeftProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllLeftProportionDF$Time<-factor(AllLeftProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllRightProportionDF<-data.frame(t(AllRightProportion))
AllRightProportionDF$Groups<-ControlGroups
AllRightProportionDF$Time<-TimeGroups
AllRightProportionDF$Groups<-factor(AllRightProportionDF$Groups,c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'))
AllRightProportionDF$Time<-factor(AllRightProportionDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllDarkTurnsDF<-data.frame(t(AllDarkTurns))
AllDarkTurnsDF$Groups<-TurnGroups
AllDarkTurnsDF$Time<-TimeGroups2
AllDarkTurnsDF$Groups<-factor(AllDarkTurnsDF$Groups,c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'))
AllDarkTurnsDF$Time<-factor(AllDarkTurnsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllLightTurnsDF<-data.frame(t(AllLightTurns))
AllLightTurnsDF$Groups<-TurnGroups
AllLightTurnsDF$Time<-TimeGroups2
AllLightTurnsDF$Groups<-factor(AllLightTurnsDF$Groups,c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'))
AllLightTurnsDF$Time<-factor(AllLightTurnsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllLightDarkTurnsDF<-data.frame(t(AllLightDarkTurns))
AllLightDarkTurnsDF$Groups<-TurnGroups
AllLightDarkTurnsDF$Time<-TimeGroups2
AllLightDarkTurnsDF$Groups<-factor(AllLightDarkTurnsDF$Groups,c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'))
AllLightDarkTurnsDF$Time<-factor(AllLightDarkTurnsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllLightApoLowTurnsDF<-data.frame(t(AllLightApoLowTurns))
AllLightApoLowTurnsDF$Groups<-TurnGroups
AllLightApoLowTurnsDF$Time<-TimeGroups2
AllLightApoLowTurnsDF$Groups<-factor(AllLightApoLowTurnsDF$Groups,c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'))
AllLightApoLowTurnsDF$Time<-factor(AllLightApoLowTurnsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllDarkApoHighTurnsDF<-data.frame(t(AllDarkApoHighTurns))
AllDarkApoHighTurnsDF$Groups<-TurnGroups
AllDarkApoHighTurnsDF$Time<-TimeGroups2
AllDarkApoHighTurnsDF$Groups<-factor(AllDarkApoHighTurnsDF$Groups,c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'))
AllDarkApoHighTurnsDF$Time<-factor(AllDarkApoHighTurnsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

AllLightDarkPTZTurnsDF<-data.frame(t(AllLightDarkPTZTurns))
AllLightDarkPTZTurnsDF$Groups<-TurnGroups
AllLightDarkPTZTurnsDF$Time<-TimeGroups2
AllLightDarkPTZTurnsDF$Groups<-factor(AllLightDarkPTZTurnsDF$Groups,c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'))
AllLightDarkPTZTurnsDF$Time<-factor(AllLightDarkPTZTurnsDF$Time,c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'))

library(reshape2)
AllLengthsStacked = melt(AllLengthsDF, id = c('Groups', 'Time'))
AllLengthsStacked = AllLengthsStacked[, -3]

AllMeanLengthOfBoutStacked = melt(AllMeanLengthOfBoutDF, id = c('Groups', 'Time'))
AllMeanLengthOfBoutStacked = AllMeanLengthOfBoutStacked[, -3]

AllNumberOfBoutsStacked = melt(AllNumberOfBoutsDF, id = c('Groups', 'Time'))
AllNumberOfBoutsStacked = AllNumberOfBoutsStacked[, -3]

AllSdLengthOfBoutStacked = melt(AllSdLengthOfBoutDF, id = c('Groups', 'Time'))
AllSdLengthOfBoutStacked = AllSdLengthOfBoutStacked[, -3]

AllScootProportionStacked = melt(AllScootProportionDF, id = c('Groups', 'Time'))
AllScootProportionStacked = AllScootProportionStacked[, -3]

AllJBendProportionStacked = melt(AllJBendProportionDF, id = c('Groups', 'Time'))
AllJBendProportionStacked = AllJBendProportionStacked[, -3]

AllCBendProportionStacked = melt(AllCBendProportionDF, id = c('Groups', 'Time'))
AllCBendProportionStacked = AllCBendProportionStacked[, -3]

AllOBendProportionStacked = melt(AllOBendProportionDF, id = c('Groups', 'Time'))
AllOBendProportionStacked = AllOBendProportionStacked[, -3]

AllEBendProportionStacked = melt(AllEBendProportionDF, id = c('Groups', 'Time'))
AllEBendProportionStacked = AllEBendProportionStacked[, -3]

AllGBendProportionStacked = melt(AllGBendProportionDF, id = c('Groups', 'Time'))
AllGBendProportionStacked = AllGBendProportionStacked[, -3]

AllHBendProportionStacked = melt(AllHBendProportionDF, id = c('Groups', 'Time'))
AllHBendProportionStacked = AllHBendProportionStacked[, -3]

AllIBendProportionStacked = melt(AllIBendProportionDF, id = c('Groups', 'Time'))
AllIBendProportionStacked = AllIBendProportionStacked[, -3]

AllRightProportionStacked = melt(AllRightProportionDF, id = c('Groups', 'Time'))
AllRightProportionStacked = AllRightProportionStacked[, -3]

AllLeftProportionStacked = melt(AllLeftProportionDF, id = c('Groups', 'Time'))
AllLeftProportionStacked = AllLeftProportionStacked[, -3]

AllDarkTurnsStacked = melt(AllDarkTurnsDF, id = c('Groups', 'Time'))
AllDarkTurnsStacked = AllDarkTurnsStacked[, -3]

AllLightTurnsStacked = melt(AllLightTurnsDF, id = c('Groups', 'Time'))
AllLightTurnsStacked = AllLightTurnsStacked[, -3]

AllLightDarkTurnsStacked = melt(AllLightDarkTurnsDF, id = c('Groups', 'Time'))
AllLightDarkTurnsStacked = AllLightDarkTurnsStacked[, -3]

AllLightApoLowTurnsStacked = melt(AllLightApoLowTurnsDF, id = c('Groups', 'Time'))
AllLightApoLowTurnsStacked = AllLightApoLowTurnsStacked[, -3]

AllDarkApoHighTurnsStacked = melt(AllDarkApoHighTurnsDF, id = c('Groups', 'Time'))
AllDarkApoHighTurnsStacked = AllDarkApoHighTurnsStacked[, -3]

AllLightDarkPTZTurnsStacked = melt(AllLightDarkPTZTurnsDF, id = c('Groups', 'Time'))
AllLightDarkPTZTurnsStacked = AllLightDarkPTZTurnsStacked[, -3]

marks<-c(1, 1.8, 2.6, 3.4, 4.2, 5.0,   7, 7.8, 8.6, 9.4, 10.2, 11.0,   13, 13.8, 14.6, 15.4, 16.2, 17.0,   19, 19.8, 20.6, 21.4, 22.2, 23.0,   25, 25.8, 26.6, 27.4, 28.2, 29)
marks<-c(marks,marks+30,marks+60)
marks<-marks[1:78]

marks2<-c(1, 1.8, 2.6, 3.4, 4.2, 5.0, 5.8, 6.6)
marks2<-c(marks2,marks2+8,marks2+16,marks2+24,marks2+32,marks2+40,marks2+48,marks2+56,marks2+64,marks2+72,marks2+80,marks2+88,marks2+96)

textPos<-rep(-100,times=78)
png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceLengthControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllLengthsStacked, at = marks, xaxt='n', ylim = c(-100, max(AllLengths)), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="sequence length")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Action sequence length through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

textPos<-rep(0.5,times=78)
png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceMeanLengthOfBoutControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllMeanLengthOfBoutStacked, at = marks, xaxt='n', ylim = c(0, max(AllMeanLengthOfBout)), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="Mean length of bout")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Action sequence mean length of bout through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

textPos<-rep(-0.5,times=78)
png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceSdLengthOfBoutControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllSdLengthOfBoutStacked, at = marks, xaxt='n', ylim = c(-0.5, max(AllSdLengthOfBout)), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="SD of bout lengths")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Action sequence SD of bout lengths through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

textPos<-rep(-25,times=78)
png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceNumberOfBoutsControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllNumberOfBoutsStacked, at = marks, xaxt='n', ylim = c(-25, max(AllNumberOfBouts)), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="Number of bouts")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Number of bouts through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

textPos<-rep(-0.05,times=78)
png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceScootProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllScootProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'ScootProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceOBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllOBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'OBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceJBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllJBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'JBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceCBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllCBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'CBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceEBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllEBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'EBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceGBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllGBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'GBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceHBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllHBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'HBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceIBendProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllIBendProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'IBendProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceLeftProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllLeftProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'LeftProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedSequenceRightProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllRightProportionStacked, at = marks, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'RightProportion through the experiment')
text(marks, textPos, c('Dark','Light','LightDark','ApoLow','ApoHigh','PTZ'),cex=1.8,srt=90)
dev.off()

textPos<-rep(-0.05,times=104)
png('../../results/plots/first_simple_analysis_turn_proportion/TimedDarkTurnsProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllDarkTurnsStacked, at = marks2, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange', 'red', 'magenta'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3.8, 11.8, 19.8, 27.8, 35.8, 43.8, 51.8, 59.8, 67.8, 75.8, 83.8, 91.8, 99.8), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Proportion of turn in dark control through the experiment')
text(marks2, textPos, c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedLightTurnsProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllLightTurnsStacked, at = marks2, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange', 'red', 'magenta'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3.8, 11.8, 19.8, 27.8, 35.8, 43.8, 51.8, 59.8, 67.8, 75.8, 83.8, 91.8, 99.8), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Proportion of turn in Light control through the experiment')
text(marks2, textPos, c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedLightDarkTurnsProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllLightDarkTurnsStacked, at = marks2, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange', 'red', 'magenta'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3.8, 11.8, 19.8, 27.8, 35.8, 43.8, 51.8, 59.8, 67.8, 75.8, 83.8, 91.8, 99.8), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Proportion of turn in LightDark control through the experiment')
text(marks2, textPos, c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedLightApoLowTurnsProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllLightApoLowTurnsStacked, at = marks2, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange', 'red', 'magenta'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3.8, 11.8, 19.8, 27.8, 35.8, 43.8, 51.8, 59.8, 67.8, 75.8, 83.8, 91.8, 99.8), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Proportion of turn in LightApoLow control through the experiment')
text(marks2, textPos, c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedDarkApoHighTurnsProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllDarkApoHighTurnsStacked, at = marks2, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange', 'red', 'magenta'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3.8, 11.8, 19.8, 27.8, 35.8, 43.8, 51.8, 59.8, 67.8, 75.8, 83.8, 91.8, 99.8), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Proportion of turn in DarkApoHigh control through the experiment')
text(marks2, textPos, c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),cex=1.8,srt=90)
dev.off()

png('../../results/plots/first_simple_analysis_turn_proportion/TimedLightDarkPTZTurnsProportionControl.png',width=4000,height=2000)
boxplots = boxplot(value~Groups + Time, data = AllLightDarkPTZTurnsStacked, at = marks2, xaxt='n', ylim = c(-0.05, 1), col = c('yellow', 'green', 'gray', 'blue', 'pink' ,'orange', 'red', 'magenta'), cex.lab=2, cex.axis=2, ylab="% of turns")
axis(side=1, at=c(3.8, 11.8, 19.8, 27.8, 35.8, 43.8, 51.8, 59.8, 67.8, 75.8, 83.8, 91.8, 99.8), labels=c('0-5 minutes','5-10 minutes','10-15 minutes','15-20 minutes','20-25 minutes','25-30 minutes','30-35 minutes','35-40 minutes','40-45 minutes','45-50 minutes','50-55 minutes','55-60 minutes','60-65 minutes'),cex.axis=2.5,  line=0.5, lwd=0)
title(cex.main=3,'Proportion of turn in LightDarkPTZ control through the experiment')
text(marks2, textPos, c('Scoot','O Bend', 'J Bend', 'C Bend', 'E Bend', 'G Bend', 'H Bend', 'I Bend'),cex=1.8,srt=90)
dev.off()
