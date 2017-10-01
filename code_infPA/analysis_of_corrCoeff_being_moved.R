#infant_norm windowed correlation coefficients
infant_norm_corr_coef <- read.csv('../proccessed_data/final_analysis/analysis/data/all_infant_norm_corr_coef.csv', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:11700))

#infant_filter windowed correlation coefficients
infant_filter_corr_coef <- read.csv('../proccessed_data/final_analysis/analysis/data/all_infant_filter_corr_coef.csv', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:11700))



#infant_norm being_moved_periods
infant_norm_being_moved_periods <- read.csv('../proccessed_data/being_moved/norm/all_infant_norm_being_moved_period.csv', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:11700))

#infant_filter being_moved_periods
infant_filter_being_moved_periods <- read.csv('../proccessed_data/being_moved/filter/all_infant_filter_being_moved_period.csv', header = FALSE, sep = ",", row.names = NULL, fill=TRUE, col.names=c(1:11700))



norm_being_moved<-c()
norm_not_being_moved<-c()


filter_being_moved<-c()
filter_not_being_moved<-c()

for(subject in 1:30){


	norm_being_moved<-rbind(norm_being_moved,cbind(rep(subject,times=length(infant_norm_corr_coef[subject,na.omit(unlist(infant_norm_being_moved_periods[subject,]))])),
				t(infant_norm_corr_coef[subject,na.omit(unlist(infant_norm_being_moved_periods[subject,]))])))

	norm_not_being_moved<-rbind(norm_not_being_moved,cbind(rep(subject,times=length(infant_norm_corr_coef[subject,-(na.omit(unlist(infant_norm_being_moved_periods[subject,])))])),
				t(infant_norm_corr_coef[subject,-(na.omit(unlist(infant_norm_being_moved_periods[subject,])))])))

	filter_being_moved<-rbind(filter_being_moved,cbind(rep(subject,times=length(infant_filter_corr_coef[subject,na.omit(unlist(infant_filter_being_moved_periods[subject,]))])),
				t(infant_filter_corr_coef[subject,na.omit(unlist(infant_filter_being_moved_periods[subject,]))])))

	filter_not_being_moved<-rbind(filter_not_being_moved,cbind(rep(subject,times=length(infant_filter_corr_coef[subject,-(na.omit(unlist(infant_filter_being_moved_periods[subject,])))])),
				t(infant_filter_corr_coef[subject,-(na.omit(unlist(infant_filter_being_moved_periods[subject,])))])))

}
norm_being_moved<-as.data.frame(norm_being_moved)
norm_not_being_moved<-as.data.frame(norm_not_being_moved)
filter_being_moved<-as.data.frame(filter_being_moved)
filter_not_being_moved<-as.data.frame(filter_not_being_moved)

norm_being_moved[,2]<-abs(norm_being_moved[,2])
norm_not_being_moved[,2]<-abs(norm_not_being_moved[,2])
filter_being_moved[,2]<-abs(filter_being_moved[,2])
filter_not_being_moved[,2]<-abs(filter_not_being_moved[,2])



colnames(norm_being_moved)<-c("Subject","CorrCoef")
colnames(norm_not_being_moved)<-c("Subject","CorrCoef")
colnames(filter_being_moved)<-c("Subject","CorrCoef")
colnames(filter_not_being_moved)<-c("Subject","CorrCoef")


mean_norm_being_moved<-aggregate(CorrCoef~Subject,data=norm_being_moved,FUN=mean)
mean_norm_not_being_moved<-aggregate(CorrCoef~Subject,data=norm_not_being_moved,FUN=mean)
mean_filter_being_moved<-aggregate(CorrCoef~Subject,data=filter_being_moved,FUN=mean)
mean_filter_not_being_moved<-aggregate(CorrCoef~Subject,data=filter_not_being_moved,FUN=mean)

ratio_norm_being_moved<-mean_norm_being_moved$CorrCoef/mean_norm_not_being_moved$CorrCoef
ratio_filter_being_moved<-mean_filter_being_moved$CorrCoef/mean_filter_not_being_moved$CorrCoef


t.test(ratio_filter_being_moved, ratio_norm_being_moved, paired=T)
t.test(ratio_filter_being_moved, ratio_norm_being_moved, paired=F)

mean(ratio_filter_being_moved)
mean(ratio_norm_being_moved)

t.test(mean_filter_being_moved[,2], mean_norm_being_moved[,2], paired=T)
t.test(mean_filter_being_moved[,2], mean_norm_being_moved[,2], paired=F)


t.test(mean_filter_not_being_moved[,2], mean_not_norm_being_moved[,2], paired=F)
t.test(mean_filter_not_being_moved[,2], mean_not_norm_being_moved[,2], paired=T)







