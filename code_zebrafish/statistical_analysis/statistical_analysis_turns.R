library(car)

Natural_Control_Light<-read.table("Natural_Control_Light.txt")

#taking counts instead of proportions
Natural_Control_Light[,4:11]<-apply(Natural_Control_Light[,4:11],2,function(x){return(x*Natural_Control_Light$BoutLength)})
colnames(Natural_Control_Light)[4:11]<-paste0(substr(colnames(Natural_Control_Light)[4:11],1,6),"Counts")

#subject is factor
Natural_Control_Light$Subject<-as.factor(Natural_Control_Light$Subject)

attach(Natural_Control_Light)

#check distributions
hist(Natural_Control_Light$BoutLength)
hist(Natural_Control_Light$ScootsCounts)

#taking the bout length and time factor
associations<-glm(ScootsCounts~TimeFactor+BoutLength, family = poisson(link = "log"))

summary(associations)
vif(associations)#seems ok

#considering that time factor will influence the bout length
associations<-glm(ScootsCounts~TimeFactor+BoutLength+TimeFactor:BoutLength, family = poisson(link = "log"))

summary(associations)
vif(associations)#not ok

#not sure if the interaction of the time on the bout length needs to be added, since the influence is not direct, but indirect through bout length and that is already included




#considering the random subject, not sure how to do this, in the control group there are 144 subjects, which is a random variable, the slope of bout length
#per time frame can be random for subject, also the scoots per time frame can be random per subject, scoots per bout length could also be random per subject

associations<-glmer(ScootsCounts~TimeFactor+BoutLength+(1|Subject), family = poisson(link = "log"))

summary(associations)



#considering the random subject, but specify that the results from exposure in time is random
associations<-glmer(ScootsCounts~TimeFactor+BoutLength+(1|Subject/TimeFactor), family = poisson(link = "log"))

summary(associations)




#considering the random subject, but restrict that the results from exposure in time is random and add the interaction
associations<-glmer(ScootsCounts~TimeFactor+BoutLength+TimeFactor:BoutLength+(1|Subject/TimeFactor), family = poisson(link = "log"))

summary(associations)




#considering the random subject, but restrict that the results from exposure in time is random, add the influence of time on the bout length
associations<-glmer(ScootsCounts~TimeFactor+BoutLength+(1+BoutLength|Subject/TimeFactor), family = poisson(link = "log"))

summary(associations)




#considering the random subject, but restrict that the results from exposure in time is random, add the influence of time on the bout length plus the interaction effect
associations<-glmer(ScootsCounts~TimeFactor+BoutLength+TimeFactor:BoutLength+(1+BoutLength|Subject/TimeFactor), family = poisson(link = "log"))

summary(associations)


# seems as if the above models want to use subject as variable interacting or being influenced by others and it is not what I want to do, maybe this is better, but the change
# in time for bout length should be random within the subject, not sure if this will be ok like this
associations<-glmer(ScootsCounts~TimeFactor+BoutLength+(1|TimeFactor/BoutLength)+(1|Subject), family = poisson(link = "log"))

summary(associations)






#add the inner time point

associations<-glm(ScootsCounts~TimeFactor+BoutLength+TimePoint, family = poisson(link = "log"))

summary(associations)
vif(associations)#ok


associations<-glmer(ScootsCounts~TimeFactor+BoutLength+TimePoint+(1|Subject), family = poisson(link = "log"))

summary(associations)



#Light should not change much in time apart from habituation, which mostly reflects in number of bouts, in Dark, the fish will fall a sleep, so change in time is substantial
associations<-glm(BoutLength~TimeFactor+TimePoint, family = poisson(link = "log"))

summary(associations)
vif(associations)#ok

Natural_Control_Dark<-read.table("Natural_Control_Dark.txt")

#taking counts instead of proportions
Natural_Control_Dark[,4:11]<-apply(Natural_Control_Dark[,4:11],2,function(x){return(x*Natural_Control_Dark$BoutLength)})
colnames(Natural_Control_Dark)[4:11]<-paste0(substr(colnames(Natural_Control_Dark)[4:11],1,6),"Counts")

#subject is factor
Natural_Control_Dark$Subject<-as.factor(Natural_Control_Dark$Subject)

detach(Natural_Control_Light)
attach(Natural_Control_Dark)

#check distributions
hist(Natural_Control_Dark$BoutLength)
hist(Natural_Control_Dark$ScootsCounts)

#check just the bout length
associations<-glm(BoutLength~TimeFactor+TimePoint, family = poisson(link = "log"))
summary(associations)
vif(associations)#ok


#taking the bout length and time factor
associations<-glm(ScootsCounts~TimeFactor+BoutLength, family = poisson(link = "log"))

summary(associations)
vif(associations)#seems ok

#considering that time factor will influence the bout length
associations<-glm(ScootsCounts~TimeFactor+BoutLength+TimeFactor:BoutLength, family = poisson(link = "log"))

summary(associations)
vif(associations)#better than light



#check PTZ with more variability in time



Disease_Control_DarkPTZ<-read.table("Disease_Control_DarkPTZ.txt")

#taking counts instead of proportions
Disease_Control_DarkPTZ[,4:11]<-apply(Disease_Control_DarkPTZ[,4:11],2,function(x){return(x*Disease_Control_DarkPTZ$BoutLength)})
colnames(Disease_Control_DarkPTZ)[4:11]<-paste0(substr(colnames(Disease_Control_DarkPTZ)[4:11],1,6),"Counts")

#subject is factor
Disease_Control_DarkPTZ$Subject<-as.factor(Disease_Control_DarkPTZ$Subject)

detach(Disease_Control_Dark)
attach(Disease_Control_DarkPTZ)

#check distributions
hist(Disease_Control_DarkPTZ$BoutLength)
hist(Disease_Control_DarkPTZ$ScootsCounts)

#check just the bout length
associations<-glm(BoutLength~TimeFactor+TimePoint, family = poisson(link = "log"))
summary(associations)
vif(associations)#ok


#taking the bout length and time factor
associations<-glm(ScootsCounts~TimeFactor+BoutLength, family = poisson(link = "log"))

summary(associations)
vif(associations)#seems ok

#considering that time factor will influence the bout length
associations<-glm(ScootsCounts~TimeFactor+BoutLength+TimeFactor:BoutLength, family = poisson(link = "log"))

summary(associations)
vif(associations)#better than light, but worse than dark

boxplot(BoutLength~TimeFactor)


