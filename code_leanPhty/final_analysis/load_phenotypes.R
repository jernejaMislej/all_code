#visit1


#-------------resistance_continuous-------------------
VIP_data_subset_visit1_complete_cases$resistance_continuous<-residuals_visit1
#-------------resistance_continuous-------------------

#-------------resistance_case_control_susceptible-------------------
empty_vector<-vector()
length(empty_vector)<-length(VIP_data_subset_visit1_complete_cases[,1])
empty_vector[]<-"NA"
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible<-empty_vector
#susceptible
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
					| (predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==2)) & VIP_data_subset_visit1_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-0
#resistant
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
					| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0)) & VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1

#-------------resistance_case_control_susceptible-------------------


#-------------resistance_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#resistant
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant[((predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
					| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==0)) & VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------resistance_case_control_compliant-------------------

#-------------susceptible_case_control_compliant-------------------
VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant[(predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==0)
				| (predicted_categorized_visit1==1 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
				| (predicted_categorized_visit1==2 & VIP_data_subset_visit1_complete_cases$bmi_category==2)]<-0
#susceptible
VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant[((predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==1)
					| (predicted_categorized_visit1==0 & VIP_data_subset_visit1_complete_cases$bmi_category==2))  & VIP_data_subset_visit1_complete_cases$sm_status!=2 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------susceptible_case_control_compliant-------------------

#visit2
#-------------resistance_case_control_susceptible-------------------
empty_vector<-vector()
length(empty_vector)<-length(VIP_data_subset_visit2_complete_cases[,1])
empty_vector[]<-"NA"
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible<-empty_vector
#susceptible
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
					| (predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==2)) & VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-0
#resistant
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible[((predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
					| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0)) & VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1

#-------------resistance_case_control_susceptible-------------------


#-------------resistance_continuous-------------------
VIP_data_subset_visit2_complete_cases$resistance_continuous<-residuals_visit2
#-------------resistance_continuous-------------------


#-------------resistance_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#resistant
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant[((predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
					| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==0)) & VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------resistance_case_control_compliant-------------------

#-------------susceptible_case_control_compliant-------------------
VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant[(predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==0)
				| (predicted_categorized_visit2==1 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
				| (predicted_categorized_visit2==2 & VIP_data_subset_visit2_complete_cases$bmi_category==2)]<-0
#susceptible
VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant[((predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==1)
					| (predicted_categorized_visit2==0 & VIP_data_subset_visit2_complete_cases$bmi_category==2))  & VIP_data_subset_visit2_complete_cases$sm_status!=2 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------susceptible_case_control_compliant-------------------


#save the data and enummers for the persistant ones

#resistance_case_control_susceptible
resistance_case_control_susceptible_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible) & VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible==0
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible==0]
resistance_case_control_susceptible_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible) & VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible==1
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible==1]
resistance_case_control_susceptible_enummers<-c(resistance_case_control_susceptible_enummers_0,resistance_case_control_susceptible_enummers_1)
resistance_case_control_susceptible_data<-VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible
resistance_case_control_susceptible_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% resistance_case_control_susceptible_enummers)]<-"NA"

#resistance_case_control_compliant
resistance_case_control_compliant_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant) & VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant==0
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant==0]
resistance_case_control_compliant_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant) & VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant==1
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant==1]
resistance_case_control_compliant_enummers<-c(resistance_case_control_compliant_enummers_0,resistance_case_control_compliant_enummers_1)
resistance_case_control_compliant_data<-VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant
resistance_case_control_compliant_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% resistance_case_control_compliant_enummers)]<-"NA"

#susceptible_case_control_compliant
susceptible_case_control_compliant_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant) 
				& !is.na(VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant) & VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant==0
				& VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant==0]
susceptible_case_control_compliant_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant) 
				& !is.na(VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant) & VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant==1
				& VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant==1]
susceptible_case_control_compliant_enummers<-c(susceptible_case_control_compliant_enummers_0,susceptible_case_control_compliant_enummers_1)
susceptible_case_control_compliant_data<-VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant
susceptible_case_control_compliant_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% susceptible_case_control_compliant_enummers)]<-"NA"

#visit1

#-------------resistance_case_control_susceptible_2-------------------
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2<-empty_vector
#susceptible
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2[log(VIP_data_subset_visit1_complete_cases$bmi)>=predicted_values_visit1_intervals[,3]
				& VIP_data_subset_visit1_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#resistant
VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2[log(VIP_data_subset_visit1_complete_cases$bmi)<=predicted_values_visit1_intervals[,2]
				& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-0

#-------------resistance_case_control_susceptible_2-------------------


#-------------resistance_case_control_compliant_2-------------------
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2[log(VIP_data_subset_visit1_complete_cases$bmi)<predicted_values_visit1_intervals[,3] & 
				log(VIP_data_subset_visit1_complete_cases$bmi)>predicted_values_visit1_intervals[,2]]<-0
#resistant
VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2[log(VIP_data_subset_visit1_complete_cases$bmi)<=predicted_values_visit1_intervals[,2]
				& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------resistance_case_control_compliant_2-------------------

#-------------susceptible_case_control_compliant_2-------------------
VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2<-empty_vector
#compliant
VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2[log(VIP_data_subset_visit1_complete_cases$bmi)<predicted_values_visit1_intervals[,3] & 
				log(VIP_data_subset_visit1_complete_cases$bmi)>predicted_values_visit1_intervals[,2]]<-0
#susceptible
VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2[log(VIP_data_subset_visit1_complete_cases$bmi)>=predicted_values_visit1_intervals[,3]
				& VIP_data_subset_visit1_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------susceptible_case_control_compliant_2-------------------

#visit2

#-------------resistance_case_control_susceptible_2-------------------
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2<-empty_vector
#susceptible
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2[log(VIP_data_subset_visit2_complete_cases$bmi)>=predicted_values_visit2_intervals[,3]
				& VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#resistant
VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2[log(VIP_data_subset_visit2_complete_cases$bmi)<=predicted_values_visit2_intervals[,2]
				& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-0

#-------------resistance_case_control_susceptible_2-------------------


#-------------resistance_case_control_compliant_2-------------------
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2[log(VIP_data_subset_visit2_complete_cases$bmi)<predicted_values_visit2_intervals[,3] & 
				log(VIP_data_subset_visit2_complete_cases$bmi)>predicted_values_visit2_intervals[,2]]<-0
#resistant
VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2[log(VIP_data_subset_visit2_complete_cases$bmi)<=predicted_values_visit2_intervals[,2]
				& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------resistance_case_control_compliant_2-------------------

#-------------susceptible_case_control_compliant_2-------------------
VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2<-empty_vector
#compliant
VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2[log(VIP_data_subset_visit2_complete_cases$bmi)<predicted_values_visit2_intervals[,3] & 
				log(VIP_data_subset_visit2_complete_cases$bmi)>predicted_values_visit2_intervals[,2]]<-0
#susceptible
VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2[log(VIP_data_subset_visit2_complete_cases$bmi)>=predicted_values_visit2_intervals[,3]
				& VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------susceptible_case_control_compliant_2-------------------



#visit1

upper_lim<-min(na.omit(log(VIP_data_subset_visit1_complete_cases$bmi)[log(VIP_data_subset_visit1_complete_cases$bmi)>=predicted_values_visit1_intervals[,3]]))

lower_lim<-max(na.omit(log(VIP_data_subset_visit1_complete_cases$bmi)[log(VIP_data_subset_visit1_complete_cases$bmi)<=predicted_values_visit1_intervals[,2]]))

#-------------case_bmi_low_control_bmi_middle-------------------
VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle<-empty_vector
#middle bmi
VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle[log(VIP_data_subset_visit1_complete_cases$bmi)<predicted_values_visit1_intervals[,3] & 
				log(VIP_data_subset_visit1_complete_cases$bmi)>lower_lim]<-0
#low bmi
VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle[log(VIP_data_subset_visit1_complete_cases$bmi)<=lower_lim
				& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------case_bmi_low_control_bmi_middle-------------------

#-------------case_bmi_high_control_bmi_middle-------------------
VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle<-empty_vector
#middle bmi
VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle[log(VIP_data_subset_visit1_complete_cases$bmi)<upper_lim & 
				log(VIP_data_subset_visit1_complete_cases$bmi)>predicted_values_visit1_intervals[,2]]<-0
#high bmi
VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle[log(VIP_data_subset_visit1_complete_cases$bmi)>=upper_lim
				& VIP_data_subset_visit1_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------case_bmi_high_control_bmi_middle-------------------


#-------------case_bmi_high_control_susceptible-------------------
VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible<-empty_vector

#high bmi
VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible[log(VIP_data_subset_visit1_complete_cases$bmi)>=upper_lim
				& log(VIP_data_subset_visit1_complete_cases$bmi)<predicted_values_visit1_intervals[,3]				
				& VIP_data_subset_visit1_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-0
#susceptible
VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible[log(VIP_data_subset_visit1_complete_cases$bmi)>=predicted_values_visit1_intervals[,3]
				& VIP_data_subset_visit1_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------case_bmi_high_control_susceptible-------------------



#-------------case_bmi_low_control_resistance-------------------
VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance<-empty_vector

#low bmi
VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance[log(VIP_data_subset_visit1_complete_cases$bmi)<=lower_lim
				& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)
				& log(VIP_data_subset_visit1_complete_cases$bmi)>predicted_values_visit1_intervals[,2]]<-0
#resistance
VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance[log(VIP_data_subset_visit1_complete_cases$bmi)<=predicted_values_visit1_intervals[,2]
				& VIP_data_subset_visit1_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit1_complete_cases$sm_status)]<-1
#-------------case_bmi_low_control_resistance-------------------


#visit2

upper_lim<-min(na.omit(log(VIP_data_subset_visit2_complete_cases$bmi)[log(VIP_data_subset_visit2_complete_cases$bmi)>=predicted_values_visit2_intervals[,3]]))

lower_lim<-max(na.omit(log(VIP_data_subset_visit2_complete_cases$bmi)[log(VIP_data_subset_visit2_complete_cases$bmi)<=predicted_values_visit2_intervals[,2]]))

#-------------case_bmi_low_control_bmi_middle-------------------
VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle<-empty_vector
#middle bmi
VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle[log(VIP_data_subset_visit2_complete_cases$bmi)<predicted_values_visit2_intervals[,3] & 
				log(VIP_data_subset_visit2_complete_cases$bmi)>lower_lim]<-0
#low bmi
VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle[log(VIP_data_subset_visit2_complete_cases$bmi)<=lower_lim
				& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------case_bmi_low_control_bmi_middle-------------------

#-------------case_bmi_high_control_bmi_middle-------------------
VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle<-empty_vector
#middle bmi
VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle[log(VIP_data_subset_visit2_complete_cases$bmi)<upper_lim & 
				log(VIP_data_subset_visit2_complete_cases$bmi)>predicted_values_visit2_intervals[,2]]<-0
#high bmi
VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle[log(VIP_data_subset_visit2_complete_cases$bmi)>=upper_lim
				& VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------case_bmi_high_control_bmi_middle-------------------


#-------------case_bmi_high_control_susceptible-------------------
VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible<-empty_vector

#high bmi
VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible[log(VIP_data_subset_visit2_complete_cases$bmi)>=upper_lim
				& VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)
				& log(VIP_data_subset_visit2_complete_cases$bmi)<predicted_values_visit2_intervals[,3]]<-0
#susceptible
VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible[log(VIP_data_subset_visit2_complete_cases$bmi)>=predicted_values_visit2_intervals[,3]
				& VIP_data_subset_visit2_complete_cases$sm_status!=2
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------case_bmi_high_control_susceptible-------------------



#-------------case_bmi_low_control_resistance-------------------
VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance<-empty_vector

#low bmi
VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance[log(VIP_data_subset_visit2_complete_cases$bmi)<=lower_lim
				& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)
				& log(VIP_data_subset_visit2_complete_cases$bmi)>predicted_values_visit2_intervals[,2]]<-0
#resistance
VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance[log(VIP_data_subset_visit2_complete_cases$bmi)<=predicted_values_visit2_intervals[,2]
				& VIP_data_subset_visit2_complete_cases$sm_status!=1 
				& !is.na(VIP_data_subset_visit2_complete_cases$sm_status)]<-1
#-------------case_bmi_low_control_resistance-------------------


#save the data and enummers for the persistant ones

#case_bmi_low_control_bmi_middle
case_bmi_low_control_bmi_middle_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle) & VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle==0
				& VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle==0]
case_bmi_low_control_bmi_middle_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle) & VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle==1
				& VIP_data_subset_visit2_complete_cases$case_bmi_low_control_bmi_middle==1]
case_bmi_low_control_bmi_middle_enummers<-c(case_bmi_low_control_bmi_middle_enummers_0,case_bmi_low_control_bmi_middle_enummers_1)
case_bmi_low_control_bmi_middle_data<-VIP_data_subset_visit1_complete_cases$case_bmi_low_control_bmi_middle
case_bmi_low_control_bmi_middle_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% case_bmi_low_control_bmi_middle_enummers)]<-"NA"

#case_bmi_high_control_bmi_middle
case_bmi_high_control_bmi_middle_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle) & VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle==0
				& VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle==0]
case_bmi_high_control_bmi_middle_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle) & VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle==1
				& VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle==1]
case_bmi_high_control_bmi_middle_enummers<-c(case_bmi_high_control_bmi_middle_enummers_0,case_bmi_high_control_bmi_middle_enummers_1)
case_bmi_high_control_bmi_middle_data<-VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle
case_bmi_high_control_bmi_middle_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% case_bmi_high_control_bmi_middle_enummers)]<-"NA"

#resistance_case_control_compliant_2
resistance_case_control_compliant_enummers_0_2<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2) & VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2==0
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2==0]
resistance_case_control_compliant_enummers_1_2<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2) & VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2==1
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_compliant_2==1]
resistance_case_control_compliant_enummers_2<-c(resistance_case_control_compliant_enummers_0_2,resistance_case_control_compliant_enummers_1_2)
resistance_case_control_compliant_data_2<-VIP_data_subset_visit1_complete_cases$resistance_case_control_compliant_2
resistance_case_control_compliant_data_2[!(VIP_data_subset_visit1_complete_cases$enummers %in% resistance_case_control_compliant_enummers_2)]<-"NA"

#susceptible_case_control_compliant_2
susceptible_case_control_compliant_enummers_0_2<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2) 
				& !is.na(VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2) & VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2==0
				& VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2==0]
susceptible_case_control_compliant_enummers_1_2<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2) 
				& !is.na(VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2) & VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2==1
				& VIP_data_subset_visit2_complete_cases$susceptible_case_control_compliant_2==1]
susceptible_case_control_compliant_enummers_2<-c(susceptible_case_control_compliant_enummers_0_2,susceptible_case_control_compliant_enummers_1_2)
susceptible_case_control_compliant_data_2<-VIP_data_subset_visit1_complete_cases$susceptible_case_control_compliant_2
susceptible_case_control_compliant_data_2[!(VIP_data_subset_visit1_complete_cases$enummers %in% susceptible_case_control_compliant_enummers_2)]<-"NA"

#resistance_case_control_susceptible_2
resistance_case_control_susceptible_enummers_0_2<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2) & VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2==0
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2==0]
resistance_case_control_susceptible_enummers_1_2<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2) 
				& !is.na(VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2) & VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2==1
				& VIP_data_subset_visit2_complete_cases$resistance_case_control_susceptible_2==1]
resistance_case_control_susceptible_enummers_2<-c(resistance_case_control_susceptible_enummers_0_2,resistance_case_control_susceptible_enummers_1_2)
resistance_case_control_susceptible_data_2<-VIP_data_subset_visit1_complete_cases$resistance_case_control_susceptible_2
resistance_case_control_susceptible_data_2[!(VIP_data_subset_visit1_complete_cases$enummers %in% resistance_case_control_susceptible_enummers_2)]<-"NA"
#resistance_case_control_susceptible_2

#case_bmi_low_control_resistance
case_bmi_low_control_resistance_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance) & VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance==0
				& VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance==0]
case_bmi_low_control_resistance_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance) & VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance==1
				& VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance==1]
case_bmi_low_control_resistance_enummers<-c(case_bmi_low_control_resistance_enummers_0,case_bmi_low_control_resistance_enummers_1)
case_bmi_low_control_resistance_data<-VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance
case_bmi_low_control_resistance_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% case_bmi_low_control_resistance_enummers)]<-"NA"
#case_bmi_low_control_resistance

#case_bmi_high_control_bmi_middle
case_bmi_high_control_bmi_middle_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle) & VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle==0
				& VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle==0]
case_bmi_high_control_bmi_middle_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle) & VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle==1
				& VIP_data_subset_visit2_complete_cases$case_bmi_high_control_bmi_middle==1]
case_bmi_high_control_bmi_middle_enummers<-c(case_bmi_high_control_bmi_middle_enummers_0,case_bmi_high_control_bmi_middle_enummers_1)
case_bmi_high_control_bmi_middle_data<-VIP_data_subset_visit1_complete_cases$case_bmi_high_control_bmi_middle
case_bmi_high_control_bmi_middle_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% case_bmi_high_control_bmi_middle_enummers)]<-"NA"
#case_bmi_high_control_bmi_middle

#case_bmi_high_control_susceptible
case_bmi_high_control_susceptible_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible) & VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible==0
				& VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible==0]
case_bmi_high_control_susceptible_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible) & VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible==1
				& VIP_data_subset_visit2_complete_cases$case_bmi_high_control_susceptible==1]
case_bmi_high_control_susceptible_enummers<-c(case_bmi_high_control_susceptible_enummers_0,case_bmi_high_control_susceptible_enummers_1)
case_bmi_high_control_susceptible_data<-VIP_data_subset_visit1_complete_cases$case_bmi_high_control_susceptible
case_bmi_high_control_susceptible_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% case_bmi_high_control_susceptible_enummers)]<-"NA"
#case_bmi_high_control_susceptible

#case_bmi_low_control_resistance
case_bmi_low_control_resistance_enummers_0<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance) & VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance==0
				& VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance==0]
case_bmi_low_control_resistance_enummers_1<-VIP_data_subset_visit1_complete_cases$enummer[!is.na(VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance) 
				& !is.na(VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance) & VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance==1
				& VIP_data_subset_visit2_complete_cases$case_bmi_low_control_resistance==1]
case_bmi_low_control_resistance_enummers<-c(case_bmi_low_control_resistance_enummers_0,case_bmi_low_control_resistance_enummers_1)
case_bmi_low_control_resistance_data<-VIP_data_subset_visit1_complete_cases$case_bmi_low_control_resistance
case_bmi_low_control_resistance_data[!(VIP_data_subset_visit1_complete_cases$enummers %in% case_bmi_low_control_resistance_enummers)]<-"NA"
#case_bmi_low_control_resistance
