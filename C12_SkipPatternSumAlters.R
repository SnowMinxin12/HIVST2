### C12_SkipPatternSumAlters.R
### author: Josh R, Minxin Lu
### date: 2021-4-14
### (1) This code re-code skip pattern of 
### the three data files from C11_Check_Variable_Class.R (index, alters, follow-up)
### (2) summarize data by index
### input: C11_Modified_Variable_Class.R
### output: index.data.summarized, alters.data.summarized, survey.data.summarized

library(tidyverse)
library(knitr)
library(data.table)
library(dplyr)
library(reshape2)

source("C11_Modified_Variable_Class.R")
index.data <- DataB
alters.data <- DataA
survey.data <- DataS
##### Index Baseline data [INCOMPLETE] ##### 
# first column is DC=confirm_code
names(index.data) <- c("confirm_code","deposit","application_date","year_of_birth","sex_birth","men_sex_1year",
                       "arm","qr_code","marital_status","education_level","monthly_income","sex_orientation",
                       "sex_orientation_disclose", "told_others_medwork","told_others_fam","told_others_friends","told_others_coll",
                       "told_others_employ","told_others_others","anal_sex_3months","anal_sex_tot","anal_sex_role",
                       "anal_sex_condom","anal_recent_condom","stable_3months","stable_condoms_3months",
                       "casual_3months","casual_condoms_3months","use_rush","rush_freq","rush_freq_sex","women_3months","women_sex_tot",
                       "women_condoms_3months","condom_recent_women","freq_discuss","share_know_hiv","share_know_test",
                       "share_know_period","explain_interpret","followup_services","share_know_kit","recipient_notshared",
                       "discuss_no","network_advice","network_information","seek_advice","volunteer_community","help_community",
                       "dating_community","nondating_community","donated_community","change_community","none_community",
                       "hang_out1","hang_out_rel1","hang_out_ident1","hang_out_orien1","hang_out_contact1",
                       "hang_out_sex1","friend_told_fam1","friend_told_coll1","friend_told_het1","friend_told_medwork1",
                       "friend_told_noone1","friend_told_notsure1","sex_friend_freq1","sex_friend_condom1",
                       "friend_supply_kit1","hang_out2","hang_out_name2","hang_out_rel2","hang_out_ident2",
                       "hang_out_orien2","hang_out_contact2","hang_out_sex2","friend_told_fam2","friend_told_coll2",
                       "friend_told_het2","friend_told_medwork2","friend_told_noone2","friend_told_notsure2",
                       "sex_friend_freq2","sex_friend_condom2","friend_supply_kit2",
                       "hang_out3","hang_out_name3","hang_out_rel3","hang_out_ident3",
                       "hang_out_orien3","hang_out_contact3","hang_out_sex3","friend_told_fam3","friend_told_coll3",
                       "friend_told_het3","friend_told_medwork3","friend_told_noone3","friend_told_notsure3",
                       "sex_friend_freq3","sex_friend_condom3","friend_supply_kit3",
                       "hang_out4","other_sex_partners","hang_out_name4","hang_out_rel4","hang_out_ident4",
                       "hang_out_orien4","friend_told_fam4","friend_told_coll4",
                       "friend_told_het4","friend_told_medwork4","friend_told_noone4","friend_told_notsure4",
                       "sex_friend_freq4","sex_friend_condom4","friend_supply_kit4",
                       "hang_out5","hang_out_name5","hang_out_rel5","hang_out_ident5", # 5 and 6 are sexual partner
                       "hang_out_orien5","friend_told_fam5","friend_told_coll5",
                       "friend_told_het5","friend_told_medwork5","friend_told_noone5","friend_told_notsure5",
                       "sex_friend_freq5","sex_friend_condom5","friend_supply_kit5","hang_out6","hang_out_name6","hang_out_rel6","hang_out_ident6",
                       "hang_out_orien6","friend_told_fam6","friend_told_coll6",
                       "friend_told_het6","friend_told_medwork6","friend_told_noone6","friend_told_notsure6",
                       "sex_friend_freq6","sex_friend_condom6","friend_supply_kit6","prior_hiv_test","prior_test_method","obtain_prior_test",
                       "last_time_tested","know_prior_result","test_kits_request","disseminate_kits","promotion_link","province","city",
                       "test_kits_actual")
# write.csv(data.frame(matrix(names(index.data),ncol=length(index.data),nrow=1)),file="baslinenames.csv")
##### SKIP pattern for 309 baseline data #####
# if N=anal_sex_3months=1, skip O=anal_sex_tot,P=anal_sex_role,Q=anal_sex_condom,
# R=anal_recent_condom,S=stable_3months,T=stable_condoms_3months,U=casual_3months,
# V=casual_condoms_3months
index.data$anal_sex_tot <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',index.data$anal_sex_tot))
index.data$anal_sex_role <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',as.character(index.data$anal_sex_role)))
index.data$anal_sex_condom <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',as.character(index.data$anal_sex_condom)))
index.data$stable_3months <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',as.character(index.data$stable_3months)))
index.data$stable_condoms_3months <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',as.character(index.data$stable_condoms_3months)))
index.data$casual_3months <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',as.character(index.data$casual_3months)))
index.data$casual_condoms_3months <- as.factor(ifelse((index.data$anal_sex_3months == 1),'SKIP',as.character(index.data$casual_condoms_3months)))

#if S=stable_3months=0, skip T=stable_condoms_3months
index.data$stable_condoms_3months <- as.factor(ifelse((index.data$stable_3months == 0),'SKIP',as.character(index.data$stable_condoms_3months)))

#if U=casual_3months=0, skip V=casual_condoms_3months
index.data$casual_condoms_3months <- as.factor(ifelse((index.data$casual_3months == 0),'SKIP',as.character(index.data$casual_condoms_3months)))

#if W=use_rush=1,skip X=rush_freq,Y=rush_freq_sex
index.data$rush_freq <- as.factor(ifelse((index.data$use_rush == 1),'SKIP',as.character(index.data$rush_freq)))
index.data$rush_freq_sex <- as.factor(ifelse((index.data$use_rush == 1),'SKIP',as.character(index.data$rush_freq_sex)))

#if X=rush_freq=0,skip Y=rush_freq_sex
index.data$rush_freq_sex <- as.factor(ifelse((index.data$rush_freq == 0),'SKIP',as.character(index.data$rush_freq_sex)))

#if Z=women_3months=0, skip AA=women_sex_tot,AB=women_condoms_3months,AC=condom_recent_women
index.data$women_sex_tot <- as.factor(ifelse((index.data$women_3months == 0),'SKIP',as.character(index.data$women_sex_tot)))
index.data$women_condoms_3months <- as.factor(ifelse((index.data$women_3months == 0),'SKIP',as.character(index.data$women_condoms_3months)))
index.data$condom_recent_women <- as.factor(ifelse((index.data$women_3months == 0),'SKIP',as.character(index.data$condom_recent_women)))

#if AP=1,skip AQ.A-AQ.F,AR,AS,AT
#if AU=1, skip to BR
#if BA=1, skip BB.A-BB.F,BC,BD,BE
#if BF=1, skip to BR
# if BL=1,skip to BR
#if BR=1,skip to CS
#if CA=1,skip to CS
#if CJ=1, skip to CS
#if CS=1, skip to CX
#if CT=prior_test_method=1,2,3,4,5, skip CU=obtain_prior_test, CV=last_time_tested,CW=know_prior_result
index.data$obtain_prior_test <- as.factor(ifelse((index.data$prior_test_method %in% c(1,2,3,4,5)),'SKIP',as.character(index.data$obtain_prior_test)))
index.data$last_time_tested <- as.factor(ifelse((index.data$prior_test_method %in% c(1,2,3,4,5)),'SKIP',as.character(index.data$last_time_tested)))
index.data$know_prior_result <- as.factor(ifelse((index.data$prior_test_method %in% c(1,2,3,4,5)),'SKIP',as.character(index.data$know_prior_result)))

##### modify variables #####
# birth year to years-old
index.data$age <- 2021-index.data$year_of_birth
#hang_out1 all=1
index.data$hang_out1 = 0
index.data$hang_out_sex4 = index.data$other_sex_partners
index.data$hang_out_sex5 = index.data$hang_out5
index.data$hang_out_sex6 = index.data$hang_out6
##### summarize friends info for 309 index data #####
#1. "hang_out1"
hang_outCol = index.data[,c("hang_out1","hang_out2","hang_out3","other_sex_partners","hang_out5","hang_out6")]
index.data$hang_outCount <- ifelse(rowSums(is.na(hang_outCol))==6,NA,rowSums(hang_outCol == "0",na.rm=TRUE)) #count rows which are not all NA

#2. "hang_out_rel1"
hang_out_relCol = index.data[,c("hang_out_rel1","hang_out_rel2","hang_out_rel3","hang_out_rel4","hang_out_rel5","hang_out_rel6")]
index.data$hang_out_rel0Count <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_rel0Proportion <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "0",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_rel1Count <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "1",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_rel1Proportion <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "1",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_rel2Count <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "2",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_rel2Proportion <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "2",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_rel3Count <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "3",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_rel3Proportion <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "3",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_rel4Count <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "4",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_rel4Proportion <- ifelse(rowSums(is.na(hang_out_relCol))==6,NA,rowSums(hang_out_relCol == "4",na.rm=TRUE)/index.data$hang_outCount)


#3. "hang_out_ident1"
hang_out_identCol = index.data[,c("hang_out_ident1","hang_out_ident2","hang_out_ident3","hang_out_ident4","hang_out_ident5","hang_out_ident6")]
index.data$hang_out_ident0Count <-      ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_ident0Proportion <- ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "0",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_ident1Count <-      ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "1",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_ident1Proportion <- ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "1",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_ident2Count <- ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "2",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_ident2Proportion <- ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "2",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_ident3Count <- ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "3",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_ident3Proportion <- ifelse(rowSums(is.na(hang_out_identCol))==6,NA,rowSums(hang_out_identCol == "3",na.rm=TRUE)/index.data$hang_outCount)

#4. "hang_out_orien1"
hang_out_orienCol = index.data[,c("hang_out_orien1","hang_out_orien2","hang_out_orien3","hang_out_orien4","hang_out_orien5","hang_out_orien6")]
index.data$hang_out_orien0Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_orien0Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "0",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_orien1Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "1",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_orien1Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "1",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_orien2Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "2",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_orien2Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "2",na.rm=TRUE)/index.data$hang_outCount)
index.data$hang_out_orien3Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "3",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_orien3Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "3",na.rm=TRUE)/index.data$hang_outCount)

#5. "hang_out_contact1-3",
hang_out_contactCol = index.data[,c("hang_out_contact1","hang_out_contact2","hang_out_contact3")]
index.data$hang_out_contact0Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_contact0Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "0",na.rm=TRUE)/rowSums(!is.na(hang_out_orienCol)))
index.data$hang_out_contact1Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "1",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_contact1Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "1",na.rm=TRUE)/rowSums(!is.na(hang_out_orienCol)))
index.data$hang_out_contact2Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "2",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_contact2Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "2",na.rm=TRUE)/rowSums(!is.na(hang_out_orienCol)))
index.data$hang_out_contact3Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "3",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_contact3Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "3",na.rm=TRUE)/rowSums(!is.na(hang_out_orienCol)))
index.data$hang_out_contact4Count <-      ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "4",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_contact4Proportion <- ifelse(rowSums(is.na(hang_out_orienCol))==3,NA,rowSums(hang_out_orienCol == "4",na.rm=TRUE)/rowSums(!is.na(hang_out_orienCol)))

#6. "hang_out_sex1"+
hang_out_sexCol = index.data[,c("hang_out_sex1","hang_out_sex2","hang_out_sex3","hang_out_sex4","hang_out_sex5","hang_out_sex6")]
index.data$hang_out_sex0Count <- ifelse(rowSums(is.na(hang_out_orienCol))==6,NA,rowSums(hang_out_orienCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$hang_out_orien0Proportion <- ifelse(is.na(index.data$hang_out_sex0Count),NA,index.data$hang_out_sex0Count/index.data$hang_outCount)

#7. "friend_told_fam1"
friend_told_famCol = index.data[,c("friend_told_fam1","friend_told_fam2","friend_told_fam3","friend_told_fam4","friend_told_fam5","friend_told_fam6")]
index.data$friend_told_fam0Count <- ifelse(rowSums(is.na(friend_told_famCol))==6,NA,rowSums(friend_told_famCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_told_fam0Proportion <- ifelse(is.na(index.data$friend_told_fam0Count),NA,index.data$friend_told_fam0Count/index.data$hang_outCount)

#8. "friend_told_coll1"
friend_told_collCol = index.data[,c("friend_told_coll1","friend_told_coll2","friend_told_coll3","friend_told_coll4","friend_told_coll5","friend_told_coll6")]
index.data$friend_told_coll0Count <- ifelse(rowSums(is.na(friend_told_collCol))==6,NA,rowSums(friend_told_collCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_told_coll0Proportion <- ifelse(is.na(index.data$friend_told_coll0Count),NA,index.data$friend_told_coll0Count/index.data$hang_outCount)

#9. "friend_told_het"
friend_told_hetCol = index.data[,c("friend_told_het1","friend_told_het2","friend_told_het3","friend_told_het4","friend_told_het5","friend_told_het6")]
index.data$friend_told_het0Count <- ifelse(rowSums(is.na(friend_told_hetCol))==6,NA,rowSums(friend_told_hetCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_told_het0Proportion <- ifelse(is.na(index.data$friend_told_het0Count),NA,index.data$friend_told_het0Count/index.data$hang_outCount)

#10. "friend_told_medwork"
friend_told_medworkCol = index.data[,c("friend_told_medwork1","friend_told_medwork2","friend_told_medwork3","friend_told_medwork4","friend_told_medwork5","friend_told_medwork6")]
index.data$friend_told_medwork0Count <- ifelse(rowSums(is.na(friend_told_medworkCol))==6,NA,rowSums(friend_told_medworkCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_told_medwork0Proportion <- ifelse(is.na(index.data$friend_told_medwork0Count),NA,index.data$friend_told_medwork0Count/index.data$hang_outCount)

#11. "friend_told_noone"
friend_told_nooneCol = index.data[,c("friend_told_noone1","friend_told_noone2","friend_told_noone3","friend_told_noone4","friend_told_noone5","friend_told_noone6")]
index.data$friend_told_noone0Count <- ifelse(rowSums(is.na(friend_told_nooneCol))==6,NA,rowSums(friend_told_nooneCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_told_noone0Proportion <- ifelse(is.na(index.data$friend_told_noone0Count),NA,index.data$friend_told_noone0Count/index.data$hang_outCount)

#12."friend_told_notsure"
friend_told_notsureCol = index.data[,c("friend_told_notsure1","friend_told_notsure2","friend_told_notsure3","friend_told_notsure4","friend_told_notsure5","friend_told_notsure6")]
index.data$friend_told_notsure0Count <- ifelse(rowSums(is.na(friend_told_notsureCol))==6,NA,rowSums(friend_told_notsureCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_told_notsure0Proportion <- ifelse(is.na(index.data$friend_told_notsure0Count),NA,index.data$friend_told_notsure0Count/index.data$hang_outCount)

#13. "sex_friend_freq"
sex_friend_freqCol = index.data[,c("sex_friend_freq1","sex_friend_freq2","sex_friend_freq3","sex_friend_freq4","sex_friend_freq5","sex_friend_freq6")]
index.data$sex_friend_freq0Count <-      ifelse(rowSums(is.na(sex_friend_freqCol))==6,NA,rowSums(sex_friend_freqCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_freq0Proportion <- ifelse(is.na(index.data$sex_friend_freq0Count),NA,index.data$sex_friend_freq0Count/index.data$hang_outCount)
index.data$sex_friend_freq1Count <-      ifelse(rowSums(is.na(sex_friend_freqCol))==6,NA,rowSums(sex_friend_freqCol == "1",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_freq1Proportion <- ifelse(is.na(index.data$sex_friend_freq1Count),NA,index.data$sex_friend_freq1Count/index.data$hang_outCount)
index.data$sex_friend_freq2Count <-      ifelse(rowSums(is.na(sex_friend_freqCol))==6,NA,rowSums(sex_friend_freqCol == "2",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_freq2Proportion <- ifelse(is.na(index.data$sex_friend_freq2Count),NA,index.data$sex_friend_freq2Count/index.data$hang_outCount)
index.data$sex_friend_freq3Count <-      ifelse(rowSums(is.na(sex_friend_freqCol))==6,NA,rowSums(sex_friend_freqCol == "3",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_freq3Proportion <- ifelse(is.na(index.data$sex_friend_freq3Count),NA,index.data$sex_friend_freq3Count/index.data$hang_outCount)
index.data$sex_friend_freq4Count <-      ifelse(rowSums(is.na(sex_friend_freqCol))==6,NA,rowSums(sex_friend_freqCol == "4",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_freq4Proportion <- ifelse(is.na(index.data$sex_friend_freq4Count),NA,index.data$sex_friend_freq4Count/index.data$hang_outCount)

#14."sex_friend_condom"
sex_friend_condomCol = index.data[,c("sex_friend_condom1","sex_friend_condom2","sex_friend_condom3","sex_friend_condom4","sex_friend_condom5","sex_friend_condom6")]
index.data$sex_friend_condom0Count <- ifelse(rowSums(is.na(sex_friend_condomCol))==6,NA,rowSums(sex_friend_condomCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_condom0Proportion <- ifelse(is.na(index.data$sex_friend_condom0Count),NA,index.data$sex_friend_condom0Count/index.data$hang_outCount)
index.data$sex_friend_condom1Count <- ifelse(rowSums(is.na(sex_friend_condomCol))==6,NA,rowSums(sex_friend_condomCol == "1",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_condom1Proportion <- ifelse(is.na(index.data$sex_friend_condom1Count),NA,index.data$sex_friend_condom1Count/index.data$hang_outCount)
index.data$sex_friend_condom2Count <- ifelse(rowSums(is.na(sex_friend_condomCol))==6,NA,rowSums(sex_friend_condomCol == "2",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_condom2Proportion <- ifelse(is.na(index.data$sex_friend_condom2Count),NA,index.data$sex_friend_condom2Count/index.data$hang_outCount)
index.data$sex_friend_condom3Count <- ifelse(rowSums(is.na(sex_friend_condomCol))==6,NA,rowSums(sex_friend_condomCol == "3",na.rm=TRUE)) #count rows which are not all NA
index.data$sex_friend_condom3Proportion <- ifelse(is.na(index.data$sex_friend_condom3Count),NA,index.data$sex_friend_condom3Count/index.data$hang_outCount)

#15. "friend_supply_kit"
friend_supply_kitCol = index.data[,c("friend_supply_kit1","friend_supply_kit2","friend_supply_kit3","friend_supply_kit4","friend_supply_kit5","friend_supply_kit6")]
index.data$friend_supply_kit0Count <- ifelse(rowSums(is.na(friend_supply_kitCol))==6,NA,rowSums(friend_supply_kitCol == "0",na.rm=TRUE)) #count rows which are not all NA
index.data$friend_supply_kit0Proportion <- ifelse(is.na(index.data$friend_supply_kit0Count),NA,index.data$friend_supply_kit0Count/index.data$hang_outCount)

##### remove unrelated variables #####
VarremovedB <- c("deposit","application_date","sex_birth","year_of_birth",
                 "hang_out1","hang_out2","hang_out3","hang_out4","other_sex_partners","hang_out5","hang_out6",
                 "hang_out_rel1","hang_out_rel2","hang_out_rel3","hang_out_rel4","hang_out_rel5","hang_out_rel6",
                 "hang_out_ident1","hang_out_ident2","hang_out_ident3","hang_out_ident4","hang_out_ident5","hang_out_ident6",
                 "hang_out_orien1","hang_out_orien2","hang_out_orien3","hang_out_orien4","hang_out_orien5","hang_out_orien6",
                 "hang_out_contact1","hang_out_contact2","hang_out_contact3",
                 "hang_out_sex1","hang_out_sex2","hang_out_sex3","hang_out_sex4","hang_out_sex5","hang_out_sex6",
                 "friend_told_fam1","friend_told_fam2","friend_told_fam3","friend_told_fam4","friend_told_fam5","friend_told_fam6",
                 "friend_told_coll1","friend_told_coll2","friend_told_coll3","friend_told_coll4","friend_told_coll5","friend_told_coll6",
                 "friend_told_het1","friend_told_het2","friend_told_het3","friend_told_het4","friend_told_het5","friend_told_het6",
                 "friend_told_medwork1","friend_told_medwork2","friend_told_medwork3","friend_told_medwork4","friend_told_medwork5","friend_told_medwork6",
                 "friend_told_noone1","friend_told_noone2","friend_told_noone3","friend_told_noone4","friend_told_noone5","friend_told_noone6",
                 "friend_told_notsure1","friend_told_notsure2","friend_told_notsure3","friend_told_notsure4","friend_told_notsure5","friend_told_notsure6",
                 "sex_friend_freq1","sex_friend_freq2","sex_friend_freq3","sex_friend_freq4","sex_friend_freq5","sex_friend_freq6",
                 "sex_friend_condom1","sex_friend_condom2","sex_friend_condom3","sex_friend_condom4","sex_friend_condom5","sex_friend_condom6",
                 "friend_supply_kit1","friend_supply_kit2","friend_supply_kit3","friend_supply_kit4","friend_supply_kit5","friend_supply_kit6")
index.data.summarized <- index.data[,!colnames(index.data) %in% VarremovedB]
remove(hang_outCol,hang_out_relCol,hang_out_identCol,hang_out_orienCol,hang_out_contactCol,
       hang_out_sexCol,friend_told_famCol,friend_told_collCol,friend_told_hetCol,friend_told_medworkCol,
       friend_told_nooneCol,friend_told_notsureCol,sex_friend_freqCol,sex_friend_condomCol,friend_supply_kitCol)
write.csv(index.data.summarized,file="../data/summarizedB.csv")
##### 269 Alter data  #####
names(alters.data) <- c("confirm_code","arm","response_date","result","index_or_alter",
                        "st_code","province","city","confirmation","relation_index","age",
                        "sex_birth","gender_identity","sex_orientation","sex_orientation_disclose",
                        "told_others_medwork","told_others_fam","told_others_friends","told_others_coll",
                        "told_others_employ","anal_sex_3months","anal_sex_tot","anal_sex_role",
                        "anal_sex_condom","anal_recent_condom","stable_3months","stable_condoms_3months",
                        "casual_3months","casual_condoms_3months","women_3months","women_sex_tot",
                        "women_condoms_3months","condom_recent_women","share_know_hiv","share_know_test",
                        "share_know_kit","explain_interpret","recipient_notneeded","recipient_present",
                        "situation_a","situation_b","situation_c","situation_d","situation_e",
                        "situation_f","situation_g","situation_h","time_to_test","kit_easy","test_with_index",
                        "index_result","index_know_result","sex_with_index","sex_prior","partner_condom",
                        "prior_hiv_test","prior_test_method","prior_test_result","current_test_result","med_confirm",
                        "anal_sex_post","anal_condoms_post","women_sex_post","women_condoms_post","test_pref",
                        "hang_out","hang_out_rel","hang_out_ident","hang_out_orien","hang_out_contact",
                        "hang_out_sex","friend_told_fam","friend_told_coll","friend_told_het","friend_told_medwork",
                        "friend_told_noone","friend_told_notsure","sex_friend_freq","sex_friend_condom",
                        "friend_supply_kit","hang_out2","hang_out_name2","hang_out_rel2","hang_out_ident2",
                        "hang_out_orien2","hang_out_contact2","hang_out_sex2","friend_told_fam2","friend_told_coll2",
                        "friend_told_het2","friend_told_medwork2","friend_told_noone2","friend_told_notsure2",
                        "sex_friend_freq2","sex_friend_condom2","friend_supply_kit2",
                        "hang_out3","hang_out_name3","hang_out_rel3","hang_out_ident3",
                        "hang_out_orien3","hang_out_contact3","hang_out_sex3","friend_told_fam3","friend_told_coll3",
                        "friend_told_het3","friend_told_medwork3","friend_told_noone3","friend_told_notsure3",
                        "sex_friend_freq3","sex_friend_condom3","friend_supply_kit3",
                        "hang_out4","other_sex_partners","hang_out_name4","hang_out_rel4","hang_out_ident4",
                        "hang_out_orien4","friend_told_fam4","friend_told_coll4",
                        "friend_told_het4","friend_told_medwork4","friend_told_noone4","friend_told_notsure4",
                        "sex_friend_freq4","sex_friend_condom4","friend_supply_kit4",
                        "hang_out5","hang_out_name5","hang_out_rel5","hang_out_ident5",
                        "hang_out_orien5","friend_told_fam5","friend_told_coll5",
                        "friend_told_het5","friend_told_medwork5","friend_told_noone5","friend_told_notsure5",
                        "sex_friend_freq5","sex_friend_condom5","friend_supply_kit5","hang_out6","hang_out_name6","hang_out_rel6","hang_out_ident6",
                        "hang_out_orien6","friend_told_fam6","friend_told_coll6",
                        "friend_told_het6","friend_told_medwork6","friend_told_noone6","friend_told_notsure6",
                        "sex_friend_freq6","sex_friend_condom6","friend_supply_kit6","marital_status","house_reg",
                        "education_level","monthly_income")
# write.csv(data.frame(matrix(names(alters.data),ncol=159,nrow=1)),file="altersnames.csv")

##### SKIP pattern for 269 alter data #####
## SKIP pattern coding using ifelse() logic


# if sex_birth=1, skip TO share_know_hiv: all sex_birth are either 0 or NA
#if anal_sex_3months=1,skip R,S,T,U,V,W,X,Y
alters.data$anal_sex_tot <- ifelse((alters.data$anal_sex_3months == 1)&(is.na(alters.data$anal_sex_tot)),'SKIP',alters.data$anal_sex_tot)
alters.data$anal_sex_role <- ifelse((alters.data$anal_sex_3months == 1)&(is.na(alters.data$anal_sex_role)),'SKIP',alters.data$anal_sex_role)
alters.data$anal_sex_role <- ifelse(alters.data$anal_sex_role=="1",0,alters.data$anal_sex_role)
alters.data$anal_sex_role <- ifelse(alters.data$anal_sex_role=="2",1,alters.data$anal_sex_role)
alters.data$anal_sex_role <- ifelse(alters.data$anal_sex_role=="3",2,alters.data$anal_sex_role)
alters.data$anal_sex_role <- as.factor(alters.data$anal_sex_role)

alters.data$anal_sex_condom <- ifelse((alters.data$anal_sex_3months == 1)&(is.na(alters.data$anal_sex_condom)),'SKIP',alters.data$anal_sex_condom)
alters.data$anal_sex_condom <- ifelse(alters.data$anal_sex_condom=="1",0,alters.data$anal_sex_condom)
alters.data$anal_sex_condom <- ifelse(alters.data$anal_sex_condom=="2",1,alters.data$anal_sex_condom)
alters.data$anal_sex_condom <- ifelse(alters.data$anal_sex_condom=="3",2,alters.data$anal_sex_condom)
alters.data$anal_sex_condom <- ifelse(alters.data$anal_sex_condom=="4",3,alters.data$anal_sex_condom)
alters.data$anal_sex_condom <- as.factor(alters.data$anal_sex_condom)

alters.data$anal_recent_condom <- ifelse((alters.data$anal_sex_3months == 1)&(is.na(alters.data$anal_recent_condom)),'SKIP',alters.data$anal_recent_condom)
# if stable_3months=0,skip stable_condoms_3months
alters.data$stable_3months <-ifelse((alters.data$anal_sex_3months == 1)&(is.na(alters.data$stable_3months)),'SKIP',alters.data$stable_3months)
alters.data$stable_condoms_3months <- ifelse((alters.data$stable_3months == 0)&(is.na(alters.data$stable_condoms_3months)),'SKIP',alters.data$stable_condoms_3months)
# if casual_3months=0,skip casual_condoms_3months
alters.data$casual_3months <- ifelse((alters.data$anal_sex_3months == 1)&(is.na(alters.data$casual_3months)),'SKIP',alters.data$casual_3months)
alters.data$casual_condoms_3months <- ifelse((alters.data$casual_3months == 0)&(is.na(alters.data$casual_condoms_3months)),'SKIP',alters.data$casual_condoms_3months)
#if women_3months=1, skip women_sex_tot, women_condoms_3months, condom_recent_women
alters.data$women_sex_tot <- ifelse((alters.data$women_3months == 1),'SKIP',alters.data$women_sex_tot)

alters.data$women_condoms_3months <- ifelse((alters.data$women_3months == 1),'SKIP',alters.data$women_condoms_3months)
alters.data$women_condoms_3months <- ifelse(alters.data$women_condoms_3months == "1",0,alters.data$women_condoms_3months)
alters.data$women_condoms_3months <- ifelse(alters.data$women_condoms_3months == "2",1,alters.data$women_condoms_3months)
alters.data$women_condoms_3months <- ifelse(alters.data$women_condoms_3months == "3",2,alters.data$women_condoms_3months)
alters.data$women_condoms_3months <- ifelse(alters.data$women_condoms_3months == "4",3,alters.data$women_condoms_3months)
alters.data$women_condoms_3months <- as.factor(alters.data$women_condoms_3months)

alters.data$condom_recent_women <- ifelse((alters.data$women_3months == 1),'SKIP',alters.data$condom_recent_women)

#if test_with_index=1,skip index_result
alters.data$index_result <- ifelse((alters.data$test_with_index == 1),'SKIP',alters.data$index_result)
alters.data$index_result <- ifelse(alters.data$index_result=="1",0,alters.data$index_result)
alters.data$index_result <- ifelse(alters.data$index_result=="2",1,alters.data$index_result)
alters.data$index_result <- ifelse(alters.data$index_result=="3",2,alters.data$index_result)
alters.data$index_result <- as.factor(alters.data$index_result)

#if AP=sex_with_index=1, skip AQ=sex_prior, AR=partner_condom
alters.data$sex_prior <- ifelse((alters.data$sex_with_index == 1),'SKIP',alters.data$sex_prior)
alters.data$partner_condom <- ifelse((alters.data$sex_with_index == 1),'SKIP',alters.data$partner_condom)

#if AS=prior_hiv_test=1, skip AT=prior_test_method, AU=prior_test_result
alters.data$prior_test_method <- ifelse((alters.data$prior_hiv_test == 1),'SKIP',alters.data$prior_test_method)
alters.data$prior_test_method <- ifelse(alters.data$prior_test_method==1,0,alters.data$prior_test_method)
alters.data$prior_test_method <- ifelse(alters.data$prior_test_method==2,1,alters.data$prior_test_method)
alters.data$prior_test_method <- as.factor(alters.data$prior_test_method)

#if AV=current_test_result=1,skip AW=med_confirm
alters.data$med_confirm <- ifelse((alters.data$current_test_result == 1),'SKIP',alters.data$med_confirm)
alters.data$med_confirm <- ifelse(alters.data$med_confirm==1,0,alters.data$med_confirm)
alters.data$med_confirm <- ifelse(alters.data$med_confirm==2,1,alters.data$med_confirm)
alters.data$med_confirm <-as.factor(alters.data$med_confirm)

#if AX=anal_sex_post=0, skip AY=anal_condoms_post
alters.data$anal_condoms_post <- ifelse((alters.data$anal_sex_post == 0),'SKIP',alters.data$anal_condoms_post)
alters.data$anal_condoms_post <- ifelse(alters.data$anal_condoms_post==1,0,alters.data$anal_condoms_post)
alters.data$anal_condoms_post <- ifelse(alters.data$anal_condoms_post==2,1,alters.data$anal_condoms_post)
alters.data$anal_condoms_post <- ifelse(alters.data$anal_condoms_post==3,2,alters.data$anal_condoms_post)
alters.data$anal_condoms_post <- ifelse(alters.data$anal_condoms_post==4,3,alters.data$anal_condoms_post)
alters.data$anal_condoms_post <- as.factor(alters.data$anal_condoms_post)

#if AZ=women_sex_post=0, skip BA=women_condoms_post
alters.data$women_condoms_post <- ifelse((alters.data$women_sex_post == 0),'SKIP',alters.data$women_condoms_post)
alters.data$women_condoms_post <- ifelse(alters.data$women_condoms_post==1,0,alters.data$women_condoms_post)
alters.data$women_condoms_post <- ifelse(alters.data$women_condoms_post==2,1,alters.data$women_condoms_post)
alters.data$women_condoms_post <- ifelse(alters.data$women_condoms_post==3,2,alters.data$women_condoms_post)
alters.data$women_condoms_post <- ifelse(alters.data$women_condoms_post==4,3,alters.data$women_condoms_post)
alters.data$women_condoms_post <- as.factor(alters.data$women_condoms_post)

##### remove alter's alters in 269 alters data#####
# remove alter's alters information BC=hang_out to DJ=friend_supply_kit6
altersalters <- c("hang_out","hang_out_rel","hang_out_ident","hang_out_orien","hang_out_contact",
                  "hang_out_sex","friend_told_fam","friend_told_coll","friend_told_het","friend_told_medwork",
                  "friend_told_noone","friend_told_notsure","sex_friend_freq","sex_friend_condom",
                  "friend_supply_kit","hang_out2","hang_out_name2","hang_out_rel2","hang_out_ident2",
                  "hang_out_orien2","hang_out_contact2","hang_out_sex2","friend_told_fam2","friend_told_coll2",
                  "friend_told_het2","friend_told_medwork2","friend_told_noone2","friend_told_notsure2",
                  "sex_friend_freq2","sex_friend_condom2","friend_supply_kit2",
                  "hang_out3","hang_out_name3","hang_out_rel3","hang_out_ident3",
                  "hang_out_orien3","hang_out_contact3","hang_out_sex3","friend_told_fam3","friend_told_coll3",
                  "friend_told_het3","friend_told_medwork3","friend_told_noone3","friend_told_notsure3",
                  "sex_friend_freq3","sex_friend_condom3","friend_supply_kit3",
                  "hang_out4","other_sex_partners","hang_out_name4","hang_out_rel4","hang_out_ident4",
                  "hang_out_orien4","friend_told_fam4","friend_told_coll4",
                  "friend_told_het4","friend_told_medwork4","friend_told_noone4","friend_told_notsure4",
                  "sex_friend_freq4","sex_friend_condom4","friend_supply_kit4",
                  "hang_out5","hang_out_name5","hang_out_rel5","hang_out_ident5",
                  "hang_out_orien5","friend_told_fam5","friend_told_coll5",
                  "friend_told_het5","friend_told_medwork5","friend_told_noone5","friend_told_notsure5",
                  "sex_friend_freq5","sex_friend_condom5","friend_supply_kit5","hang_out6","hang_out_name6","hang_out_rel6","hang_out_ident6",
                  "hang_out_orien6","friend_told_fam6","friend_told_coll6",
                  "friend_told_het6","friend_told_medwork6","friend_told_noone6","friend_told_notsure6",
                  "sex_friend_freq6","sex_friend_condom6","friend_supply_kit6")
alters.data <- alters.data[,!colnames(alters.data) %in% altersalters]
##### remove from analysis for now #####
VarRemovedA <- c("response_date","index_or_alter")
alters.data <- alters.data[,!colnames(alters.data) %in% VarRemovedA]

##### summarize alters for 269 alter data #####
alters.varname = colnames(alters.data)[2]
alters.data.summarized <- reshape2::dcast(data=alters.data,
                                          as.formula(paste0("confirm_code ~", alters.varname)),
                                          fun.aggregate = length,
                                          value.var = alters.varname)
colnames(alters.data.summarized)=c("confirm_code",paste0(alters.varname,"_",colnames(alters.data.summarized)[-1]))
for (i in 3:ncol(alters.data)){
  alters.varname = colnames(alters.data)[i]
  alters.var2 <- reshape2::dcast(data=alters.data,
                                 as.formula(paste0("confirm_code ~", alters.varname)),
                                 fun.aggregate = length,
                                 value.var = alters.varname)
  colnames(alters.var2)=c("confirm_code",paste0(alters.varname,"_",colnames(alters.var2)[-1]))
  alters.data.summarized=merge(x=alters.data.summarized,y=alters.var2, by="confirm_code")
}

write.csv(alters.data.summarized,file="../data/summarizedA.csv")

##### 207 Survey data names #####
##I tried making variable names as uniform as possible between the data sets, but there are likely some differences
names(survey.data) <- c("survey_date","confirm_code","kits_no","kits_dist",
                        "sex_first","sex_second","sex_third","sex_fourth","sex_fifth",
                        "relation_first","relation_second","relation_third",
                        "relation_fourth","relation_fifth","pt_first",
                        "pt_second","pt_third","pt_fourth","pt_fifth",
                        "hivr_first","hivr_second","hivr_third","hivr_fourth","hivr_fifth",
                        "pt_sex_first","pt_sex_second","pt_sex_third","pt_sex_fourth",
                        "pt_sex_fifth","sex_before_first","sex_before_second",
                        "sex_before_third","sex_before_fourth","sex_before_fifth",
                        "condom_first","condom_second","condom_third","condom_fourth",
                        "condom_fifth","share_know_hiv","share_know_test","share_know_kit",
                        "explain_interpret","recipient_notneeded","recipient_present",
                        "situation_a","situation_b","situation_c","situation_d","situation_e",
                        "situation_f","situation_g","situation_h","selftest_no","self_present",
                        "most_important","result_mostrecent","health_center",
                        "anal_sex_3months","anal_sex_tot","anal_sex_role","anal_sex_condom",
                        "anal_recent_condom","stable_3months","stable_condoms_3months",
                        "casual_3months","casual_condoms_3months","use_rush","rush_freq",
                        "new_drugs_1month","women_3months","women_condoms_3months","test_pref")
# write.csv(data.frame(matrix(names(survey.data),ncol=73,nrow=1)),file="surveynames.csv")
##### SKIP pattern for 207 survey data #####
# SKIP pattern coding using ifelse() logic 

survey.data$health_center <- ifelse((survey.data$result_mostrecent %in% c(1,2))&is.na(survey.data$health_center),'SKIP',survey.data$health_center)
survey.data$health_center <- ifelse(survey.data$health_center=="1","0",survey.data$health_center)# correction: previous step automatically writes 0 as 1
survey.data$health_center <- as.factor(survey.data$health_center)

#numeric -> factor because added "skip"
survey.data$anal_sex_tot <- ifelse((survey.data$anal_sex_3months == 1)&is.na(survey.data$anal_sex_tot),'SKIP',survey.data$anal_sex_tot)
survey.data$anal_sex_tot <- as.factor(survey.data$anal_sex_tot)

survey.data$anal_sex_role <- ifelse((survey.data$anal_sex_3months == 1)&is.na(survey.data$anal_sex_role),'SKIP',survey.data$anal_sex_role)
survey.data$anal_sex_role <- ifelse(survey.data$anal_sex_role == "1",'0',survey.data$anal_sex_role)
survey.data$anal_sex_role <- ifelse(survey.data$anal_sex_role == "2",'1',survey.data$anal_sex_role)
survey.data$anal_sex_role <- ifelse(survey.data$anal_sex_role == "3",'2',survey.data$anal_sex_role)
survey.data$anal_sex_role <- as.factor(survey.data$anal_sex_role)

survey.data$anal_sex_condom <- ifelse(survey.data$anal_sex_3months == 1,'SKIP',survey.data$anal_sex_condom)
survey.data$anal_sex_condom <- ifelse(survey.data$anal_sex_condom == "1",0,survey.data$anal_sex_condom)
survey.data$anal_sex_condom <- ifelse(survey.data$anal_sex_condom == "2",1,survey.data$anal_sex_condom)
survey.data$anal_sex_condom <- ifelse(survey.data$anal_sex_condom == "3",2,survey.data$anal_sex_condom)
survey.data$anal_sex_condom <- ifelse(survey.data$anal_sex_condom == "4",3,survey.data$anal_sex_condom)
survey.data$anal_sex_condom <- as.factor(survey.data$anal_sex_condom)


survey.data$anal_recent_condom <- ifelse(survey.data$anal_sex_3months == 1,'SKIP',survey.data$anal_recent_condom)
survey.data$anal_recent_condom <- as.factor(survey.data$anal_recent_condom)

survey.data$stable_condoms_3months <- ifelse(survey.data$stable_3months == 0,'SKIP',survey.data$stable_condoms_3months)
survey.data$stable_condoms_3months <- ifelse(survey.data$stable_condoms_3months == "1",0,survey.data$stable_condoms_3months)
survey.data$stable_condoms_3months <- ifelse(survey.data$stable_condoms_3months == "2",1,survey.data$stable_condoms_3months)
survey.data$stable_condoms_3months <- ifelse(survey.data$stable_condoms_3months == "3",2,survey.data$stable_condoms_3months)
survey.data$stable_condoms_3months <- ifelse(survey.data$stable_condoms_3months == "4",3,survey.data$stable_condoms_3months)
survey.data$casual_condoms_3months <- ifelse(survey.data$casual_3months == 0,'SKIP',survey.data$casual_condoms_3months)
survey.data$casual_condoms_3months <- as.factor(survey.data$casual_condoms_3months)

survey.data$rush_freq <- ifelse(survey.data$use_rush == 1,'SKIP',survey.data$rush_freq)
survey.data$rush_freq <- ifelse(survey.data$rush_freq == "1",0,survey.data$rush_freq)
survey.data$rush_freq <- ifelse(survey.data$rush_freq == "2",1,survey.data$rush_freq)
survey.data$rush_freq <- ifelse(survey.data$rush_freq == "3",2,survey.data$rush_freq)
survey.data$rush_freq <- ifelse(survey.data$rush_freq == "4",3,survey.data$rush_freq)
survey.data$rush_freq <-as.factor(survey.data$rush_freq)

survey.data$new_drugs_1month <- ifelse(survey.data$use_rush == 1,'SKIP',survey.data$new_drugs_1month)
survey.data$new_drugs_1month <- ifelse(survey.data$new_drugs_1month == "1",0,survey.data$new_drugs_1month)
survey.data$new_drugs_1month <- ifelse(survey.data$new_drugs_1month == "2",1,survey.data$new_drugs_1month)
survey.data$new_drugs_1month <- ifelse(survey.data$new_drugs_1month == "3",2,survey.data$new_drugs_1month)
survey.data$new_drugs_1month <- ifelse(survey.data$new_drugs_1month == "4",3,survey.data$new_drugs_1month)
survey.data$new_drugs_1month <- as.factor(survey.data$new_drugs_1month)

survey.data$women_condoms_3months <- ifelse((survey.data$women_3months == 0)&is.na(survey.data$women_condoms_3months),'SKIP',survey.data$women_condoms_3months)
survey.data$women_condoms_3months <- ifelse(survey.data$women_condoms_3months == "1",0,survey.data$women_condoms_3months)
survey.data$women_condoms_3months <- as.factor(survey.data$women_condoms_3months)

survey.data$pt_sex_first <- ifelse(survey.data$relation_first %in% 2,'SKIP',survey.data$pt_sex_first)
survey.data$pt_sex_second <- ifelse(survey.data$relation_second %in% 2,'SKIP',survey.data$pt_sex_second)
survey.data$pt_sex_third <- ifelse(survey.data$relation_third %in% 2,'SKIP',survey.data$pt_sex_third)
survey.data$pt_sex_fourth <- ifelse(survey.data$relation_fourth %in% 2,'SKIP',survey.data$pt_sex_fourth)
survey.data$pt_sex_fifth <- ifelse(survey.data$relation_fifth %in% 2,'SKIP',survey.data$pt_sex_fifth)

survey.data$sex_before_first <- ifelse(survey.data$pt_sex_first %in% c(1,'SKIP'),'SKIP',survey.data$sex_before_first)
survey.data$sex_before_second <- ifelse(survey.data$pt_sex_second %in% c(1,'SKIP'),'SKIP',survey.data$sex_before_second)
survey.data$sex_before_third <- ifelse(survey.data$pt_sex_third %in% c(1,'SKIP'),'SKIP',survey.data$sex_before_third)
survey.data$sex_before_fourth <- ifelse(survey.data$pt_sex_fourth %in% c(1,'SKIP'),'SKIP',survey.data$sex_before_fourth)
survey.data$sex_before_fifth <- ifelse(survey.data$pt_sex_fifth %in% c(1,'SKIP'),'SKIP',survey.data$sex_before_fifth)

survey.data$condom_first <- ifelse(survey.data$pt_sex_first %in% c(1,'SKIP'),'SKIP',survey.data$condom_first)
survey.data$condom_second <- ifelse(survey.data$pt_sex_second %in% c(1,'SKIP'),'SKIP',survey.data$condom_second)
survey.data$condom_third <- ifelse(survey.data$pt_sex_third %in% c(1,'SKIP'),'SKIP',survey.data$condom_third)
survey.data$condom_fourth <- ifelse(survey.data$pt_sex_fourth %in% c(1,'SKIP'),'SKIP',survey.data$condom_fourth)
survey.data$condom_fifth <- ifelse(survey.data$pt_sex_fifth %in% c(1,'SKIP'),'SKIP',survey.data$condom_fifth)

# "I don't know" code as missing
survey.data$hivr_first[survey.data$hivr_first==2] <- NA
survey.data$hivr_second[survey.data$hivr_second==2] <- NA
survey.data$hivr_third[survey.data$hivr_third==2] <- NA
survey.data$hivr_fourth[survey.data$hivr_fourth==2] <- NA
survey.data$hivr_fifth[survey.data$hivr_fifth==2] <- NA

#####  summarize alters for 207 survey data #####
# summarize alters information as counts and proportions, if all alter information is NA then the summary=NA,
# if any of the alters have information then the result is not NA
#1. sex_first
AlterSexCol = survey.data[,c("sex_first","sex_second","sex_third","sex_fourth","sex_fifth")]
survey.data$AlterSex0Count <- ifelse(rowSums(is.na(AlterSexCol))==5,NA,rowSums(AlterSexCol == "0",na.rm=TRUE)) #count rows which are not all NA
survey.data$AlterSex0Proportion <- ifelse(rowSums(is.na(AlterSexCol))==5,NA,rowSums(AlterSexCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterSexCol))))

#2. relation_first
AlterRelationCol = survey.data[,c("relation_first","relation_second","relation_third","relation_fourth","relation_fifth")]
survey.data$AlterRelation0Count <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                          rowSums(AlterRelationCol == "0",na.rm=TRUE))
survey.data$AlterRelation0Proportion <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                           rowSums(AlterRelationCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationCol))))
survey.data$AlterRelation1Count <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                          rowSums(AlterRelationCol == "1",na.rm=TRUE))
survey.data$AlterRelation1Proportion <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                               rowSums(AlterRelationCol == "1",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationCol))))
survey.data$AlterRelation2Count <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                          rowSums(AlterRelationCol == "2",na.rm=TRUE))
survey.data$AlterRelation2Proportion <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                               rowSums(AlterRelationCol == "2",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationCol))))
survey.data$AlterRelation3Count <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                          rowSums(AlterRelationCol == "3",na.rm=TRUE))
survey.data$AlterRelation3Proportion <- ifelse(rowSums(is.na(AlterRelationCol))==5,NA,
                                               rowSums(AlterRelationCol == "3",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationCol))))

#3. pt_first
AlterPTCol = survey.data[,c("pt_first","pt_second","pt_third","pt_fourth","pt_fifth")]
survey.data$AlterPT0Count <- ifelse(rowSums(is.na(AlterPTCol))==5,NA,
                                          rowSums(AlterPTCol == "0",na.rm=TRUE))
survey.data$AlterPT0Proportion <- ifelse(rowSums(is.na(AlterPTCol))==5,NA,
                                               rowSums(AlterPTCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterPTCol))))
#4. hivr_first
AlterHivrCol = survey.data[,c("hivr_first","hivr_second","hivr_third","hivr_fourth","hivr_fifth")]
survey.data$AlterHivr0Count <- ifelse(rowSums(is.na(AlterHivrCol))==5,NA,
                                    rowSums(AlterHivrCol == "0",na.rm=TRUE))
survey.data$AlterHivr0Proportion <- ifelse(rowSums(is.na(AlterHivrCol))==5,NA,
                                         rowSums(AlterHivrCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterHivrCol))))

#5. pt_sex_first Y,Z,AA,AB,AC
AlterPT_SexCol = survey.data[,c("pt_sex_first","pt_sex_second","pt_sex_third","pt_sex_fourth","pt_sex_fifth")]
survey.data$AlterPT_Sex0Count <- ifelse(rowSums(is.na(AlterPT_SexCol))==5,NA,
                                      rowSums(AlterPT_SexCol == "0",na.rm=TRUE))
survey.data$AlterPT_Sex0Proportion <- ifelse(rowSums(is.na(AlterPT_SexCol))==5,NA,
                                           rowSums(AlterPT_SexCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterPT_SexCol))))


#6. sex_before_first AD,AE,AF,AG,AH
AlterSex_BeforeCol = survey.data[,c("sex_before_first","sex_before_second","sex_before_third","sex_before_fourth","sex_before_fifth")]
survey.data$AlterSex_Before0Count <- ifelse(rowSums(is.na(AlterSex_BeforeCol))==5,NA,
                                        rowSums(AlterSex_BeforeCol == "0",na.rm=TRUE))
survey.data$AlterSex_Before0Proportion <- ifelse(rowSums(is.na(AlterSex_BeforeCol))==5,NA,
                                             rowSums(AlterSex_BeforeCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterSex_BeforeCol))))


#7. condom_first AI,AJ,AK,AL,AM
AlterCondomCol = survey.data[,c("condom_first","condom_second","condom_third","condom_fourth","condom_fifth")]
survey.data$AlterCondom0Count <- ifelse(rowSums(is.na(AlterCondomCol))==5,NA,
                                            rowSums(AlterCondomCol == "0",na.rm=TRUE))
survey.data$AlterCondom0Proportion <- ifelse(rowSums(is.na(AlterCondomCol))==5,NA,
                                                 rowSums(AlterCondomCol == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterCondomCol))))



VarRemovedS <- c("sex_first","sex_second","sex_third","sex_fourth","sex_fifth",
                    "relation_first","relation_second","relation_third","relation_fourth","relation_fifth",
                    "pt_first","pt_second","pt_third","pt_fourth","pt_fifth",
                    "hivr_first","hivr_second","hivr_third","hivr_fourth","hivr_fifth",
                    "pt_sex_first","pt_sex_second","pt_sex_third","pt_sex_fourth","pt_sex_fifth",
                    "sex_before_first","sex_before_second","sex_before_third","sex_before_fourth","sex_before_fifth",
                    "condom_first","condom_second","condom_third","condom_fourth","condom_fifth")
survey.data.summarized <- survey.data[,!colnames(survey.data) %in% VarRemovedS]
remove(AlterSexCol,AlterRelationCol,AlterPTCol,AlterHivrCol,AlterPT_SexCol,
       AlterSex_BeforeCol,AlterCondomCol)
write.csv(survey.data.summarized, file="../data/summarizedS.csv")

