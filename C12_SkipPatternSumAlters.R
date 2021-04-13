### C12_SkipPatternSumAlters.R
### author: Josh R, Minxin Lu
### date: 2021-4-13
### (1) This code re-code skip pattern different from missing data of 
### the three data files from C11_Check_Variable_Class.R (index, alters, follow-up)
### (2) summarize alter data by index
### input: C11_Check_Variable_Class.R
### output: 

library(tidyverse)
library(knitr)

# setwd("")
source("C11_Check_Variable_Class.R")
survey.data <- DataS
alters.data <- DataA


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
# write.csv(data.frame(matrix(names(survey.data),ncol=118,nrow=1)),file="surveynames.csv")
##### SKIP pattern for 207 survey data #####
# SKIP pattern coding using ifelse() logic 
survey.data$health_center <- ifelse(survey.data$result_mostrecent %in% c(1,2),'SKIP',survey.data$health_center)

survey.data$anal_sex_tot <- ifelse(survey.data$anal_sex_3months == 1,'SKIP',survey.data$anal_sex_tot)
survey.data$anal_sex_role <- ifelse(survey.data$anal_sex_3months == 1,'SKIP',survey.data$anal_sex_role)
survey.data$anal_sex_condom <- ifelse(survey.data$anal_sex_3months == 1,'SKIP',survey.data$anal_sex_condom)
survey.data$anal_recent_condom <- ifelse(survey.data$anal_sex_3months == 1,'SKIP',survey.data$anal_recent_condom)

survey.data$stable_condoms_3months <- ifelse(survey.data$stable_3months == 0,'SKIP',survey.data$stable_condoms_3months)

survey.data$casual_condoms_3months <- ifelse(survey.data$casual_3months == 0,'SKIP',survey.data$casual_condoms_3months)

survey.data$rush_freq <- ifelse(survey.data$use_rush == 1,'SKIP',survey.data$rush_freq)
survey.data$new_drugs_1month <- ifelse(survey.data$use_rush == 1,'SKIP',survey.data$new_drugs_1month)

survey.data$women_condoms_3months <- ifelse(survey.data$women_3months == 0,'SKIP',survey.data$women_condoms_3months)

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
AlterSexColumns = survey.data[,c("sex_first","sex_second","sex_third","sex_fourth","sex_fifth")]
survey.data$AlterSex0Count <- ifelse(rowSums(is.na(AlterSexColumns))==5,NA,rowSums(AlterSexColumns == "0",na.rm=TRUE)) #count rows which are not all NA
survey.data$AlterSex0Proportion <- ifelse(rowSums(is.na(AlterSexColumns))==5,NA,rowSums(AlterSexColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterSexColumns))))

#2. relation_first
AlterRelationColumns = survey.data[,c("relation_first","relation_second","relation_third","relation_fourth","relation_fifth")]
survey.data$AlterRelation0Count <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                          rowSums(AlterRelationColumns == "0",na.rm=TRUE))
survey.data$AlterRelation0Proportion <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                           rowSums(AlterRelationColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationColumns))))
survey.data$AlterRelation1Count <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                          rowSums(AlterRelationColumns == "1",na.rm=TRUE))
survey.data$AlterRelation1Proportion <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                               rowSums(AlterRelationColumns == "1",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationColumns))))
survey.data$AlterRelation2Count <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                          rowSums(AlterRelationColumns == "2",na.rm=TRUE))
survey.data$AlterRelation2Proportion <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                               rowSums(AlterRelationColumns == "2",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationColumns))))
survey.data$AlterRelation3Count <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                          rowSums(AlterRelationColumns == "3",na.rm=TRUE))
survey.data$AlterRelation3Proportion <- ifelse(rowSums(is.na(AlterRelationColumns))==5,NA,
                                               rowSums(AlterRelationColumns == "3",na.rm=TRUE)/(5-rowSums(is.na(AlterRelationColumns))))

#3. pt_first
AlterPTColumns = survey.data[,c("pt_first","pt_second","pt_third","pt_fourth","pt_fifth")]
survey.data$AlterPT0Count <- ifelse(rowSums(is.na(AlterPTColumns))==5,NA,
                                          rowSums(AlterPTColumns == "0",na.rm=TRUE))
survey.data$AlterPT0Proportion <- ifelse(rowSums(is.na(AlterPTColumns))==5,NA,
                                               rowSums(AlterPTColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterPTColumns))))
#4. hivr_first
AlterHivrColumns = survey.data[,c("hivr_first","hivr_second","hivr_third","hivr_fourth","hivr_fifth")]
survey.data$AlterHivr0Count <- ifelse(rowSums(is.na(AlterHivrColumns))==5,NA,
                                    rowSums(AlterHivrColumns == "0",na.rm=TRUE))
survey.data$AlterHivr0Proportion <- ifelse(rowSums(is.na(AlterHivrColumns))==5,NA,
                                         rowSums(AlterHivrColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterHivrColumns))))

#5. pt_sex_first Y,Z,AA,AB,AC
AlterPT_SexColumns = survey.data[,c("pt_sex_first","pt_sex_second","pt_sex_third","pt_sex_fourth","pt_sex_fifth")]
survey.data$AlterPT_Sex0Count <- ifelse(rowSums(is.na(AlterPT_SexColumns))==5,NA,
                                      rowSums(AlterPT_SexColumns == "0",na.rm=TRUE))
survey.data$AlterPT_Sex0Proportion <- ifelse(rowSums(is.na(AlterPT_SexColumns))==5,NA,
                                           rowSums(AlterPT_SexColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterPT_SexColumns))))


#6. sex_before_first AD,AE,AF,AG,AH
AlterSex_BeforeColumns = survey.data[,c("sex_before_first","sex_before_second","sex_before_third","sex_before_fourth","sex_before_fifth")]
survey.data$AlterSex_Before0Count <- ifelse(rowSums(is.na(AlterSex_BeforeColumns))==5,NA,
                                        rowSums(AlterSex_BeforeColumns == "0",na.rm=TRUE))
survey.data$AlterSex_Before0Proportion <- ifelse(rowSums(is.na(AlterSex_BeforeColumns))==5,NA,
                                             rowSums(AlterSex_BeforeColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterSex_BeforeColumns))))


#7. condom_first AI,AJ,AK,AL,AM
AlterCondomColumns = survey.data[,c("condom_first","condom_second","condom_third","condom_fourth","condom_fifth")]
survey.data$AlterCondom0Count <- ifelse(rowSums(is.na(AlterCondomColumns))==5,NA,
                                            rowSums(AlterCondomColumns == "0",na.rm=TRUE))
survey.data$AlterCondom0Proportion <- ifelse(rowSums(is.na(AlterCondomColumns))==5,NA,
                                                 rowSums(AlterCondomColumns == "0",na.rm=TRUE)/(5-rowSums(is.na(AlterCondomColumns))))


# check = survey.data[,c("relation_first","relation_second","relation_third","relation_fourth","relation_fifth","AlterRelation0Count", "AlterRelation0Proportion")]

VartobeRemoved <- c("sex_first","sex_second","sex_third","sex_fourth","sex_fifth",
                    "relation_first","relation_second","relation_third","relation_fourth","relation_fifth",
                    "pt_first","pt_second","pt_third","pt_fourth","pt_fifth",
                    "hivr_first","hivr_second","hivr_third","hivr_fourth","hivr_fifth",
                    "pt_sex_first","pt_sex_second","pt_sex_third","pt_sex_fourth","pt_sex_fifth",
                    "sex_before_first","sex_before_second","sex_before_third","sex_before_fourth","sex_before_fifth",
                    "condom_first","condom_second","condom_third","condom_fourth","condom_fifth")
remove(AlterSexColumns,AlterRelationColumns,AlterPTColumns,AlterHivrColumns,AlterPT_SexColumns,
       AlterSex_BeforeColumns,AlterCondomColumns)

##### 269 Alter data names #####
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
## SKIP pattern coding using ifelse() logic.  INCOMPLETE
alters.data$anal_sex_tot <- ifelse(alters.data$anal_sex_3months == 1,'SKIP',alters.data$anal_sex_tot)
alters.data$anal_sex_role <- ifelse(alters.data$anal_sex_3months == 1,'SKIP',alters.data$anal_sex_role)
alters.data$anal_sex_condom <- ifelse(alters.data$anal_sex_3months == 1,'SKIP',alters.data$anal_sex_condom)
alters.data$anal_recent_condom <- ifelse(alters.data$anal_sex_3months == 1,'SKIP',alters.data$anal_recent_condom)

alters.data$stable_condoms_3months <- ifelse(alters.data$stable_3months == 0,'SKIP',alters.data$stable_condoms_3months)

alters.data$casual_condoms_3months <- ifelse(alters.data$casual_3months == 0,'SKIP',alters.data$casual_condoms_3months)

alters.data$women_condoms_3months <- ifelse(alters.data$women_3months == 0,'SKIP',alters.data$women_condoms_3months)



alters.data$confirm_code_miss <- ifelse(is.na(alters.data$confirm_code),NA,1)

##### summarize alters for 269 alter data

##### Index Baseline data names #####
names(index.data) <- c("deposit","application_date","year_of_birth","sex_birth","men_sex_1year",
                       "arm","qr_code","marital_status","education_level","monthly_income","sex_orientation",
                       "sex_orientation_disclose", "told_others_medwork","told_others_fam","told_others_friends","told_others_coll",
                       "told_others_employ","told_others_others","anal_sex_3months","anal_sex_tot","anal_sex_role",
                       "anal_sex_condom","anal_recent_condom","stable_3months","stable_condoms_3months",
                       "casual_3months","casual_condoms_3months","use_rush","rush_freq","rush_freq_sex","women_3months","women_sex_tot",
                       "women_condoms_3months","condom_recent_women","freq_discuss","share_know_hiv","share_know_test",
                       "share_know_period","explain_interpret","followup_services","share_know_kit","recipient_notshared",
                       "discuss_no","network_advice","network_information","seek_advice","volunteer_community","help_community",
                       "dating_community","nondating_community","donated_community","change_community","none_community",
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
                       "sex_friend_freq4","sex_friend_condom4","friend_supply_kit4","hant_out5",
                       "hang_out_name5","hang_out_rel5","hang_out_ident5",
                       "hang_out_orien5","friend_told_fam5","friend_told_coll5",
                       "friend_told_het5","friend_told_medwork5","friend_told_noone5","friend_told_notsure5",
                       "sex_friend_freq5","sex_friend_condom5","friend_supply_kit5","hang_out6","hang_out_name6","hang_out_rel6","hang_out_ident6",
                       "hang_out_orien6","friend_told_fam6","friend_told_coll6",
                       "friend_told_het6","friend_told_medwork6","friend_told_noone6","friend_told_notsure6",
                       "sex_friend_freq6","sex_friend_condom6","friend_supply_kit6","prior_hiv_test","prior_test_method","obtain_prior_test",
                       "last_time_tested","know_prior_result","test_kits_request","disseminate_kits","promotion_link","province","city","confirm_code",
                       "test_kits_actual")

## Determining how many missing/the missingness pattern of the three data sets
sapply(survey.data, function(x) sum(is.na(x)))
sapply(alters.data, function(x) sum(is.na(x)))
sapply(index.data, function(x) sum(is.na(x)))
sapply(survey.data.misstab, function(x) sum(is.na(x)))
sapply(alters.data.misstab, function(x) sum(is.na(x)))

