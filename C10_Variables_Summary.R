###  C10_Variables_Summary.R
###  summary of things done in this file
###  author: Ziqi Ye
###  date: 2021-4-29
###  input: jointdataB_c22_0422.csv, jointdataBS_c22_0422.csv, jointdataBSA_c22_0422.csv
###  output: output_file_name

library(dplyr)
library(gtsummary)
library(readr)

DataB <- read_csv("/Users/ziqiye/Desktop/BIOS841/Project1/data/jointdataB_c22_0422.csv")
DataBS <- read_csv("/Users/ziqiye/Desktop/BIOS841/Project1/data/jointdataBS_c22_0422.csv")
DataBSA <- read_csv("/Users/ziqiye/Desktop/BIOS841/Project1/data/jointdataBSA_c22_0422.csv")

##### Baseline 309 data: DataB #####
str(DataB)
summary(DataB)

binary_summaryB <- DataB %>% select(sex_orientation_disclose, anal_sex_3months, women_3months, volunteer_community,
                                    help_community, dating_community, nondating_community, donated_community,
                                    change_community, none_community, prior_hiv_test)

binary_summaryB %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))


factor_summaryB <- DataB %>% select(arm, marital_status, education_level, monthly_income, sex_orientation,
                                   anal_sex_role, anal_sex_condom, stable_condoms_3months, casual_condoms_3months,
                                   freq_discuss, province, city)

factor_summaryB %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))


cont_summaryB <- DataB %>% select(anal_sex_tot, stable_3months, casual_3months, test_kits_request,
                                 test_kits_actual, age, hang_outCount, hang_out_rel0Count, hang_out_rel0Proportion, 
                                 hang_out_rel1Count, hang_out_rel1Proportion, hang_out_rel2Count, hang_out_rel2Proportion,
                                 hang_out_rel3Count, hang_out_rel3Proportion, hang_out_rel4Count, hang_out_rel4Proportion,
                                 hang_out_ident0Count, hang_out_ident0Proportion, hang_out_ident1Count,
                                 hang_out_ident1Proportion, hang_out_ident3Count, hang_out_ident3Proportion,
                                 hang_out_orien0Count, hang_out_orien0Proportion, hang_out_orien1Count,
                                 hang_out_orien1Proportion, hang_out_orien2Count, hang_out_orien2Proportion,
                                 hang_out_orien3Count, hang_out_orien3Proportion, hang_out_sex0Count, hang_out_sex0Proportion)
cont_summaryB %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))

##### 207 data: DataBS #####

factor_summaryBS<- DataBS %>% select(B.arm, B.marital_status, B.education_level, B.monthly_income, B.sex_orientation,
                                B.anal_sex_role, B.anal_sex_condom, B.stable_condoms_3months, B.casual_condoms_3months, 
                                B.freq_discuss, B.province, B.city, B.test_kits_actual, S.most_important, B.casual_3months,
                                S.result_mostrecent, S.health_center, S.anal_sex_role, S.anal_sex_condom, 
                                S.anal_recent_condom, S.rush_freq, S.new_drugs_1month, S.women_condoms_3months, S.test_pref)

binary_summaryBS <- DataBS %>% select(B.sex_orientation_disclose, B.anal_sex_3months, B.women_3months, 
                                      B.volunteer_community, B.help_community, B.dating_community, B.nondating_community, 
                                      B.donated_community, B.change_community, B.none_community, B.prior_hiv_test, 
                                      S.situation_a, S.situation_b, S.situation_c, S.situation_d, S.situation_g, S.situation_h,
                                      S.self_present, S.anal_sex_3months, S.use_rush)

cont_summaryBS<- DataBS %>% select(B.anal_sex_tot, B.stable_3months, B.test_kits_request, B.age, 
                                   B.hang_outCount, B.hang_out_rel0Count,B.hang_out_rel0Proportion, B.hang_out_rel1Count, 
                                   B.hang_out_rel1Proportion, B.hang_out_rel2Count, B.hang_out_rel2Proportion, 
                                   B.hang_out_rel3Count, B.hang_out_rel3Proportion, B.hang_out_rel4Count, B.hang_out_rel4Proportion,
                                   B.hang_out_ident0Count, B.hang_out_ident0Proportion, B.hang_out_ident1Count,
                                   B.hang_out_ident1Proportion, B.hang_out_ident3Count, B.hang_out_ident3Proportion,
                                   B.hang_out_orien0Count, B.hang_out_orien0Proportion, B.hang_out_orien1Count,
                                   B.hang_out_orien1Proportion, B.hang_out_orien2Count, B.hang_out_orien2Proportion,
                                   B.hang_out_orien3Count, B.hang_out_orien3Proportion, B.hang_out_sex0Count,
                                   B.hang_out_sex0Proportion, S.kits_no, S.kits_dist, S.selftest_no, S.anal_sex_tot,
                                   S.women_3months)
  
factor_summaryBS %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
binary_summaryBS %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
cont_summaryBS %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))

##### 98 data: DataBSA #####

factor_summaryBSA<- DataBSA %>% select(B.arm, B.marital_status, B.education_level, B.monthly_income, B.sex_orientation, 
                                       B.anal_sex_role, B.anal_sex_condom, B.stable_condoms_3months, B.casual_3months, 
                                       B.casual_condoms_3months, B.freq_discuss, B.volunteer_community, B.help_community, 
                                       B.dating_community, B.nondating_community, B.donated_community, B.change_community, 
                                       B.none_community, B.prior_hiv_test, B.province, B.city, S.situation_a, S.situation_b, 
                                       S.situation_c, S.situation_d, S.situation_g, S.situation_h, S.most_important, 
                                       S.result_mostrecent, S.health_center, S.anal_sex_role, S.anal_sex_condom, 
                                       S.anal_recent_condom, S.rush_freq, S.new_drugs_1month, S.women_condoms_3months, S.test_pref, 
                                       A.arm_0, A.arm_1, A.arm_2, A.result_0, A.relation_index_0, A.relation_index_1, 
                                       A.relation_index_2, A.relation_index_3, A.relation_index_4, A.relation_index_7, 
                                       A.gender_identity_0, A.sex_orientation_0, A.sex_orientation_1, A.sex_orientation_2, 
                                       A.sex_orientation_3, A.sex_orientation_disclose_0, A.sex_orientation_disclose_1, 
                                       A.told_others_medwork_0, A.told_others_medwork_1, A.told_others_fam_0, A.told_others_fam_1, 
                                       A.told_others_friends_0, A.told_others_friends_1, A.told_others_coll_0, A.told_others_coll_1, 
                                       A.told_others_employ_1, A.stable_3months_0, A.stable_3months_1, A.stable_3months_3, 
                                       A.stable_condoms_3months_1, A.stable_condoms_3months_2, A.stable_condoms_3months_3, 
                                       A.casual_3months_0, A.casual_3months_1, A.casual_condoms_3months_2, A.casual_condoms_3months_3, 
                                       A.women_3months_0, A.women_3months_1, A.women_sex_tot_1, A.condom_recent_women_0, 
                                       A.share_know_hiv_0, A.share_know_hiv_1, A.share_know_test_0, A.share_know_test_1, 
                                       A.share_know_kit_0, A.share_know_kit_1, A.explain_interpret_0, A.explain_interpret_1, 
                                       A.recipient_notneeded_0, A.recipient_notneeded_1, A.recipient_present_0, 
                                       A.recipient_present_1, A.situation_a_0, A.situation_a_1, A.situation_c_0, 
                                       A.situation_c_1, A.situation_h_0, A.situation_h_1, A.time_to_test_0, A.time_to_test_1, 
                                       A.time_to_test_2, A.time_to_test_3, A.time_to_test_4, A.kit_easy_0, A.kit_easy_1, 
                                       A.test_with_index_0, A.test_with_index_1, A.index_result_1, A.index_result_2, 
                                       A.index_know_result_1, A.index_know_result_2, A.sex_with_index_0, A.sex_with_index_1, 
                                       A.sex_prior_0, A.sex_prior_1, A.partner_condom_0, A.partner_condom_1, A.prior_hiv_test_0, 
                                       A.prior_hiv_test_1, A.prior_test_method_0, A.prior_test_method_1, A.prior_test_result_1, 
                                       A.current_test_result_1, A.women_sex_post_0, A.women_sex_post_1, A.test_pref_0, 
                                       A.test_pref_2, A.marital_status_0, A.marital_status_1, A.marital_status_2, A.house_reg_0, 
                                       A.house_reg_1, A.house_reg_2, A.house_reg_3, A.education_level_0, A.education_level_1, 
                                       A.education_level_2, A.monthly_income_0, A.monthly_income_1, A.monthly_income_2, 
                                       A.monthly_income_3, A.monthly_income_4, A.province0_0, A.province0_1)

binary_summaryBSA <- DataBSA %>% select(B.sex_orientation_disclose, B.anal_sex_3months, B.women_3months, S.share_know_hiv, 
                                        S.share_know_test, S.share_know_kit, S.explain_interpret, S.recipient_notneeded, 
                                        S.recipient_present, S.self_present, S.anal_sex_3months, S.use_rush, S.women_3months, 
                                        A.age_17, A.age_18, A.age_19, A.age_20, A.age_21, A.age_22, A.age_23, A.age_24, A.age_25, 
                                        A.age_26, A.age_27, A.age_28, A.age_29, A.age_30, A.age_31, A.age_32, A.age_33, A.age_34, 
                                        A.age_35, A.age_36, A.age_38, A.age_39, A.age_40, A.age_41, A.age_42, A.age_43, A.age_45, 
                                        A.age_47, A.age_48, A.age_49, A.age_51, A.result_1, A.result_2, A.result_4, A.confirmation_0, 
                                        A.confirmation_1, A.relation_index_5, A.relation_index_6, A.gender_identity_1, A.gender_identity_3, 
                                        A.told_others_employ_0, A.stable_3months_2, A.stable_3months_4, A.stable_3months_5, 
                                        A.casual_3months_10, A.casual_3months_2, A.casual_3months_3, A.casual_3months_4, A.casual_3months_5, 
                                        A.casual_3months_6, A.casual_condoms_3months_1, A.women_sex_tot_0, A.women_sex_tot_10, 
                                        A.women_sex_tot_2, A.women_sex_tot_24, A.women_condoms_3months_0, A.women_condoms_3months_1, 
                                        A.women_condoms_3months_2, A.women_condoms_3months_3, A.condom_recent_women_1, A.situation_f_0, 
                                        A.kit_easy_2, A.index_know_result_0, A.prior_test_result_0, A.prior_test_result_2, 
                                        A.current_test_result_0, A.current_test_result_2, A.med_confirm_0, A.med_confirm_1, 
                                        A.women_sex_post_2, A.women_sex_post_74, A.women_condoms_post_0, A.women_condoms_post_1, 
                                        A.women_condoms_post_2, A.women_condoms_post_3, A.test_pref_1, A.test_pref_3) 

cont_summaryBSA<- DataBSA %>% select(B.anal_sex_tot, B.stable_3months, B.test_kits_request, B.test_kits_actual, B.age, 
                                     B.hang_outCount, B.hang_out_rel0Count, B.hang_out_rel0Proportion, B.hang_out_rel1Count, 
                                     B.hang_out_rel1Proportion, B.hang_out_rel2Count, B.hang_out_rel2Proportion, 
                                     B.hang_out_rel3Count, B.hang_out_rel3Proportion, B.hang_out_rel4Count, B.hang_out_rel4Proportion, 
                                     B.hang_out_ident0Count, B.hang_out_ident0Proportion, B.hang_out_ident1Count, 
                                     B.hang_out_ident1Proportion, B.hang_out_ident3Count, B.hang_out_ident3Proportion, 
                                     B.hang_out_orien0Count, B.hang_out_orien0Proportion, B.hang_out_orien1Count, 
                                     B.hang_out_orien1Proportion, B.hang_out_orien2Count, B.hang_out_orien2Proportion, 
                                     B.hang_out_orien3Count, B.hang_out_orien3Proportion, B.hang_out_sex0Count, 
                                     B.hang_out_sex0Proportion, S.kits_no, S.kits_dist, S.selftest_no, S.anal_sex_tot, 
                                     S.AlterSex0Count, S.AlterSex0Proportion, S.AlterRelation0Count, S.AlterRelation0Proportion, 
                                     S.AlterRelation1Count, S.AlterRelation1Proportion, S.AlterRelation2Count, 
                                     S.AlterRelation2Proportion, S.AlterRelation3Count, S.AlterRelation3Proportion, 
                                     S.AlterPT0Count, S.AlterPT0Proportion, S.AlterHivr0Count, S.AlterHivr0Proportion, 
                                     S.AlterPT_Sex0Count, S.AlterPT_Sex0Proportion)

factor_summaryBSA %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
binary_summaryBSA %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
cont_summaryBSA %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"))
