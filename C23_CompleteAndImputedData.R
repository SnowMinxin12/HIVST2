### C23_DataforModel
### author: Ziqi Ye
### date: 2021-4-12
### 
### (1) generate complete data
### (2) generate imputed data
### input: jointdataB.c22.csv
### output: datasets ready for analysis 

library(readr)
library(missForest)
library(mice)

DataB <- read_csv("/Users/ziqiye/Desktop/BIOS841/Project1/data/jointdataB_c22_0422.csv")
DataBS <- read_csv("/Users/ziqiye/Desktop/BIOS841/Project1/data/jointdataBS_c22_0422.csv")
DataBSA <- read_csv("/Users/ziqiye/Desktop/BIOS841/Project1/data/jointdataBSA_c22_0422.csv")

# summary of variables - with missing or not
colSums(is.na(DataB))
colSums(is.na(DataBS))
colSums(is.na(DataBSA))

#Generate 10% missing values at Random
DataB.mis <- prodNA(DataB, noNA = 0.1)

#Check missing values introduced in the data
summary(DataB.mis)

# factor variables
# not sure with variable freq_discuss, is this one ordinal variable?
ordinals_varB <- c("stable_3months","stable_condoms_3months","casual_3months",
                 "casual_condoms_3months")
ordinals_varBS <- c("B.stable_3months","B.stable_condoms_3months","S.self_present","S.most_important",
                    "S.result_mostrecent","S.health_center","S.new_drugs_1month","S.women_3months")
ordinals_varBSA <- c("S.share_know_hiv","S.share_know_test","S.share_know_kit","S.explain_interpret",
                     "S.recipient_notneeded","S.recipient_present","S.self_present","S.most_important",
                     "S.result_mostrecent","S.health_center","S.women_3months")
continuous_varBSA <- c("S.AlterSex0Count","S.AlterSex0Proportion","S.AlterRelation0Count",
                       "S.AlterRelation0Proportion","S.AlterRelation1Count","S.AlterRelation1Proportion",
                       "S.AlterRelation2Count","S.AlterRelation2Proportion","S.AlterRelation3Count",
                       "S.AlterRelation3Proportion","S.AlterPT0Count","S.AlterPT0Proportion",
                       "S.AlterHivr0Count","S.AlterHivr0Proportion","S.AlterPT_Sex0Count",
                       "S.AlterPT_Sex0Proportion")

# ordinals as factors
DataB.c23 <-DataB
DataB.c23[ordinals_varB] = lapply(DataB.c23[ordinals_varB],factor)

DataBS.c23 <-DataBS
DataBS.c23[ordinals_varBS] = lapply(DataBS.c23[ordinals_varBS],factor)

DataBSA.c23 <-DataBSA
DataBSA.c23[ordinals_varBSA] = lapply(DataBSA.c23[ordinals_varBSA],factor)
DataBSA.c23[continuous_varBSA] = lapply(DataBSA.c23[continuous_varBSA],as.numeric)

# impute data
imputed_DataB <- mice(DataB.c23[ordinals_varB], m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(imputed_DataB)

imputed_DataBS <- mice(DataBS.c23[ordinals_varBS], m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(imputed_DataBS)

imputed_DataBSA_ordinal <- mice(DataBSA.c23[ordinals_varBSA], m=5, maxit = 50, method = 'polyreg', seed = 500)
imputed_DataBSA_cont <- mice(DataBSA.c23[continuous_varBSA], m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_DataBSA_ordinal)
summary(imputed_DataBSA_cont)

# imputed dataset
DataB_imputed <- complete(imputed_DataB,1)
DataBS_imputed <- complete(imputed_DataBS,1)
DataBSA_imputed_ordinal <- complete(imputed_DataBSA_ordinal,1)
DataBSA_imputed_cont <- complete(imputed_DataBSA_cont,1)

# complete dataset
DataB_complete <- DataB
DataB_complete[ordinals_varB] <- DataB_imputed

DataBS_complete <- DataBS
DataBS_complete[ordinals_varBS] <- DataBS_imputed

DataBSA_complete <- DataBSA
DataBSA_complete[ordinals_varBSA] <- DataBSA_imputed_ordinal
DataBSA_complete[continuous_varBSA] <- DataBSA_imputed_cont

colSums(is.na(DataB_complete))
colSums(is.na(DataBS_complete))
colSums(is.na(DataBSA_complete))

# check imputed results

check_originalB <- DataB %>% select(stable_3months, stable_condoms_3months, casual_3months,
                                             casual_condoms_3months)
check_imputedB <- DataB_complete %>% select(stable_3months, stable_condoms_3months, casual_3months,
                                   casual_condoms_3months)

check_originalB %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
check_imputedB %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))

check_originalBS <- DataBS %>% select(B.stable_3months, B.stable_condoms_3months, S.self_present,
                                      S.most_important, S.result_mostrecent, S.health_center, 
                                      S.new_drugs_1month, S.women_3months)
check_imputedBS <- DataBS_complete %>% select(B.stable_3months, B.stable_condoms_3months, S.self_present,
                                              S.most_important, S.result_mostrecent, S.health_center, 
                                              S.new_drugs_1month, S.women_3months)

check_originalBS %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
check_imputedBS %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))

check_originalBSA <- DataBSA %>% select(S.share_know_hiv, S.share_know_test, S.share_know_kit,
                                        S.explain_interpret, S.recipient_notneeded, S.recipient_present,
                                        S.self_present, S.most_important, S.result_mostrecent, S.health_center,
                                        S.women_3months, S.AlterSex0Count, S.AlterSex0Proportion, 
                                        S.AlterRelation0Count, S.AlterRelation0Proportion, S.AlterRelation1Count, 
                                        S.AlterRelation1Proportion, S.AlterRelation2Count, S.AlterRelation2Proportion,
                                        S.AlterRelation3Count, S.AlterRelation3Proportion, S.AlterPT0Count,  
                                        S.AlterPT0Proportion, S.AlterHivr0Count, S.AlterHivr0Proportion,
                                        S.AlterPT_Sex0Count, S.AlterPT_Sex0Proportion)
check_imputedBSA <- DataBSA_complete %>% select(S.share_know_hiv, S.share_know_test, S.share_know_kit,
                                                S.explain_interpret, S.recipient_notneeded, S.recipient_present,
                                                S.self_present, S.most_important, S.result_mostrecent, S.health_center,
                                                S.women_3months, S.AlterSex0Count, S.AlterSex0Proportion, 
                                                S.AlterRelation0Count, S.AlterRelation0Proportion, S.AlterRelation1Count, 
                                                S.AlterRelation1Proportion, S.AlterRelation2Count, S.AlterRelation2Proportion,
                                                S.AlterRelation3Count, S.AlterRelation3Proportion, S.AlterPT0Count,  
                                                S.AlterPT0Proportion, S.AlterHivr0Count, S.AlterHivr0Proportion,
                                                S.AlterPT_Sex0Count, S.AlterPT_Sex0Proportion)

check_originalBSA %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))
check_imputedBSA %>% tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))

# the imputed results look good, the values with most percentages are given to missing values.
