### C11_Check_Variable_Class.R
### add 1 to all factor and binary variables 
### Check factors as factors, numbers as numbers
### author: Zhentao Yu, Minxin Lu
### date: 2021-3-31
### input: C01_ReadData.R
### output: a new dataset with variables in the correct class
###         DataB for baseline 307 data
###         DataS for Survey 207 data
###         DataA for Alter 269 data

library(dplyr)
source("C01_ReadData.R")
##### Baseline 309 data #####

# date variables
date_varB <-"B"
# binary variables\
binary_varB <- c("D","E","G","l","M.A","M.B","M.C","M.D","M.E",
                 "M.F","M.N","R","W","Z","AC","AE.A","AE.B",
                 "AE.C","AE.D","AE.E","AE.F","AE.G",
                 "AJ.A","AJ.B","AJ.C","AJ.D","AJ.E","AJ.F","AJ.G",
                 "AP","AQ.A","AQ.B","AQ.C","AQ.D","AQ.E","AQ.F","AT",
                 "AU","BA","BB.A","BB.B","BB.C","BB.D","BB.E","BB.F","BE",
                 "BF","BL","BM.A","BM.B","BM.C","BM.D","BM.E","BM.F","BP",
                 "BQ","BR","BW.A","BW.B","BW.C","BW.D","BW.E","BW.F","BZ",
                 "CA","CF.A","CF.B","CF.C","CF.D","CF.E","CF.F","CI",
                 "CJ","CO.A","CO.B","CO.C","CO.D","CO.E","CO.F","CR",
                 "CS","CY","CZ")
# factor variables
factor_varB <- c("F","H","K","P","AH","AL","AM","AN",
                 "AW","AX","AY",  "BH","BI","BJ",  "BT","BU","BV",
                 "CC","CD","CE","CL","CM","CN",
                 "CT","CU","CW","DA","DB")
#ordinal variables
ordinal_varB <- c("I","J","Q","T","V","X","Y","AB","AD","AF","AG","AI",
                  "AO","AR","AS","AZ","BC","BD","BK","BN","BO","BX","BY","CG","CH",
                  "CP","CQ","CV")
#unrelated variables 
tobedeleted_varB <- c("AK","AV","BG","BS","CB","CK")#initial name of people who index usually hang out with
                      
#continous variables
cont_varB <- setdiff(colnames(DataB),c(date_varB,binary_varB,factor_varB,ordinal_varB,tobedeleted_varB))


# factors and ordinals as factors
dataB.c11 <-DataB
dataB.c11[factor_varB] = lapply(dataB.c11[factor_varB],factor)
dataB.c11[ordinal_varB] = lapply(dataB.c11[ordinal_varB],factor)
dataB.c11[cont_varB] = lapply(dataB.c11[cont_varB],as.numeric)


# remove the initial names of closest people
# dataB.c11 <- dataB.c11[ , -which(names(dataB.c11) %in% tobedeleted_varB)]
DataB <- dataB.c11

##### Survey 207 data #####

# date variables
date_varS <- c('A')

# binary variables
binary_varS <- c('E','F','G','H','I','O','P','Q','R','S','Y','Z',
                 'AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','AK','AL','AM',
                 'AN','AO','AP','AQ','AR','AS',
                 "AT.A","AT.B","AT.C","AT.D","AT.E","AT.F","AT.G","AT.H",
                 'AV','AZ','BD','BI')

# factor variables
factor_varS <- c('J','K','L','M','N','T','U','V','W','X','AW','AX','AY',
                 'BB','BN')

#ordinal variables
ordinal_varS <- c('BC','BF','BH','BJ','BK','BM')

#unrelated variables 
unrelated_varS <- c()

#continous variables
cont_varS <- setdiff(colnames(DataS),c(date_varS,binary_varS,factor_varS,ordinal_varS,unrelated_varS))


# factors and ordinals as factors; continuous variables as numerical 
dataS.c11 <- DataS
dataS.c11[factor_varS] = lapply(dataS.c11[factor_varS],factor)
dataS.c11[ordinal_varS] = lapply(dataS.c11[ordinal_varS],factor)
dataS.c11[cont_varS] = lapply(dataS.c11[cont_varS],as.numeric)
DataS <- dataS.c11


##### Alter 269 data #####

# date variables
date_varA <- c('C')

# binary variables
binary_varA <- c('L','O','Q','U','Z',
                 'AC','AD','AE','AF','AG','AH','AI',
                 'AJ.A','AJ.B','AJ.C','AJ.D','AJ.E','AJ.F','AJ.G','AJ.H',
                 'AM','AP','AQ','AR','AS',
                 'BH','BI.A','BI.B','BI.C','BI.D','BI.E','BI.F','BL','BM','BS',
                 'BT.A','BT.B','BT.C','BT.D','BT.E','BT.F','BW','BX',
                 'CD','CE.A','CE.B','CE.C','CE.D','CE.E','CE.F',
                 'CH','CI','CJ','CO.A','CO.B','CO.C','CO.D','CO.E','CO.F',
                 'CR','CS','CX.A','CX.B','CX.C','CX.D','CX.E','CX.F',
                 'DA','DB','DG.A','DG.B','DG.C','DG.D','DG.E','DG.F',
                 'DJ')

# factor variables
factor_varA <- c('B','D','J','M','N','S',
                 'AL','AN','AO','AT','AU','AV','AW',
                 'BB','BD','BE','BF','BO','BP','BQ',
                 'BZ','CA','CB','CL','CM','CN','CU','CV','CW',
                 'DD','DE','DF','DK','DL')

#ordinal variables
ordinal_varA <- c('W','T','Y',
                  'AB','AK','AY',
                  'BA','BG','BJ','BK','BR','BU','BV',
                  'CC','CF','CG','CP','CQ','CY','CZ',
                  'DH','DI','DM','DN')

#unrelated variables 
unrelated_varA <- c('AJ.H', # none of above happened
                    'P',
                    'BC','BN','BY',
                    'CK','CT',
                    'DC')

#continous variables
cont_varA <- setdiff(colnames(DataA),c(date_varA,binary_varA,factor_varA,ordinal_varA,unrelated_varA))


# factors and ordinals as factors; continuous variables as numerical 
dataA.c11 <- DataA
dataA.c11[factor_varA] = lapply(dataA.c11[factor_varA],factor)
dataA.c11[ordinal_varA] = lapply(dataA.c11[ordinal_varA],factor)
dataA.c11[cont_varA] = lapply(dataA.c11[cont_varA],as.numeric)
DataA <- dataA.c11
