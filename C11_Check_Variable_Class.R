### C11_Check_Variable_Class.R
### Check factors as factors, numbers as numbers
### author: Zhentao Yu, Minxin Lu
### date: 2021-3-22
### input: C01_ReadData.R
### output: a new dataset with variables in the correct class

source(C01_ReadData.R)

# for baseline 309 index data I did not checke the following de-selected variables:
dataB.c14 <- DataB %>% 
  select(-c( "A",# deposit
             "B", # application date
             "G", # whether you have a QR code
             "AK","AL","AM","AN","AO","AP","AQ.A","AQ.B","AQ.C","AQ.D","AQ.E","AQ.F","AR","AS","AT",
             "AU","AV","AW","AX","AY","AZ","BA","BB.A","BB.B","BB.C","BB.D","BB.E","BB.F","BC","BD","BE",
             "BF","BG","BH","BI","BJ","BK","BL","BM.A","BM.B","BM.C","BM.D","BM.E","BM.F","BN","BO","BP","BQ",
             "BR","BS","BT","BU","BV","BW.A","BW.B","BW.C","BW.D","BW.E","BW.F","BX","BY","BZ", 
             "CA","CB","CC","CD","CE","CF.A","CF.B","CF.C","CF.D","CF.E","CF.F","CG","CH","CI",
             "CJ","CK","CL","CM","CN","CO.A","CO.B","CO.C","CO.D","CO.E","CO.F","CP","CQ","CR",
             "DC", #confirmation code                
  ))

# factor variables
factor_varB <- c("F","H","K","P","AH","CT","CU","CW","DA","DB")
#ordinal variables
ordinal_varB <- c("I","J","Q","T","V","X","Y","AB","AD","AF","AG","AI","CV")
#continous variables
cont_varB <- setdiff(colnames(dataB.c14),c(factor_varB,ordinal_varB))

dataB.c14[factor_varB] = lapply(dataB.c14[factor_varB],factor)
dataB.c14[ordinal_varB] = lapply(dataB.c14[ordinal_varB],factor)
# contiunous variable S and U are missclassified as factor variables
dataB.c14$S <- as.numeric(dataB.c14$S)
dataB.c14$U <- as.numeric(dataB.c14$U)