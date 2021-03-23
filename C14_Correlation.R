### C14_Correlation.R
### generate correlation plot and tables, and check if anything stands out
### author: Minxin Lu
### date: 2021-3-22
### input: C01_ReadData.R
### output: corrplot, corrtable

library(fastDummies)
library(caret)
source("C01_ReadData.R")


# remove the things that are not of interest in examining correlation
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
dataB.c14$S <- as.numeric(dataB.c14$S)
dataB.c14$U <- as.numeric(dataB.c14$U)


# change factors to dummies
dummyB.c14 <- fastDummies::dummy_cols(dataB.c14, remove_first_dummy = TRUE)
dummyB.c14 <- dummyB.c14 %>% select(-all_of(c(factor_varB,ordinal_varB)))

# remove zero variance varibales
nearZeroVarName <- nearZeroVar(numericB.c14,names = TRUE)
dummyB.c14.cleaned <- dummyB.c14 %>% select(-all_of(c(nearZeroVarName)))
  
##### factors to numbers #####
numericB.c14 <- dummyB.c14.cleaned
numericB.c14 = apply(numericB.c14,2,as.numeric)


##### corr #####
corr1 <- numericB.c14 %>%
  as.matrix %>%
  cor(use = "pairwise.complete.obs")%>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)
write.csv(corr1, file = "../output/correlation.csv")

##### check for large correlation >0.8#####
large_corr1 <- filter(corr1, (abs(value) > .8 & value != 1))
large_corr1$value <- round(large_corr1$value,3)
large_corr2 <- large_corr1 %>% mutate(corrName=paste(var1,"+",var2),
                                      sign = ifelse(value>0,"pos","neg"),
                                      absValue = abs(value))
# plot 
pdf("../output/large_corrplot0.8.pdf") 
ggplot(data = large_corr2, aes(x=reorder(corrName,absValue),y=absValue,fill=sign))+
  geom_bar(stat="identity")+
  geom_text(aes(label=value),vjust=0.5,hjust=1.5,color="black",size=3.5)+
  ggtitle("Variable pairs with correlation > 0.8 or <-0.8") +
  xlab("Variable pairs") + ylab("correlation")+
  coord_flip()
dev.off()
write.csv(large_corr1, file = "../output/large_corr.csv")

##### check for correlation with DD #####
DD_corr1 <-corr1 %>% filter(var1=="DD", var2!="DD")
DD_corr1$value <- round(DD_corr1$value,3)
DD_corr2 <- DD_corr1 %>% mutate(corrName=paste(var1,"+",var2),
                                      sign = ifelse(value>0,"pos","neg"),
                                      absValue = abs(value))
DD_corr3 <- filter(DD_corr2, (absValue) > .2)
# plot 
pdf("../output/corrplot_DD.pdf") 
ggplot(data = DD_corr3, aes(x=reorder(corrName,absValue),y=absValue,fill=sign))+
  geom_bar(stat="identity")+
  geom_text(aes(label=value),vjust=0.5,hjust=1.5,color="black",size=3)+
  ggtitle("Correlation between actual distribution and other variables") +
  xlab("Variable pairs") + ylab("correlation")+
  coord_flip()
dev.off()
write.csv(DD_corr1, file = "../output/corr_DD.csv")

##### check with individual variables #####
summary(dataB.c14$Y)
summary(dataB.c14$X)
table(dataB.c14$W)
check = dummyB.c14.cleaned %>% select(Y_3,AB_3)
table(dummyB.c14.cleaned$AB_3)

summary(dataB.c14$T)
summary(dataB.c14$Q)
