### C22_Correlation.R 
### generate correlation plot and tables, remove highly correlated variables
### author: Minxin Lu
### date: 2021-4-14
### input: C11_Modified_Variable_Class.R
### output: corrplot, corrtable

library(fastDummies)
library(caret)
jointdataB <- read_csv("../data/jointdataB_c21.csv")
jointdataB.c22 <- jointdataB[,colnames(jointdataB)!="X1"]
jointdataBS <- read_csv("../data/jointdataBS_c21.csv")
jointdataBS.c22 <- jointdataBS[,colnames(jointdataBS)!="X1"]
jointdataBSA <- read_csv("../data/jointdataBSA_c21.csv")
jointdataBSA.c22 <- jointdataBSA[,colnames(jointdataBSA)!="X1"]


jointdata1.c22 = jointdataB.c22
var_removeB <-c(
  "hang_out_ident2Count","hang_out_ident2Proportion")
var_removeBS <- c(# very low prevalence
  "B.hang_out_ident2Count","B.hang_out_ident2Proportion",
  "S.situation_e","S.situation_f"
)
var_removeBSA <- c(# very low prevalence
  "B.hang_out_ident2Count","B.hang_out_ident2Proportion",
  "A.casual_3months_8","A.casual_3months_7","A.stable_3months_8","A.women_sex_post_8",
  "S.situation_f","S.situation_e","S.health_center_SKIP",
  "A.sex_birth_0",# all should be zero so count does not make sense,
  "A.situation_b_1",# all ones so count does not make sense
  "A.situation_d_0", "A.situation_d_1","A.situation_e_1","A.situation_g_1","A.situation_f_1"
)
jointdata1.c22 <- jointdata1.c22[,!colnames(jointdata1.c22) %in% var_removeB]

write.csv(jointdata1.c22,file="../data/jointdataB.c22.csv")

# change factors to dummies
dummy.c22 <- fastDummies::dummy_cols(jointdata1.c22, remove_first_dummy = TRUE, ignore_na=TRUE)

##### factors to numbers #####
numeric.c22 <- dummy.c22
numeric.c22 <- apply(numeric.c22,2,as.numeric)

# remove repeated correlations
corr1_bar <- numeric.c22 %>% 
  as.matrix %>%
  cor(use = "pairwise.complete.obs")%>%
  as.data.frame
corr1_bar[lower.tri(corr1_bar,diag = TRUE)]<-"repeated"
corr1_bar_col <- corr1_bar %>% 
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>% 
  filter(!value =="repeated")
corr1_bar_col$value <- as.numeric(corr1_bar_col$value)

# plot large correlation
large_corr90_bar <- filter(as.data.frame(corr1_bar_col), (abs(value) > .9))
large_corr95_bar <- filter(as.data.frame(corr1_bar_col), (abs(value) > .95))


large_corr95_bar$value <- round(large_corr95_bar$value,3)
large_corr95_bar <- large_corr95_bar %>% mutate(corrName=paste(var1,"+",var2),
                                      sign = ifelse(value>0,"pos","neg"),
                                      absValue = abs(value))

pdf("../output/large_corrplot0.95.pdf") 
ggplot(data = large_corr95_bar, aes(x=reorder(corrName,absValue),y=absValue,fill=sign))+
  geom_bar(stat="identity")+
  geom_text(aes(label=value),vjust=0.5,hjust=1.5,color="black",size=3.5)+
  ggtitle("Variable pairs with correlation > 0.95 or <-0.95") +
  xlab("Variable pairs") + ylab("correlation")+
  coord_flip()
dev.off()

#[INCOMPLETE]
##### check for correlation with DD #####
DD_corr1 <-corr1 %>% filter(var1=="DD",var2!="DD")
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
  ggtitle("Correlation between actual distribution and other variables >0.2") +
  xlab("Variable pairs") + ylab("correlation")+
  coord_flip()
dev.off()
write.csv(DD_corr1, file = "../output/corr_DD.csv")

##### check with individual variables #####
na.omit(as.data.frame(numericB.c14) %>% select(Y_3,AB_3))# only 2 obs
na.omit(as.data.frame(numericB.c14) %>% select(Y_2,AB_3)) # only 2 obs
na.omit(as.data.frame(numericB.c14) %>% select(AC,Y_3)) # only 2 obs
na.omit(as.data.frame(numericB.c14) %>% select(AC,Y_2)) # only 2 obs
na.omit(as.data.frame(numericB.c14) %>% select(AA,Y_3)) # only 2 obs
na.omit(as.data.frame(numericB.c14) %>% select(AA,Y_2)) # only 2 obs
dim(na.omit(as.data.frame(numericB.c14) %>% select(Q_3,T_3))) #reasonable

check = na.omit(as.data.frame(numericB.c14) %>% select(DD,CX))
cor(check)
