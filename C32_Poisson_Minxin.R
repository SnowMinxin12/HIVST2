### C32_Poisson.R
### author: Minxin Lu
### date: 2021-5-4
### Possion or negative binomial model for aim 2 & 3
### (1) check model assumption
### (2) fit model with model selection: Elestic Net
### input: (1) the function F32_Poisson.R, 
###         or negative binomial or zero-inflated poisson/negative binomial 
###        (2) dataset
### output: (1) model (2) important variables (3) coefficients for all variables

library(caret)
library(glmnet)
library(dplyr)
library("ggplot2")

dataB.c32<-read.csv("../data/jointdataB_c22_0422.csv")[,-1]
dataB.c32[dataB.c32=="SKIP"]<-0
dataB.c32 = dataB.c32[,!colnames(dataB.c32)=="confirm_code"]

# change province to be 0 and 1=not equal to 0
dataB.c32$province01 <- ifelse(dataB.c32$province==0,0,1)
dataB.c32 = dataB.c32[,!colnames(dataB.c32)=="province"]
# city has too many levels so remove city
dataB.c32 = dataB.c32[,!colnames(dataB.c32)=="city"]


# factor variables
factor_varB <- c("arm","marital_status","sex_orientation","anal_sex_role")

#ordinal variables are treated as continous
cont_varB<-setdiff(colnames(dataB.c32),factor_varB)
dataB.c32[cont_varB] <- lapply(dataB.c32[cont_varB],as.numeric)
dataB.c32[factor_varB] <- lapply(dataB.c32[factor_varB],factor)


dataB.c32.narm<-na.omit(dataB.c32)
##### Aim 2: baseline 309 ######
# model with test_kits_request
cv_5 = trainControl(method = "cv", number = 5)
if(FALSE){
  set.seed(1)
  
  dataB_elnet_cv = train(
    test_kits_actual ~ ., 
    data = dataB.c32.narm,
    family = "poisson",
    method = "glmnet",
    trControl = cv_5
  )
  dataB_elnet <- glmnet(x = data.matrix(subset(dataB.c32.narm,select=-c(test_kits_actual))), 
                        y = matrix(dataB.c32.narm$test_kits_actual,ncol=1),
                        family = "poisson", 
                        lambda = dataB_elnet_cv$bestTune$lambda, 
                        alpha =dataB_elnet_cv$bestTune$alpha)
  dataB_elnet$beta
}


# model without test_kits_request
set.seed(1)
dataB_elnet_cv2 = train(
  test_kits_actual ~ .-test_kits_request, 
  data = dataB.c32.narm,
  family = "poisson",
  method = "glmnet",
  trControl = cv_5
)

dataB_elnet2 <- glmnet(x = data.matrix(subset(dataB.c32.narm,select=-c(test_kits_actual,test_kits_request))), 
                      y = matrix(dataB.c32.narm$test_kits_actual,ncol=1),
                      family = "poisson", 
                      lambda = dataB_elnet_cv2$bestTune$lambda, 
                      alpha =dataB_elnet_cv2$bestTune$alpha)
importantVarB = data.frame(dataB_elnet2$beta[,1])
importantVarB$Variable = rownames(importantVarB)
colnames(importantVarB)=c("beta","Variable")
importantVarB = importantVarB %>% filter(beta!=0)
importantVarB$Variable
write.csv(matrix(importantVarB$Variable),file="../output/importantVarB.csv")


##### Aim 2 bootstrap frequency #####
B=500
boot_coef = matrix(data = NA, nrow = nrow(data.frame(dataB_elnet2$beta[,1])),ncol=B)
rownames(boot_coef)=rownames(data.frame(dataB_elnet2$beta[,1]))
for(i in 1:B){
  set.seed(i)
  indices = sample(1:nrow(dataB.c32.narm),nrow(dataB.c32.narm),replace=T)
  xx = data.matrix(subset(dataB.c32.narm,select=-c(test_kits_actual,test_kits_request)))[indices, ]
  yy = matrix(dataB.c32.narm[indices,"test_kits_actual"],ncol=1)
  aim2modelfit <- glmnet(x = xx, 
                         y = yy,
                         family = "poisson", 
                         lambda = dataB_elnet_cv2$bestTune$lambda, 
                         alpha =dataB_elnet_cv2$bestTune$alpha)
  boot_coef[,i] = matrix(aim2modelfit$beta)
}
boot_coef = as.data.frame(boot_coef)
boot_coef1 =boot_coef
boot_coef$Zeros<-rowSums(boot_coef==0)
boot_coef$selectedfreqency = (500-boot_coef$Zeros)/500
boot_coef$Varname = rownames(boot_coef)

boot_coef2 = boot_coef %>% filter(selectedfreqency>0.6)

ggplot(boot_coef2,aes(x= reorder(Varname,-selectedfreqency),selectedfreqency))+
  geom_bar(stat ="identity")+
  labs(title="Important Variables for Aim 2",
       x ="Variable name", y = "Probability of being selected as important")
  

##### Aim 3: baseline+survey 207 ######
dataBSA.c32<-read.csv("../data/jointdataBSA_c22_0422.csv")[,-1]
dataBSA.c32[dataBSA.c32=="SKIP"]<-0
dataBSA.c32 = dataBSA.c32[,colnames(dataBSA.c32) %in% c("A.prior_hiv_test_1","B.confirm_code")]

dataB.c32<-read.csv("../data/jointdataB_c22_0422.csv")[,-1]
dataB.c32[dataB.c32=="SKIP"]<-0

joint = merge(dataB.c32,dataBSA.c32,by.x = "confirm_code",by.y="B.confirm_code",all.x=TRUE)
joint$A.prior_hiv_test_1 = ifelse(is.na(joint$A.prior_hiv_test_1),0,joint$A.prior_hiv_test_1)
joint = joint[,!colnames(joint)=="confirm_code"]

# change province to be 0 and 1=not equal to 0
joint$province01 <- ifelse(joint$province==0,0,1)
joint = joint[,!colnames(joint)=="province"]
# city has too many levels so remove city
joint = joint[,!colnames(joint)=="city"]


# factor variables
factor_varB <- c("arm","marital_status","sex_orientation","anal_sex_role")

#ordinal variables are treated as continous
cont_varB<-setdiff(colnames(joint),factor_varB)
joint[cont_varB] <- lapply(joint[cont_varB],as.numeric)
joint[factor_varB] <- lapply(joint[factor_varB],factor)


joint.narm<-na.omit(joint)

set.seed(1)
cv_5 = trainControl(method = "cv", number = 5)
joint.narm_elnet_cv2 = train(
  A.prior_hiv_test_1 ~ .-test_kits_request-test_kits_actual, 
  data = joint.narm,
  family = "poisson",
  method = "glmnet",
  trControl = cv_5
)

joint.narm_elnet2 <- glmnet(x = data.matrix(subset(joint.narm,select=-c(A.prior_hiv_test_1,test_kits_request,test_kits_actual))), 
                       y = matrix(joint.narm$A.prior_hiv_test_1,ncol=1),
                       family = "poisson", 
                       lambda = joint.narm_elnet_cv2$bestTune$lambda, 
                       alpha =joint.narm_elnet_cv2$bestTune$alpha)

importantVarNewAlter = data.frame(joint.narm_elnet2$beta[,1])
importantVarNewAlter$Variable = rownames(importantVarNewAlter)
colnames(importantVarNewAlter)=c("beta","Variable")
importantVarNewAlter = importantVarNewAlter %>% filter(beta!=0)
importantVarNewAlter$Variable
write.csv(matrix(importantVarNewAlter$Variable),file="../output/importantVar_Aim3.csv")

##### Aim 3 bootstrap frequency #####
B=500
boot_coef3 = matrix(data = NA, nrow = nrow(data.frame(joint.narm_elnet2$beta[,1])),ncol=B)
rownames(boot_coef3)=rownames(data.frame(joint.narm_elnet2$beta[,1]))
for(i in 1:B){
  set.seed(i)
  indices = sample(1:nrow(joint.narm),nrow(joint.narm),replace=T)
  xx = data.matrix(subset(joint.narm,select=-c(A.prior_hiv_test_1,test_kits_request,test_kits_actual)))[indices, ]
  yy = matrix(joint.narm[indices,"A.prior_hiv_test_1"],ncol=1)
  aim3modelfit <- glmnet(x = xx, 
                         y = yy,
                         family = "poisson", 
                         lambda = joint.narm_elnet_cv2$bestTune$lambda, 
                         alpha =joint.narm_elnet_cv2$bestTune$alpha)
  boot_coef3[,i] = matrix(aim3modelfit$beta)
}
boot_coef3 = as.data.frame(boot_coef3)

boot_coef3$Zeros<-rowSums(boot_coef3==0)
boot_coef3$selectedfreqency = (500-boot_coef3$Zeros)/500
boot_coef3$Varname = rownames(boot_coef3)

boot_coef3 = boot_coef3 %>% filter(selectedfreqency>0.6)

ggplot(boot_coef3,aes(x= reorder(Varname,-selectedfreqency),selectedfreqency))+
  geom_bar(stat ="identity")+
  labs(title="Important Variables for Aim 3",
       x ="Variable name", y = "Probability of being selected as important")
