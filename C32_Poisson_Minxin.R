### C32_Poisson.R
### author: 
### date: 2021-4-12
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
set.seed(1)
cv_5 = trainControl(method = "cv", number = 5)
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

##### Aim 3: baseline+survey 207 ######
dataBS.c32<-read.csv("../data/jointdataBS_c22_0422.csv")[,-1]
dataBS.c32[dataBS.c32=="SKIP"]<-0
dataBS.c32 = dataBS.c32[,!colnames(dataBS.c32)=="confirm_code"]
# change province to be 0 and 1=not equal to 0
dataBS.c32$B.province01 <- ifelse(dataBS.c32$B.province==0,0,1)
dataBS.c32 = dataBS.c32[,!colnames(dataBS.c32)=="B.province"]
# city has too many levels so remove city
dataBS.c32 = dataBS.c32[,!colnames(dataBS.c32) %in% c("B.city",'S.health_center')]


# factor variables
factor_varBS <- c("B.arm","B.marital_status","B.sex_orientation","B.anal_sex_role",
                  'S.most_important','S.result_mostrecent',
                  'S.anal_sex_role','S.test_pref')

#ordinal variables are treated as continous
cont_varBS<-setdiff(colnames(dataBS.c32),factor_varBS)
dataBS.c32[cont_varBS] <- lapply(dataBS.c32[cont_varBS],as.numeric)
dataBS.c32[factor_varBS] <- lapply(dataBS.c32[factor_varBS],factor)

dataBS.c32.narm<-na.omit(dataBS.c32)

set.seed(1)
dataBS_elnet_cv2 = train(
  B.test_kits_actual ~ .-B.test_kits_request, 
  data = dataBS.c32.narm,
  family = "poisson",
  method = "glmnet",
  trControl = cv_5
)

dataBS_elnet2 <- glmnet(x = data.matrix(subset(dataBS.c32.narm,select=-c(B.test_kits_actual,B.test_kits_request))), 
                       y = matrix(dataBS.c32.narm$B.test_kits_actual,ncol=1),
                       family = "poisson", 
                       lambda = dataBS_elnet_cv2$bestTune$lambda, 
                       alpha =dataBS_elnet_cv2$bestTune$alpha)

importantVarBS = data.frame(dataBS_elnet2$beta[,1])
importantVarBS$Variable = rownames(importantVarBS)
colnames(importantVarBS)=c("beta","Variable")
importantVarBS = importantVarBS %>% filter(beta!=0)
importantVarBS$Variable
write.csv(matrix(importantVarB$Variable),file="../output/importantVarB.csv")
