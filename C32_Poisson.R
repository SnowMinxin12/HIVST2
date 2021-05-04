### C32_Poisson.R
### author: 
### date: 2021-4-12
### Possion or negative binomial model for aim 2 & 3
### (1) check model assumption
### (2) fit model with model selection: Elestic Net, best subset selection, AIC
### input: (1) the function F32_Poisson.R, 
###         or negative binomial or zero-inflated poisson/negative binomial 
###        (2) dataset
### output: (1) model (2) important variables (3) coefficients for all variables
setwd("/Users/ZhentaoYu/Desktop/SASUniversityEdition/myfolders/bios841")
joint<-read.csv("jointdataB.c22.csv",1)
summary(joint)
ncol(joint)
view(joint)
summary(m1 <- glm(test_kits_actual ~.-confirm_code-arm, family="poisson", data=joint))

joint[joint=="SKIP"]<-0
factor_varB<-c("sex_orientation",'marital_status','education_level','sex_orientation_disclose',
               'freq_discuss','province','city')
cont_varB<-setdiff(colnames(joint),factor_varB)
joint[cont_varB] <- lapply(joint[cont_varB],as.numeric)
joint[factor_varB] <- lapply(joint[factor_varB],factor)

joint1<-na.omit(joint)
library(caret)
library(glmnet)
set.seed(103)
cv_5 = trainControl(method = "cv", number = 5)
joint_elnet = train(
  test_kits_actual ~ .-confirm_code-arm, 
  data = joint1,
  family = "poisson",
  method = "glmnet",
  trControl = cv_5
)
joint2 <- glmnet(data.matrix(subset(joint1,select=-c(confirm_code,arm,test_kits_actual))), 
                        matrix(joint1$test_kits_actual,ncol=1),
                        family = "poisson", 
                        lambda = joint_elnet$bestTune$lambda, 
                        alpha =joint_elnet$bestTune$alpha)

#sensitivity analysis of aim2
estimate<-function(data,indices){
  a<-data[indices, ]
  modelfit <- glm(test_kits_actual ~ arm +education_level+stable_3months+casual_3months+women_3months
                    +freq_discuss+volunteer_community+help_community+nondating_community+age+hang_out_rel0Proportion
                    +hang_out_rel1Count +hang_out_rel4Count+hang_out_ident0Count+hang_out_ident0Proportion
                    +hang_out_ident3Proportion+hang_out_orien0Proportion, data = joint1,  family="poisson")
  modelfit$coefficients
}
library(boot)
boots <- boot(data = joint1, statistic = estimate, R=500)
ci<-c()
for (i in 1:22){
  c<-boot.ci(boots, type="perc",index=i)$percent[c(4:5)]
  ci<-rbind(ci,c)
}
rownames(ci)<-rownames(as.matrix(modelfit$coefficients))
ci<-round(ci,digit=3)
write.csv(ci,"ci.csv",row.names = T)
