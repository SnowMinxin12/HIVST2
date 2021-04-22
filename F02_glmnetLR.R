setwd("C:\\Users\\joshu\\Desktop\\BIOS 841\\Project 1")

library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(glmnet)
library(caret)
library(MLmetrics)

data <- read_csv("jointdataB.c22skipchange.csv") #Change SKIP in stable_3months to 0 for number = 0, Change SKIP in stable_condoms_3months to 0 for NEVER = 0,
                                                 #Change SKIP in casual_3months to 0 for number = 0, Change SKIP in casual_condoms_3months to 0 for NEVER = 0

data <- data %>% mutate(test_kits_dich = ifelse(test_kits_actual > 0,1,0)) #Dichotomize test kits disseminated

table(data$test_kits_actual,data$test_kits_dich)

y <- data %>% drop_na() %>% select(test_kits_dich)
x <- data %>% select(-c(obs,confirm_code,starts_with("test_kits_"),ends_with("Proportion"),test_kits_dich)) #Used count data for this analysis; sensitivity w/ proportion
x <- x %>% drop_na() #Complete cases (dropped = 3)
x <- data.matrix(x)
y <- data.matrix(y)


set.seed(1272)
n <- 306

train_rows <- sample(1:306, 0.5*n)
x.train <- x[train_rows,]
x.test <- x[-train_rows,]

y.train <- y[train_rows]
y.test <- y[-train_rows]

fit.lasso.best <- glmnet(x.test,y.test,family="binomial",alpha=0,type.measure="class") #lambda=0.872; fit.train0$min = 0.872

for(i in 0:10){
  assign(paste("fit.train", i, sep=""), cv.glmnet(x.train,y.train,type.measure="mse",alpha=i/10,family="binomial"))
}

yhat0 <- predict(fit.train0, s=fit.train0$lambda.1se, type="class", newx=x.test)
yhat1 <- predict(fit.train1, s=fit.train1$lambda.1se, type="class", newx=x.test)
yhat2 <- predict(fit.train2, s=fit.train2$lambda.1se, type="class", newx=x.test)
yhat3 <- predict(fit.train3, s=fit.train3$lambda.1se, type="class", newx=x.test)
yhat4 <- predict(fit.train4, s=fit.train4$lambda.1se, type="class", newx=x.test)
yhat5 <- predict(fit.train5, s=fit.train5$lambda.1se, type="class", newx=x.test)
yhat6 <- predict(fit.train6, s=fit.train6$lambda.1se, type="class", newx=x.test)
yhat7 <- predict(fit.train7, s=fit.train7$lambda.1se, type="class", newx=x.test)
yhat8 <- predict(fit.train8, s=fit.train8$lambda.1se, type="class", newx=x.test)
yhat9 <- predict(fit.train9, s=fit.train9$lambda.1se, type="class", newx=x.test)
yhat10 <- predict(fit.train10, s=fit.train10$lambda.1se, type="class", newx=x.test)

#Log Loss/Cross-Entropy for best fitting model
logloss0 <- LogLoss(as.numeric(yhat0),y.test)
logloss1 <- LogLoss(as.numeric(yhat0),y.test)
logloss2 <- LogLoss(as.numeric(yhat0),y.test)
logloss3 <- LogLoss(as.numeric(yhat0),y.test)
logloss4 <- LogLoss(as.numeric(yhat0),y.test)
logloss5 <- LogLoss(as.numeric(yhat0),y.test)
logloss6 <- LogLoss(as.numeric(yhat0),y.test)
logloss7 <- LogLoss(as.numeric(yhat0),y.test)
logloss8 <- LogLoss(as.numeric(yhat0),y.test)
logloss9 <- LogLoss(as.numeric(yhat0),y.test)
logloss10 <- LogLoss(as.numeric(yhat0),y.test)

par(mfrow=c(4,3))
plot(fit.train0,main="Alpha = 0")
plot(fit.train1,main="Alpha = 1/10")
plot(fit.train2,main="Alpha = 2/10")
plot(fit.train3,main="Alpha = 3/10")
plot(fit.train4,main="Alpha = 4/10")
plot(fit.train5,main="Alpha = 1/2")
plot(fit.train6,main="Alpha = 6/10")
plot(fit.train7,main="Alpha = 7/10")
plot(fit.train8,main="Alpha = 8/10")
plot(fit.train9,main="Alpha = 9/10")
plot(fit.train10,main="Alpha = 1")

alpha <- c(0,1/10,2/10,3/10,4/10,5/10,6/10,7/10,8/10,9/10,1)
log.loss <- c(logloss0,logloss1,logloss2,logloss3,logloss4,logloss5,logloss6,logloss7,logloss8,logloss9,logloss10)

log.loss.list <- data.frame(cbind(alpha,log.loss))
log.loss.list[which.min(log.loss.list$log.loss),]

#All models have equal log loss, need to do PCA

