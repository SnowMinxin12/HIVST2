library(powerMediation)


power_list = seq(from = 0.8, to = 0.95, length.out=100)
sample_size = SSizeLogisticBin(p1 = 0.3, p2 = 0.4, B = 0.5, alpha = 0.05, power = power_list)
par(mfrow=c(1,2))
plot(power_list,sample_size,type="l",ylim=c(600,2000),
     main="Sample size logistic binary covariate",
     xlab="power",ylab="sample size")


sample_size0.3 = SSizeLogisticBin(p1 = 0.3, p2 = 0.4, B = 0.3, alpha = 0.05, power = power_list)
lines(power_list,sample_size0.3, col="red", lwd=2)

sample_size0.2 = SSizeLogisticBin(p1 = 0.3, p2 = 0.4, B = 0.2, alpha = 0.05, power = power_list)
lines(power_list,sample_size0.2, col="orange", lwd=2)

# SSizeLogisticBin(p1 = 0.3, p2 = 0.4, B = 0.5, alpha = 0.05, power = 0.9)



power_list = seq(from = 0.8, to = 0.95, length.out=100)
sample_size_cont = SSizeLogisticCon(p1 = 0.5, OR = exp(0.405), alpha = 0.05, power = power_list)
plot(power_list,sample_size_cont,type="l",ylim=c(200,500),
     main="Sample size logistic continuous covariate",
     xlab="power",ylab="sample size")

sample_size_count0.3 = SSizeLogisticCon(p1 = 0.3, OR = exp(0.405), alpha = 0.05, power = power_list)
lines(power_list,sample_size_count0.3, col="red", lwd=2)

sample_size_count0.2 = SSizeLogisticCon(p1 = 0.2, OR = exp(0.405), alpha = 0.05, power = power_list)
lines(power_list,sample_size_count0.2, col="orange", lwd=2)
