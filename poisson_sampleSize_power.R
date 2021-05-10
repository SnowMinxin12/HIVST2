#R code for paper "Power and Sample Size Calculations for Poisson and Zero-Inflated Poisson Regression Models" in Computational Statistics and Data Analysis.

#Written by Nabil Channouf, Marc Fredette, and Brenda MacGibbon.

#Function to compute the variance of the ML estimates.

PoisMultinormalVar<-function(beta,v,p){
  b0<-beta[1]
  b<-matrix(ncol=1,nrow=p)
  b[,1]<-beta[2:(p+1)]	
  mgf<-exp(b0+t(b)%*%v%*%b/2)
  mat<-matrix(ncol=p+1,nrow=p+1)
  mat[1,1]<-1
  for (i in 1:p){
    mat[1,i+1]<-sum(b%*%v[i,])
    mat[i+1,1]<-mat[1,i+1]
  }
  for (l in 2:(p+1)){
    for (k in 2:(p+1)){
      tmpl<-sum(b%*%v[l-1,])
      tmpk<-sum(b%*%v[k-1,])
      mat[l,k] = v[l-1,k-1]+tmpl%*%tmpk
      mat[k,l] = mat[l,k]
    }
  }
  solve(mat)[2,2]/mgf[1,1]
}


#Function to compute the sample size via MC simulations when outcomes are Poisson and covariates are Multinormal. 

PoisMultinormal<-function(alpha,gamma,v,p,K,MC){
  #alpha=the significance level
  #gamma=1-desired power
  #v=the given covariance matrix of the multinormal vector
  #p=the dimension of the vector of covariates
  #K=the initial number of simulated covariates
  #MC=the number of MC replications 
  OverLambda<-0.05 #overall fixed mean 
  bb<-matrix(ncol=1,nrow=p+1)
  bb[2:(p+1),]<-log(2)
  #Correlation matrix
  sig<-matrix(0,ncol=p,nrow=p)
  za<-qnorm(1-alpha/2)
  zg<-qnorm(1-gamma)
  Size<-numeric(MC)
  x<-matrix(ncol=p+1,nrow=K)
  x[,1]<-1
  for (i in 1:(p-1)){
    sig[i,i]<-1
    for (j in (i+1):p){
      sig[i,j]<-v[i,j]/sqrt(v[i,i]*v[j,j])
      sig[j,i]<-sig[i,j]
    }
  }
  sig[p,p]<-1		
  bb[1,1]<-log(OverLambda/exp((t(bb[2:(p+1),])%*%v%*%bb[2:(p+1),])/2))
  if (qr(sig)$rank<p) return("Non feasible matrix")
  else if (qr(sig)$rank==p){
    for (k in 1:MC){
      for (i in 2:(p+1))
        x[,i]<-rnorm(K)
      x[,2:(p+1)]<-t(chol(sig)%*%t(x[,2:(p+1)]))
      eta<-x%*%bb
      lambda<-exp(eta)
      #Function to compute beta* under the score function
      PoisMultinormalScore<-function(beta){
        b<-matrix(ncol=1,nrow=p)
        b[,1]<-beta
        tem<-cbind(x[,1],x[,3:(p+1)])
        ret1<-lambda-exp(tem%*%b)
        ret2<-numeric(p)
        for (j in 1:p)
          ret2[j]<-sum(tem[,j]*ret1)^2
        sum(ret2)     
      }
      bstar <- optim(c(bb[1,1],bb[3:(p+1),1]),PoisMultinormalScore,NULL,control=list(maxit=20000))$par
      Size[k]<-((sqrt(PoisMultinormalVar(c(bstar[1],0,bstar[2:p]),v,p))*za+sqrt(PoisMultinormalVar(bb,v,p))*zg)/bb[2,1])^2
    }
    print(c(mean(Size),sd(Size)))
  }
  
}

#Example with p=2, rho=0.3, power=90%, K=100.

#Covariance matrix
library(randcorr)
p=2
# vv<-matrix(diag(p))

gamma=0.1
# p=2
PoisMultinormal(alpha=0.05,gamma=0.1,v=diag(p),p=2,K=100,MC=100)
p_list = c(2:20)
power_list = matrix(data = NA, nrow=19, ncol=2)
for (p in p_list){
  vv<-diag(p)
  vv[abs(row(vv) - col(vv)) == 1] <- 0.1
  power_list[p-1,]= PoisMultinormal(alpha=0.05,gamma=0.1,
                                     v=vv,p=p,K=100,MC=100)
}
power_list = as.data.frame(power_list)
