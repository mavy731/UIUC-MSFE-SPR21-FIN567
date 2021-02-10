# Title     : TODO
# Objective : TODO
# Created by: mavy731
# Created on: 2/10/21

setwd("/home/mavy731/Documents/FIN567")
data<-read.csv("returns4symbols.2004-2018(2).csv")
begin<-which(data$date==20080102)
end<-which(data$date==20091231)

#calculate equal weight VaR
Equal_weight_VaR=numeric(end-begin+1)
for(j in 1:(end-begin+1)){
  sample <- data[(begin+j-250):(begin+j-1),2:5]
  Equal_weight_cov=cov.wt(sample,wt=rep(1/nrow(sample),nrow(sample)),cor=FALSE,center=TRUE)
  x=sample[1,]
  sum<-x[1,1]+x[1,2]+x[1,3]+x[1,4]
  portfolio_std=sqrt(as.matrix(x) %*% Equal_weight_cov$cov %*% t(as.matrix(x)))
  Equal_weight_VaR[j]=-2.326*portfolio_std/sum
}

#calculate exponentially weight VaR
for(j in 1:(end-begin+1)){
  sample <- data[(begin+j-75):(begin+j-1),2:5]
  weight=numeric(75)
  for (m in 1:75){
    weight[m]=0.94^(m-1)*(1-0.94)/(1-0.94^75)
  }
  Exp_weight_cov=cov.wt(sample,wt=weight,cor=FALSE,center=TRUE)
  x=sample[1,]
  sum<-x[1,1]+x[1,2]+x[1,3]+x[1,4]
  portfolio_std=sqrt(as.matrix(x) %*% Exp_weight_cov$cov %*% t(as.matrix(x)))
  Exp_weight_VaR[j]=-2.326*portfolio_std/sum
}
