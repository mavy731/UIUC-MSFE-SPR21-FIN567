# Title     : TODO
# Objective : TODO
# Created by: mavy731
# Created on: 2/10/21

setwd("C:/Users/mavy7/Box/2020-2021 SEM B/FIN567 Financial Risk Management/Week3")
data<-read.csv("returns4symbols.2004-2018(2).csv")
begin<-which(data$date==20080102)
end<-which(data$date==20091231)
v <- c(5000000,3000000,1000000,1000000)

#calculate equal weight VaR
Equal_weight_VaR=numeric(end-begin+1)
for(j in 1:(end-begin+1)){
  sample <- data[(begin+j-250):(begin+j-1),2:5]
  Equal_weight_cov=cov.wt(sample,wt=rep(1/nrow(sample),
                                        nrow(sample)),cor=FALSE,center=TRUE)
  x=v
  portfolio_std=sqrt(t(x)%*% Equal_weight_cov$cov %*% x)
  Equal_weight_VaR[j]=-2.326*portfolio_std
}

#calculate exponentially weight VaR
Exp_weight_VaR=numeric(end-begin+1)
for(j in 1:(end-begin+1)){
  sample <- data[(begin+j-150):(begin+j-1),2:5]
  weight=numeric(150)
  for (m in 1:150){
    weight[m]=0.94^(m-1)*(1-0.94)/(1-0.94^150)
  }
  Exp_weight_cov=cov.wt(sample,wt=weight,cor=FALSE,center=TRUE)
  x=v
  portfolio_std=sqrt(t(x) %*% Exp_weight_cov$cov %*% x)
  Exp_weight_VaR[j]=-2.326*portfolio_std
}
