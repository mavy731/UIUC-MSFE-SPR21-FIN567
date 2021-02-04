# Title     : TODO
# Objective : TODO
# Created by: mavy731
# Created on: 2/3/21

K=100
Sigma=0.3
r=0.01
q=0.02
T=0.25
pie=3.14
x=c(80:100)

deltacall=function(x,t){
  Nd1=pnorm((log(x/K)+(r-q+sigma^2/2)*(T-t))/(Sigma*sqrt(T-t)))
  Nd2=pnorm(Nd1-Sigma*sqrt(T-t))
  return (exp(-q*(T-t))*Nd1)
}

Delta1=deltacall(x,0)
t=2/12
Delta2=deltacall(x,t)
t=12/52
Delta3=deltacall(x,t)

png("delta call")
plot(x,Delta1,type="b",col=10,ylim=c(-0.05,0.55),
     ylab="Delta",xlab="Value",main="Delta Call",pch=4)
lines(x, Delta2, type = "b", lty = 1, pch = 4, col = 6)
lines(x, Delta3, type = "b", lty = 1, pch = 4, col = 4)
legend("topleft",legend=c("t=0","t=2/12","t=12/52"),col=c(10,6,4),pch=c(4,4,4))
dev.off()

deltaput=function(x,t){
  Nd1=pnorm((log(x/K)+(r-q+Sigma^2/2)*(T-t))/(Sigma*sqrt(T-t)))
  Nd2=pnorm(Nd1-Sigma*sqrt(T-t))
  return (exp(-q*(T-t))*Nd1-1)
}

Delta4=deltaput(x,0)
Delta5=deltaput(x,2/12)
Delta6=deltaput(x,12/52)

png("delta put")
plot(x,Delta4,type="b",col=10,ylim=c(-1.05,-0.4),
     ylab="Delta",xlab="Value",main="Delta Put",pch=4)
lines(x, Delta5, type = "b", lty = 1, pch = 4, col = 6)
lines(x, Delta6, type = "b", lty = 1, pch = 4, col = 4)
legend("topleft",legend=c("t=0","t=2/12","t=12/52"),col=c(10,6,4),pch=c(4,4,4))
dev.off()

gamma=function(x,t){
  d1=(log(x/K)+(r-q+Sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
  d2=d1-Sigma*sqrt(T-t)
  a=exp(-q*(T-t)-(d1^2/2))
  b=x*Sigma*sqrt((T-t)*2*3.14)
  return (a/b)
}

Gamma1=gamma(x,0)
Gamma2=gamma(x,2/12)
Gamma3=gamma(x,12/52)

png("gamma")
plot(x,Gamma1,type="b",col=10,ylim=c(-0.01,0.11),
     ylab="Gamma",xlab="Value",main="Gamma",pch=4,xaxs="r",yaxs="r")
lines(x, Gamma2, type = "b", lty = 1, pch = 4, col = 6)
lines(x, Gamma3, type = "b", lty = 1, pch = 4, col = 4)
legend("topleft",legend=c("t=0","t=2/12","t=12/52"),col=c(10,6,4),pch=c(4,4,4))
dev.off()

theta=function(x,t){
  d1=(log(x/K)+(r-q+Sigma^2/2)*(T-t))/(Sigma*sqrt(T-t))
  d2=d1-Sigma*sqrt(T-t)
  a=(-exp(-q*(T-t))*x*Sigma/(2*sqrt(T-t)*sqrt(2*pie)))*exp(-(d1^2)/2)
  b=r*K*exp(-r*(T-t))*pnorm(d2)
  c=q*x*exp(-q*(T-t))*pnorm(d1)
  return(a-b+c)
}

Theta1=theta(x,0)
Theta2=theta(x,2/12)
Theta3=theta(x,12/52)

png("theta")
plot(x,Theta1,type="b",col=10,ylim=c(-20,0.05),
     ylab="Theta",xlab="Value",main="Theta",pch=4,xaxs="r",yaxs="r")
lines(x, Theta2, type = "b", lty = 1, pch = 4, col = 6)
lines(x, Theta3, type = "b", lty = 1, pch = 4, col = 4)
legend("topleft",legend=c("t=0","t=2/12","t=12/52"),col=c(10,6,4),pch=c(4,4,4))
dev.off()

vega=function(x,t,sigma){
  d1=(log(x/K)+(r-q+sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
  d2=d1-sigma*sqrt(T-t)
  a=exp(-q*(T-t))*x*sqrt(T-t)
  b=1/sqrt(2*3.14)
  c=exp(-d1^2/2)
  return(a*b*c)
}

Vega1=vega(x,0,Sigma)
Vega2=vega(x,2/12,Sigma)
Vega3=vega(x,12/52,Sigma)

png("vega")
plot(x,Vega1,type="b",col=10,ylim=c(-0.05,20),
     ylab="Vega",xlab="Value",main="Vega",pch=4,xaxs="r",yaxs="r")
lines(x, Vega2, type = "b", lty = 1, pch = 4, col = 6)
lines(x, Vega3, type = "b", lty = 1, pch = 4, col = 4)
legend("topleft",legend=c("t=0","t=2/12","t=12/52"),col=c(10,6,4),pch=c(4,4,4))
dev.off()

S1=101
S2=95
K=100
sigma1=0.3
sigma2=0.35
r=0.01
q=0.02
T=0.25
t1=2/12
t2=2/12+1/365
Delta=deltacall(S1,t1)
Gamma=gamma(S1,t1)
Theta=theta(S1,t1)
Vega=vega(S1,t1,sigma1)
change=(S2-S1)*Delta+0.5*(S2-S1)^2*Gamma+(sigma2-sigma1)*Vega+Theta*(1/365)
print(Delta)
print(Gamma)
print(Theta)
print(Vega)
print(change)

Delta3_a=delta(101,t)
Delta3_b=delta(95,t)
print(Delta3_a)
print(Delta3_b)

Sigma=0.3
r=0.01
q=0.02
t=2/12
T=1/4
K=100
x=c(80:120)

call=function(x){
  d1=(log(x/K)+(r-q+sigma^2/2)*(T-t))/(Sigma*sqrt(T-t))
  Nd1=pnorm(d1)
  d2=d1-Sigma*sqrt(T-t)
  Nd2=pnorm(d2)
  return(x*exp(-q*(T-t))*Nd1-exp(-r*(T-t))*K*Nd2)
}

portfolio=function(x){
  Call=-call(x)*200
  Equity=109*x
  B=-10000
  return(Call+Equity+B)
}
C=-call(x)*200
P=portfolio(x)
E=109*x
B=-10000

png("porfolio")
plot(x,P,type="b",col=10,
     ylab="Portfolio Value",xlab="Index Value",main="Portfolio",pch=4,xaxs="r",yaxs="r")
lines(x, C, type = "b", lty = 1, pch = 4, col = 6)
legend("topleft",legend=c("Portfolio","Call Position"),col=c(10,6),pch=c(4,4))
dev.off()
