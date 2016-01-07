# model 1

library(astsa)
ts.plot(jj)
length(jj)
lv1=log(jj)
ts.plot(lv1)
t=1:84
s=4
n=length(lv1)/s
plot(lv1~t,pch=16,xlab="t",ylab="logv")

x1<-rep(0,times=s*n)
x2<-rep(0,times=s*n)
x3<-rep(0,times=s*n)
x4<-rep(0,times=s*n)

x1[seq(from=1,to=(s*n),by=s)]<-1
x2[seq(from=2,to=(s*n),by=s)]<-1
x3[seq(from=3,to=(s*n),by=s)]<-1
x4[seq(from=4,to=(s*n),by=s)]<-1

lv_si=lm(lv1~t+x1+x2+x3)
summary(lv_si)
resid_si=residuals(lv_si)
par(mfrow=c(2,2))
ts.plot(resid_si)
acf(resid_si)
pacf(resid_si)

t1=c(81:84)
s=4
X1=c(1,0,0,0)
X2=c(0,1,0,0)
X3=c(0,0,1,0)
cells1=c(t1,X1,X2,X3)
mat1=matrix(cells1,nrow=4, ncol=4,dimnames = list(c(),c("t","x1","x2","x3")))
new=data.frame(mat1)
jjn=predict(lm(lv1~t+x1+x2+x3),new,se.fit=TRUE)
jjn$fit
xn=jjl[81:84]
en=xn-jjn$fit
mape=100*(mean(abs((en)/xn)))
orgm1=exp(jjn$fit)
yn=jj[81:84]
eno1=orgm1-yn
mape1original=100*(mean(abs((eno1)/yn)))

d=0   # no differencing needed
np=4    #maximum AR order
nq=4    # maximum MA order
# Set up loops to run several possible models
for (p in 0:np){
   for (q in 0:nq) {
     resid.fit = sarima(resid_si,p,d,q)
      outarima <- c(p,q,resid.fit$BIC,resid.fit$AIC)
         write(outarima,file="P://5825//hw8//outarima2",append=TRUE)
          }
 }
outarima.all <- read.table("P://5825//hw8//outarima2")
colnames(outarima.all) = c("p", "q","BIC","AIC")
outarima.all

resid1.fit =sarima(resid_si,4,0,0)
resid1.fit
n.ahead=4
resid1.fit.fore=sarima(jjl,n.ahead=4,4,0,0)
resid1.fit.fore

