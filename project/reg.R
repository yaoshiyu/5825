
y=ts(y, frequency=12)
par=y
/*# pacakge with intercept with not sure
#library(partsm)
#detcomp <- list(regular=c(0,0,c(1,2,3,4,5,6,7,8,9,10,11,12)), seasonal=c(1,0), regvar=0)
#out.par<- fit.ar.par(wts=y, type="PAR", detcomp=detcomp, p=1)
#summary(out.par)
#attributes(summary(out.par))
# residuals is OK
#residuals(summary(out.par))####
S=12

#use model in paper ols
tol=12*40
x1=rep(0,times=tol)
x2=rep(0,times=tol)
x3=rep(0,times=tol)
x4=rep(0,times=tol)
x5=rep(0,times=tol)
x6=rep(0,times=tol)
x7=rep(0,times=tol)
x8=rep(0,times=tol)
x9=rep(0,times=tol)
x10=rep(0,times=tol)
x11=rep(0,times=tol)
x12=rep(0,times=tol)


x1[seq(from=1,to=tol,by=S)]=1
x2[seq(from=2,to=tol,by=S)]=1
x3[seq(from=3,to=tol,by=S)]=1
x4[seq(from=4,to=tol,by=S)]=1
x5[seq(from=5,to=tol,by=S)]=1
x6[seq(from=6,to=tol,by=S)]=1
x7[seq(from=7,to=tol,by=S)]=1
x8[seq(from=8,to=tol,by=S)]=1
x9[seq(from=9,to=tol,by=S)]=1
x10[seq(from=10,to=tol,by=S)]=1
x11[seq(from=11,to=tol,by=S)]=1
x12[seq(from=12,to=tol,by=S)]=1
y
yt1<-x1*y[2:tol]
yt1
yt2<-x2*y[2:tol]
yt3<-x3*y[2:tol]
yt4<-x4*y[2:tol]
yt5<-x5*y[2:tol]
yt6<-x6*y[2:tol]
yt7<-x7*y[2:tol]
yt8<-x8*y[2:tol]
yt9<-x9*y[2:tol]
yt10<-x10*y[2:tol]
yt11<-x11*y[2:tol]
yt12<-x12*y[2:tol]
fit1<-lm(y~0+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+yt1+yt2+yt3+yt4+yt5+yt6+yt7+yt8+yt9+yt10
         +yt11+yt12)
summary(fit1)
en<-residuals(fit1)
phi.fit<-coefficients(summary(fit1))[13:24,1]
sigma.e<-1/(tol-2*S)*sum(en**2)
sigma.e




#use value of phi nls 

phi=phi.fit
i<-c(1:12)
m.phi<-nls(phi~a+b*cos(2*pi*i/S-c*pi),start=list(a=0.2,b=0.2,c=0.2), trace=T)
fit<-summary(m.phi)
fit
fit.a<-coefficients(fit)[,1]
fit.a
us<-residuals(m.phi)
sigma.u<-1/S*sum(us^2)
sigma.u

x1

#ml max need to use simplified log likelyhood 

ml<-funciton(){
  phi.s.bar=a+b*cos(2*pi*i/S-c*pi)
  T<-c(1:tol)
  sigma.phi.s=sqrt(sigma.u^-2+sum(
}


