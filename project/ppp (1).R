
library(perARMA)
makepar
S=12
nlen=1200
p=1

a0=0.5
a1=0.5
a2=0.5
a=c(a0,a1,a2)

phi0=matrix(0,S,p)
np=S*p
for (s in 1:np)
{
phi0[s,]=a0+a1*cos(2*pi*s/S-a2*pi)
}

del0=matrix(1,S,1)
PAR1=makepar(nlen,phi0,del0)
par=PAR1$y+1
plot(ts(par))
par=ts(par, frequency=12)
mean(par)

# pacakge without intercept
library(pear)
pa=ts(par, frequency=12)
pear(par,1)
# pacakge with intercept with not sure
library(partsm)
detcomp <- list(regular=c(0,0,c(1,2,3,4,5,6,7,8,9,10,11,12)), seasonal=c(1,0), regvar=0)
out.par<- fit.ar.par(wts=par, type="PAR", detcomp=detcomp, p=1)
attributes(summary(out.par))
# residuals is OK
residuals(summary(out.par))

#use model in paper ols
tol=12*100
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
lag(par,12)
yt1<-lag(par,12)*x1
yt2<-lag(par,12)*x2
yt3<-lag(par,12)*x3
yt4<-lag(par,12)*x4
yt5<-lag(par,12)*x5
yt6<-lag(par,12)*x6
yt7<-lag(par,12)*x7
yt8<-lag(par,12)*x8
yt9<-lag(par,12)*x9
yt10<-lag(par,12)*x10
yt11<-lag(par,12)*x11
yt12<-lag(par,12)*x12
fit1<-lm(par~0+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+yt1+yt2+yt3+yt4+yt5+yt6+yt7+yt8+yt9+yt10
         +yt11+yt12)
summary(fit1)
en<-residuals(fit1)
phi.fit<-coefficients(summary(fit1))[13:24,1]
sigma.e<-1/(tol-2*S)*sum(en**2)
sigma.e




#use value of phi nls 
phi=c(0.73, 0.86, 0.85, 1.03, 0.79, 0.52, 0.02, 0.11, -0.43, 0.14, 0.29, 0.42)

i<-c(1:12)
m.phi<-nls(phi~a+b*cos(2*pi*i/S-c*pi),start=list(a=0.2,b=0.2,c=0.2), trace=T)
fit<-summary(m.phi)
fit
fit.a<-coefficients(fit)[,1]
fit.a
us<-residuals(m.phi)
sigma.u<-1/S*sum(us^2)
sigma.u



#ml max need to use simplified log likelyhood 

ml<-funciton(){
  phi.s.bar=a+b*cos(2*pi*i/S-c*pi)
  T<-c(1:tol)
  sigma.phi.s=sqrt(sigma.u^-2+sum(
}


