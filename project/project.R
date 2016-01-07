#parameter
p=1
T=40
S=12
a0=0.5
a1=0.5
a2=0.5
u=c(1,1,1,1,1,1,1,1,1,1,1,1)
y0=1

#compute phi
phi0=matrix(0,S,p)
np=S*p
for (s in 1:np)
{
phi0[s,]=a0+a1*cos(2*pi*s/S-a2*pi)
}
w=rnorm(12, mean = 0, sd = 0.2)
phi=phi0+w

#generate et
e=rnorm(T*S, mean = 0, sd = 1)

#generate yt
n=S*T
y=matrix(0,n,1)
y[1,]=u[1]+phi[1,]*y0
for (t in 2:n)
{ 
s=t%%12
if (s==0)
{s=12}
{y[t,]=u[s]+phi[s,]*y[t-1,]+e[t]} 
}

y=ts(y)
ts.plot(y)

y=ts(y, frequency=12)
par=y
/*# pacakge with intercept with not sure
library(partsm)
detcomp <- list(regular=c(0,0,c(1,2,3,4,5,6,7,8,9,10,11,12)), seasonal=c(1,0), regvar=0)
out.par<- fit.ar.par(wts=y, type="PAR", detcomp=detcomp, p=1)
summary(out.par)
attributes(summary(out.par))
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
lagy<-c(1,y[1:tol-1])

yt1<-x1*lagy
yt2<-x2*lagy
yt3<-x3*lagy
yt4<-x4*lagy
yt5<-x5*lagy
yt6<-x6*lagy
yt7<-x7*lagy
yt8<-x8*lagy
yt9<-x9*lagy
yt10<-x10*lagy
yt11<-x11*lagy
yt12<-x12*lagy
fit1<-lm(y~0+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+yt1+yt2+yt3+yt4+yt5+yt6+yt7+yt8+yt9+yt10
         +yt11+yt12)
summary(fit1)
en<-residuals(fit1)
fit1.coefficents<-coefficients(fit1)
fit1.coefficents
phi.fit<-coefficients(summary(fit1))[13:24,1]
sigma.e<-sqrt(1/(tol-2*S)*sum(en**2))
sigma.e




#use value of phi nls 

phi=phi.fit
i<-c(1:12)
m.phi<-nls(phi~a+b*cos(2*pi*i/S-c*pi),start=list(a=0.2,b=0.2,c=0.2), trace=T)
fit2<-summary(m.phi)
fit2
fit2.a<-coefficients(fit)[,1]
fit2.a
us<-residuals(m.phi)
sigma.u<-sqrt(1/S*sum(us^2))
sigma.u

#change sigma v value ignore sigma.u matrix
sigma.v=(sigma.e^2*diag(tol))


#get vt

fitall<-function(para) 
{
  v=as.vector(y-(para[1]*x1+para[2]*x2+para[3]*x3+para[4]*x4+para[5]*x5+para[6]*x6+para[7]*x7
                 +para[8]*x8+para[9]*x9+para[10]*x10
            +para[11]*x11+para[12]*x12+(para[13]+para[14]*cos(2*pi*1/S-para[15]*pi))*yt1
            +(para[13]+para[14]*cos(2*pi*2/S-para[15]*pi))*yt2+ 
              (para[13]+para[14]*cos(2*pi*3/S-para[15]*pi))*yt3+
              (para[13]+para[14]*cos(2*pi*4/S-para[15]*pi))*yt4
            +(para[13]+para[14]*cos(2*pi*5/S-para[15]*pi))*yt5
              +(para[13]+para[14]*cos(2*pi*6/S-para[15]*pi))*yt6
            +(para[13]+para[14]*cos(2*pi*7/S-para[15]*pi))*yt7
            +(para[13]+para[14]*cos(2*pi*8/S-para[15]*pi))*yt8
            +(para[13]+para[14]*cos(2*pi*9/S-para[15]*pi))*yt9+
              (para[13]+para[14]*cos(2*pi*10/S-para[15]*pi))*yt10
            +(para[13]+para[14]*cos(2*pi*11/S-para[15]*pi))*yt11+
              (para[13]+para[14]*cos(2*pi*12/S-para[15]*pi))*yt12))
  return(t(v)%*%solve(sigma.v)%*%v)
}


# ingnore sigma u

sigma.v=(sigma.e^2*diag(tol))
initial=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,
          0.8,0.8,0.8,0.8,0.8,0.5,0.5,0.5)
library(nlme)


fitoptim1<-optim(initial, fitall,gr=NULL,method="BFGS", hessian=T)
fitoptim1$par



