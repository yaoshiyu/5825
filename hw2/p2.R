p2=read.table("P://5825//hw2//JJcsv.csv",header=TRUE,sep=",")  
v=p2$V1
t=p2$t
lv=log(v)
lv=ts(lv)
lv1=lv[1:80]
length(lv1)
t=1:80
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
fitted_si=fitted(lv_si)
plot(resid_si~fitted_si,pch=16,xlab="residuals",ylab="fitted value",main="Fitted value vs Residuals for M1")
abline(h=0,lty=2)
qqnorm(resid_si,ylab="Residual")
qqline(resid_si)
AIC(lv_si)/length(lv1)

c1=cos(2*pi*t/s)
s1=sin(2*pi*t/s)
lv_trig=lm(lv1~t+c1+s1)
summary(lv_trig)
plot(resid_trig~fitted_trig,pch=16,xlab="residuals",ylab="fitted value",main="Fitted value vs Residuals for M2")
abline(h=0,lty=2)
qqnorm(resid_trig,ylab="Residual")
qqline(resid_trig)
AIC(lv_trig)/length(lv1)
resid_trig=residuals(lv_trig)
fitted_trig=fitted(lv_trig)



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
xn=lv[81:84]
en=xn-jjn$fit
mape=100*(mean(abs((en)/xn)))
orgm1=exp(jjn$fit)
yn=v[81:84]
eno1=orgm1-yn
mape1original=100*(mean(abs((eno1)/yn)))

t=c(81:84)
c1=cos(2*pi*t/4)
s1=sin(2*pi*t/4)
newq=data.frame(cbind(t,c1,s1))
jjn1=predict(lv_trig,newq,se.fit=T)
jjn1$fit
xn1=lv[81:84]
en1=xn1-jjn1$fit
mape1=100*(mean(abs((en1)/xn1)))
orgm2=exp(jjn1$fit)
yn=v[81:84]
eno2=orgm2-yn
mape2original=100*(mean(abs((eno2)/yn)))
