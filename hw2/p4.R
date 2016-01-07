
data1=read.table("P://5825/hw2/cmort.dat",sep=",")
data2=read.table("P://5825/hw2/temp.dat",sep=",")
data3=read.table("P://5825/hw2/part.dat",sep=",")

cmort=data1[,1]
tempr=data2[,1]
part=data3[,1]


pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
cmort1 = cmort[5:508]
num=length(cmort)
temp  = tempr-mean(tempr)  # center temperature    
temp1 = temp[5:508]
length(temp1)
temp2 = temp1^2             # square it  
length(temp2)
trend1=5:508
part = ts(part)
part1 = part[5:508]
length(part1)
part4 = part[1:504]
length(part4)
fit = lm(cmort1~ trend1 + temp1 + temp2 + part1 + part4, na.action=NULL)
summary(fit)
  
par(mfrow=c(4,1))
plot(cmort1, main="Cardiovascular Mortality", xlab="", ylab="")
plot(temp1, main="Temperature", xlab="", ylab="")
plot(part1, main="Particulates", xlab="", ylab="")
plot(part4, main="Particulates4", xlab="", ylab="") 

dev.new()  
pairs(cbind(Mortality=cmort1, Temperature=temp1, Particulates=part1, Particulates4=part4))
data=cbind(cmort1,temp1,part1,part4)
pairs(data)
cor(data)

AIC(fit)/num - log(2*pi) 
AIC(fit, k=log(num))/num - log(2*pi)                     
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2))
