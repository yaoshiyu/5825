p2=read.table("P://5825//hw2//JJcsv.csv",header=TRUE,sep=",")  
v=p2$V1
t=p2$t
lv=log(v)
lv1=lv[1:80]
lv1=ts(lv1,frequency=4)
v1=v[1:80]
v1=ts(v1,frequency=4)


hw.add=HoltWinters(lv1,seasonal="additive")
hw.add
p.add=predict(hw.add,4,prediction.interval=TRUE)
p.add
plot(hw.add,p.add,xlab="time",ylab="V1")
fitadd=p.add[,1]
xn=lv[81:84]
en=xn-fitadd
mape=100*(mean(abs((en)/xn)))
mape
efitadd=exp(fitadd)
xn1=v[81:84]
en1=xn1-efitadd
mape3original=100*(mean(abs((en1)/xn1)))



hw.mult=HoltWinters(v1,seasonal="multiplicative")
hw.mult
p.mult=predict(hw.mult,4,prediction.interval=TRUE)
p.mult
plot(hw.mult,p.mult,xlab="time",ylab="V1")
fitmult=p.mult[,1]
xn1=v[81:84]
en1=xn1-fitmult
mape=100*(mean(abs((en1)/xn1)))
mape





