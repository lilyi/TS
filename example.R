mydata=read.csv("C:/Users/Lily/Documents/TS/example.csv",header=T) 
tsdata=matrix(0, 21, 10)
tsdata[,1]=mydata[,1]
tsdata[,2]=mydata[,2]
tsdata[,3]=mydata[,3]
colnames(tsdata)=c("time", "season", "x", "CMA", "d", "S", "y", "x_hat", "res", "res_sq")
CMA=numeric(17) 
for(i in 1:17)
{ 
  CMA[i]=(tsdata[i,3]+2*tsdata[i+1,3]+2*tsdata[i+2,3]+2*tsdata[i+3,3]+tsdata[i+4,3])/8
}
# plotting a smooth curve
plot(CMA, type= "b")
tsdata[,4]=c(NA, NA, CMA, NA, NA)
tsdata[,5]=tsdata[,3]-tsdata[,4]
# seasonal index
dm=tapply(tsdata[,5], tsdata[,2], mean, na.rm =T) 
dm
sdm=sum(dm) 
index=dm-sdm/4 ; index
# seasonal index
tsdata[,6]=c( rep(index, 5), index[1] )
plot(tsdata[,6], type= "b")
# y (去除季節指數的數列)
tsdata[,7]=tsdata[,3]-tsdata[,6]
plot(tsdata[,7], type= "b")
# x=lm(y~t)+S
reg=lm(tsdata[,7]~tsdata[,1])
tsdata[,8]=reg$fitted.values+tsdata[,6]
plot(reg$fitted.values, type= "b")
plot(tsdata[,8], type= "b")
# residual value
tsdata[,9]=tsdata[,3]-tsdata[,8]
# squared residual value
tsdata[,10]=tsdata[,9]^2 


###############################################
mydata=read.csv("C:/Users/Lily/Documents/TS/example.csv",header=T) 
tsdata=matrix(0, 21, 12)
tsdata[,1]=mydata[,1]
tsdata[,2]=mydata[,2]
tsdata[,3]=mydata[,3]
tsdata[,4]=mydata[,4]
colnames(tsdata)=c("time", "year", "season", "x", "CMA", "r", "S", "y", "TR", "CI", "C", "I")
CMA=numeric(36)
for(i in 1:36)
{
  CMA[i]=(tsdata[i,4]+2*tsdata[i+1,4]+2*tsdata[i+2,4]+2*tsdata[i+3,4]+tsdata[i+4,4])/8
}
# plotting a smooth curve
plot(CMA, type= "b")
tsdata[,5]=c(NA, NA, CMA, NA, NA)
tsdata[,6]=tsdata[,4]/tsdata[,5]
# seasonal index
rm=tapply(tsdata[,6], list( tsdata[,2], tsdata[,3]), mean, na.rm =T) ; rm
trm=apply(rm, 2, mean, na.rm =T) 
index=trm/mean(trm) ; index
# seasonal index
tsdata[,7]=rep(index, 10)
plot(tsdata[,7], type= "b")
# y (去除季節指數的數列)
tsdata[,8]=tsdata[,4]/tsdata[,7]
# x=lm(y~t)+S
reg=lm(tsdata[,8]~tsdata[,1])
plot(tsdata[,8], type= "b")
lines(reg$fitted.values, type= "l")
#計算去除季節指數後的數列 y 與 TR 之比值 CI
tsdata[,9]=reg$fitted.values
tsdata[,10]=tsdata[,8]/tsdata[,9]
# 計算 C
tsdata[,11]=NA
for (i in 1:36)
{
  C=(tsdata[i,10]+ tsdata[i+1,10]+ tsdata[i+2,10]+ tsdata[i+3,10]+ tsdata[i+4,10])/5
  tsdata[i+2, 11]=C
}
plot(tsdata[,11], type= "b")
# 計算 I 
tsdata[,12]=tsdata[,4]/(tsdata[,9]*tsdata[,7]*tsdata[,11])
plot(tsdata[,12], type= "b") 
