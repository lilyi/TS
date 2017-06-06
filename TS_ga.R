mydata=read.csv("C:/Users/Lily/Documents/TS/Taiwan.csv",header=T) 
tsdata=matrix(0, 120, 10)
tsdata[,1]=mydata[,1]
tsdata[,2]=mydata[,2]
tsdata[,3]=mydata[,3]
colnames(tsdata)=c("time", "season", "x", "CMA", "d", "S", "y", "x_hat", "res", "res_sq")
CMA=numeric(114) 
for(i in 1:114)
{ 
  CMA[i]=(tsdata[i,3]+2*tsdata[i+1,3]+tsdata[i+2,3]+tsdata[i+3,3]+tsdata[i+4,3]+tsdata[i+5,3]+tsdata[i+6,3])/7
}
# plotting a smooth curve
plot(CMA, type= "b")
tsdata[,4]=c(NA, NA, NA, CMA, NA, NA, NA)
tsdata[,5]=tsdata[,3]-tsdata[,4]
# seasonal index
dm=tapply(tsdata[,5], tsdata[,2], mean, na.rm =T) 
dm
sdm=sum(dm) 
index=dm-sdm/7 ; index
# seasonal index
tsdata[,6]=c(index[3:7], rep(index, 16), index[6:8])
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
write.csv(tsdata, "C:/Users/Lily/Documents/TS/ana.csv")
