#ARIMA

setwd("j:/时间序列/论文")
data=read.csv(paste0('artist_id',98,'.csv'),he=F)  #-------
names(data)=c('artist_id','Ds','s1')
d=data[,c(2,3)]
song=as.character(data[1,1])



#ts1<-ts(d$s1, start=0, frequency=7)
ts1<-ts(d$s1[1:175], start=0, frequency=7)
#原始数据图


#时间序列建模
library(TSA)
library(urca)

#
par(mfrow=c(1,2))
plot(ts1,
     ylab = '总播放量',xlab = '记录时间')
acf(as.vector(ts1),lag.max=36)   #查看平稳性-非平稳
par(mfrow=c(1,1))


####差分除去趋势，确定d的阶数
ts2=diff(ts1)

par(mfrow=c(1,2))
plot(ts2) 
acf(as.vector(ts2),lag.max=36)          #季节趋势
par(mfrow=c(1,1))
#pacf(as.vector(ele1),lag.max=36)


####消除季节波动 确定D的阶数
ts3=diff(diff(ts1),lag=7)

par(mfrow=c(1,2))
plot(ts3) 
acf(as.vector(ts3),lag.max=36)          #截尾
par(mfrow=c(1,1))
#pacf(as.vector(ele2),lag.max=36)


#平稳检验-ts3
m1=ur.df(ts3,type='none')
summary(m1)    #无单位根

####模型识别
par(mfrow=c(1,2))
acf(as.vector(ts3),lag.max=36)      #截尾(2,1)
pacf(as.vector(ts3),lag.max=36)     #拖尾的
par(mfrow=c(1,1))


####模型估计
m1=arima(ts1,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=7))
m1
m2=arima(ts1,order=c(3,1,1),seasonal=list(order=c(0,1,1),period=7))
m2



###模型诊断
plot(m1$resid)
acf(as.vector(m1$resid),lag.max=72)   #白噪声
pacf(as.vector(m1$resid),lag.max=72)   #白噪声
#平稳检验
Box.test(m1$resid)   
#Box.test图
Box_test_plot=function(m){
  B=NULL
  for(i in 1:20){
    B=c(B,Box.test(m$residuals,lag=i,type = 'Ljung-Box')$p.value)
  }
  plot(B,ylim=c(0,1),main='Ljung-Box')
  abline(h=.05,lty=2)
}
Box_test_plot(m1)

qqnorm(m1$resid)           #正态性
qqline(m1$resid)

#adf.test(m1$resid)
#adf.test(ts3)

####预测
win.graph(width=4.875, height=3,pointsize=8)
plot(ts1)
par(mfrow=c(2,1))
plot(m1,n.ahead=8,col='red',xlab='Year',type='o')
plot(m1,n1=c(24),n.ahead=8,col='red',ylab='播放量',xlab='周',type='o')
lines(ts(d$s1[176:183], start=25, frequency=7),col='blue')   #真实
legend("bottomleft", lwd =1,cex=0.5,legend = c('95%的置信区间','真实值'),col=c('red','blue'))




p_ts=predict(m1,n1=c(25),n.ahead=8)  
p_ts$pred   #预测值

pre=as.numeric(p_ts$pred)
real=d$s1[176:183]

compare=data.frame('预测值'=as.numeric(p_ts$pred),'真实值'=d$s1[176:183])
compare

write.csv(compare,'对比.csv')

#平均绝对误差
mae=mean(abs(pre-real))
mae
#平均绝对百分误差
mape=mean(abs(pre-real)/real)
mape





