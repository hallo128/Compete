#  ARIMA for volume

setwd("/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour")

tollgate_id=1              #-------收费站（123）
direction=0                #-------方向（0进1出）
data=read.table(paste0(" 18day_8hour   tollgate:",tollgate_id,'_',direction,'.txt'),
                he=T)  
names(data)
data=data[order(data$start_day),]    #每天的数据排序归类

#-------------------------求平均值(在给定路线情况下)
hour=c(6,7,8,9,15,16,17,18)     #小时
minute=c(0,20,40)


#---给定小时、分钟求均值
mean_hm=function(hour,minute){
  select=data[which(data$"start_hour"==hour),]
  select_m=select[which(select$"start_minute"==minute),]
  m=mean(select_m$volume)
}
#mean_hm(hour[3],minute[1])
mean_hm1=NULL
name=c(6,6.2,6.4,7,7.2,7.4,8,8.2,8.4,9,9.2,9.4,
       15,15.2,15.4,16,16.2,16.4,17,17.2,17.4,18,18.2,18.4)
names(mean_hm1)<-name
for (i in 1:length(hour)) {
  for (j in 1:length(minute)) {
    mean_hm1[((i-1)*3+j)]=mean_hm(hour[i],minute[j])
  }
}
mean_hm_route=round(mean_hm1)
mean_hm_route
x=name+0.1
plot(mean_hm_route~x,type='b',main=paste0("18days_(6-10/15-19)hour_avg_volume",":",tollgate_id,'_',direction))     
#-------------------------

#ff=data[order(data$start_hour),] 
#f1=ff[order(ff$start_minute),] 

#-----------------------------------选择几天的几小时的数据
start_month=9
start_day=25:25    #19:28或者19：19
start_hour=c(6,7,8,9)     #小时----0-23，15,16,17,18

select_hour_volume=function(start_month,start_day,start_hour){
  select_data=data.frame()
  select=data[which(data$"start_month"==start_month),]
  #天
  select1=data.frame()
  for (i in 1:length(start_day)) {
    select_l=select[which(select$"start_day"==start_day[i]),]
    select1=rbind(select1,select_l)
  }
  #小时
  select2=data.frame()
  for (i in 1:length(start_hour)) {
    select_l=select1[which(select1$"start_hour"==start_hour[i]),]
    select2=rbind(select2,select_l)
  }
  #确定的时间范围内的数据
  select_data=rbind(select_data,select2) 
}

select_hour_volume1=select_hour_volume(start_month,start_day,start_hour)
#-------------------------------

#-----------回归
route_volume=select_hour_volume1$volume
#route_volume=select_hour_volume1$volume-mean_hm_route[1:12]         #减去均值
x1=c(6,6.2,6.4,7,7.2,7.4)+0.1
x2=c(8,8.2,8.4,9,9.2,9.4)+0.1
lm1=lm(route_volume[1:6]~x1)
lm2=lm(route_volume[7:12]~x2)
#lm1$coefficients[2]

plot(route_volume~c(x1,x2),type='b',main=paste0(start_month,"/",start_day," tollgate:",tollgate_id,'_',direction))
abline(lm1,col = "blue")
abline(lm2,col = "red")

#-----------------------时间序列
ts1<-ts(route_volume, start=start_hour[1], frequency=3)
#原始数据图
plot(ts1,main=paste0(start_month,"/",start_day," tollgate:",tollgate_id,'_',direction))
#---
ts2=ts1-mean_hm_route[1:12]         #减去均值
plot(ts2)


#时间序列建模
library(TSA)
library(urca)

#
par(mfrow=c(1,2))
plot(ts1,
     ylab = '总播放量',xlab = '记录时间')
acf(as.vector(ts1),lag.max=36)   #查看平稳性-非平稳
par(mfrow=c(1,1))

#平稳检验-ts3
m1=ur.df(ts1,type='none')
summary(m1)    #无单位根










