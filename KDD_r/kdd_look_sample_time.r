#查看kdd个体、总体的趋势特征

setwd("/Users/apple/code_tool/KDD_run/KDD_r")

filename="training_split_avg_travel_time1"
data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  #-------
names(data)
nrow(data)
data[1,]
data[1,"time_window"]


#-------------------按要求选数据
start_month=9         #-------
start_day=20          #-------
#start_hour=6
#end_hour=8
intersection_id='A'
tollgate_id=2            #------收费站


#------------------------------------------------到天
#------时间为月、日，各个收费站的选取数据的函数
select_day_time=function(start_month,start_day,tollgate_id,direction){
  select_data=data.frame()
  select1=data[which(data$"start_month"==start_month & data$"start_day"==start_day),]
  #确定的时间范围内的数据
  select2=select1[which(select1$"intersection_id"==intersection_id),]              
  #-确定收费站的数据
  select3=select2[which(select2$"tollgate_id"==tollgate_id),]              
  #-确定方向
  select_data=rbind(select_data,select3) 
}

select_day_time1=select_day_time(start_month,start_day,tollgate_id,direction)

#----time的时序图
ts_time<-ts(select_day_time1$avg_travel_time, start=0, frequency=3)
#原始数据图
plot(ts_time,main=paste0(start_month,"/",start_day," tollgate:",intersection_id,'_',tollgate_id))
#------------------------------------------------到天



tollgate_id=3             #------收费站
direction=1              #----方向：0进；1出
#------------------------------------------------到月
#------时间为月、日，各个收费站的选取数据的函数
select_month_volume=function(tollgate_id,direction){
  select_data=data.frame()
  select2=data[which(data$"tollgate_id"==tollgate_id),]              
  #-确定收费站的数据
  select3=select2[which(select2$"direction"==direction),]              
  #-确定方向
  select_data=rbind(select_data,select3) 
}

select_month_volume1=select_month_volume(tollgate_id,direction)

#----volume的时序图
ts_volume<-ts(select_month_volume1$volume, start=0, frequency=72)
#原始数据图
plot(ts_volume,main=paste0("9/19-10/17"," tollgate:",tollgate_id,'_',direction))
#------------------------------------------------到月






