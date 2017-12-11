#平均流量的波动因素
#volume_model

setwd("/Users/apple/code_tool/KDD_run/KDD_r/volume")
load('data_fea_X.rda')

#合并数据框-merge
d1=merge(vehicle_type_X,vehicle_model_X)
d2=merge(data_weekdays_X,d1)

#----------------剔除节假日9.29-10.9
rmFestival=function(data){
  select1=NULL
  start_day=19:28
  for (i in 1:length(start_day)) {
    select_l=data[which(data$"start_month"== 9 & data$"start_day"== start_day[i]),]
    select1=rbind(select1,select_l)
  }
  select2=NULL
  start_day=10:17
  for (i in 1:length(start_day)) {
    select_l=data[which(data$"start_month"== 10 & data$"start_day"== start_day[i]),]
    select2=rbind(select2,select_l)
  }
  use_data=rbind(select1,select2)
  return(use_data)
}
use_data=rmFestival(data)

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


