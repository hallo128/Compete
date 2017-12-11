setwd("/Users/apple/code_tool/KDD_run/KDD_r")

filename="training_split_avg_volume1"
data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  #-------
names(data)

#-----------------------------------剔除9/29-10/9
#9/19-9/28
start_month=9
start_day=19:28
start_hour=c(6,7,8,9,15,16,17,18)     #小时----0-23
 
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

#10/10-10/17
start_month=10
start_day=10:17
select_hour_volume2=select_hour_volume(start_month,start_day,start_hour)

#总可用数据
select_hour_volume=rbind(select_hour_volume1,select_hour_volume2) 



#---------------------分路线

tollgate_id=3             #------收费站
direction=1             #----方向：0进；1出

select_route_volume=function(tollgate_id,direction){
  select_data=data.frame()
  #必须用----select_hour_volume的数据
  select2=select_hour_volume[which(select_hour_volume$"tollgate_id"==tollgate_id),]              
  #-确定收费站的数据
  select3=select2[which(select2$"direction"==direction),]              
  #-确定方向
  select_data=rbind(select_data,select3) 
}

select_route_volume1=select_route_volume(tollgate_id,direction)


#------------
write.table(select_route_volume1,
            paste0(" 18day_8hour   tollgate:",tollgate_id,'_',direction,'.txt'),
            row.names = F,
            col.names = T)





