

setwd("/Users/apple/code_tool/KDD_run/KDD_r/travel_time")

data2=read.csv('/Users/apple/code_tool/KDD_run/KDD_python/training_split_avg_travel_time1.csv')  

#select_way_volume1=select_way_volume(data2,tollgate_id,direction)
#start_month='07'
#start_day=paste0_h(19:31)
intersection_id='C'
tollgate_id=3              #-------收费站（123）

start_month=10           #7,8,9,10
start_day=c(1:17)     #c(19:31)   c(1:31)   c(1:30) c(1:17)
start_hour=c(0:23)
select_month_time7=select_hour_time(data2,start_month,start_day,start_hour,tollgate_id,intersection_id)

time_numeric=round(select_month_time7$start_day+((select_month_time7$start_hour+select_month_time7$start_minute/60)+1/6)/24,2)

data_t=cbind(select_month_time7,time_numeric)
data_t=data_t[order(data_t$time_numeric),]    #每天的数据排序归类

plot(data_t$avg_travel_time~data_t$time_numeric,
     main=paste0(intersection_id,'_',tollgate_id,":",start_month,'/',start_day[1],'-',start_day[length(start_day)]),
     type='l')

#---------------------------


data_t=select_month_time7[order(select_month_time7$start_day),]    #每天的数据排序归类




ts1=ts(select_month_time7$avg_travel_time,frequency = 7)
plot(ts1,main=paste0(intersection_id,'_',tollgate_id,":",start_month,'/',start_day[1],'-',start_day[length(start_day)]))





select_day_volume1=select_day_volume(data,start_month,start_day,tollgate_id,direction)
