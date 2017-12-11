
setwd("/Users/apple/code_tool/KDD_run/KDD_r/travel_time")


data_test=read.csv('/Users/apple/code_tool/KDD_run/KDD_python/trajectories(table 5)_test1.csv')  
data_test1=data_test



#---------------------------------将分割时间添加到最后列
col_append=function(data){
  n=nrow(data)
  #------------------------------时间分割
  month=NULL
  day=NULL
  hour=NULL
  minute=NULL
  for (i in 1:n) {
    bbb<- unlist(strsplit(as.character(data$starting_time[i]),split=" ")) 
    month[i]=unlist(strsplit(as.character(bbb[1]),split="-"))[2]
    day[i]=unlist(strsplit(as.character(bbb[1]),split="-"))[3]
    hour[i]=unlist(strsplit(as.character(bbb[2]),split=":"))[1]
    minute[i]=unlist(strsplit(as.character(bbb[2]),split=":"))[2]
  }
  #---------------------------------将分割时间添加到最后列
  d1=cbind(month,day)
  d2=cbind(d1,hour)
  d3=cbind(d2,minute)
  data=cbind(data,d3)
  names(data) <- c("intersection_id","tollgate_id","vehicle_id","starting_time", 
                   "travel_seq","travel_time",
                   "start_month","start_day","start_hour","start_minute")
  #---------------------
  return(data)
}
#--
data_test <- col_append(data_test)


#--------------------------每条路线分为2个时间段来建模
intersection_id='A'
tollgate_id=3              #-------收费站（123）
links=c(110,123,107,108,119,114,118,122)
length_link=c(109,59,34,40,9,198,206,197)



start_month='10'    #08
start_day=18:24    #c(17,18)  #17,18  '01','02'
select_day_time_test1=select_day_time(data_test,start_month,start_day,tollgate_id,intersection_id)

#---------------------------计算每行的link速度并将添加到最后列select_day_time1
d_links=NULL
for (i in 1:nrow(select_day_time_test1)) {
  link_v1=f_link_v(length_link,links,select_day_time_test1$travel_seq[i])
  d_links=rbind(d_links,link_v1)
}
d_links=as.data.frame(d_links)
names(d_links)<-as.character(links)

d_links_a=cbind(select_day_time_test1,d_links)
#数据排序归类
d_links_a=d_links_a[order(d_links_a$start_day,d_links_a$start_hour,d_links_a$start_minute),]      


#---------------查看每天不同时刻的车辆数
day='23' #--
select1_start_hour=d_links_a[which(d_links_a$"start_day"==day),]$start_hour #--
table(select1_start_hour)
plot(select1_start_hour)
par(family='STKaiti')
title(paste0('流量',start_month,'-',day))
#---------------



travel_time=d_links_a[,"travel_time"]
#异常值识别 
dotchart(travel_time)  #单变量散点图
sp=boxplot(travel_time)
sp$out
#--异常点
#--剔除异常点
#异常数据处理（sub-异常值所在的行）
sub=NULL
for (i in 1:length(sp$out)) {
  sub[i] = which(d_links_a$travel_time==sp$out[i])
}

d_links_normal=d_links_a[-sub,]    #剔除异常数据
d_outlines=d_links_a[sub,]         #异常数据
#--


#--------------------------------平均旅行时间的描述统计
library(fBasics)
d1=basicStats(d_links_full$travel_time)
d2=basicStats(d_links_normal$travel_time)
cbind(d1,d2)






