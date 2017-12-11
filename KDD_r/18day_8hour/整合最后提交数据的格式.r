#对与volume


#setwd("/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour")
#sub_data=read.csv('p_volume_sample.csv',he=T)  

#sub_pre1_10=pre_cluster1
#sub_pre2_19=pre_cluster1
sub_pre=cbind(sub_pre1_10,sub_pre2_19)

day_sub=NULL
for (i in 1:7) {
  day_sub=c(day_sub,as.numeric(sub_pre[i,]))
}


#--9月、10月的归总
#-------9月份
start_month=9
start_day=19:27    #19:28或者19：19
data_v1=select_hour_volume(data,start_month,start_day,start_hour)
#--------10月份
start_month=10
start_day=10:17    #19:28或者19：19
data_v2=select_hour_volume(data,start_month,start_day,start_hour)
#--9月、10月的归总
data_v=rbind(data_v1,data_v2)
#-------------




#提交数据
sub_data=read.csv('volume.csv',he=T)  
sub_data$volume <- round(day_sub,0)
#导出
write.csv(sub_data,"sub_volume.csv",row.names = F)
