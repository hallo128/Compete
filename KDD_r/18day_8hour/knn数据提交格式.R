#knn for volume，数据提交

sub_pre=cbind(sub_pre1_10,sub_pre2_19)

#day_sub=NULL
for (i in 1:7) {
  day_sub=c(day_sub,as.numeric(sub_pre[i,]))
}

#提交数据
sub_data=read.csv('volume.csv',he=T)  
sub_data$volume <- day_sub
#导出
write.csv(sub_data,"sub_volume_knn2.csv",row.names = F)

#mean(abs(sub_data$volume-day_sub)/sub_data$volume)
