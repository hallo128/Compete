#change-sub
#--交换数据后的更新

#---------------------volume
#1-新建提交时间格式
setwd("/Users/apple/code_tool/KDD_run/KDD_r")
time_windows=read.table("25-31time_wondiow.txt")

#2-读入volume最好的提交数据
sub=read.csv("./18day_8hour/best_sub_volume.csv")

#3-交换时间
sub$time_window <- rep(as.character(time_windows[,1]),5)

#4-导出提交数据
write.csv(sub,"sub_volume_0530.csv",row.names = F)



