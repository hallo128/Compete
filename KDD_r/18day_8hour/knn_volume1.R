#knn for volume

setwd("/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour")

#--------每条路线分为2个时间段来建模
tollgate_id=3              #-------收费站（123）
direction=1                #-------方向（0进1出）
start_hour=c(6,7,8,9)    #小时----0-23，(6,7,8,9)(15,16,17,18)


#1----------------------按时间段产生训练数据
#读入原始每条路的数据
data=read.table(paste0(" 18day_8hour   tollgate:",tollgate_id,'_',direction,'.txt'),
                he=T)  
data=data[order(data$start_day),]    #每天的数据排序归类


#2----------------读入需要的函数
source('/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour/cluster_function.R', encoding = 'UTF-8')



#----------------将数据格式转换为我们需要的格式
data_use=function(data,start_month,start_day,start_hour){
  x_frame=NULL
  for (i in 1:length(start_day)) {
    x=NULL
    data_temp=select_hour_volume(data,start_month,start_day[i],start_hour)
    data_temp=data_temp[order(data_temp$start_day),] 
    x=data_temp$volume
    row_x=c(start_month,start_day[i],x)
    x_frame=rbind(x_frame,row_x)
  }
  
  x_frame=data.frame(x_frame,row.names=NULL)
  {if(start_hour[1]==6) x_label=paste0('x',1:12)
    else x_label=paste0('y',1:12)}
  names(x_frame)=c('start_month','start_day',x_label)
  return(x_frame)
}


#3-----------------------------对异常数据进行剔除,并转换为我们需要的格式
#--9月、10月的归总
#-------9月份
start_month=9
start_day=19:27    #19:28或者19：19
data1=data_use(data,start_month,start_day,start_hour)
#--------10月份
start_month=10
start_day=10:17    #19:28或者19：19
data2=data_use(data,start_month,start_day,start_hour)
#--9月、10月的归总
data_all=rbind(data1,data2)
#-------------


set.seed(12)
n <- nrow(data_all)
val <- sample(1:n, size = round(n/3), replace = FALSE) 
data_train<- data_all[-val,]
data_test <- data_all[val,]

data_train_x=data_all[-val,c(3:8)]
data_train_y=data_all[-val,c(9:14)]
data_test_x=data_all[val,c(3:8)]
data_test_y=data_all[val,c(9:14)]

library(kknn)
fit_kknn=kknn(data_train_y[,1]~.,data_train_x,data_test_x)

kknn.dist(data_test_x, data_train_x, k = 2, distance = 2) 

fit_knn=knn(data_train_x,data_test_x,data_train_y[,1])

error=mean(abs((data_test_y[,1]-as.numeric(as.character(fit_knn)))/data_test_y[,1]))




