#knn for volume

setwd("/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour")

#--------每条路线分为2个时间段来建模
tollgate_id=3              #-------收费站（123）
direction=1                #-------方向（0进1出）
start_hour=c(15,16,17,18)     #小时----0-23，(6,7,8,9)(15,16,17,18)


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
  
  if(ncol(x_frame)==8) n=6
  if(ncol(x_frame)==14) n=12
  
  {if(start_hour[1]==6) x_label=paste0('x',1:n)
  else x_label=paste0('y',1:n)}
  names(x_frame)<-c('start_month','start_day',x_label)
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



#-------------------------------------------相似度，预测
filename='test1_split_avg_volume1' #"training_split_avg_volume1"      #--------文件名
data_test=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  
#head(data_test)

#--分线路
data_test_route1=lda_data_route(data_test,tollgate_id,direction)

#--------10月份
start_month=10
start_day=18:24    #10:17或者18:24
test_route_volume=data_use(data_test_route1,start_month,start_day,start_hour)

#--------------固定数据格式
data_train_x=data_all[,c(3:8)]
data_train_y=data_all[,c(9:14)]
data_test_x=test_route_volume[,c(3:8)]


#标准化欧式距离
test2train_dist=function(data_train_x,data_test_x){
  dist_data=NULL
  for (j in 1:7) {
    data_temp=rbind(data_train_x,data_test_x[j,]) #最后一列为test内容
    #按行标准化
    data_temp1= scale(t(data_temp), center=T,scale=T)
    dist_temp = NULL
    {for (i in 1:(nrow(data_temp)-1))
      dist_temp[i] = sqrt(sum((data_temp1[,i] - data_temp1[,nrow(data_temp)])^2))} 
    dist_data=rbind(dist_data,dist_temp)
  }
  dist_data=data.frame(dist_data,row.names=NULL)
  return(dist_data)
}

#1-7天到前面17天的距离
dist7=test2train_dist(data_train_x,data_test_x)


#根据不同的k值，得到后面7天不同的预测值
pre_data=function(k,dist7,data_train_y){
  pre_data=NULL
  for (day in 1:7) {
    pre_dist_data=NULL
    post=order(dist7[day,])[1:k]   #选出距离最小的前k个数据对应的位置
    #data_train_y[post,]            #选出距离最小的前k个数据的预测值部分
    dist_temp_v=dist7[day,post]    #选出距离最小的前k个数据对应的距离
    #order(dist7[1,])   #从小到大的位置
    #rank(dist7[1,])    #从小到大，对应位置的级别
    pre_dist_data=cbind(data_train_y[post,],t(dist_temp_v))
    
    pre_day=NULL
    for (j in 1:6) {#1-6个预测时刻
      pre_day[j]=sum(pre_dist_data[,j]*(1/pre_dist_data[,7])/sum((1/pre_dist_data[,7])))
    }
    pre_day=round(pre_day)   #4舍5入取整
    pre_data=rbind(pre_data,pre_day)
  }
  pre_data=data.frame(pre_data,row.names=NULL)
  return(pre_data)
}



#------------预测
pre_data1=pre_data(k=5,dist7,data_train_y)
if(start_hour[1]==6) {
  sub_pre1_10=pre_data1
}
if(start_hour[1]==15) {
  sub_pre2_19=pre_data1
}

