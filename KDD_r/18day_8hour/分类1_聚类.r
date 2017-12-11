#  聚类 for volume

setwd("/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour")

tollgate_id=1              #-------收费站（123）
direction=0                #-------方向（0进1出）
data=read.table(paste0(" 18day_8hour   tollgate:",tollgate_id,'_',direction,'.txt'),
                he=T)  
names(data)
data=data[order(data$start_day),]    #每天的数据排序归类


#-----------------------------------选择几天的几小时的数据
start_month=9
start_day=25    #19:28或者19：19
start_hour=c(6,7,8,9)     #小时----0-23，15,16,17,18

#任意指定data数据，选择特定小时数据（train和test的数据都适合）
select_hour_volume=function(data,start_month,start_day,start_hour){
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

select_hour_volume1=select_hour_volume(data,start_month,start_day,start_hour)
#-------------------------------

#-----------回归(求b)   #只针对某1天求
b_route_volume=function(route_volume){
  if(start_hour[1]==6) {x1=c(6,6+2/6,6+4/6,7,7+2/6,7+4/6)+1/6;
                       x2=c(8,8+2/6,8+4/6,9,9+2/6,9+4/6)+1/6}
  if(start_hour[1]==15) {x1=c(15,15+2/6,15+4/6,16,16+2/6,16+4/6)+1/6;
                        x2=c(17,17+2/6,17+4/6,18,18+2/6,18+4/6)+1/6}
  lm1=lm(route_volume[1:6]~x1)
  {if(length(route_volume)==12)
    {lm2=lm(route_volume[7:12]~x2)
    l=list("b1"=lm1$coefficients[2],"b2"=lm2$coefficients[2])}
  else
    l=list("b1"=lm1$coefficients[2])
  }
  return(l)
}

route_volume=select_hour_volume1$volume
b_route_volume1=b_route_volume(route_volume)
b_route_volume1

#route_volume=select_hour_volume1$volume-mean_hm_route[1:12]         #减去均值
#lm1$coefficients[2]
#画图
#plot(route_volume~c(x1,x2),type='b',main=paste0(start_month,"/",start_day," tollgate:",tollgate_id,'_',direction))
#abline(lm1,col = "blue")
#abline(lm2,col = "red")
#-----------------------

#-------------极值函数
range_route_volume=function(route_volume){
  x1=route_volume[1:6]
  {if(length(route_volume)==12)
    {x2=route_volume[7:12]
    l=list("range1"=x1[6]-x1[1],"range2"=x2[6]-x2[1])}
  else
    l=list("range1"=x1[6]-x1[1])
  }
  return(l)
}
#----最值函数
top_route_volume=function(route_volume){
  x1=route_volume[1:6]
  {if(length(route_volume)==12)
    l=list("top1"=max(x1),"top"=max(route_volume))
  else
    l=list("top1"=max(x1))
  }
  return(l)
}
#----最高点位置函数（4个小时-12个数据，只针对train，也就说test数据不存在）
top_point_volume=function(route_volume){
  x1=route_volume[1:12]
  l=list("top_point"=which.max(x1))
  return(l)
}
#-------------

#-----------------------构造聚类的特征
#start_hour为c(6,7,8,9)或者c(15,16,17,18)，必须是12个数据;只针对data数据
distinct_route_volume_month=function(data,start_month,start_day,start_hour){
  b1_v=NULL
  b2_v=NULL
  range1_v=NULL
  range2_v=NULL
  top1_v=NULL
  top_v=NULL
  top_point_v=NULL
  for (i in 1:length(start_day)) {
    select_hour_volume1=select_hour_volume(data,start_month,start_day[i],start_hour)
    route_volume=select_hour_volume1$volume
    b_route_volume1=b_route_volume(route_volume)
    b1_v[i]=b_route_volume1$b1
    b2_v[i]=b_route_volume1$b2
    range_volume1=range_route_volume(route_volume)
    range1_v[i]=range_volume1$range1
    range2_v[i]=range_volume1$range2
    top_route_volume1=top_route_volume(route_volume)
    top1_v[i]=top_route_volume1$top1
    top_v[i]=top_route_volume1$top
    top_point_v[i]=top_point_volume(route_volume)
  }
  distinct_route_volume1=data.frame(tollgate_id,direction,start_month,start_day,
                                    b1_v,b2_v,range1_v,range2_v,top1_v,top_v,top_point_v)
}
#--9月、10月的归总
#-------9月份
start_month=9
start_day=19:27    #19:28或者19：19
start_hour=c(6,7,8,9) 
distinct_route_volume_month1=distinct_route_volume_month(data,start_month,start_day,start_hour)
#--------10月份
start_month=10
start_day=10:17    #19:28或者19：19
start_hour=c(6,7,8,9) 
distinct_route_volume_month2=distinct_route_volume_month(data,start_month,start_day,start_hour)
#--9月、10月的归总
distinct_route_volume=rbind(distinct_route_volume_month1,distinct_route_volume_month2)
#---------------------------

#----------------------k均值聚类
x_name=c('b1_v','b2_v','range1_v','range2_v','top1_v','top_v','top_point_v')
d_k=distinct_route_volume[,x_name]   #-------所有属性数据
fit_km1=kmeans(d_k,centers =4)   #------k
print(fit_km1)
fit_km1$centers
number_cluster=fit_km1$cluster   #分类排序
data_v_cluster=cbind(distinct_route_volume,number_cluster)      #------------聚类后的分类结果


#-----选择最优类别数
result=rep(0,nrow(d_k)-1)
for (k in 1:length(result)) {
  fit_km=kmeans(d_k,centers=k)
  result[k]=fit_km$betweenss/fit_km$totss
}
round(result,2)

plot(1:length(result),result,type='b',main="Choosing the Optimal Number of Cluster",
     xlab='namber of cluster:k',ylab='between/totss')
result[k]


#---------------------判别分析
#tollgate_id             #-------收费站-一开始就已知
#direction               #-------方向-一开始就已知
filename='test1_split_avg_volume1' #"training_split_avg_volume1"      #--------文件名
lda_data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  
names(lda_data)

#--------------选取特定路线的函数（收费站-方向）
lda_data_route=function(tollgate_id,direction){
  lda_data_route=data.frame()
  f_l1=lda_data[which(lda_data$"tollgate_id"==tollgate_id),]                  #-确定收费站的数据
  lda_data_route=rbind(lda_data_route, f_l1[which(f_l1$"direction"==direction),])    #-确定交叉口
}

lda_data_route1=lda_data_route(tollgate_id,direction)

#--------------从特定路线中提取出需要预测的时间段
#-----------------9月、10月的归总
#-------9月份
#start_month=9
#start_day=19:27    #19:28或者19：19
#start_hour=c(6,7,8,9) 
#distinct_route_volume_month1=distinct_route_volume_month(lda_data_route1,start_month,start_day,start_hour)
#聚类算法
test_distinct_route_volume_month=function(data,start_month,start_day,start_hour){
  b1_v=NULL
  range1_v=NULL
  top1_v=NULL
  for (i in 1:length(start_day)) {
    select_hour_volume1=select_hour_volume(data,start_month,start_day[i],start_hour)
    route_volume=select_hour_volume1$volume
    b_route_volume1=b_route_volume(route_volume)
    b1_v[i]=b_route_volume1$b1
    range_volume1=range_route_volume(route_volume)
    range1_v[i]=range_volume1$range1
    top_route_volume1=top_route_volume(route_volume)
    top1_v[i]=top_route_volume1$top1
  }
  distinct_route_volume1=data.frame(tollgate_id,direction,start_month,start_day,
                                    b1_v,range1_v,top1_v)
}

#--------10月份
start_month=10
start_day=18:24    #10:17或者18:24
start_hour=c(6,7,8,9) 
distinct_route_volume_month2=test_distinct_route_volume_month(lda_data_route1,start_month,start_day,start_hour)
#--9月、10月的归总
#test_route_volume=rbind(distinct_route_volume_month1,distinct_route_volume_month2)
test_route_volume=distinct_route_volume_month2


#------------进行判别分析(x_name(训练数据：d_k,data_v_cluster,分类变量：number_cluster;)
#x_name=c('b1_v','b2_v','range1_v','range2_v','top1_v','top_v')
x_name=c('b1_v','range1_v','top1_v')
library('MASS')
data_train=data_v_cluster[,x_name]
fit_lda1=lda(number_cluster~.,data_train)
#进行预测
data_test=test_route_volume[,x_name]
pre_lda1=predict(fit_lda1,data_test)
pre_lda1$class      #排序结果
#table(number_cluster,pre_lda1$class)       #实际与预测的判别情况
#error_lda=sum(as.numeric(as.numeric(pre_lda1$class) != as.numeric(number_cluster)))/nrow(data_test)
#error_lda           #错误率（只针对训练数据自己判别时用）


#----------k近邻
#library('class')
#默认k=1
#fit=knn(data_train,data_test,data_v_cluster$b2_v,k=2)
#fit




