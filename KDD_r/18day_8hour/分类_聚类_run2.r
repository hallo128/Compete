#  聚类 for volume

setwd("/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour")

#--------每条路线分为2个时间段来建模
tollgate_id=3              #-------收费站（123）
direction=1                #-------方向（0进1出）
start_hour=c(15,16,17,18)    #小时----0-23，(6,7,8,9)(15,16,17,18)

#1----------------------按时间段产生训练数据
#读入train数据
#data=read.table(paste0(" 18day_8hour   tollgate:",tollgate_id,'_',direction,'.txt'),
#                he=T) 

#-------------------------
#读入train数据(法二)
#2.1
filename='training_split_avg_volume1' #"training_split_avg_volume1"      #--------文件名
lda_data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  
names(lda_data)
#2.2分线路
data=lda_data_route(lda_data,tollgate_id,direction)
#-------------------------
data=data[order(data$start_day),]    #每天的数据排序归类

#2----------------读入需要的函数
source('/Users/apple/code_tool/KDD_run/KDD_r/18day_8hour/cluster_function.R', encoding = 'UTF-8')

#3-----------------------------对train数据进行聚类
##---------构造聚类的特征
#--9月、10月的归总
#-------9月份
start_month=9
start_day=19:27    #19:28或者19：19
distinct_route_volume_month1=distinct_route_volume_month(data,start_month,start_day,start_hour)
#--------10月份
start_month=10
start_day=10:17    #19:28或者19：19
distinct_route_volume_month2=distinct_route_volume_month(data,start_month,start_day,start_hour)
#--9月、10月的归总
distinct_route_volume=rbind(distinct_route_volume_month1,distinct_route_volume_month2)
#-------------

##--------------k均值聚类
x_name=c('b1_v','b2_v','range1_v','range2_v','top1_v','top_v','top_point_v')
d_k=distinct_route_volume[,x_name]   #-------所有属性数据

#-----选择最优类别数k(函数自动识别)
k=select_k(d_k)

#------确定K后
fit_km1=kmeans(d_k,centers =k)   #k
#print(fit_km1)
fit_km1$centers                  #每一类的平均值
number_cluster=fit_km1$cluster   #分类排序
data_v_cluster=cbind(distinct_route_volume,number_cluster)      #------------聚类后的分类结果

#table(data_v_cluster$top_point_v,data_v_cluster$number_cluster)



#---------------------判别分析
#tollgate_id             #-------收费站-一开始就已知
#direction               #-------方向-一开始就已知
filename='test1_split_avg_volume1' #"training_split_avg_volume1"      #--------文件名
lda_data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  
names(lda_data)

#--分线路
lda_data_route1=lda_data_route(lda_data,tollgate_id,direction)

#--------10月份
start_month=10
start_day=18:24    #10:17或者18:24
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


#------------预测
pre_cluster1=pre_cluster_1_0_10(fit_km1,pre_lda1,test_route_volume)
if(start_hour[1]==6) {
  sub_pre1_10=pre_cluster1
}
if(start_hour[1]==15) {
  sub_pre2_19=pre_cluster1
}







