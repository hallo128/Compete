
#ts
#d_k
#distinct_route_volume
#data_v_cluster
#data


#filename='training_split_avg_volume1' #"training_split_avg_volume1"      #--------文件名
#lda_data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  
#names(lda_data)

#--分线路
#data=lda_data_route(lda_data,tollgate_id,direction)



#------------进行判别分析(x_name(训练数据：d_k,data_v_cluster,分类变量：number_cluster;)
#x_name=c('b1_v','b2_v','range1_v','range2_v','top1_v','top_v')


#-------------

x_name=c('b1_v','range1_v','top1_v')
library('MASS')
data_train=data_v_cluster[,x_name]
fit_lda1=lda(number_cluster~.,data_train)
#进行预测
data_test=distinct_route_volume[,x_name]   #---
pre_lda1=predict(fit_lda1,data_test)
pre_lda1$class      #排序结果

table(number_cluster,pre_lda1$class)       #实际与预测的判别情况
#error_lda=sum(as.numeric(as.numeric(pre_lda1$class) != as.numeric(number_cluster)))/nrow(data_test)
#error_lda           #错误率（只针对训练数据自己判别时用）


#------------预测
pre_cluster1=pre_cluster_1_0_10(fit_km1,pre_lda1,distinct_route_volume)
if(start_hour[1]==6) {
  sub_pre1_10=pre_cluster1
}
if(start_hour[1]==15) {
  sub_pre2_19=pre_cluster1
}


