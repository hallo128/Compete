#构造特征的函数

#输入：data = 数据框（test/train都可以,需要进入函数手动调风险标记）；type = test/train（选择-与后面的特征有关）
#放回：进行建模的数据框

library(dplyr)
structute_feature = function(data_new,type = 'test'){
  #-------------------------------------------构造计数特征
  data_by_id = group_by(data_new, id)
  struc_data1 = summarise(data_by_id, 
                          login_count = n(),                   #登录总次数
                          timelong_mean = round(mean(timelong),2),      #登录时长的平均
                          timelong_sd = round(sd(timelong),2),          #登录时长的标准差
                          result_num = n_distinct(result),     #result出现的可能情况种类
                          city_num = n_distinct(city),         #city出现的可能情况种类
                          device_num = n_distinct(device),     #device出现的可能情况种类
                          ip_num = n_distinct(ip),             #ip出现的可能情况种类
                          #id_have_risk = first(id_have_risk)   #---------------------------需要手动（只有训练集存在）
                          #该id是否有风险的标记
  )
  #--result!=1的数据集
  data_result_NOT_1 = filter(data_by_id, result != 1)
  struc_data2 = summarise(data_result_NOT_1, 
                          result_NOT_1_count = n(),              #不为1的result出现的总次数
                          result_NOT_1_num = n_distinct(result)  #不为1的result出现的可能情况种类
  )
  #--合并数据集(横向合并)
  struc_data = merge(struc_data1,struc_data2,by="id",all =TRUE)
  #-----------3个类型数据的统计情况
  data_type = struc_data
  for(i in unique(data$'type')){
    data_temp = summarise(filter(data_by_id, type == i), 
                          type_count   = n())              #type==i出现的总次数
    names(data_temp) = c('id',paste0('type', i, '_count'))                       
    data_type = merge(data_type,data_temp,by="id",all =TRUE)
  }
  #------2次或以上登录次数的数据，空值转换为0
  data_login = data_type[data_type$'login_count'>1, ]
  data_login[is.na(data_login)] <- 0
  #-------平均特征的构造
  #需要计算平均值的特征
  X_mean = c("login_count","city_num","device_num","ip_num","result_NOT_1_count",
             "type1_count","type3_count","type2_count")
  #还需要加入的变量
  {if(type == 'train')
      X_add = c('id','id_have_risk',"timelong_mean","timelong_sd","result_num","result_NOT_1_num")
   else
      X_add = c('id',"timelong_mean","timelong_sd","result_num","result_NOT_1_num")
  }
  
  data_login_mean = cbind(data_login[X_add],round(data_login[X_mean]/6,2))
  
  return(data_login_mean)
}

#登录-训练数据集
train_login_mean = structute_feature(data_new,type = 'train')
train_login_mean_All = structute_feature(data,type = 'train')
#登录-测试数据集
test_login_mean = structute_feature(test_login,type = 'test')


#write.csv(train_login_mean,'train_login_mean.csv',row.names = FALSE)
#write.csv(test_login_mean,'test_login_mean.csv',row.names = FALSE)










