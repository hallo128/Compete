
setwd('E:/京东比赛/数据/Risk_Detection_Qualification/')

data = read.csv('t_login.csv')

data_trade = read.csv('t_trade.csv')

#交易数据中，判断为有风险的id账号
is_risk_id = unique(data_trade[data_trade$is_risk==1, 'id'])

data$'id_have_risk' = as.numeric(data$id %in% is_risk_id)


#----------------1导出登录次数为1的登录数据
library(dplyr)
data_by_id2 = group_by(data, id)
ll1 = summarise(data_by_id2, 
                login_count = n())                  #登录总次数
id1 = unlist(ll1[ll1$login_count ==1,'id'])         #登录次数为1的id号
da = data[data$id %in% id1,]
#write.csv(da,'login_count1_data.csv',row.names = FALSE)
prop.table(table(da$'id_have_risk'))

#----------------2导出登录次数为1的交易数据
da2 = data_trade[data_trade$id %in% id1,]
#write.csv(da2,'trade_count1_data.csv',row.names = FALSE)
#---------------------------------------------


#-------------平衡样本
#1查看类别相差的比例
prop.table(table(data$'id_have_risk'))
#2.将y变量转换为因子型
data$'id_have_risk' = as.factor(data$'id_have_risk')
#3.失衡处理
n_id = sort(unique(data$'id'))  #登录数据的所有id号
#提取下标集(从不含有风险的id里面，提取出与由风险的id2数量相同的id数——2倍)
set.seed(1234)
good_id=setdiff(n_id, is_risk_id) #不含有风险的id
set.seed(567)
select_good_id = sample(good_id, 2*length(is_risk_id), replace = F)

data_new = data[data$id %in% c(select_good_id,is_risk_id),]
prop.table(table(data_new$'id_have_risk'))


#write.csv(data_new,'is_riskId_login_BlanceById2.csv',row.names = FALSE)
#write.csv(data,'is_riskId_login.csv')

#----------------------------------------进行分组统计
library(dplyr)
data_by_id = group_by(data_new, id)
struc_data1 = summarise(data_by_id, 
               login_count = n(),                   #登录总次数
               timelong_mean = mean(timelong),      #登录时长的平均
               timelong_sd = sd(timelong),          #登录时长的标准差
               result_num = n_distinct(result),     #result出现的可能情况种类
               city_num = n_distinct(city),         #city出现的可能情况种类
               device_num = n_distinct(device),     #device出现的可能情况种类
               ip_num = n_distinct(ip),             #ip出现的可能情况种类
               id_have_risk = first(id_have_risk)   #该id是否有风险的标记
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

#write.csv(data_type,'login_struc_data.csv',row.names = FALSE)    #导出构造出特征的数据

#------------------------------------数据预处理和选取
#---------只有一次登录信息的数据
data_login_count1 = data_type[data_type$'login_count'==1, ]
#------2次或以上登录次数的数据，空值转换为0
data_login = data_type[data_type$'login_count'>1, ]
data_login[is.na(data_login)] <- 0

#names(data_login)
#-------平均特征的构造
#需要计算平均值的特征
X_mean = c("login_count","city_num","device_num","ip_num","result_NOT_1_count",
           "type1_count","type3_count","type2_count")
#还需要加入的变量
X_add = c('id','id_have_risk',"timelong_mean","timelong_sd","result_num","result_NOT_1_num")
data_login_mean = cbind(data_login[X_add],round(data_login[X_mean]/6,2))
#最后建模选用的特征




#导出2次或以上登录次数的构造出特征的数据
write.csv(data_login,'login_struc_data2.csv',row.names = FALSE)


#--type==1的数据集
#data_type1 = filter(data_by_id, type == 1)
#type1 = summarise(filter(data_by_id, type == 1), 
#                 'type1_count' = n())              #type==1出现的总次数
                 








t1=data$timestamp[1]
t1
time1 = data$time[1]
time1
mode(time1)
mode(t1)
as.POSIXct(datetime, origin="1970-01-01 00:00:00")

#----------------日期型转换为时间戳
#
ll=as.POSIXct('2015-04-21 20:34:11.0', origin="1970-01-01 00:00:00")
mode(ll)
as.numeric(ll)

l2=strptime(time1,"%Y-%m-%d %H:%M:%S")
mode(l2)
as.numeric(l2)
