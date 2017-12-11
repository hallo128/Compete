#2017-12-3

#构建新的特征表  login_trade_X
#1.先对交易数据按小时处理（转换时间戳；按id，按小时）
#2.合并交易数据和登录数据，并补全交易的登录，还有登录的判断
#3.对登录数据按天进行处理

#setwd('/Volumes/KINGSTON/京东比赛/data/Risk_Detection_Qualification/')
setwd('E:/京东比赛/数据/Risk_Detection_Qualification/')

data_login = read.csv('t_login_test.csv')
data_trade = read.csv('t_trade_test.csv')
#
n_test_id = sort(unique(data_trade$'id'))  #待预测数据的所有id号
#---直接合并所有登录数据，取出待预测的登录数据
#data_login = rbind(read.csv('t_login_test.csv'),read.csv('t_login.csv'))
#write.csv(data_login,'login_ALL.csv',row.names = FALSE)  
data_login = read.csv('login_ALL.csv')
data_login = data_login[data_login$id %in% n_test_id,]  


#-------------------------------------对交易数据的处理
#1-日期型转换为时间戳(添加一列时间戳的变量timestamp)
data_trade$'timestamp' = as.numeric(as.POSIXct(data_trade$time, origin="1970-01-01 00:00:00"))
data_trade$'hour' = strftime(data_trade$time, format = "%m-%d %H")

#2-按id，按小时进行分组统计
#转换为因子
data_trade$id <- as.factor(data_trade$id)
data_trade$hour <- as.factor(data_trade$hour)
#分组统计
library(dplyr)
data_by_idHour <- data_trade %>% group_by(id,hour)
#最终的整理交易数据
trade_idHour = summarise(data_by_idHour, 
                timestamp = mean(timestamp),       #时间戳（平均时间）
                #is_risk = as.numeric(any(is_risk == 1)),          #是否有风险(只要出现就视为有风险)
                time = first(time),                #首次的时间
                trade_count = n())                  #登录总次数
#----为1小时的交易数据，添加‘day’变量，为了进行数据的合并
#trade_idHour$'day' = strftime(trade_idHour$time, format = "%m-%d")
#trade_idHour$'day' = as.factor(trade_idHour$'day')
#as.POSIXct(1420834319, origin="1970-01-01 00:00:00")   #时间戳转换为time
#ll = data_trade[order(data_trade$id,data_trade$timestamp),] #排序查看



#-----------------------------#2.合并交易数据和登录数据，并补全交易的登录，还有登录的判断
#转换为因子
data_login$id <- as.factor(data_login$id)
data_login$time <- as.factor(data_login$time)
#转换为字符
data_login$log_id <- as.character(data_login$log_id)
#--合并整理的交易数据和全部的登录数据
#str(trade_idHour)
#str(data_login)
login_trade1 = merge(trade_idHour,data_login,by=c("time",'timestamp','id'),all =TRUE)
#-按id，与时间戳进行排序
login_trade2 = login_trade1[order(login_trade1$id,login_trade1$timestamp),]   #排序查看
login_trade2$'hour' = strftime(login_trade2$time, format = "%m-%d %H")        #hour为了使都变为登录对应的hour
#-补全交易数据的登录信息
login_names = c('hour','log_id',"timelong","device","log_from","ip","city","result",
                "type","is_scan","is_sec")
login_trade3 = login_trade2   #先拷贝一份数据
index_na=which(is.na(login_trade3$ip))
while(length(index_na) != 0){
  login_trade3[index_na,login_names]= login_trade3[(index_na-1),login_names]
  index_na=which(is.na(login_trade3$ip))
}

#write.csv(login_trade3,'login_trade_new2.csv',row.names = FALSE)


#---------------------------------------------------3.对登录数据进行处理
#3.对登录数据按天进行处理
#1-添加一列日期精确到天
data_login$'day' = strftime(data_login$time, format = "%m-%d")
data_login$'only_hour' = strftime(data_login$time, format = "%H")
data_login$'timeOfDay' = sapply(data_login$only_hour, 
                               function(h){
                                 if(h %in% c('00','01','02','03','04','05'))
                                   return('T1')
                                 else if(h %in% c('06','07','08','09',10,11))
                                   return('T2')
                                 else if(h %in% 12:17)
                                   return('T3')
                                 else
                                   return('T4')
                               })  #一天中的时间段(T1[0-5],T2[6-11],T3[12-17],T4[18-23])
#2-按id，按小时进行分组统计
#转换为因子
data_login$id <- as.factor(data_login$id)
data_login$day <- as.factor(data_login$day)
data_login$only_hour <- as.factor(data_login$only_hour)
data_login$timeOfDay <- as.factor(data_login$timeOfDay)
#
data_login = data_login[order(data_login$id,data_login$timestamp),] #排序查看
#分组统计
library(dplyr)
#针对登录数据：按id，天进行分组统计，构造特征
struc_feature_login_by_idDay = function(data_login){
  #进行分组
  data_login_by_idDayHour <- data_login %>% group_by(id,day,timeOfDay)
  #最终的整理交易数据
  struc_data1 = summarise(data_login_by_idDayHour,   #------------
                          timestamp = mean(timestamp),       #时间戳（平均时间）
                          login_count = n(),                   #登录总次数
                          timelong_mean = round(mean(timelong),2),      #登录时长的平均
                          timelong_IQR = round(IQR(timelong),2),          #登录时长的四分位距（波动）
                          result_num = n_distinct(result),     #result出现的可能情况种类
                          city_num = n_distinct(city),         #city出现的可能情况种类
                          device_num = n_distinct(device),     #device出现的可能情况种类
                          ip_num = n_distinct(ip),             #ip出现的可能情况种类
                          exist_type1 = as.numeric(any(type==1)),          #是否存在type1
                          exist_type2 = as.numeric(any(type==2)),          #是否存在type1
                          exist_type3 = as.numeric(any(type==3))          #是否存在type1
  )     
  #--result!=1的数据集
  data_result_NOT_1 = filter(data_login_by_idDayHour, result != 1)
  struc_data2 = summarise(data_result_NOT_1, 
                          result_NOT_1_count = n(),              #不为1的result出现的总次数
                          result_NOT_1_num = n_distinct(result)  #不为1的result出现的可能情况种类
  )
  #--合并数据集(横向合并)
  struc_data = merge(struc_data1,struc_data2,by=c("id",'day','timeOfDay'),all =TRUE)
  #-对na位置进行填充
  struc_data[is.na(struc_data)] <- 0
  
  return(struc_data)
  
}
#-调用特征函数
login_by_idDay = struc_feature_login_by_idDay(data_login)
#--将时间戳改为登录平均时刻的时间戳
names(login_by_idDay)[4] = 'login_timestamp'    #重命名
#ll = data_login[order(data_login$id,data_login$timestamp),] #排序查看

#------------------------------------------------------分4个时间段的登录数据
login_T1 = login_by_idDay[which(login_by_idDay$timeOfDay=='T1'),]
login_T2 = login_by_idDay[which(login_by_idDay$timeOfDay=='T2'),]
login_T3 = login_by_idDay[which(login_by_idDay$timeOfDay=='T3'),]
login_T4 = login_by_idDay[which(login_by_idDay$timeOfDay=='T4'),]

T_temp1 = merge(login_T1,login_T2,by=c("id",'day'),all =TRUE, suffixes=c("_T1", "_T2"))
T_temp2 = merge(login_T3,login_T4,by=c("id",'day'),all =TRUE, suffixes=c("_T3", "_T4"))
login_T = merge(T_temp1,T_temp2,by=c("id",'day'),all =TRUE)    #1天4个时间段的登录数据


#去除T标识
login_T = login_T[,-c(3,17,31,45)]
login_T[is.na(login_T)] <- 0

#----------------------------------------------------


#unlist(strsplit(last_login$hour[1],split=" "))[1]
#str(last_login)

#-------------------------------------4.当前交易的上一次登录的前一个小时范围的所有登录
#-当前交易
index_trade = which(!is.na(login_trade3$trade_count))  #所有交易的index
#-最后一次登录数据
#1.先所有交易的前一次信息为登录，即使前面是交易也无所谓（交易的登录也是最近的填补的）
#2.寻找前1小时的登录
#--1.先所有交易的前一次信息为登录（交易的index，对应该次交易的所有信息）
last_login = data.frame(login_trade3_index = index_trade, login_trade3[index_trade-1,c('id','hour','log_id','ip')])
#---连接对应的平均登录信息
#-增加一个day变量
library(splitstackshape)  #kk=concat.split.multiple(last_login, "hour", " ")
last_login$'day' = concat.split.multiple(last_login, "hour", " ")$'hour_1'
#-按id与day连接每日的登录数据
#tradeId_loginM = merge(last_login,login_by_idDay,by=c("id",'day'),all =TRUE) #----
tradeId_loginM = merge(last_login,login_T,by=c("id",'day'),all =TRUE)
#------只取出有交易的数据
sum(complete.cases(tradeId_loginM))    #完整的数量就是交易数据的数量
tradeId_login_full = na.omit(tradeId_loginM)   #只保留完整数据
tradeId_login_full = tradeId_login_full[order(tradeId_login_full$login_trade3_index),] #排序



#--2.连接交易、登录数据
trade_full = login_trade3[tradeId_login_full$login_trade3_index, c('id','time','timestamp','trade_count')]
names(trade_full)[3] = 'trade_timestamp'    #重命名时间戳
#---------------------------------------

#--连接
#names(tradeId_login_full)
#names_use = c( "login_timestamp","login_count", "timelong_mean","timelong_IQR",
#               "result_num","city_num","device_num","ip_num","exist_type1","exist_type2","exist_type3",
#               "result_NOT_1_count","result_NOT_1_num")
trade_login_full_test_temp = cbind(trade_full,tradeId_login_full[,-c(1:5)])  #--------------

#------3.连接ip数据
data_login_ip = read.csv('login_by_IP_7.csv')
names(data_login_ip)[-1] <- paste0(names(data_login_ip)[-1],'_ip') #重命名

trade_login_full_test = merge(trade_login_full_test_temp, data_login_ip,
                         by=c("ip"),all.x =TRUE)


#write.csv(trade_login_full_test,'trade_login_full_test_T_ip.csv',row.names = FALSE)




