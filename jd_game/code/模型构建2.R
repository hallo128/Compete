#模型构建2
setwd('/Volumes/KINGSTON/京东比赛/data/Risk_Detection_Qualification/')
#2017/11/25
#trade_login_full                #训练数据
#trade_login_full_test           #测试数据
trade_login_full = read.csv('trade_login_full2.csv')
trade_login_full_test = read.csv('trade_login_full_test2.csv')
#---待预测的数据
t_trade_test = read.csv('t_trade_test.csv')


#-------------------------------------------整理成可以处理的格式
#1.添加一列时间差
trade_login_full$'time_interval' = trade_login_full$trade_timestamp - trade_login_full$login_timestamp
trade_login_full_test$'time_interval' = trade_login_full_test$trade_timestamp - trade_login_full_test$login_timestamp
#2.只取能建模的数据
names_temp = c("is_risk","trade_count","time_interval","login_count","timelong_mean","timelong_IQR",
               "result_num","city_num","device_num","ip_num","exist_type1","exist_type2","exist_type3",
               "result_NOT_1_count","result_NOT_1_num")
train_data = trade_login_full[,names_temp]
test_data = trade_login_full_test[,names_temp[-1]]

#write.csv(train_data,'train_data1.csv',row.names = FALSE)
#write.csv(test_data,'test_data1.csv',row.names = FALSE)
#-------------------------------------------



#######_____---------------------数据平衡


#--------------------------------




#-----1.构建训练集与测试集
set.seed(1234)
library(caret)
ind = createDataPartition(train_data$is_risk, times = 1, p=0.80, list = F)
traindata = train_data[ind, ]  #训练集
testdata = train_data[-ind, ]  #测试集

#------2.选择最优参数
control = trainControl(method = 'repeatedcv',number = 10, repeats = 3)   #10折交叉验证
rpart.model = train(is_risk~., data = train_data, method = 'rpart', trControl = control)
rf.model = train(is_risk~., data = train_data, method = 'rf', trControl = control)
nnet.model = train(is_risk~., data = train_data, method = 'nnet', trControl = control)
rpart.model
rf.model
nnet.model

str(train_data)

#-----3.建模
library(rpart)
rpart.model = rpart::rpart(is_risk~., data = train_data, control = (cp=0.01333333))
rf.model = randomForest::randomForest(is_risk~., data = train_data, mtry=2)
nnet.model = nnet::nnet(is_risk~., data = train_data, size=5, decay = 0.1)


ll=predict(rpart.model,
        newdata=traindata,
        type="class")

#-----4.预测结果，并构建混淆矩阵，查看准确率
# 构建result，存放预测结果
result <- data.frame(arithmetic=c("决策树","随机森林","人工神经网络"),
                     errTR=rep(0,3),errTE=rep(0,3))
for(i in 1:3){
  # 预测结果
  carTR_predict <- predict(switch(i,rpart.model,rf.model,nnet.model),
                           newdata=traindata,
                           type="class") # 训练集数据
  carTE_predict <- predict(switch(i,rpart.model,rf.model,nnet.model),
                           newdata=testdata,
                           type="class") # 测试集数据
  # 构建混淆矩阵
  tableTR <- table(actual=traindata$is_risk,predict=carTR_predict)
  tableTE <- table(actual=testdata$is_risk,predict=carTE_predict)
  # 计算误差率
  result[i,2] <- paste0(round((sum(tableTR)-sum(diag(tableTR)))*100/sum(tableTR),
                              2),"%")
  result[i,3] <- paste0(round((sum(tableTE)-sum(diag(tableTE)))*100/sum(tableTE),
                              2),"%")
}
# 查看结果
result




#------------------将导出结果按id和小时进行匹配

kk = test_data
kk$'y' = 1:nrow(test_data)
kk$'id' = trade_login_full_test$'id'
kk$'hour' = strftime(trade_login_full_test$time, format = "%m-%d %H")        #hour为了使都变为登录对应的hour

#---待预测的数据
#t_trade_test = read.csv('t_trade_test.csv')
t_trade_test = t_trade_test[order(t_trade_test$id,t_trade_test$time),] #排序查看
t_trade_test$'hour' = strftime(t_trade_test$time, format = "%m-%d %H")        #hour为了使都变为登录对应的hour
#----进行预测数据的匹配
kkl = merge(t_trade_test,kk,by=c("id",'hour'),all =TRUE)

#t_trade_test$'y' = kk[(kk$'id'== t_trade_test$'id') & (kk$'hour' == t_trade_test$'hour'),'y']
#for(i in 1:nrow(t_trade_test)){
#  for(j in 1:nrow(kk)){
#    if((t_trade_test$'id'[i] == kk$'id'[j]) & ( t_trade_test$'hour'[i] == kk$'hour'[j])){
#      t_trade_test$'y'[i] = kk$y[j];break;
#    }
#  }
#}
#-------------------




#---------------------------随机森林预测结果导出
predict <- predict(rf.model,newdata=test_data,type="class")  # 测试数据

#predict
result_rf = data.frame(id = trade_login_full_test$'id',
                       hour = strftime(trade_login_full_test$time, format = "%m-%d %H"),
                       y=predict)

#write.csv(result_rf ,'result_rf.csv',row.names = FALSE)
all(result_rf$id %in% n_test_id)

#--------------------将预测结果id与最终数据进行匹配
nrow(data_trade_test)

ll <- merge(data_trade_test,result_rf ,by="id", all=TRUE)


#k =ll[!complete.cases(ll),]
#unique(k$id)
#length(unique(k$id))


#write.csv(ll,'submit_11_22.csv',row.names = FALSE)
#----------------

ll[is.na(ll)] <- 0
prop.table(table(ll$y))


#------------------导出格式匹配
kk = ll[c("rowkey","y" )]
kk$y <- as.numeric(as.character(kk$y))
k1=kk[order(kk$rowkey,decreasing=F),]

write.table(k1,'submit_11_23_1.csv',row.names = FALSE,col.names = F, sep = ',')
str(k1)

