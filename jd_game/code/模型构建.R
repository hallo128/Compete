#模型构建

#train_login_mean  #登录-训练数据集

#-----1.构建训练集与测试集
set.seed(1234)
library(caret)
ind = createDataPartition(train_login_mean$id_have_risk, times = 1, p=0.80, list = F)
traindata = train_login_mean[ind, ]  #训练集
testdata = train_login_mean[-ind, ]  #测试集

#------2.选择最优参数
control = trainControl(method = 'repeatedcv',number = 10, repeats = 3)   #10折交叉验证
rpart.model = train(id_have_risk~., data = train_login_mean, method = 'rpart', trControl = control)
rf.model = train(id_have_risk~., data = train_login_mean, method = 'rf', trControl = control)
nnet.model = train(id_have_risk~., data = train_login_mean, method = 'nnet', trControl = control)
rpart.model
rf.model
nnet.model

#-----3.建模
rpart.model = rpart::rpart(id_have_risk~., data = train_login_mean, control = (cp=0.01333333))
rf.model = randomForest::randomForest(id_have_risk~., data = train_login_mean, mtry=2)
nnet.model = nnet::nnet(id_have_risk~., data = train_login_mean, size=5, decay = 0.1)


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
  tableTR <- table(actual=traindata$id_have_risk,predict=carTR_predict)
  tableTE <- table(actual=testdata$id_have_risk,predict=carTE_predict)
  # 计算误差率
  result[i,2] <- paste0(round((sum(tableTR)-sum(diag(tableTR)))*100/sum(tableTR),
                              2),"%")
  result[i,3] <- paste0(round((sum(tableTE)-sum(diag(tableTE)))*100/sum(tableTE),
                              2),"%")
}
# 查看结果
result







#---------------------------随机森林预测结果导出
predict <- predict(rf.model,newdata=login_X_All,type="class") # 训练集数据

#predict
result_rf = data.frame(id = login_X_All$id, y=predict)

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

