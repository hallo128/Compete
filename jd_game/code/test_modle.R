library(rpart)

setwd('/Volumes/KINGSTON/京东比赛/data/Risk_Detection_Qualification/')

rpart.model = rpart(is_risk~., data = traindata, 
                    parms = list(loss = matrix(c(0,1,1000,0),2,2)),
                    #parms = list(loss = matrix(c(0,100,1,0),2,2)),
                    control = (cp=0.01333333))


library(ROSE)
#集成算法
library(adabag)
bagging.model = bagging(is_risk~., data = traindata)  
boosting.model = boosting(is_risk~., data = traindata) 
rf.model = randomForest(is_risk~., data = traindata)
#-----4.预测结果，并构建混淆矩阵，查看准确率
# 构建result，存放预测结果
result <- data.frame(arithmetic=c("bagging","boosting","随机森林"),
                     errTR=rep(0,3),errTE=rep(0,3),
                     TEerr_minor_yes = rep(0,3), TEerr_minor_no = rep(0,3))
for(i in 1:3){
  # 预测结果
  carTR_predict <- predict(switch(i,bagging.model,boosting.model,rf.model),
                           newdata=traindata) # 训练集数据
  carTE_predict <- predict(switch(i,bagging.model,boosting.model,rf.model),
                           newdata=testdata) # 测试集数据
  # 构建混淆矩阵
  tableTR <- table(actual=traindata$is_risk,
                   predict=switch(i, carTR_predict$class, carTR_predict$class, carTR_predict))
  tableTE <- table(actual=testdata$is_risk,
                   predict=switch(i, carTE_predict$class, carTE_predict$class, carTE_predict))
  # 计算误差率
  result[i,2] <- paste0(round((sum(tableTR)-sum(diag(tableTR)))*100/sum(tableTR),2),"%")
  result[i,3] <- paste0(round((sum(tableTE)-sum(diag(tableTE)))*100/sum(tableTE),2),"%")
  #召回率、准确率
  print(accuracy.meas(testdata$is_risk, switch(i, carTE_predict$class, carTE_predict$class, carTE_predict)))
  #--计算2个结果中的判别情况
  #-----针对测试集
  result[i,4] <- paste0(round(tableTE[2,1]*100/rowSums(tableTE)[2],2),"%")
  result[i,5] <- paste0(round(tableTE[1,2]*100/rowSums(tableTE)[1],2),"%")
}
# 查看结果
result


#-------
tableTE[2,1]*100/rowSums(tableTE)[2]
tableTE[1,2]*100/rowSums(tableTE)[1]

library(ROSE)
accuracy.meas(testdata$is_risk, carTE_predict)


sub_yes = which(testdata$is_risk ==1)
sub_no = which(testdata$is_risk ==0)
#少数类"yes"的错误率
sub_yes = which(testdata$is_risk ==1)
sub_no = which(testdata$is_risk ==0)
#少数类"yes"的错误率
err_minor_yes = sum(carTE_predict$class[sub_yes] != testdata$is_risk[sub_yes])/length(sub_yes)
err_minor_no = sum(carTE_predict$class[sub_no] != testdata$is_risk[sub_no])/length(sub_no)
err_minor_yes
err_minor_no

err_minor_yes = switch(i,
                       sum(carTE_predict$class[sub_yes] != testdata$is_risk[sub_yes])/length(sub_yes),
                       sum(carTE_predict$class[sub_yes] != testdata$is_risk[sub_yes])/length(sub_yes),
                       sum(carTE_predict[sub_yes] != testdata$is_risk[sub_yes])/length(sub_yes))
err_minor_no = switch(i,
                      sum(carTE_predict$class[sub_no] != testdata$is_risk[sub_no])/length(sub_no),
                      sum(carTE_predict$class[sub_no] != testdata$is_risk[sub_no])/length(sub_no),
                      sum(carTE_predict[sub_no] != testdata$is_risk[sub_no])/length(sub_no))
result[i,4] <- paste0(round(100*(err_minor_yes),2),"%")
result[i,5] <- paste0(round(100*(err_minor_no),2),"%")

#-----------------------------------------------


#针对随机森林（选择树的数目）
library(randomForest)

n = 500
NMSE = NMSE0 <- rep(0,n)
for(i in 1:n){
  a = randomForest(is_risk~., data = traindata, ntree = i)
  y0 = predict(a, traindata)
  y1 = predict(a, testdata)
  NMSE0[i] = sum(traindata[,'is_risk'] != y0)/nrow(traindata)
  NMSE[i] = sum(testdata[,'is_risk'] != y1)/nrow(testdata)
}
plot(1:n, NMSE, type = 'l', 
     ylim = c(min(NMSE,NMSE0),max(NMSE,NMSE0)),
     xlab = '树的数目', ylab = '误差率', lty=2)
lines(1:n, NMSE0)
legend('topright', lty = 1:2, c('训练集','测试集'))

names(rf.model)
rf.model$votes[1:30,]

rf.model$predicted[1:30]
plot(rf.model)

#k近邻算法



