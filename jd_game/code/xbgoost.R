
#xgboost算法
library('xgboost')
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

str(train)

# fit model
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "binary:logistic")
# predict
pred <- predict(bst, test$data)


traindata #训练集
testdata  #测试集

f1_metric = function(prob,dtrain){
  preds = as.integer((prob>0.05)*1)
  labels = getinfo(dtrain, 'label')
  recall = sum(preds==1 & labels ==1) / sum(labels)
  precise = sum(preds==1 & labels ==1) / sum(preds)
  f1 = 1.01 * (precise*recall) / (0.01*precise+recall)
  return(list(metric = 'f1', value = f1))
}

xgb <- xgboost(data = data.matrix(traindata[,-1]), 
               label = as.vector(as.numeric(traindata[,1]))-1,  
               max_depth = 2, eta = 1, nthread = 2, nrounds = 2, 
               eval_metric = f1_metric,
               objective = "binary:logistic")

y_pred <- predict(xgb, data.matrix(test_data))

length(y_pred)

table(ifelse(y_pred>0.9,1,0))

#----------
dtrain <- xgb.DMatrix(data.matrix(traindata[,-1]), label = as.list(as.numeric(traindata[,1])-1))
param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
              objective = "binary:logistic", eval_metric = f1_metric)
bst <- xgb.train(param, dtrain, nrounds = 2)


