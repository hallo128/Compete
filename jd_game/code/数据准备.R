
setwd('E:/京东比赛/数据/Risk_Detection_Qualification/')

data_trade_test = read.csv('t_trade_test.csv')

n_test_id = sort(unique(data_trade_test$'id'))  #待预测数据的所有id号

#登录数据的所有id号   n_d

all(n_test_id %in% n_id)  #训练数据中并没有包含所有预测的id

#----------------------------找到所有待预测id的登录数据(训练+测试)
train_login = read.csv('t_login.csv')
test_login = read.csv('t_login_test.csv')


#-------------------构建针对测试id的数据集
#train_login_mean   #登录-训练数据集
#test_login_mean   #登录-测试数据集
names(train_login_mean)


#----id在测试集中的数据
test_temp= test_login_mean[test_login_mean$id  %in% n_test_id,] 
#---id不在7月份的训练集数据
n_train_id = setdiff(n_test_id, unique(test_temp$id))
train_temp= train_login_mean_All[train_login_mean_All$id %in% n_train_id,names(test_temp)]
#---最终的预测集
login_X_All = rbind(train_temp, test_temp)

#test = login_X_All[login_X_All$id %in% n_test_id, ]       #用于建模的测试集


#
length(unique(test$id))
all(login_X_All$id %in% n_test_id) 

