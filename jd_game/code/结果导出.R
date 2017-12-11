#结果导出

setwd('/Volumes/KINGSTON/京东比赛/data/Risk_Detection_Qualification/')
#setwd('E:/京东比赛/数据/Risk_Detection_Qualification/')

result = read.csv('trade_login_full_test_T_risk.csv')
trade_login_full_test = read.csv('trade_login_full_test3.csv')
#trade_login_full_test = read.csv('trade_login_full_test_T_ip.csv')


prop.table(table(result$X.C.is_risk))
summary(result$X.CC.is_risk)

y=ifelse(result$X.CC.is_risk>0.99 & result$X.C.is_risk==1,1,0)
table(y)

y=ifelse(predict[,2]>0.99,1,0)
summary(y)
table(y)

result_temp = trade_login_full_test[which(y==1),]

result_rf = data.frame(id = trade_login_full_test$id,
                       #y=result$X.C.is_risk,
                       y=y,
                       hour=strftime(trade_login_full_test$time, format = "%m-%d %H")) #hour为了使都变为登录对应的hour

#---待预测的数据
t_trade_test = read.csv('t_trade_test.csv')
t_trade_test = t_trade_test[order(t_trade_test$id,t_trade_test$time),] #排序查看
t_trade_test$'hour' = strftime(t_trade_test$time, format = "%m-%d %H")        #hour为了使都变为登录对应的hour
#----进行预测数据的匹配
ll = merge(t_trade_test,result_rf,by=c("id",'hour'),all =TRUE)

n_test_id=unique(t_trade_test$id)
all(ll$id %in% n_test_id)

#any(is.na(ll))
ll[is.na(ll)] <- 0
prop.table(table(ll$y))


#------------------导出格式匹配
kk = ll[c("rowkey","y" )]
kk$y <- as.numeric(as.character(kk$y))
k1=kk[order(kk$rowkey,decreasing=F),]

#write.table(k1,'submit_12_5_1.csv',row.names = FALSE,col.names = F, sep = ',')
str(k1)

