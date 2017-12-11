#最优数据的原始查看
#2017/11/30
#setwd('/Volumes/KINGSTON/京东比赛/data/Risk_Detection_Qualification/')
setwd('E:/京东比赛/数据/Risk_Detection_Qualification/')

submit_best = read.csv('submit_11_29_2.csv',header = F)
t_trade_test = read.csv('t_trade_test.csv') #---待预测的数据
#table(submit_best$V2)

#--合并待预测数据
submit_best_trade = t_trade_test
submit_best_trade$'is_risk' = paste0('p_',submit_best[,2])


#只取出风险为1的进一步查看
submit_best_trade_1 = submit_best_trade[which(submit_best_trade$is_risk == 'p_1'),]
names(submit_best_trade_1)[2] <- 'trade_time' #重命名
id_have_risk = unique(submit_best_trade_1$id)

#----符合要求的登录数据
t_login = read.csv('t_login.csv')
t_login_test = read.csv('t_login_test.csv')
t_trade = read.csv('t_trade.csv')
t_trade_test = read.csv('t_trade_test.csv')

data_login = rbind(t_login, t_login_test)
data_trade = rbind(t_trade, submit_best_trade)
#table(data_trade$is_risk)

#匹配
dtemp1 = data_login[which(data_login$id %in% id_have_risk),-1]
#names(dtemp1)
dtemp2 = data_trade[which(data_trade$id %in% id_have_risk),]
#names(dtemp2)
table(dtemp2$is_risk)


ll = merge(dtemp1,dtemp2,by=c("id",'time'),all =TRUE)
table(ll$is_risk)
ll$'timestamp'=as.numeric(as.POSIXct(ll$time, origin="1970-01-01 00:00:00"))


k1 = ll[order(ll$id,ll$timestamp),] #排序查看

#write.csv(k1,'54个预测1的所有登录与交易信息.csv',row.names = FALSE)

