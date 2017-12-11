#排错程序-时间间隔

as.POSIXct(1420101863, origin="1970-01-01 00:00:00")

length(trade_login_full$time_interval<0)

ll= trade_login_full[which(trade_login_full$time_interval<0),]
l1=ll[c('id','time_interval')]
l1$'trade'=as.POSIXct(ll[,"trade_timestamp"], origin="1970-01-01 00:00:00")

l1$'login'=as.POSIXct(ll[,"login_timestamp"], origin="1970-01-01 00:00:00")


