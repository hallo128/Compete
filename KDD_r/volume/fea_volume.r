#构造平均流量特征矩阵

setwd("/Users/apple/code_tool/KDD_run/KDD_r/volume")

filename="training_split_avg_volume1"
data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  #-------
names(data)




#时间格式转换
#---------------------------------------------
#1将时间框time_windows取出开始时间，并转换为时间格式向量
time_windows2forma=function(time){
  t=as.character(time)
  weekday_start_time=NULL
  n=length(t)
  for (i in 1:n) {
    t1=unlist(strsplit(t[i],split=","))[1]
    t3=strptime(t1, "[%Y-%m-%d %H:%M:%S") 
    weekday_start_time[i]=format(t3, "%a")	
  }
  return(weekday_start_time)
}


#将单个星期转换为一周的0/1表示（周一——周日）
weekdays_row=function(x){
  row_x=rep(0,7)
  if(x == "一" )
    row_x[1]=1
  else if(x == "二" )
    row_x[2]=1
  else if(x == "三" )
    row_x[3]=1
  else if(x == "四" )
    row_x[4]=1
  else if(x == "五" )
    row_x[5]=1
  else if(x == "六" )
    row_x[6]=1
  else
    row_x[7]=1
  return(row_x)
}

#----1将时间框time_windows按样本转换为星期数据框(调用time_windows2forma和weekdays_row)
time_windows2weekdays_X=function(time){
  weekday_start_time=time_windows2forma(time)  #--time_windows2forma
  weekdays_X=NULL
  for (i in 1:length(weekday_start_time)) {
    l=weekdays_row(weekday_start_time[i])      #--weekdays_row
    weekdays_X=rbind(weekdays_X,l)
  }
  weekdays_X=as.data.frame(weekdays_X)
  names(weekdays_X)=c("一","二","三","四","五","六","日")
  return(weekdays_X)
}


weekdays_X=time_windows2weekdays_X(data$time_window)
data_weekdays_X=cbind(data,weekdays_X)         #--
#save(weekdays_X,file = 'data_weekdays_X.rda')
#load('data_weekdays_X.rda')
#---------------------------------------------------------------##############


#--------------------------天气插值函数insert_weather_X
#------函数1：2个数之间（包括第一个数）的9个指标值
insert_weather=function(x1,x2){
  x=NULL
  x[1:3]=x1
  x[4:7]=mean(c(x1,x2))
  x[8:9]=x2
  return(x)
}
#fea_insert=insert_weather(x1,x2)

#-------函数2：2个数之间（包括第一个数）的9个时间数
time_windows_h12=function(d1,d2,h1,h2){
  if(h1 == 21){h=21:23}
  if(h1 != 21){h=h1:(h2-1)}         #小时----0-23
  stable_h=c(":00:00",":20:00",":40:00")  
  stable=NULL
  for (i in 1:length(h)) {
    if(h[i]<10) {hl=paste0("0",h[i])}
    else{hl=h[i]}
    for (j in 1:2) {
      stable_i=paste0("[",d1," ",hl,stable_h[j],',',d1," ",hl,stable_h[j+1],")")
      stable = c(stable,stable_i)
    }
    top=as.numeric(h[i])+1
    if(top<10) {top=paste0("0",top)}
    if(h[i] == 23){stable_i=paste0("[",d1," ",hl,stable_h[3],',',d2," ","00",stable_h[1],")")}
    if(h[i] != 23){stable_i=paste0("[",d1," ",hl,stable_h[3],',',d1," ",top,stable_h[1],")")}
    stable = c(stable,stable_i)
  }
  return(stable[1:9])
}
#time_windows=time_windows_h12(d1,d2,h1,h2)

#最后调用的函数--天气插值函数insert_weather_X
insert_weather_X=function(dl){
  d_w=data.frame()
  for (j in 1:(nrow(dl)-1)) {
    d1=as.character(dl$date[j])
    d2=as.character(dl$date[j+1])
    stable_h=c(":00:00",":20:00",":40:00")
    h1=dl$hour[j]
    h2=dl$hour[j+1]
    #----针对2个指标数中插入9个数
    fea_name=c("pressure","sea_pressure","wind_direction",
               "wind_speed","temperature","rel_humidity","precipitation")
    dll=NULL
    for (i in 1:length(fea_name)) {
      x1=dl[fea_name[i]][[1]][j]
      x2=dl[fea_name[i]][[1]][j+1]
      fea_insert=insert_weather(x1,x2)
      dll=cbind(dll,fea_insert)
    }
    colnames(dll) <- fea_name
    time_windows=time_windows_h12(d1,d2,h1,h2)
    dll=cbind(time_windows,dll)
    #--
    d_w=rbind(d_w,dll)
  }
  return(d_w)
}

weather_X=insert_weather_X(data_weather)    #---最后天气的数据框

#--
vehicle_model_X=read.csv('/Users/apple/code_tool/KDD_run/KDD_r/volume/split_volume/data_vehicle_model.csv')
names(vehicle_model_X)[c(9,10,11,12)] <- c("start_month","start_day","start_hour","start_minute")
#--
vehicle_type_X=read.csv('/Users/apple/code_tool/KDD_run/KDD_r/volume/split_volume/data_vehicle_type.csv')
names(vehicle_type_X)[c(9,10,11,12)] <- c("start_month","start_day","start_hour","start_minute")
vehicle_model_X=vehicle_model_X[,2:12]
vehicle_type_X=vehicle_type_X[,2:12]
#--
setwd("/Users/apple/code_tool/KDD_run/KDD_r/volume")
save(weather_X,vehicle_model_X,vehicle_type_X,data_weekdays_X,file = 'data_fea_X.rda')     
load('data_fea_X.rda')


#---------------






#---------------------------------------------##############
#按车型取出数据，再用python进行计数

filename="volume(table 6)_training"
data2=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  #-------
names(data2)

d0=data2[which(data2$vehicle_model==0),]
d1=data2[which(data2$vehicle_model==1),]
d2=data2[which(data2$vehicle_model==2),]
nrow(d0)/nrow(data2)
nrow(d1)/nrow(data2)
nrow(d2)/nrow(data2)
(nrow(d0)+nrow(d1)+nrow(d2))/nrow(data2)

write.csv(d2,row.names = F,'volume_vehicle_model2_training.csv')

#---------------------------------------------##############



#---------------------------------------------##############
#按车型取出数据，再用python进行计数

filename="volume(table 6)_training"
data2=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  #-------
names(data2)

dt1=data2[which(is.na(data2$vehicle_type)),]
dt2=data2[which(data2$vehicle_type==0),]
dt3=data2[which(data2$vehicle_type==1),]
detc0=data2[which(data2$has_etc==0),]
detc1=data2[which(data2$has_etc==1),]
nrow(dt1)/nrow(data2)
nrow(dt2)/nrow(data2)
nrow(dt3)/nrow(data2)
nrow(detc0)/nrow(data2)
nrow(detc1)/nrow(data2)
(nrow(d0)+nrow(d1)+nrow(d2))/nrow(data2)

write.csv(dt1,row.names = F,'volume_vehicle_typeNa_training.csv')
write.csv(dt2,row.names = F,'volume_vehicle_type0_training.csv')
write.csv(dt3,row.names = F,'volume_vehicle_type1_training.csv')

write.csv(detc0,row.names = F,'volume_has_etc0_training.csv')
write.csv(detc1,row.names = F,'volume_has_etc1_training.csv')
#---------------------------------------------##############












filename="vehicle_split_vehicle_model0"
data_vehicle_model=read.csv(paste0('./split_volume/',filename,'.csv'))  #-------
names(data_vehicle_mode)


data$"vehicle_mode0"=0
time=as.character(data$time_window)
k=1
tollgate_id=1:3
i=1
direction=c(0,1)
j=1
d4=data[(which(data[,"tollgate_id"]==tollgate_id[i]) && 
        which(data[,"direction"]==direction[j]) &&
        which(data[,"time_window"]==time[k])),]
d2=d1[which(d1[,"direction"]==direction[j]),]
d3=d2[which(d2[,"time_window"]==time[k]),]



