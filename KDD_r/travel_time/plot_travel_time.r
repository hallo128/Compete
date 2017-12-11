#plot_travel_time

setwd("/Users/apple/code_tool/KDD_run/KDD_r/travel_time")


data=read.csv('/Users/apple/code_tool/KDD_run/KDD_python/trajectories(table 5)_training.csv')  
data1=data

#summary(data)
n=nrow(data)

#data$travel_seq[1:5]
#data$starting_time[1:5]

#------------------------------时间分割
month=NULL
day=NULL
hour=NULL
minute=NULL
for (i in 1:n) {
  bbb<- unlist(strsplit(as.character(data$starting_time[i]),split=" ")) 
  month[i]=unlist(strsplit(as.character(bbb[1]),split="-"))[2]
  day[i]=unlist(strsplit(as.character(bbb[1]),split="-"))[3]
  hour[i]=unlist(strsplit(as.character(bbb[2]),split=":"))[1]
  minute[i]=unlist(strsplit(as.character(bbb[2]),split=":"))[2]
}

#---------------------------------将分割时间添加到最后列
d1=cbind(month,day)
d2=cbind(d1,hour)
d3=cbind(d2,minute)
data=cbind(data,d3)
names(data) <- c("intersection_id","tollgate_id","vehicle_id","starting_time", 
                 "travel_seq","travel_time",
                 "start_month","start_day","start_hour","start_minute")
#---------------------



#--------------------------每条路线分为2个时间段来建模
intersection_id='A'
tollgate_id=3              #-------收费站（123）
links=c(110,123,107,108,119,114,118,122)
length_link=c(109,59,34,40,9,198,206,197)

#------------------------------------------------到天
select_way_time=function(data,tollgate_id,direction){
  select_data=data.frame()
  select2=data[which(data$"tollgate_id"==tollgate_id),]              
  #-确定收费站的数据
  select3=select2[which(select2$intersection_id==intersection_id),]              
  #-确定交叉路口
  select_data=rbind(select_data,select3) 
}

#------时间为月、日，各个收费站的选取数据的函数
select_hour_time=function(data,start_month,start_day,start_hour,tollgate_id,intersection_id){
  select_data=data.frame()
  #--确定路线
  d1=data[which(data$"tollgate_id"==tollgate_id),] #-确定收费站的数据             
  d2=d1[which(d1$intersection_id==intersection_id),]  #-确定交叉路口
  #月
  select=d2[which(d2$"start_month"==start_month),]
  #天
  select1=data.frame()
  for (i in 1:length(start_day)) {
    select_l=select[which(select$"start_day"==start_day[i]),]
    select1=rbind(select1,select_l)
  }
  #小时
  select2=data.frame()
  for (i in 1:length(start_hour)) {
    select_l=select1[which(select1$"start_hour"==start_hour[i]),]
    select2=rbind(select2,select_l)
  }
  #确定的时间范围内的数据
  select_data=rbind(select_data,select2) 
}

select_day_time=function(data,start_month,start_day,tollgate_id,intersection_id){
  select_data=data.frame()
  select1=data[which(data$"start_month"==start_month & data$"start_day"==start_day),]
  #确定的时间范围内的数据
  select2=select1[which(select1$"tollgate_id"==tollgate_id),]              
  #-确定收费站的数据
  select3=select2[which(select2$intersection_id==intersection_id),]              
  #-确定交叉路口
  select_data=rbind(select_data,select3) 
}

#-----将输入数字不满2位的进行补零函数
paste0_h=function(h){
  for (i in 1:length(h)) {
    if(h[i]<10) h[i] <- paste0("0",h[i])
  }
  return(h)
}
#-------
#paste0_h(1:20)


start_month='08'    #08
start_day=c(17,18)  #17,18  '01','02'
select_day_time1=select_day_time(data,start_month,start_day,tollgate_id,intersection_id)

#------link速度的函数
#每次针对一个travel_seq
f_link_v=function(length_link,links,travel_seq){
  #-------link分割时间
  link_time=NULL
  link_a<- unlist(strsplit(as.character(travel_seq),split=";")) 
  link_a
  for (j in 1:length(links)) {
    link_time[j]=unlist(strsplit(as.character(link_a[j]),split="#"))[3]
  }
  link_time=as.numeric(link_time)
  #------link速度
  link_v=NULL
  link_v=length_link/link_time
  return(link_v)
}

#link_v1=f_link_v(length_link,links,select_day_volume1$travel_seq[1])

#---------------------------计算每行的link速度并将添加到最后列select_day_time1
d_links=NULL
for (i in 1:nrow(select_day_time1)) {
  link_v1=f_link_v(length_link,links,select_day_time1$travel_seq[i])
  d_links=rbind(d_links,link_v1)
}
d_links=as.data.frame(d_links)
names(d_links)<-as.character(links)

d_links_a=cbind(select_day_time1,d_links)
#数据排序归类
d_links_a=d_links_a[order(d_links_a$start_day,d_links_a$start_hour,d_links_a$start_minute),]      

#---------------选时间

start_hour='14'
select1=d_links_a[which(d_links_a$"start_hour"==start_hour),]

plot(as.numeric(select1[5,as.character(links)]))



plot(d_links[1,])



plot(as.numeric(d_links[,2]))

plot(as.numeric(d_links[,8]))
#------------------------------







#---------------查看每天不同时刻的车辆数
day='02' #--
select1_start_hour=d_links_a[which(d_links_a$"start_day"==day),]$start_hour #--
table(select1_start_hour)
plot(select1_start_hour)
par(family='STKaiti')
title(paste0('流量',start_month,'-',day))
#---------------

#-----------------检查数据是否存在缺失
sum(complete.cases(d_links_a))
sum(!complete.cases(d_links_a))
dl=d_links_a[!complete.cases(d_links_a),]
#write.table(dl,'缺失数据.txt')

#-------查看缺失的时间
travel_seq=data1[which(data1$vehicle_id==dl$vehicle_id[2]),]$travel_seq  #123-
link_time=NULL
link_a<- unlist(strsplit(as.character(travel_seq),split=";"))  #--
link_a
for (j in 1:length(link_a)) {
  link_time[j]=unlist(strsplit(as.character(link_a[j]),split="#"))[3]
}
link_time=as.numeric(link_time)
sum(link_time)
#-
data1[which(data1$vehicle_id==dl$vehicle_id[2]),]$travel_time   #123-
#-------

d_links_full=d_links_a[complete.cases(d_links_a),]    #完整数据
#--
link_a=unlist(strsplit(as.character(d_links_full$travel_seq[1]),split=";")) #--
link_time=NULL
for (j in 1:length(link_a)) {
  link_time[j]=unlist(strsplit(as.character(link_a[j]),split="#"))[3]
}
link_time=as.numeric(link_time)
sum(link_time)
#--





travel_time=d_links_full[,"travel_time"]
#异常值识别 
dotchart(travel_time)  #单变量散点图
sp=boxplot(travel_time)
sp$out
#--异常点
#--剔除异常点
#异常数据处理（sub-异常值所在的行）
sub=NULL
for (i in 1:length(sp$out)) {
  sub[i] = which(d_links_full$travel_time==sp$out[i])
}

d_links_normal=d_links_full[-sub,]    #剔除异常数据
d_outlines=d_links_full[sub,]         #异常数据
#--




#--聚类分析
x_name=c("travel_time",links)
#--------------------------------k均值聚类
#--确定K后
result=rep(0,10)
for (k in 1:10) {
  fit_km=kmeans(d_links_normal[,x_name],centers=k)
  result[k]=fit_km$betweenss/fit_km$totss
}
plot(result,type = 'o',xlab = 'k')
#--
fit_km1=kmeans(d_links_normal[,x_name],centers =4)   #k
#print(fit_km1)
fit_km1$betweenss/fit_km1$totss
fit_km1$centers                  #每一类的平均值
number_cluster=fit_km1$cluster   #分类排序
table(number_cluster)
d_links_cluster=cbind(d_links_normal,number_cluster)      #------------聚类后的分类结果
#--------------------------------

center_fit10=fit_km1

center_fit8=fit_km1

list('10_center'=center_fit10$centers,'8_center'=center_fit8$centers )
list('10_cluster'=table(center_fit10$cluster),'8_cluster'=table(center_fit8$cluster) )


#--------------------------------k中心点聚类
library(mclust)
fit_EM=Mclust(d_links_normal[,x_name])
summary(fit_EM)
plot(fit_EM)


#--------------------------------平均旅行时间的描述统计
library(fBasics)
d1=basicStats(d_links_full$travel_time)
d2=basicStats(d_links_normal$travel_time)
cbind(d1,d2)








