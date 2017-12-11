#查看kdd个体、总体的趋势特征

setwd("/Users/apple/code_tool/KDD_run/KDD_r")

filename="training_20min_avg_travel_time"
data=read.csv(paste0('/Users/apple/code_tool/KDD_run/KDD_python/',filename,'.csv'))  #-------
names(data)
nrow(data)
data[1,]
data[1,"time_window"]

#------------stable为一天的72种组成方法
m='07'              #月----
d=19             #日----
h=NULL           #小时----0-23
for (i in 1:24) {
  if(i<11) 
    h[i]=paste0("0",i-1)
  else 
    h[i]=i-1
}
stable_h=c(":00:00",":20:00",":40:00")
stable=NULL
for (i in 1:2) {
  stable_i=paste0("[","2016-",m,"-",d," ",h,stable_h[i],',',"2016-",m,"-",d," ",h,stable_h[i+1],")")
  stable = c(stable,stable_i)
}

for (i in 1:23) {
  stable_i=paste0("[","2016-",m,"-",d," ",h[i],stable_h[3],',',"2016-",m,"-",d," ",h[i+1],stable_h[1],")")
  stable = c(stable,stable_i)
}
#当h=23时，需要这条语句
stable = c(stable,paste0("[","2016-",m,"-",d," ",'23:40:00,',"2016-",m,"-",d+1," ","00:00:00)"))
#---------------


#data[which(data$"time_window"==stable),]
#ff=data.frame()
#for (i in 1:72) {
#  ff=rbind(ff,data[which(data$"time_window"==stable[i]),])
#}

#-------------------按要求选数据
intersection_id='B'        #-------交叉口
tollgate_id=1             #------收费站
ff2=data.frame()
for (i in 1:72) {
  f_l=data[which(data$"time_window"==stable[i]),]      #确定的时间范围内的数据
  f_l1=f_l[which(f_l$"tollgate_id"==tollgate_id),]                  #-确定收费站的数据
  ff2=rbind(ff2, f_l1[which(f_l1$"intersection_id"==intersection_id),])    #-确定交叉口
}



ts1<-ts(ff2$"avg_travel_time", start=0, frequency=3)
#原始数据图
plot(ts1)

#-----------起始时间
start_time=NULL
ll=unlist(strsplit(as.character(ff2$time_window), split=","))
for (i in 1:nrow(ff2)) {
  start_time[i]=ll[2*i-1]
}







#------------stable为一天的组成方法
m='07'              #月----
d=19             #日----
h=c('08','09','17','18')           #小时----0-23
stable_h=c(":00:00",":20:00",":40:00")  
stable=NULL
for (i in 1:length(h)) {
  for (j in 1:2) {
    stable_i=paste0("[","2016-",m,"-",d," ",h[i],stable_h[j],',',"2016-",m,"-",d," ",h[i],stable_h[j+1],")")
    stable = c(stable,stable_i)
  }
  top=as.numeric(h[1])+1
  if(top<10) top=paste0("0",top)
  stable_i=paste0("[","2016-",m,"-",d," ",h[i],stable_h[3],',',"2016-",m,"-",d," ",top,stable_h[1],")")
  stable = c(stable,stable_i)
}

#-------------------------------------10/18-10/24的时间窗口
m='10'              #月----
d=25:31             #日----
h=c('08','09','17','18')           #小时----0-23
stable_h=c(":00:00",":20:00",":40:00")  
stable=NULL

for (k in 1:length(d)) {
  for (i in 1:length(h)) {
    for (j in 1:2) {
      stable_i=paste0("[","2016-",m,"-",d[k]," ",h[i],stable_h[j],',',"2016-",m,"-",d[k]," ",h[i],stable_h[j+1],")")
      stable = c(stable,stable_i)
    }
    top=as.numeric(h[i])+1
    if(top<10) top=paste0("0",top)
    stable_i=paste0("[","2016-",m,"-",d[k]," ",h[i],stable_h[3],',',"2016-",m,"-",d[k]," ",top,stable_h[1],")")
    stable = c(stable,stable_i)
  }
}

write.table(as.vector(stable),'25-31time_wondiow.txt',row.names = F,
          col.names = F)
#---------------





