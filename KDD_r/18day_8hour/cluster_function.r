#聚类-判别所需要的函数
#一开始就会给定start_hour为c(6,7,8,9)或者c(15,16,17,18)

#1---任意指定data数据，选择特定小时数据（train和test的数据都适合）
#每次改变data,start_month,start_day（可以是17:28）
select_hour_volume=function(data,start_month,start_day,start_hour){
  select_data=data.frame()
  select=data[which(data$"start_month"==start_month),]
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

#2----回归(求b)   #只针对某1天求（train和test的数据都适合
#对于train数据，第一段用前7个数据
#对于test数据，第一段用前6个数据
b_route_volume=function(route_volume){
  if(start_hour[1]==6) {x=c(6,6+2/6,6+4/6,7,7+2/6,7+4/6,8,8+2/6,8+4/6,9,9+2/6,9+4/6)+1/6}
  if(start_hour[1]==15) {x=c(15,15+2/6,15+4/6,16,16+2/6,16+4/6,17,17+2/6,17+4/6,18,18+2/6,18+4/6)+1/6}
  {if(length(route_volume)==12)
    #train数据
    {x1=x[1:7]
    x2=x[8:12]
    lm1=lm(route_volume[1:7]~x1)
    lm2=lm(route_volume[8:12]~x2)
    l=list("b1"=lm1$coefficients[2],"b2"=lm2$coefficients[2])}
  else
    #test数据
      {x1=x[1:6]
      lm1=lm(route_volume[1:6]~x1)
      lm.pred = predict(lm1,data.frame(x1=x[7]),interval='prediction',level=0.95) #个体单值预测
      #lm.pred[1],向后一格的预测
      l=list("b1"=lm1$coefficients[2],"v2_1st"=lm.pred[1])}
  }
  return(l)
}

#3----幅度函数（train和test的数据都适合
range_route_volume=function(route_volume){
  x1=route_volume[1:6]
  {if(length(route_volume)==12)
  {x2=route_volume[7:12]
  l=list("range1"=x1[6]-x1[1],"range2"=x2[6]-x2[1])}
    else
      l=list("range1"=x1[6]-x1[1])
  }
  return(l)
}

#4----最值函数（train和test的数据都适合
top_route_volume=function(route_volume){
  if(start_hour[1]==6) {
    x1=route_volume[1:6]
    {if(length(route_volume)==12)
      l=list("top1"=max(x1),"top"=max(route_volume))
      else
        l=list("top1"=max(x1))
    }
  }
  if(start_hour[1]==15) {
    x1=route_volume[1:6]
    {if(length(route_volume)==12)
      l=list("top1"=min(x1),"top"=min(route_volume))
    else
      l=list("top1"=min(x1))
    }
  }
  return(l)
}

#5----最高点位置函数（4个小时-12个数据，只针对train，也就说test数据不存在）
top_point_volume=function(route_volume){
  if(start_hour[1]==6){
    x1=route_volume[1:12]
    top_point=which.max(x1)
  }
  if(start_hour[1]==15){
    x1=route_volume[1:12]
    top_point=which.min(x1)
  }
  return(top_point)
}

#-----------------------构造聚类的特征
#start_hour为c(6,7,8,9)或者c(15,16,17,18)，必须是12个数据;
#只对trian 
distinct_route_volume_month=function(data,start_month,start_day,start_hour){
  b1_v=NULL
  b2_v=NULL
  range1_v=NULL
  range2_v=NULL
  top1_v=NULL
  top_v=NULL
  top_point_v=NULL
  v2_1st=NULL   #----
  for (i in 1:length(start_day)) {
    select_hour_volume1=select_hour_volume(data,start_month,start_day[i],start_hour)
    route_volume=select_hour_volume1$volume
    b_route_volume1=b_route_volume(route_volume)
    b1_v[i]=b_route_volume1$b1
    b2_v[i]=b_route_volume1$b2
    range_volume1=range_route_volume(route_volume)
    range1_v[i]=range_volume1$range1
    range2_v[i]=range_volume1$range2
    top_route_volume1=top_route_volume(route_volume)
    top1_v[i]=top_route_volume1$top1
    top_v[i]=top_route_volume1$top
    top_point_v[i]=top_point_volume(route_volume)
    #----
    v2_1st[i]=select_hour_volume(data,start_month,start_day[i],start_hour[length(start_hour)]+1)$volume[1]
    
  }
  distinct_route_volume1=data.frame(tollgate_id,direction,start_month,start_day,
                                    b1_v,b2_v,range1_v,range2_v,top1_v,top_v,top_point_v,v2_1st)
}

#6----选取特定路线的函数（收费站-方向）对test数据先分路线
lda_data_route=function(lda_data,tollgate_id,direction){
  lda_data_route=data.frame()
  f_l1=lda_data[which(lda_data$"tollgate_id"==tollgate_id),]                  #-确定收费站的数据
  lda_data_route=rbind(lda_data_route, f_l1[which(f_l1$"direction"==direction),])    #-确定交叉口
}

#-----------------------构造聚类的特征（只对test 
test_distinct_route_volume_month=function(data,start_month,start_day,start_hour){
  b1_v=NULL
  range1_v=NULL
  top1_v=NULL
  v2_1st=NULL
  for (i in 1:length(start_day)) {
    select_hour_volume1=select_hour_volume(data,start_month,start_day[i],start_hour)
    route_volume=select_hour_volume1$volume
    b_route_volume1=b_route_volume(route_volume)
    b1_v[i]=b_route_volume1$b1
    v2_1st[i]=b_route_volume1$v2_1st
    range_volume1=range_route_volume(route_volume)
    range1_v[i]=range_volume1$range1
    top_route_volume1=top_route_volume(route_volume)
    top1_v[i]=top_route_volume1$top1
  }
  distinct_route_volume1=data.frame(tollgate_id,direction,start_month,start_day,
                                    b1_v,range1_v,top1_v,v2_1st)
}


#-----------------------选择最优类别数
#<=4或者>=0.85
select_k=function(d_k){
  result=rep(0,nrow(d_k)-1)
  for (k in 1:length(result)) {
    fit_km=kmeans(d_k,centers=k)
    result[k]=fit_km$betweenss/fit_km$totss
  }
  k=4
  for (i in 1:4) {
    if(round(result,2)[i]>=0.85){ k=i;break;}
  }
  return(k)
}


#7----------
##--预测函数(给定部分的回归预测下一段的第1个点，用分类后平均的斜率作为下一段的斜率；
#如果平均最高位置的预测值<平均最高点值，则如果平均最高位置的预测值=平均最高点值)
#给定聚类与判别结果
pre_cluster_1_0_10=function(fit_km1,pre_lda1,test_route_volume){
  pre_v2=data.frame()
  if(start_hour[1]==6) {x=c(6,6+2/6,6+4/6,7,7+2/6,7+4/6,8,8+2/6,8+4/6,9,9+2/6,9+4/6)+1/6}
  if(start_hour[1]==15) {x=c(15,15+2/6,15+4/6,16,16+2/6,16+4/6,17,17+2/6,17+4/6,18,18+2/6,18+4/6)+1/6}
  for (i in 1:length(pre_lda1$class)) {   #待预测的总数n
    cluster_attribute=fit_km1$centers[pre_lda1$class[i],]#对应分类的特征
    x0=x[7]
    y0=test_route_volume$v2_1st[i]
    x2=x[8:12]
    y=(cluster_attribute['b2_v'])*(x2-x0)+y0
    y=c(y0,y)
    #y[9.5]自动默认向下取整
    if(start_hour[1]==6)
    {#如果最值点在前6个点（为已知点），那就不用进行最值特别赋值
    if((y[(cluster_attribute['top_point_v']-6)]<as.numeric(cluster_attribute['top_v']))
        && (cluster_attribute['top_point_v']>6)) 
      y[(cluster_attribute['top_point_v']-6)]=as.numeric(cluster_attribute['top_v'])}
    if(start_hour[1]==15)
    {if((y[(cluster_attribute['top_point_v']-6)]>as.numeric(cluster_attribute['top_v']))
        && (cluster_attribute['top_point_v']>6)) 
      y[(cluster_attribute['top_point_v']-6)]=as.numeric(cluster_attribute['top_v'])}
    
    #pre_v2=rbind(pre_v2,c(y0,y))
    pre_v2=rbind(pre_v2,y)
  }
  names(pre_v2)<-as.character(round(x[7:12],2))
  return(pre_v2)
}

