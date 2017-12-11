

setwd("/Users/apple/code_tool/KDD_run/KDD_r/weather")

data_weather=read.csv('weather (table 7)_training_update.csv')  

#选用指标
fea=c("pressure","sea_pressure","wind_speed","temperature","rel_humidity","precipitation")

date=as.character(unique(data_weather$date))      #所有包含的日期
#------按天平均的天气
mean_day_weather=NULL                   
for (i in 1:length(date)) {
  d_temp=data_weather[which(data_weather$date==date[i]),]
  mean_day_weather=rbind(mean_day_weather,round(apply(d_temp[,fea], 2,mean)))
}
mean_day_weather=as.data.frame(cbind(date,mean_day_weather))
mean_day_weather
#------


#一共108天
summary(mean_day_weather)
names(mean_day_weather)
index=7             #------选择查看指标
x=as.numeric(as.character(mean_day_weather[,index]))
summary(x)
hist(x,xlab =names(mean_day_weather)[index])

#R语言之拉格朗日多项式插值
LagrangePolynomial <- function(x,y) {
  len = length(x)
  if(len != length(y))
    stop("length not equal!")
  
  if(len < 2)
    stop("dim size must more than 1")
  
  #pretreat data abd alloc memery
  xx <- paste("(","a -",x,")")
  m <- c(rep(0,len))
  
  #combin express
  for(i in 1:len) {
    td <- 1
    tm <- "1"
    for(j in 1:len) {
      if(i != j) {
        td <- td*(x[i] - x[j])
        tm <- paste(tm,"*",xx[j])
      }
    }
    tm <- paste(tm,"/",td)
    m[i]<-tm #m[i] <- parse(text=tm)
  }
  
  #combin the exrpession
  m <- paste(m,"*",y)
  r <- paste(m,collapse="+")
  
  #combin the function
  fbody <- paste("{ return(",r,")}")
  f <- function(a) {}
  
  #fill the function's body
  body(f) <- parse(text=fbody)
  
  return(f)
}
a = 4:6 
b = c(10, 5.25, 1) 
f <- LagrangePolynomial(a,b) 
f(4.5) 
plot(a,b)



