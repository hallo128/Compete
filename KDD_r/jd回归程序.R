#jd回归程序

setwd("F:/python/京东数据爬虫")

data=read.csv('手机数据.csv')


#---------------删除不完整行
p=16
n=nrow(data)
n_rm=NULL
if(ncol(data)!=16){
  for(i in 1:n){
    if (!(is.na(data[i,17]))) n_rm=append(n_rm,i)
    if (is.na(data[i,16])) n_rm=append(n_rm,i)
  }
}

data1=data[-n_rm,][,1:p]  #统一为16个属性
data3=data1    #保留初步读入的数据


#数据约规
source('F:/python/京东数据爬虫/code/数据约规.R', encoding = 'UTF-8')



#--------------剔除缺失
#检查没有缺失的个数
sum(complete.cases(data1))

#------缺失数据
data_NA=data1[!complete.cases(data1),]   #缺失-机身内存
#data_NA=rbind(data_NA,data1[3,])        #缺失-分辨率
#--机身内存，用同等价位的手机填补，或者用运行内存与机身内存的关系填补


#----完整的数据
data2=data1[complete.cases(data1),]#[-3,]     


#---重命名
names(data2)<-c("id","brand","version","year","CPU","ROM","RAM",
                "screen_size","pixel","b_pixel","f_pixel","b_capacity",
                "volume","weigth","price","evaluate","sys")
data_use=data2[c("sys","CPU","ROM","RAM","pixel","b_pixel","f_pixel","b_capacity",
                 "evaluate","year","brand",
                 "screen_size","volume","weigth",
                 "price")]


#----------------ROM(128-7)-(ROM)
#(128-7)
ROM=data_use$ROM
data_use$ROM=log2(ROM) 
#----------------RAM
#----------------pixel
pixel=data_use$pixel
data_use$pixel=pixel/10000 

#----------------volume
volume=data_use$volume
data_use$volume=volume/10000 
#----------------------数据约规结束




#------------------------------分训练集与测试集
#sample(c(1:111),21)
#which(data_use$sys=='ios')
set.seed(1)
s1=sample(which(data_use$sys=='ios'),2)   #ios抽2个,安卓19个
set.seed(2)
s2=sample(c(c(1:55),c(64:111)),19)
s=c(s1,s2)
data_use2=data_use[s,]     #测试集
data_use1=data_use[-s,]    #训练集



#数值型变量
data_numeric=data_use1[c("ROM","RAM",'pixel',"b_pixel","f_pixel","b_capacity",
                        "evaluate","year",
                        "screen_size","volume","weigth",
                        "price")]    #只有数值型的数据
str(data_numeric)
cor(data_numeric)

str(data)
cor(data)


#price
#数据正态检验
EDA=function(x){
  par(mfrow=c(2,2))				#同时做4个图
  hist(x);								#直方图
  plot(x);							#点图
  boxplot(x,horizontal=T);			#箱型图
  qqnorm(x);qqline(x);				#QQ图
  par(mfrow=c(1,1))
}#EDA(x)
price=data_numeric$price
y=sort(log(price))
y=sort(price)
EDA(y)
table(y)






#回归数据检查
str(data_numeric)
cor(data_numeric)



#lm1
lm1=lm(log(price)~.,data_numeric)
slm1=summary(lm1)
slm1

cor(data_numeric$ROM,data_numeric$RAM)
cor(data_numeric$b_pixel,data_numeric$f_pixel)
EDA(data_numeric$b_pixel/100)


#---------------查看系数的函数
print_coeff=function(slm){
  options(digits = 4)
  for(i in 1:length(slm$coefficients[,1])){
    print(names(slm$coefficients[,1])[i])
    print(slm$coefficients[i,1])
    print(NULL)
  }
}

print_coeff(slm1)


#逐步回归
lm.step1=step(lm1,direction = 'both')
lm.step1
summary(lm.step1)


#b/f_pixel
#bf_pixel=(data_numeric$b_pixel+data_numeric$f_pixel)/100
#EDA(bf_pixel)


#lm2
lm2=lm(log(price)~ROM +  pixel+ b_pixel+ f_pixel + b_capacity + 
        year +  volume + weigth,data_numeric)
slm2=summary(lm2)
slm2

print_coeff(slm2)



#lm3(sys名义变量)
data_s=with(data_numeric,
            data.frame('sys'=data_use1$sys,ROM ,pixel,b_pixel,f_pixel,b_capacity, 
                       year,volume,weigth,price))
lm3=lm(log(price)~sys+ROM +  pixel + b_pixel+ f_pixel + b_capacity + 
         year +  volume + weigth,data_s)
slm3=summary(lm3)
slm3

print_coeff(slm3)

cor(data.frame(b=data_numeric$b_pixel,f=data_numeric$f_pixel,bf=bf_pixel,p=data_numeric$pixel))
#逐步回归
lm.step3=step(lm3,direction = 'both')
lm4=lm.step3
slm4=summary(lm.step3)

print_coeff(summary(lm.step3))


#回归检验
res=lm4$residuals
r=rstandard(lm4)
fit=fitted(lm4)
plot(res)
plot(res,fit)
plot(rstandard(lm4),type="o")
#plot(r~fit,main="预测值与标准化残差的散点图")


#data_apple=data_numeric[which(sys=='ios'),]


library(car)
#------残差图
par(mfrow=c(1,2))
plot(r~fit,main="预测值与标准化残差的散点图")
qqPlot(lm4,ylab = '标准化残差r',main = '标准化残差Q-Q图')
par(mfrow=c(1,1))


durbinWatsonTest(lm4)      #DW检验
vif(lm4)                   #共线性
#crPlots(lm3) 
ncvTest(lm3)         #计分检验 H0:误差方差不变
#spreadLevelPlot(lm3)
influencePlot(lm3,main='杠杆-残差图')          #强影响点/异常点



#-----------------去除异常点
data_new=data_s[-c(51,53),]
id_spe=c(51,53)
data_spe=data_use[id_spe,]


lm4=lm(log(price)~sys+ROM +  pixel + b_pixel+ f_pixel + 
         year +  volume + weigth,data_new)
slm4=summary(lm4)
slm4

print_coeff(slm4)

#回归检验
res=lm4$residuals
r=rstandard(lm4)
fit=fitted(lm4)

#------残差图
par(mfrow=c(1,2))
plot(r~fit,main="预测值与标准化残差的散点图")
qqPlot(lm4,ylab = '标准化残差r',main = '标准化残差Q-Q图')
par(mfrow=c(1,1))


durbinWatsonTest(lm4)      #DW检验(序列相关)
#Box.test(res,lag=1)

vif(lm4)                   #共线性
#crPlots(lm3) 
ncvTest(lm4)         #计分检验 H0:误差方差不变
#spreadLevelPlot(lm3)
influencePlot(lm4)          #强影响点/异常点





#--------------------预测测试集(data_use2)



#预测集变量
x=data_use2[c('sys',"ROM",'pixel',"b_pixel","f_pixel",
              "year",
              "volume","weigth")]

lm.pred = predict(lm4,x,interval='prediction',level=0.95) #个体单值预测
lm.pred
p_price=exp(lm.pred[,1])    #预测值
price=data_use2$price
e=(price-p_price)/price
compose=data.frame('预测值'=p_price,'真实值'=price,'相对误差'=e)
compose

x=c(1000:7000)
y=x
z=lm(y~x)
abline(z)

hat_e=mean(e^2)
sqrt(mean(e^2))
mean(abs(e))





#------预测图
par(mfrow=c(1,2))
plot(p_price,data_use2$price,
     ylab ='真实值',xlab='预测值',main='真实值与预测值相差图(测试集)' )
abline(z)
plot(exp(fitted(lm4)),data_new$price,
     ylab ='真实值',xlab='预测值',main='真实值与预测值相差图(训练集)' )
abline(z)
par(mfrow=c(1,1))


#输出数据
setwd("F:/python/京东数据爬虫/data")
write.csv(compose,'对比.csv')


