VHLSS2014 <- read.csv
library(readxl)
VHLSS2014_HN <- read_excel("C:/Users/ducvu/Downloads/VHLSS2014_HN.xlsx")
View(VHLSS2014_HN)

attach(VHLSS2014)

install.packages("lmtest")
library(lmtest)
install.packages("zoo")
library(zoo)
install.packages("sandwich")
library(sandwich)
library(car)

size <- VHLSS2014_HN$`size`
income <- VHLSS2014_HN$`income`
install.packages("e1071")
library(e1071)
tobaco <- VHLSS2014_HN$`tobaco`
rice <- VHLSS2014_HN$`rice`
nrice <- VHLSS2014_HN$`nrice`
nfood <- VHLSS2014_HN$`nfood`

#PHẦN 1 DESCRIPTIVE STATISTIC
mean(size)
median(income/size)
skewness(size)

skewness(size)
skewness(income)
#correlation
cor(income,size)
#correlation test between income and tobacco
cor.test(income,tobaco)

#median of standardized of income per head sẽ tính zscored rồi tính median của nó
#zscore<-(x(giá trị của income , .. đồ hen) -mean)/sd
inh<-income/size
standardized<-(inh-mean(inh))/sd(inh)
median(standardized)
#Nhìn vào mấy cái ggiátri để so sánh với standardized, hoặc với các gtri với nhau
standardized[1:5]  
rh<-rice/size
srh<-(rh-mean(rh))/sd(rh)
median(srh)
  srh[1:5]
#which strongest correlation/weakest to income
data1<-data.frame(income, rice,nrice,nfood,tobaco)
cor(data1)
cor(inh,rh)
#regression
reg1<-lm(rice~income)
rice_new <- 100000
reg_new <- lm(rice_new ~ income)
summary(reg1)
#không điên được e thì e+ thì 10^ dương còn trừ thì 10^âm
#coeffiecient=r-sq
resid(reg1)[1:5]
fitted(reg1)
fitted(reg1)[1:5]
#test for slope (Nhìn sao của biến)

reg2<-lm(rice~income+size)
summary(reg2)
#covariance của 2 slope
vcov(reg2)
#test for significant of income and size
cor.test(income,size)
# Reg function ( peak.... nhìn hệ số của biến nhìn slide 95,96 )
reg3<-lm(rice~ I(income^2) + income + size)
summary(reg3)
ggplot(reg2)
#log-log
reg4<-lm(log(rice)~log(income))
summary(reg4)

library(lmtest)
resettest(reg4)

bptest(reg4)
#tính elasticity, công thức trong vo nha
#(la he so log = 0.278)


#using f test to test hypothesis that slope is equal to 1, F stat is
linearHypothesis(reg4, "log(income) = 1")

summary(reg4)$fstatistic
#what can be concluded
#tổng beta =1
install.packages("AER")
library(AER)
linearHypothesis(reg5, "log(income) + log(nrice) = 1")
reg5<-lm(log(rice)~log(income)+ log(nrice))
summary(reg5)
linearHypothesis(reg5, "log(income)+ log(nrice) = 0.3")


#câu có dummy
area <- VHLSS2014_HN$`area`
d<- 2 - area
reg6<-lm(rice~income+d)
summary(reg6)
#differ thì so sánh pvalue của dummy 

#intercept của rural là intercept luôn còn của urban là cộng khi dummy=1 với intercept 
reg7<-lm(rice~income+I(d*income))
summary(reg7)
#câu 39,38 nhìn p_value
reg8<-lm(rice~ income + d + I(d*income))
summary(reg8)
reg9<-lm(rice~ income + d + d*income)
summary(reg9)
#câu 40 test slide 83

#chú ý muốn so sánh biến nào có tác động mạnh hơn thì phải chuẩn hóa các biến phần standarrdized ấy
#eg
i<-(income-mean(income))/sd(income)
si<-(size-mean(size))/sd(size)
nr<-(nrice-mean(nrice))/sd(nrice)
r<-(rice-mean(rice))/sd(rice)
reg10<-lm(r~ 0+i+si+nr)  #phải có 0 vì luc này intercept =0
summary(reg10)
