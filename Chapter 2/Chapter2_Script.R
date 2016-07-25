########################Datasets used########################

names(Cars93)
summary(Cars93)

fivenum(Cars93$MPG.highway)

library(Hmisc)
library(MASS)
describe(Cars93)
Cars93
n_cars93<-Cars93[,c(5,7,8)]
c_cars93<-Cars93[,c(3,9,16)]

apply(n_cars93,2,length)

skewness<-function(x){
  m3<-sum((x-mean(x))^3)/length(x)
  s3<-sqrt(var(x))^3
  m3/s3 }
library(e1071)
apply(n_cars93,2,skewness)


library(ggplot2)
library(gridExtra)
ggplot(Cars93, aes(Cars93$Price,Cars93$MPG.highway))+geom_point(aes(colour=(Cars93$Type)))+geom_smooth()

library(corrplot)
m<-as.matrix(n_cars93)
corrplot(m,method = "ellipse")

pairs(n_cars93,main="Correlation Plot", col="blue")

skewness(Cars93$Price)

ggplot(data=Cars93, aes(Cars93$Price)) + geom_density(fill="blue")


sd(Cars93$MPG.highway)

pnorm(35,mean(Cars93$MPG.highway),sd(Cars93$MPG.highway),lower.tail = F)

pbinom(1,93,prob = 0.1)

ppois(250,200,lower.tail = F)

x<-fitdistr(Cars93$MPG.highway,densfun = "normal")
x$estimate
x$sd
x$vcov
x$loglik
x$n

qqnorm(Cars93$MPG.highway)
qqline(Cars93$MPG.highway)

dnorm(Cars93$MPG.highway)

Cars93<-Cars93

freq<-table(Cars93$Type)
rel.freq<-freq/nrow(Cars93)*100
options(digits = 2)
rel.freq
cbind(freq,rel.freq)
barplot(freq, main = "Distribution of Categorical Variable")
pie(freq)

range(Cars93$Fuel.tank.capacity)
cat<-seq(9.2,27.0,by = 4)
cat
options(digits = 2)
t<-cut(Cars93$Fuel.tank.capacity,cat)
as.data.frame(cbind(table(t)))

names(Cars93)
table(Cars93$Type)
table(Cars93$AirBags)
contTable<-table(Cars93$Type,Cars93$AirBags)
contTable

prop.table(contTable)
prop.table(contTable,1)
prop.table(contTable,2)

summary(contTable)

library(gmodels)
CrossTable(Cars93$Type, Cars93$AirBags)

contTable<-table(Cars93$Type,Cars93$AirBags,Cars93$Origin)
contTable

margin.table(contTable,1)
margin.table(contTable,2)
margin.table(contTable,3)

summary(contTable)

library(nortest)
ad.test(Cars93$Price) # Anderson-Darling test
cvm.test(Cars93$Price) # Cramer-von Mises test
lillie.test(Cars93$Price) # Lilliefors (KS) test
pearson.test(Cars93$Price) # Pearson chi-square
sf.test(Cars93$Price) # Shapiro-Francia test


library(corrplot)
t<-cor(Cars93[,c("Price","MPG.city","RPM","Rev.per.mile","Width","Weight","Horsepower","Length")])
corrplot(t,method = "ellipse")

#################################
Cars93<-Cars93
names(Cars93)
table(Cars93$Manufacturer)

mu<-mean(Cars93$MPG.highway)
mu
sigma<-sd(Cars93$MPG.highway)
sigma
n<-length(Cars93$MPG.highway)
n
xbar=  35
z<-(xbar-mu)/(sigma/sqrt(n)) 
z

#computing the critical value at 5% alpha level
alpha = .05 
z1 = qnorm(1-alpha/2) 
c(-z1,z1)
  

ifelse(z > z1 | z < -z1,"Reject the Null Hypothesis","Accept the Null Hypothesis")

options(digits = 4)

mileage<-subset(Cars93,Cars93$RPM > 5000)
table(mileage$Origin)

p1<-17/57
p0<- 0.4

n <- length(mileage)

z <- (p1-p0)/sqrt(p0*(1-p0)/n)
z

#computing the critical value at 5% alpha level
alpha = .05 
z1 = qnorm(1-alpha/2)
c(-z1,z1)


ifelse(z > z1 | z < -z1,"Reject the Null Hypothesis","Accept the Null Hypothesis")

t.test(Cars93$MPG.city, Cars93$MPG.highway, paired = F)
t.test(Cars93$MPG.city~Cars93$Man.trans.avail, data=Cars93)       
wilcox.test(Cars93$MPG.city~Cars93$Man.trans.avail, correct = F)       
wilcox.test(Cars93$MPG.city, Cars93$MPG.highway, paired = T)
wilcox.test(Cars93$MPG.city~Cars93$Man.trans.avail, data=Cars93)   
ks.test(Cars93$MPG.city~Cars93$Man.trans.avail, data=Cars93)
var.test(Cars93$MPG.highway~Cars93$Man.trans.avail, data=Cars93)
bartlett.test(Cars93$MPG.highway~Cars93$Man.trans.avail, data=Cars93)

shapiro.test(Cars93$MPG.city)
hist(Cars93$MPG.city,breaks=25)
qqnorm(Cars93$MPG.city,pch="*")
qqline(Cars93$MPG.city)

kruskal.test(Cars93$MPG.city~Cars93$Cylinders, data= Cars93)
aov(Cars93$RPM~Cars93$Cylinders)  
summary(aov(Cars93$RPM~Cars93$Cylinders))

TukeyHSD(aov(Cars93$RPM~Cars93$Cylinders))

aov(Cars93$RPM~Cars93$Origin + Cars93$AirBags)
summary(aov(Cars93$RPM~Cars93$Origin + Cars93$AirBags))
TukeyHSD(aov(Cars93$RPM~Cars93$Origin + Cars93$AirBags))
