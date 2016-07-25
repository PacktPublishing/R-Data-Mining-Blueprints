setwd("E:/iPredict Analytics/PACKT Publishing/Chapter 9")
library(neuralnet)
art<- read.csv("ArtPiece_1.csv")
str(art)
#data conversion for categorical features
art$Art.Auction.House<-as.factor(art$Art.Auction.House)
art$IsGood.Purchase<-as.factor(art$IsGood.Purchase)
art$Art.Category<-as.factor(art$Art.Category)
art$Prominent.Color<-as.factor(art$Prominent.Color)
art$Brush<-as.factor(art$Brush)
art$Brush.Size<-as.factor(art$Brush.Size)
art$Brush.Finesse<-as.factor(art$Brush.Finesse)
art$Art.Nationality<-as.factor(art$Art.Nationality)
art$Top.3.artists<-as.factor(art$Top.3.artists)
art$GoodArt.check<-as.factor(art$GoodArt.check)
art$AuctionHouseGuarantee<-as.factor(art$AuctionHouseGuarantee)
art$Is.It.Online.Sale<-as.factor(art$Is.It.Online.Sale)

#data conversion for numeric features
art$Critic.Ratings<-as.numeric(art$Critic.Ratings)
art$Acq.Cost<-as.numeric(art$Acq.Cost)
art$CurrentAuctionAveragePrice<-as.numeric(art$CurrentAuctionAveragePrice)
art$CollectorsAverageprice<-as.numeric(art$CollectorsAverageprice)
art$Min.Guarantee.Cost<-as.numeric(art$Min.Guarantee.Cost)

#removing NA, Missing values from the data
fun1<-function(x){
  ifelse(x=="#VALUE!",NA,x)
}
art<-as.data.frame(apply(art,2,fun1))
art<-na.omit(art)

#keeping only relevant variables for prediction
art<-art[,c("Art.Auction.House","IsGood.Purchase","Art.Category",
            "Prominent.Color","Brush","Brush.Size","Brush.Finesse",
            "Art.Nationality","Top.3.artists","GoodArt.check",
            "AuctionHouseGuarantee","Is.It.Online.Sale","Critic.Ratings",
            "Acq.Cost","CurrentAuctionAveragePrice","CollectorsAverageprice",
            "Min.Guarantee.Cost")]

#creating dummy variables for the categorical variables
library(dummy)
art_dummy<-dummy(art[,c("Art.Auction.House","IsGood.Purchase","Art.Category",
                        "Prominent.Color","Brush","Brush.Size","Brush.Finesse",
                        "Art.Nationality","Top.3.artists","GoodArt.check",
                        "AuctionHouseGuarantee","Is.It.Online.Sale")],int=F)
art_num<-art[,c("Critic.Ratings",
                "Acq.Cost","CurrentAuctionAveragePrice","CollectorsAverageprice",
                "Min.Guarantee.Cost")]
art<-cbind(art_num,art_dummy)

## 70% of the sample size
smp_size <- floor(0.70 * nrow(art))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(art)), size = smp_size)

train <- art[train_ind, ]
test <- art[-train_ind, ]

fun2<-function(x){
  as.numeric(x)
}
train<-as.data.frame(apply(train,2,fun2))
test<-as.data.frame(apply(test,2,fun2))

################Neuralnet model for prediction#########################
library(neuralnet)
fit<-neuralnet(CurrentAuctionAveragePrice~Critic.Ratings+Acq.Cost+
                 CollectorsAverageprice+Min.Guarantee.Cost,data=train,
               hidden = 15,err.fct = "sse",linear.output = F)
fit$result.matrix

output<-cbind(fit$covariate,fit$result.matrix[[1]])
head(output)

fit<-nnet(CurrentAuctionAveragePrice~Critic.Ratings+Acq.Cost+
                 CollectorsAverageprice+Min.Guarantee.Cost,data=train,
               size=100)
fit

plot(fit)


################Neuralnet model for classification#########################
library(neuralnet)
fit<-neuralnet(IsGood.Purchase_1~Brush.Size_1+Brush.Size_2+Brush.Size_3+
                 Brush.Finesse_Coarse+Brush.Finesse_Fine+
                 Art.Nationality_American+Art.Nationality_Asian+
                 Art.Nationality_European+GoodArt.check_YES,data=train[1:2000,],
               hidden = 25,err.fct = "ce",linear.output = F)
fit
output<-cbind(fit$covariate,fit$result.matrix[[1]])
head(output)

fit.nnet<-nnet(factor(IsGood.Purchase_1)~Brush.Size_1+Brush.Size_2+Brush.Size_3+
            Brush.Finesse_Coarse+Brush.Finesse_Fine+
            Art.Nationality_American+Art.Nationality_Asian+
            Art.Nationality_European+GoodArt.check_YES,data=train[1:2000,],
          size=9)
fit.nnet

plot(fit)
################Neuralnet model for forecasting#########################
library(forecast)
AirPassengers
plot(AirPassengers,lwd=5)
fit<-nnetar(AirPassengers, p=9,P=,size = 10, repeats = 50,lambda = 0)
plot(forecast(fit,10))
summary(fit)
