#Regression Techniques
library(car);attach(Cars93_1)
scatterplot.matrix(Cars93_1)
cor(Cars93_1)
#multiple linear regression model
fit<-lm(MPG.Overall~.,data=Cars93_1)

#model summary
summary(fit)

#estimated coefficients
fit$coefficients

#residual values
fit$residuals

#fitted values from the model
fit$fitted.values

#what happened to NA
fit$na.action

#ANOVA table from the model
summary.aov(fit)

#visualizing the model statistics
par(mfrow=c(1,2))
plot(fit, col="dodgerblue4")
dev.off()

confint(fit,level=0.95)
head(predict(fit,interval="predict"))

# Deletion Diagnostics
influence.measures(fit)

# Index Plots of the influence measures
influenceIndexPlot(fit, id.n=3) 

# A user friendly representation of the above
influencePlot(fit,id.n=3, col="red") 

## Regression after deleting the 28th observation
fit.1<-lm(MPG.Overall~., data=Cars93_1[-28,])
summary(fit.1)

## Regression after deleting the 28,39,42,59,60,77 observations
fit.2<-lm(MPG.Overall~., data=Cars93_1[-c(28,42,39,59,60,77),])
summary(fit.2)

# Diagnostic Plots
par(mfrow=c(1,4))

# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(fit.2) 

# Residuals vs Regressors
plot(fit.2) 
avPlots(fit.2, id.n=2, id.cex=0.7) # Added Variable Plots

# QQ plots of studentized residuals, helps identify outliers
qqPlot(fit.2, id.n=5) 


## Diagnostic Plots ###
influenceIndexPlot(fit.2, id.n=3)
influencePlot(fit.2, id.n=3, col="blue")

### Variance Inflation Factors
vif(fit.2)

## Regression after deleting the weight variable
fit.3<-lm(MPG.Overall~ Price+EngineSize+Horsepower+RPM+Rev.per.mile+
            Fuel.tank.capacity+Length+Wheelbase+Width+Turn.circle+
            Rear.seat.room+Luggage.room, data=Cars93_1[-c(28,42,39,59,60,77),])
summary(fit.3)
vif(fit.3)

## Regression after deleting the Enginesize variable
fit.4<-lm(MPG.Overall~ Price+Horsepower+RPM+Rev.per.mile+
            Fuel.tank.capacity+Length+Wheelbase+Width+Turn.circle+
            Rear.seat.room+Luggage.room, data=Cars93_1[-c(28,42,39,59,60,77),])
summary(fit.4)
vif(fit.4)

## Regression after deleting the Length variable
fit.5<-lm(MPG.Overall~ Price+Horsepower+RPM+Rev.per.mile+
            Fuel.tank.capacity+Wheelbase+Width+Turn.circle+
            Rear.seat.room+Luggage.room, data=Cars93_1[-c(28,42,39,59,60,77),])
summary(fit.5)
vif(fit.5)
coefficients(fit.5)


###########################################################################

#data conversion
Artpiece$IsGood.Purchase<-as.factor(Artpiece$IsGood.Purchase)
Artpiece$Is.It.Online.Sale<-as.factor(Artpiece$Is.It.Online.Sale)

#removing NA, Missing values from the data
Artpiece<-na.omit(Artpiece)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(Artpiece))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(Artpiece)), size = smp_size)

train <- Artpiece[train_ind, ]
test <- Artpiece[-train_ind, ]

#Logistic regression Model
model1<-glm(IsGood.Purchase ~.,family=binomial(logit),data=train)

#Model results/ components
summary(model1)

# 95% CI for the coefficients
confint(model1) 

# exponentiated coefficients
exp(coef(model1)) 

# 95% CI for exponentiated coefficients
exp(confint(model1)) 

# predicted values
predict(model1, type="response") 

# residuals
residuals(model1, type="deviance") 

#ANOVA
anova(model1,test="Chisq")

#Plotting the model
plot(model1$fitted)


#Predicted Probability
test$goodP<-predict(model1,newdata=test,type="response")
test$goodP<-predict(model1,newdata=test,type="link")

vif(model1)

#auto detection of model
fit_step<-stepAIC(model1,method="both")

summary(fit_step)

library(MASS);library(plyr);library(car)

vif(fit_step)

train$prob=predict(fit_step,type=c("response"))

library(pROC)

g <- roc(IsGood.Purchase ~ prob, data = train)
g
plot(g)

exp(coef(fit_step))

exp(cbind(OR = coef(fit_step), confint(fit_step)))

#Label the prediction result above the certain threshold as Yes and No
#Change the threshold value and check which give the better result.
train$prob <- ifelse(prob > 0.5, "Yes", "No")

#print the confusion matrix between the predicted and actual response on testdata.
t<-table(train$prob,train$IsGood.Purchase)

#accuracy
prop.table(t)

#################################################################
## Regression after deleting the Length variable
fit.6<-lm(MPG.Overall~ I(Price)^3+I(Horsepower)^3+I(RPM)^3+
            Wheelbase+Width+Turn.circle, data=Cars93_1[-c(28,42,39,59,60,77),])
summary(fit.6)
vif(fit.6)
coefficients(fit.6)

###########stepwise linear regression###############

#base model
fit<-lm(MPG.Overall~.,data=Cars93_1)

#stepwise regression
library(MASS)
model<-stepAIC(fit)
stepwise(fit)


#######Penalized regression in R########
#installing the library
library(glmnet)

#removing the missing values from the dataset
Cars93_2<-na.omit(Cars93_1)

#independent variables matrix
x<-as.matrix(Cars93_2[,-1])

#dependent variale matrix
y<-as.matrix(Cars93_2[,1])

#fitting the regression model
mod<-glmnet(x,y,family = "gaussian",alpha = 0,lambda = 0.001)

#summary of the model
summary(mod)

#Making predictions
pred<-predict(mod,x,type = "link")

#estimating the error for the model.
mean((y-pred)^2)


#######################LASSO####################################
#installing the library
library(lars)

#removing the missing values from the dataset
Cars93_2<-na.omit(Cars93_1)

#independent variables matrix
x<-as.matrix(Cars93_2[,-1])

#dependent variale matrix
y<-as.matrix(Cars93_2[,1])

#fitting the LASSO regression model
model<-lars(x,y,type = "lasso")
model<-lars(x,y,type = "lar")#least angle regression
model<-lars(x,y,type = "forward.stagewise")
model<-lars(x,y,type = "stepwise")

#summary of the model
summary(model)

#select best step with a minin error

best_model<-model$df[which.min(model$RSS)]
best_model
#Making predictions
pred<-predict(model,x,s=best_model,type = "fit")$fit

#estimating the error for the model.
mean((y-pred)^2)

plot(model)
