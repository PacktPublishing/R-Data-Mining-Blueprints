setwd("H:\\PACKT Publishing\\Chapter 8")
default<-read.csv("default.csv")

df<-default[-1,-c(1,3:5,7:12,25)]

func1<-function(x){
  as.numeric(x)
}

df<-as.data.frame(apply(df,2,func1))

dn<-function(x){
  (x-mean(x))
}

#transformed dataset
dn<-as.data.frame(apply(df,2,dn))

#calculating correlation between variables
cor(df)
pairs(df[1:900,])

corrplot::corrplot(cor(df),method="ellipse")

#Principal Component Analysis
options(digits = 2)
pca<-prcomp(df,scale = T, center = T)
summary(pca)$rotation
head(summary(pca)$x)
biplot(prcomp(df,scale. = T))

#calculating Eigen vectors
eig<-eigen(cor(df))

eigen(cor(df),TRUE)$values
head(eigen(cor(df),TRUE)$vectors)

#Compute the new dataset
eigvec<-t(eig$vectors) #transpose the eigen vectors
df_scaled<-t(dn) #transpose the adjusted data

df_new<-eigvec %*% df_scaled
df_new<-t(df_new)
colnames(df_new)<-c("PC1","PC2","PC3","PC4",
                    "PC5","PC6","PC7","PC8",
                    "PC9","PC10","PC11","PC12",
                    "PC13","PC14")
head(df_new)

#Principal Component Analysis
options(digits = 2)
pca1<-princomp(df,scores = T, cor = T)
summary(pca1)
diag(cov(df))

#Loadings of Principal Components
pca1$loadings
loadings(pca1)

#scree plot of the Eigen values
plot(pca1, main = "Percentage Variation Explained by Principal Components")
screeplot(pca1, type = "line", main = "Scree Plot")

#Biplot of the score variables
biplot(pca1)

#scores of the components
pca1$scores[1:10,]

#standard deviation of the principal components
pca1$sdev

#mean of all the principal component
pca1$center

#Principal Component Analysis
options(digits = 2)
pca2<-princomp(dn)
result<-round(summary(pca2)[1]$sdev,0)

#scree plot of the Eigen values
plot(result, main = "Standard Deviation by Principal Components",
     xlab="Principal Components",ylab="Standard Deviation",type='o')
screeplot(pca2, type = "line", main = "Scree Plot")

