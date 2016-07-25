
#getting the library
library(ggplot2);head(diamonds);names(diamonds)

#starting a basic ggplot plot object
gg<-ggplot(diamonds,aes(price,carat))+geom_point(color="brown4")
gg

#adding a title or label to the graph
gg<-gg+ggtitle("Diamond Carat & Price")
gg

gg<-gg+labs("Diamond Carat & Price")
gg

#adding theme to the plot
gg<-gg+theme(plot.title= element_text(size = 20, face = "bold"))
gg

#adding labels to the graph
gg<-gg+labs(x="Price in Dollar", y="Carat", title="Price by Carat",
            color="red")
gg

#removing text and ticks from a axis
gg<-gg+theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())
gg

#rotating the text in any axis
gg<-gg + theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5))
gg

#adding color to the axis names
gg<-gg + theme(axis.text.x=element_text(color = "seagreen1", vjust=0.45),
               axis.text.y=element_text(color = "violet", vjust=0.45))
gg

#setting limits to both axis
gg<-gg + ylim(0,0.8)+xlim(250,1500)
gg

#how to set legends in a graph
gg<-ggplot(diamonds,aes(price,carat,color=factor(cut)))+geom_point()
gg
gg<-ggplot(diamonds,aes(price,carat,color=factor(color)))+geom_point()
gg
gg<-ggplot(diamonds,aes(price,carat,color=factor(clarity)))+geom_point()
gg
gg<-gg+theme(legend.title=element_blank())
gg

#changing the title name of the legend
gg<-gg+theme(legend.title = element_text(colour="darkblue", size=16, 
                                         face="bold"))+scale_color_discrete(name="By Clarity")
gg

#changing the backgroup boxes in legend
gg<-gg+theme(legend.key=element_rect(fill='dodgerblue1'))
gg

#changing the size of the symbols used in legend
gg<-gg+guides(colour = guide_legend(override.aes = list(size=4)))
gg

#adding line to the data points
gg<-gg+geom_line(color="darkcyan")
gg

#changing the background of an image
gg<-gg+theme(panel.background = element_rect(fill = 'orange'))
gg

#changing plot background
gg<-gg+theme(plot.background = element_rect(fill = 'brown4'))
gg

table(diamonds$cut);table(diamonds$clarity);table(diamonds$color)

#adding a multi-variable cut to the graph
gg<-gg+facet_wrap(~cut, nrow=4)
gg

#adding two variables as cut to display the relationship
gg<-gg+facet_wrap(~cut+clarity, nrow=4)
gg


#scale free graphs in multi-panels
gg<-gg+facet_wrap(~color, ncol=2, scales="free")
gg

#bi-variate plotting using ggplot2
gg<-gg+facet_grid(color~cut)
gg

library(ggplot2);
library(scales)



#changing discrete category colors
ggplot(diamonds, aes(price, carat, color=factor(cut)))+
  geom_point() + 
  scale_color_brewer(palette="Set1")

#Using tableau colors
library(ggthemes)
ggplot(diamonds, aes(price, carat, color=factor(cut)))+
  geom_point() + 
  scale_color_tableau()

#using color gradient
ggplot(diamonds, aes(price, carat))+
  geom_point() + 
  scale_color_gradient(low = "blue", high = "red")

#plotting a distribution on a graph
mid<-mean(diamonds$price)
ggplot(diamonds, aes(price, carat, color=table))+geom_point()+
  scale_color_gradient2(midpoint=mid,
                        low="blue", mid="white", high="red" )

########################################################
##creating different charts
#######################################################
library(MASS);names(Cars93)

#creating bar chart
barplot <- ggplot(Cars93,aes(Type))+
  geom_bar(width = 0.5,fill="royalblue4",color="red")+
  ggtitle("Vehicle Count by Category")
barplot

#creating boxplot
boxplot <- ggplot(Cars93,aes(Type,Price))+
  geom_boxplot(width = 0.5,fill="firebrick",color="cadetblue2",
               outlier.colour = "purple",outlier.shape = 2)+
  ggtitle("Boxplot of Price by Car Type")
boxplot

#creatting Bubble chart
bubble<-ggplot(Cars93, aes(x=EngineSize, y=MPG.city)) +
  geom_point(aes(size=Price,color="red")) +
  scale_size_continuous(range=c(2,15)) +
  theme(legend.position = "bottom")
bubble

#creating Donut charts
ggplot(Cars93) + geom_rect(aes(fill=Cylinders, ymax=Max.Price, 
                               ymin=Min.Price, xmax=4, xmin=3)) +
  coord_polar(theta="y") + xlim(c(0, 4))


#creating geomap
library(googleVis)
head(state.x77)

states <- data.frame(state.name, state.x77)

gmap <- gvisGeoMap(states, "state.name", "Area",
                 options=list(region="US", dataMode="regions",
                              width=900, height=600))
plot(gmap)

#creating histograms
histog <- ggplot(Cars93,aes(RPM))+
  geom_histogram(width = 0.5,fill="firebrick",color="cadetblue2",
                 bins = 20)+
  ggtitle("Histogram")
histog

#creating line charts
linechart <- ggplot(Cars93,aes(RPM,Price))+
  geom_line(color="cadetblue4")+
  ggtitle("Line Charts")

linechart

#creating pie charts
pp <- ggplot(Cars93, aes(x = factor(1), fill = factor(Type))) +
  geom_bar(width = 1)
pp + coord_polar(theta = "y")

# 3D Pie Chart from data frame
library(plotrix)
t <- table(Cars93$Type);par(mfrow=c(1,2))
pct <- paste(names(t), "\n", t, sep="")
pie(t, labels = pct, main="Pie Chart of Type of cars")
pie3D(t,labels=pct,main="Pie Chart of Type of cars")

#Creating scatterplot

library(gridExtra)
sp <- ggplot(Cars93,aes(Horsepower,MPG.highway))+
  geom_point(color="dodgerblue",size=5)+ggtitle("Basic Scatterplot")+
  theme(plot.title= element_text(size = 12, face = "bold"))
sp
#adding a cantinuous variable Length to scale thee scatterplot points
sp2<-sp+geom_point(aes(color=Length), size=5)+
  ggtitle("Scatterplot: Adding Length Variable")+
  theme(plot.title= element_text(size = 12, face = "bold"))
sp2

grid.arrange(sp,sp2,nrow=1)

#adding a factor variable Origin to scale the scatterplot points
sp3<-sp+geom_point(aes(color=factor(Origin)),size=5)+
  ggtitle("Scatterplot: Adding Origin Variable")+
  theme(plot.title= element_text(size = 12, face = "bold"))
sp3

#adding custom color to the scatterplot
sp4<-sp+geom_point(aes(color=factor(Origin)),size=5)+
  scale_color_manual(values = c("red","blue"))+
  ggtitle("Scatterplot: Adding Custom Color")+
  theme(plot.title= element_text(size = 12, face = "bold"))
sp4

grid.arrange(sp3,sp4,nrow=1)

#adding lines to the scatterplot
sp5<-sp+geom_point(color="blue",size=5)+geom_line()+
  ggtitle("Scatterplot: Adding Lines")+
  theme(plot.title= element_text(size = 12, face = "bold"))
sp5

#adding regression lines to the scatterplot
sp6<-sp+geom_point(color="firebrick",size=5)+
  geom_smooth(method = "lm",se =T)+
  geom_smooth(method = "rlm",se =T)+
  ggtitle("Adding Regression Lines")+
  theme(plot.title= element_text(size = 12, face = "bold"))
sp6

grid.arrange(sp5,sp6,nrow=1)

#datasets with n < 1000 default is loess. For datasets with 1000 or 
#more observations defaults to gam
#adding more regression lines
sp7<-sp+geom_point(color="firebrick",size=5)+
  geom_smooth(method = "auto",se =T)+
  geom_smooth(method = "glm",se =T)+
  ggtitle("Adding Regression Lines")+
  theme(plot.title= element_text(size = 20, face = "bold"))
sp7

#adding regression lines to the scatterplot
sp8<-sp+geom_point(color="firebrick",size=5)+
  geom_smooth(method = "gam",se =T)+
  ggtitle("Adding Regression Lines")+
  geom_smooth(method = "loess",se =T)+
  theme(plot.title= element_text(size = 20, face = "bold"))
sp8

grid.arrange(sp7,sp8,nrow=1)

#creating 3D scatterplot
library(scatterplot3d);library(Rcmdr)
scatter3d(MPG.highway~Length+Width|Origin, data=Cars93, fit="linear",residuals=TRUE, parallel=FALSE, bg="black", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)


#stacked bar chart
qplot(factor(Type), data=Cars93, geom="bar", fill=factor(Origin))

#or

ggplot(Cars93, aes(Type, fill=Origin)) + geom_bar()


#stem and leaf plot
stem(Cars93$MPG.city)

#Word cloud representation
library(wordcloud)
words<-c("data","data mining","analytics","statistics","graphs",
           "visualization","predictive analytics","modeling","data science",
           "R","Python","Shiny","ggplot2","data analytics")
freq<-c(123,234,213,423,142,145,156,176,214,218,213,234,256,324)
d<-data.frame(words,freq)

set.seed(1234)
wordcloud(words = d$words, freq = d$freq, min.freq = 1,c(8,.3),
          max.words=200, random.order=F, rot.per=0.35, 
          colors=brewer.pal(7, "Dark2"))

#coxcomb chart = bar chart + pie chart
cox<- ggplot(Cars93, aes(x = factor(Type))) +
  geom_bar(width = 1, colour = "goldenrod1",fill="darkred")
cox + coord_polar()

#a second variant of coxcomb plot
cox + coord_polar(theta = "y")
