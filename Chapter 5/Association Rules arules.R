## create a basket format 
data <- paste(
  "Bread, Butter, Jam", 
  "Bread, Butter", 
  "Bread", 
  "Butter, Banana", 
  sep="\n")
cat(data)
write(data, file = "basket_format")

## read data
library(arules)
tr <- read.transactions("basket_format", format = "basket", sep=",")
inspect(tr)


## create single format
data <- paste(
  "trans1 Bread", 
  "trans2 Bread",
  "trans2 Butter",
  "trans3 Jam",
  sep ="\n")
cat(data)
write(data, file = "single_format")

## read data
tr <- read.transactions("single_format", format = "single", cols = c(1,2))
inspect(tr)

#############################################################################

# Load the libraries
library(arules);library(arulesViz);library(igraph)

# Load the data set
data(Groceries) #directly reading from library

Groceries<-read.transactions("groceries.csv",sep=",") #reading from local computer

summary(Groceries)

inspect(Groceries[1:3])


# Create an item frequency plot for the top 20 items
cbind(itemFrequency(Groceries[,1:10])*100)

# plot the frequency of items
itemFrequencyPlot(Groceries, support=0.01, main="ItemFreq Plot",type="absolute")
itemFrequencyPlot(Groceries,topN=50,type="relative",main="Relative Freq Plot")
itemFrequencyPlot(Groceries, support = 0.001)

# Get the association rules based on apriori algo
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.10, minlen=2))
summary(rules)
length(rules)
inspect(rules[1:8])

support<-seq(0.01,0.1,0.01)
support
rules_count<-c(435,128,46,26,14, 10, 10,8,8,8)
rules_count
plot(support,rules_count,type = "l",main="Number of rules at different support %",
     col="darkred",lwd=3)

rules <- apriori(Groceries, parameter = list(supp = 0.02, conf = 0.30, minlen=2))
summary(rules)
length(rules)
inspect(rules[1:8])

conf<-seq(0.10,1.0,0.10)
conf
rules_count<-c(427,231,125,62,15,0,0,0,0,0)
rules_count

plot(conf,rules_count,type = "l",main="Number of rules at different confidence %",
     col="darkred",lwd=3)



# Get the association rules based on eclat algo

rules_ec <- eclat(Groceries, parameter = list(supp = 0.05))
summary(rules_ec)



#visualizign the rules
plot(rules,method='graph',interactive = T,shading = T)
plot(rules_ec,method='graph',interactive = T,shading = N)


#sorting out the most relevant rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])


#Implementation- Targeting
#???What are customers likely to buy before buying whole milk?
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="yogurt"),
               control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#???What are customers likely to buy if they purchase butter?
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.10,minlen=2), 
               appearance = list(default="rhs",lhs="yogurt"),
               control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


# sorting grocery rules by lift
inspect(sort(rules, by = "lift")[1:5])

# finding subsets of rules containing any items
itemname_rules <- subset(rules, items %in% "item name")
inspect(itemname_rules[1:5])

# finding subsets of rules that precede soda purchases
sodarules <- subset(rules, lhs %pin% "milk")
inspect(sodarules[1:5])
top.soda.rules <- head(sort(sodarules, by = "lift"), 5)
inspect(top.soda.rules)

# writing the rules to a CSV file
write(rules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(rules, "data.frame")

