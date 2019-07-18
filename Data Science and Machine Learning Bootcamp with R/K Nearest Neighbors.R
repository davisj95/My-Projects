str(Caravan)
summary(Caravan$Purchase)

any(is.na(Caravan))
var(Caravan[,1])
purchase <- Caravan[,86]

standardized.Caravan <- scale(Caravan[,-86])
var(standardized.Caravan[,1])

#Train Test Split

test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

#Train Data
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

#Use KNN Algorithm

#KNN Model
library(class)
set.seed(101)
predicted.purchase <- knn(train.data, test.data, train.purchase,k=3)
head(predicted.purchase)

misclass.error <- mean(test.purchase != predicted.purchase)
# 11.6% misclassification, which isn't terrible, but want to improve, so change k

predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase,k=i)
  error.rate[i] <- mean(test.purchase!=predicted.purchase)
}

#Visualize k elbow method
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
ggplot(error.df, aes(k.values,error.rate))+geom_point() + geom_line()
#k=9 is the optimal k value because the value stops dropping after 9


################################################################################################
################################################################################################
#KNN Project

library(ISLR)
head(iris)

#Standardize data (even though technically for this data set it's already standardized)
standardized.iris <- scale(iris[,c(1:4)])
var(standardized.iris[,1])
Species <- iris$Species
standardized.iris <- (cbind(standardized.iris,Species))
head(standardized.iris)

#Split data into train/test
library(caTools)

train.row <- sample(1:length(standardized.iris$Sepal.Length),120)

train.data <- standardized.iris[train.row,]
test.data <- standardized.iris[-train.row,]

train.species <- Species[train.row]
test.species <- Species[-train.row]


library(class)

for(i in 1:20){
  predicted.species <- knn(train.data[,1:4],test.data[1:4],train.species,k=i)
  error.rate[i] <- mean(test.species!=predicted.species)
}

library(ggplot2)
k.values<-1:20
error.df <- data.frame(error.rate,k.values)
ggplot(error.df,aes(k.values,error.rate))+geom_point()+geom_line()































