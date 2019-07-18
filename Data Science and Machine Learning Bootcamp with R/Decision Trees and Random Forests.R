#Decision Trees and Random Forests with R

#install.packages(rpart)
# Use kyphosis dataset (kyphosis is a deformation of the spine)

head(kyphosis)

#build a tree model

tree <- rpart(Kyphosis~., method='class', data=kyphosis)
#Check out notebook for this lecture for some insights on how to examine the model

printcp(tree)
plot(tree,uniform = TRUE, main="Kyphosis Tree")
text(tree, use.n= TRUE, all=TRUE)

library(rpart.plot)
prp(tree) #Better way to plot decision tree

#Random Forest - large number of bootstrap trees and combining results across all trees
library(randomForest)
rf.model <- randomForest(Kyphosis~., data=kyphosis)
rf.model$ntree #Shows number of trees used. 500 is default

##################################################################################################
##################################################################################################
library(ISLR)
head(College)
df <- College

#Scatterplot of Grad.Rate vs Room.Board, colored by the Private column
ggplot(df,aes(Room.Board,Grad.Rate))+geom_point(aes(color=factor(Private)))

#Histogram of full time undergrad students, colored by Private
ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black', bins=50, alpha=0.5) + theme_bw()

#Histrogram of Grad.Rate colored by Private
ggplot(df,aes(Grad.Rate))+geom_histogram(aes(fill=Private),color='black')

max(df$Grad.Rate) #There is a school here with a 118% grad rate...what?
for(i in 1:length(df$Grad.Rate)){
  if(df$Grad.Rate[i]>100){
    df$Grad.Rate[i]<-100
  }
}

#Train/Test Data
set.seed(101)
train.row <- sample(1:777,(777*0.7))
train.data <- df[train.row,]
test.data <- df[-train.row,]

#Decision Tree Time
library(rpart)
tree <- rpart(Private~., method="class",data=train.data)
tree.preds <- predict(tree,test.data)
head(tree.preds)

tree.preds <- as.data.frame(tree.preds)
joiner<- function(x){
  if(x>=0.5){
    return('Yes') }
  else {
    return('No')
  }
}

tree.preds$Private <- sapply(tree.preds,joiner)
head(tree.preds)
table(tree.preds$Private,test.data$Private)
prp(tree)

#Random Forest Function
library(randomForest)
rf.model <- randomForest(Private~., data=train.data, importance=TRUE)
rf.model$confusion
rf.model$importance

rf.preds <- predict(rf.model,test.data)
table(rf.preds,test.data$Private)
