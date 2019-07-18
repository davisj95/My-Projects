#Titanic Practice
library(ggplot2)

#Read in Data################################################################################################
setwd("~/My Projects/Titanic")
train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)



head(train)
head(test)

train$Cabin <- NULL     #Mostly empty column
str(train)




#EDA#########################################################################################################
plot(train$Age, train$Survived)

ggplot(train, aes(x=Age, y=Survived)) + geom_smooth() + geom_point(aes(x=Age, y=Survived))
ggplot(train, aes(x=Fare, y=Survived)) + geom_smooth() + geom_point(aes(x=Fare, y=Survived))
ggplot(train, aes(x=Pclass, y=Survived)) + geom_jitter(aes(x=Pclass, y=Survived))

#Find NA's and fix
#install.packages("Amelia")
library(Amelia)
missmap(train,col=c("black","yellow"))

#What is the mean age of men by class?
mean(na.omit(train[train$Pclass==1&train$Sex=="male","Age"]))  #41.28
mean(na.omit(train[train$Pclass==2&train$Sex=="male","Age"]))  #30.74
mean(na.omit(train[train$Pclass==3&train$Sex=="male","Age"]))  #26.51

#What is the mean age of women by class?
mean(na.omit(train[train$Pclass==1&train$Sex=="female","Age"]))  #34.61
mean(na.omit(train[train$Pclass==2&train$Sex=="female","Age"]))  #28.72
mean(na.omit(train[train$Pclass==3&train$Sex=="female","Age"]))  #21.75

#Fill in blank ages according to above data
for(i in 1:length(train$Age)){
  if(is.na(train$Age[i])==TRUE){
    if(train$Pclass[i]==1){
      if(train$Sex[i]=="male"){
        train$Age[i]<-41.28
      } else {
        train$Age[i]<-34.61
      }
    }
    if(train$Pclass[i]==2){
      if(train$Sex[i]=="male"){
        train$Age[i]<-30.74
      } else {
        train$Age[i]<-28.72
      }
    }
    if(train$Pclass[i]==3){
      if(train$Sex[i]=="male"){
        train$Age[i]<-26.51
      } else {
        train$Age[i]<-21.75
      }
    }
  }
}



#Model

min.model <- glm(Survived~+1, data=train, family=binomial("logit"))
max.model <- formula(glm(Survived~., data=train, family=binomial("logit")))
out1 <- step(min.model, direction="forward", scope=max.model)
out2 <- step(glm(Survived~., data=train.tr, family="binomial"))
out3 <- step(min.model, direction="both", scope=max.model)
summary(out1)
summary(out2)
summary(out3)

#Predictions
library(ROCR)
train.pred <- prediction(predict(out1,train, type="response"),train$Survived)
train.perf<-performance(train.pred, measure="tpr", x.measure="fpr")
plot(train.perf)
abline()


train.result <- ifelse(train.pred[[1]]>0.5,1,0)
train$Est <- train.result
t <- train[,c("Survived","Est")]
table(train$Survived,train$Est)

test.pred <- prediction(predict(out1, newdata=train.ts, type="response"), train.ts$Survived)
test.perf<-performance(test.pred, measure="tpr", x.measure="fpr")
plot(test.perf, add=TRUE, col="royalblue")

performance(test.pred, measure="auc")
exp(-2.634) #Male
exp(-1.319) #PClass
exp(-0.047) #Age
exp(-0.354) #SibSp



for(i in 1:length(test$Age)){
  if(is.na(test$Age[i])==TRUE){
    if(test$Pclass[i]==1){
      if(test$Sex[i]=="male"){
        test$Age[i]<-41.28
      } else {
        test$Age[i]<-34.61
      }
    }
    if(test$Pclass[i]==2){
      if(test$Sex[i]=="male"){
        test$Age[i]<-30.74
      } else {
        test$Age[i]<-28.72
      }
    }
    if(test$Pclass[i]==3){
      if(test$Sex[i]=="male"){
        test$Age[i]<-26.51
      } else {
        test$Age[i]<-21.75
      }
    }
  }
}

test$Survived <- ifelse(predict(out1, newdata=test, type="response")>0.5,1,0)
q <- test[,c("PassengerId","Survived")]

write.csv(q, "Titanic Submission.csv", row.names = FALSE)
















