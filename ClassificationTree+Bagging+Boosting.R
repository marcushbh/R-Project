Train_Raw_Data<-read.csv("train.csv")
summary(Train_Raw_Data)
class(Train_Raw_Data$Pclass)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyr)
library(adabag)
library(plyr)

#Replace the age having NA value with the mean of each group of people
Titanic.df<-separate(Train_Raw_Data, Name, into = c("First_Name","Name"), sep=", ", remove=TRUE, convert=FALSE)
Titanic.df<-separate(Titanic.df, Name, into = c("Title","Last_Name"), sep=". ", remove=TRUE, convert=FALSE)
NA_Row_Name<-which(is.na(Titanic.df$Age))
for (row in NA_Row_Name) {
  Titanic.df[row,"Age"]<-mean(Titanic.df[which(Titanic.df$Title==Titanic.df[row,"Title"]),'Age'],na.rm = TRUE)
}


#Basic exploration of dataset
summary(Titanic.df)
names(Titanic.df)
Titanic.df<-Titanic.df[,-c(4,5,6,11,13)]
summary(Titanic.df)
names(Titanic.df)
Titanic.df$Survived<-as.factor(Titanic.df$Survived)
cor(Titanic.df$Pclass,Titanic.df$Fare)
Titanic.df$Pclass<-as.factor(Titanic.df$Pclass)
class(Titanic.df$Title)
Titanic.df$Title<-as.factor(Titanic.df$Title)
levels(Titanic.df$Title)

set.seed(1)
#Train.df<-Titanic.df
#Partitioning Dataset into 70% Training and 30% Validation
train.index <- sample(c(1:dim(Titanic.df)[1]), dim(Titanic.df)[1]*0.7)  
Train.df <- Titanic.df[train.index, ]
Valid.df <- Titanic.df[-train.index, ]

#Run classification tree
Titanic.ct <- rpart(Survived ~ ., data = Train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 15)
prp(Titanic.ct, type = 1, extra = 1, under = TRUE, split.font = 1, 
    box.col=ifelse(Titanic.ct$frame$var == "<leaf>", 'gray', 'white'))  
printcp(Titanic.ct)
length(Titanic.ct$frame$var[Titanic.ct$frame$var == "<leaf>"])

#Predict Validation Dataset and Show Accuracy
Titanic.ct.point.pred.train <- predict(Titanic.ct,Train.df,type = "class")
confusionMatrix(Titanic.ct.point.pred.train, Train.df$Survived)
Titanic.ct.point.pred.val <- predict(Titanic.ct,Valid.df,type = "class")
confusionMatrix(Titanic.ct.point.pred.val, Valid.df$Survived)

#Prune by cp of Smallest tree within 1 std. error of min. error
pruned.ct <- prune(Titanic.ct, cp = 0.0113475)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

#Predict Validation Dataset and Show Accuracy
pruned.ct.point.pred.train <- predict(pruned.ct,Train.df,type = "class")
confusionMatrix(pruned.ct.point.pred.train, Train.df$Survived)
pruned.ct.point.pred.val <- predict(pruned.ct,Valid.df,type = "class")
confusionMatrix(pruned.ct.point.pred.val, Valid.df$Survived)

#bagging
bag<-bagging(Survived ~ ., data = Train.df)
bag_pred<-predict(bag, Valid.df, type = "class")
confusionMatrix(as.factor(bag_pred$class), Valid.df$Survived)
#Improve compared with Classification Tree after pruning


#boosting
boost<-boosting(Survived ~ ., data = Titanic.df)
boost_pred<-predict(boost, Valid.df, type = "class")
confusionMatrix(as.factor(boost_pred$class),Valid.df$Survived)
#Overfitting

#Import Test Dataset to be predicted and clean the data
Test_Raw_Data<-read.csv("test.csv")
summary(Test_Raw_Data)
Test.df<-separate(Test_Raw_Data, Name, into = c("First_Name","Name"), sep=", ", remove=TRUE, convert=FALSE)
Test.df<-separate(Test.df, Name, into = c("Title","Last_Name"), sep=". ", remove=TRUE, convert=FALSE)
NA_Row_Name<-which(is.na(Test.df$Age))
for (row in NA_Row_Name) {
  Test.df[row,"Age"]<-mean(Test.df[which(Test.df$Title==Test.df[row,"Title"]),'Age'],na.rm = TRUE)
}
summary(Test.df)
names(Test.df)
Test.df<-Test.df[,-c(3,4,5,10,12)]
Test.df$Pclass<-as.factor(Test.df$Pclass)

#Use Bagging to predict Test datset
Final<-predict(bag, Test.df, type = "class")
submission<-data.frame(PassengerId = Test.df$PassengerId, Survived = Final$class)

#Use pruned.ct to predict Test datset
Final2<-predict(pruned.ct, Test.df, type = "class")
submission.CT<-data.frame(PassengerId = Test.df$PassengerId, Survived = Final2)

#Use Boosting to predict Test datset
Final.boost<-predict(boost, Test.df, type = "class")
submission.boost<-data.frame(PassengerId = Test.df$PassengerId, Survived = Final.boost$class)

#Export the table to a csv file
write.csv(submission, "Submission.csv", row.names = FALSE)
write.csv(submission.CT, "CT.Submission.csv", row.names = FALSE)
write.csv(submission.boost, "Boost.Submission.csv", row.names = FALSE)

#Combine the results of 3 models
Combine.df<-data.frame(PassengerId = as.numeric(as.character(Test.df$PassengerId)), 
                       Survived.ct = as.numeric(as.character(submission.CT$Survived)), 
                       Survived.Bagging = as.numeric(as.character(submission$Survived)), 
                       Survived.Boosting = as.numeric(as.character(submission.boost$Survived)))
for (row in (1:418)) {
  Combine.df[row,"Survived"]<-ifelse(mean(Combine.df[row,2],Combine.df[row,3],Combine.df[row,4])>0.5, 1, 0)
}
Combine.submission<-Combine.df[,-c(2:4)]

#Export the table to a csv file
write.csv(Combine.submission, "Combine.Submission.csv", row.names = FALSE)
