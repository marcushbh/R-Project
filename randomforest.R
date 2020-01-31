library(dplyr)
library(ggplot2)
library(corrplot)
library(plyr)
library(caret)
train <- read.csv("train.csv")
test <- read.csv("test.csv")

entire<- bind_rows(train,test)
options(warn=-1)
summary(entire)

##EDA
## 1. sex vs survived
ggplot(entire[1:891,], aes(Sex, fill = factor(Survived))) + geom_bar(stat = "count", position = 'dodge')+ 
  xlab("Sex") +ylab("Count") +scale_fill_discrete(name = "Survived") + ggtitle("Sex vs Survived")

##2. age vs survived
ggplot(entire[1:891,], aes(Age, fill = factor(Survived))) + geom_histogram(bins=20) + 
  xlab("Age") +ylab("Survived")+scale_fill_discrete(name = "Survived") + ggtitle("Age vs Survived")

##3. Pclass vs Sex vs Survived
ggplot(entire[1:891,], aes(Pclass, fill = factor(Survived))) + geom_bar(stat = "count")+
  xlab("Pclass") +facet_grid(.~Sex)+ylab("Count") +scale_fill_discrete(name = "Survived") + ggtitle("Pclass vs Sex vs Survived")

######## Replace, Create and Fill missing value
#1. Embarked
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(entire$Embarked)
print(result)

#Result shows value of "S"
entire$Embarked[is.na(entire$Embarked)] <- "S"
sum(is.na(entire$Embarked))

#2. fare
entire$Fare[is.na(entire$Fare)]<-median(entire$Fare,na.rm=TRUE)
sum(is.na(entire$Fare))

#3. Title  
entire$Title <- gsub(".*,"," ", entire$Name)
entire$Title <- gsub("\\..*"," ", entire$Title)
entire$Title<-gsub(" ", "",entire$Title)
unique(entire$Title)

# Reassign mlle, ms, and mme, and Special Identity
entire$Title[entire$Title == "Mlle"]<- "Miss" 
entire$Title[entire$Title == "Ms"] <- "Miss"
entire$Title[entire$Title == "Mme"]<- "Mrs" 
entire$Title<-ifelse(entire$Title=="Dona"|entire$Title=="Lady"|entire$Title=="theCountess"|entire$Title=="Sir"
                     |entire$Title=="Jonkheer","Royalty",entire$Title)
entire$Title<-ifelse(entire$Title=="Capt"|entire$Title=="Col"|entire$Title=="Don"|entire$Title=="Dr"
                     |entire$Title=="Major"|entire$Title=="Rev","Officer",entire$Title)

unique(entire$Title)
ggplot(entire[1:891,], aes(Title,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('Title') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("Title vs Survived")


#3. age (Fill age with median age of specific title group)
title.age <- aggregate(entire$Age,by = list(entire$Title), FUN = median,na.rm=TRUE)
entire[is.na(entire$Age), "Age"] <- apply(entire[is.na(entire$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])
sum(is.na(entire$Age))

#4 Family Size
entire$Fsize <- entire$SibSp + entire$Parch + 1
entire$FsizeRank[entire$Fsize == 1] <- 'Alone'
entire$FsizeRank[entire$Fsize < 4 & entire$Fsize > 1] <- 'Small'
entire$FsizeRank[entire$Fsize > 3] <- 'Big'

##Whole Table


entire$Sex  <- factor(entire$Sex)
entire$Embarked  <- factor(entire$Embarked)
entire$Title  <- factor(entire$Title)
entire$Pclass  <- factor(entire$Pclass)
entire$FsizeRank  <- factor(entire$FsizeRank)

##Correlation graph

train <- entire[1:891,]
test <- entire[892:1309,]

corr_data <- entire[1:891,]
corr_data$Title <- revalue(corr_data$Title, 
                           c("Mr" = 1, "Master" = 2,"Officer" = 3, 
                             "Mrs" = 4,"Royalty" = 5,"Miss" = 6))
corr_data$Title <- as.numeric(corr_data$Title)
corr_data$Pclass <- as.numeric(corr_data$Pclass)
corr_data$Survived <- as.numeric(corr_data$Survived)

corr_data <-corr_data[,c("Age","Title","Pclass","Survived")]
mcorr_data <- cor(corr_data)
corrplot(mcorr_data,method="circle")




# Partition data
set.seed(3)
train.index <- sample(row.names(train ), 0.6*dim(train )[1])  
valid.index <- setdiff(row.names(train ), train.index) 

train.df <- train [train.index, ]
valid.df <- train [valid.index, ]

set.seed(1)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked +  Title +
                           FsizeRank, data = train.df)
prediction <- predict(rf_model, valid.df)
confusionMatrix(prediction,factor(valid.df$Survived))


# random forest
library("randomForest")

set.seed(111)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title +
                           FsizeRank, data = train)

rf_model
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId,Survived = prediction)

# .csv
write.csv(solution, file = 'rf_sol.csv', row.names = F)

