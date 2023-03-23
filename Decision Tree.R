##Import library
library(dplyr)
library(plyr)
library(splitTools)
library(corrplot)
library(ggplot2)
library(GGally)
library(psych)
library(knitr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(forcats)
library(caret)
library(tidyr)

#Import dataset and omit NA
data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv', na.strings = "")
print(colSums(is.na(data)))
data <- na.omit(data)

##Data Cleaning
for(i in c(2:10,15,21)){
  data[,i] <- as.factor(data[,i])
  print(class(data[,i]))
}

data_tree <- data[,-c(5,10,13,16,20)]
describe(data_tree)

set.seed(2023)
split <- partition(data_tree$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data_tree[split$train, ]
valid <- data_tree[split$valid, ]
test <- data_tree[split$test, ]

##Modelling Classification Tree
#Classification Tree
set.seed(2023)

minsplit=100
positiveWeight = 1.0 / (nrow(subset(train, y == "yes")) / nrow(train))
negativeWeight = 1.0 / (nrow(subset(train, y!= "yes")) / nrow(train))
modelWeights <- ifelse(train$y== "yes", positiveWeight, negativeWeight)

train.Model <- rpart(y ~., data=train, method="class", weights=modelWeights,
                     control=rpart.control(minsplit=100, minbucket=round(minsplit/3),
                                           maxdepth = 4, cp=0.015))        
rpart.plot(train.Model, type=2, extra=4)

#Variable Importance
train.Model$variable.importance

#training performance
train.pred <- predict(train.Model, newdata=train[,-16],type="class")
table(train[,16], train.pred)

#validation performance
valid.pred <- predict(train.Model, newdata=valid[,-16],type="class")
table(valid[,16], valid.pred)

#Evaluation
train.table <- table(train[,16], train.pred)
train.table[1,1]/sum(train.table[1,]) #training sensitivity
train.table[2,2]/sum(train.table[2,]) #training specificity
(train.table[1,1]+train.table[2,2])/sum(train.table) #training accuracy

valid.table <- table(valid[,16], valid.pred)
valid.table[1,1]/sum(valid.table[1,]) #validation sensitivity
valid.table[2,2]/sum(valid.table[2,]) #validation specificity
(valid.table[1,1]+valid.table[2,2])/sum(valid.table) #validation accuracy

#Model Justification
Ynew.pred <- predict(train.Model, newdata=test, type="class")
test.table <- table(test[,16], Ynew.pred);test.table
test.table[1,1]/sum(test.table[1,]) #sensitivity
test.table[2,2]/sum(test.table[2,]) #specificity
(test.table[1,1]+test.table[2,2])/sum(test.table) #accuracy

#Random Forest
#Select mtry
n<-length(names(train))     
rate=1     
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(y~.,data=train, mtry=i, ntree=1000)
  rate[i]<-mean(rf_train$err.rate)   
  print(rf_train)    
}
rate     
plot(rate, main="'mtry' Slection of Random Forset Model", xlab="value of mtry")

#Select ntree
set.seed(2023)
rf_train<-randomForest(y~.,data=train, mtry=5, ntree=1000)
plot(rf_train)   

#Random Forest model building
set.seed(2023)
rf <- randomForest(y~., data=train, mtry=5, ntree=200, importance=TRUE, weights=modelWeights)
rf

#validation performance
valid.pred <- predict(rf, newdata=valid[,-16],type="class")
table(valid[,16], valid.pred)

#Evaluation
train.table <- rf$confusion[,-3]
train.table[1,1]/sum(train.table[1,]) #training sensitivity
train.table[2,2]/sum(train.table[2,]) #training specificity
(train.table[1,1]+train.table[2,2])/sum(train.table) # Total accuracy

valid.table <- table(valid[,16], valid.pred)
valid.table[1,1]/sum(valid.table[1,]) #validation sensitivity
valid.table[2,2]/sum(valid.table[2,]) #validation specificity
(valid.table[1,1]+valid.table[2,2])/sum(valid.table) #Validation accuracy

#Variable Importance
sort(randomForest::importance(rf)[, 1], decreasing=TRUE)
rf_df <- data_frame(var = rownames(randomForest::importance(rf)),
                     MeanDecreaseGini = randomForest::importance(rf)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))
rf_ggplot <- ggplot(rf_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting subscription in Bank Marketing data",
       x = NULL, y = "Mean Decrease Gini") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))
rf_ggplot

#Model Justification
Ynew.pred <- predict(rf, newdata=test, type="class")
test.table <- table(test[,16], Ynew.pred);test.table
test.table[1,1]/sum(test.table[1,]) #sensitivity
test.table[2,2]/sum(test.table[2,]) #specificity
(test.table[1,1]+test.table[2,2])/sum(test.table) #accuracy
