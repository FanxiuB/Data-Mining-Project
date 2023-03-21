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
library(dplyr)
library(tidyr)

data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv', na.strings = "")
data <- na.omit(data)

##Data Cleaning
for(i in c(2:10,15,21)){
  data[,i] <- as.factor(data[,i])
  print(class(data[,i]))
}

set.seed(2023)
split <- partition(data$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]

##Modelling Decision Tree
set.seed(2023)
train.Model <- rpart(y ~., data=train, method="class")
rpart.plot(train.Model, type=2, extra=4)
Ynew.pred <- predict(train.Model, newdata=test, type="class")

minsplit=20
set.seed(2023)
train.Model2 <- rpart(y~., data=train, method="class",
                      control=rpart.control(minsplit=30, minbucket=round(minsplit/3),
                                            maxdepth = 4, cp=0.01))
rpart.plot(train.Model2, type=2, extra=4)

set.seed(2023)
train.model.full <- rpart(y~., data=train, method="class",
                                 parms=list(split='information'),
                                 cp=-1, minsplit=2, minbucket=1)
printcp(train.model.full)
train_model_full_pruned <- prune(train.model.full, cp=.014)
rpart.plot(train_model_full_pruned)

#Bagging
set.seed(2023)
bagging <- randomForest(y~., data=train, mtry=4, ntree=200)
bagging

predictions_bagging <- predict(bagging, test, "class")
overall_class_rate_bag <- mean(predictions_bagging==test$y)

set.seed(2023)
single_tree <- rpart(y~., data=train)
predictions <- predict(single_tree, test, "class")
overall_class_rate_tree <- mean(predictions==test$y)
print(c(overall_class_rate_bag,overall_class_rate_tree))


bag_df <- data.frame(var = rownames(randomForest::importance(bagging)),
                     MeanDecreaseGini = randomForest::importance(bagging)[, 1]) %>%
  mutate(var = fct_reorder(var, MeanDecreaseGini, median))
bag_ggplot <- ggplot(bag_df, aes(var, MeanDecreaseGini)) +
  geom_point() +
  coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       subtitle = "Bagging", x = NULL, y = "Average decrease in the Gini Index") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        plot.subtitle = element_text(hjust = 0.5))
bag_ggplot

#Random Forest
set.seed(2023)
rf <- randomForest(y~., data=train, ntree=200)
rf

###########################################
rf_df <- data.frame(var = rownames(randomForest::importance(rf)),
                    `Random forest` = randomForest::importance(rf)[,1]) %>%
  left_join(data_frame(var = rownames(randomForest::importance(rf)),
                       Bagging = randomForest::importance(bag)[,1])) %>%
  mutate(var = fct_reorder(var, Bagging, median)) %>%
  gather(model, gini, -var)
rf_ggplot <- ggplot(rf_df,aes(var, gini, color = model, shape=model)) +
  geom_point() + coord_flip() +
  labs(title = "Predicting credit approval on the German Credit Data",
       x = NULL,y = "Average decrease in the Gini Index",
       color = "Method",shape="Method") +
  theme(plot.title = element_text(hjust = 0.5))

val.imputed <- na.roughfix(val)
bagging_prob <- predict(bagging, val.imputed, type="prob")
rf_prob <- predict(rf, val.imputed, type="prob")


#result of bagging and rf
set.seed(2023)
n <- nrow(train)
idx <- sample(1:n, round(0.2*n))
val <- train[idx,]
train <- train[-idx,]

val.imputed <- na.roughfix(val)
bagging_prob <- predict(bagging, val.imputed, type="prob")
rf_prob <- predict(rf, val.imputed, type="prob")

bagging_pred <- prediction(bagging_prob[,2], val$y)
bagging_AUC  <- performance(bagging_pred, "auc")@y.values[[1]]
rf_pred <- prediction(rf_prob[,2], val$y)
rf_AUC  <- performance(rf_pred, "auc")@y.values[[1]]
print(c(bagging_AUC,rf_AUC))









