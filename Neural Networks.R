#Import library
install.packages("dplyr")
install.packages("knitr")
install.packages("skimr")
install.packages("GGally")
install.packages("neuralnet")
install.packages("NeuralNetTools")
install.packages("splitTools")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("psych")
library(skimr)
library(GGally)
library(neuralnet)
library(NeuralNetTools)
library(splitTools)
library(corrplot)
library(ggplot2)
library(psych)
library(plyr)
library(knitr)
library(dplyr)

##Neural Networks Model##
#Step 1
#read in the data and delete the row with empty values
data <- read.csv("https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv", na.strings = "")
data <- na.omit(data)
str(data)

#split data and data cleaning on training data
set.seed(2023)
split <- partition(data$job, p = c(train = 0.6, valid = 0.2,  test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]

train <- train[,c(-1,-5,-9,-10,-11,-12,-13,-14)]
str(train)
train$job <- as.factor(train$job)
train$marital <- as.factor(train$marital)
train$education <- as.factor(train$education)
train$housing <- as.factor(train$housing)
train$loan <- as.factor(train$loan)
train$contact <- as.factor(train$contact)
train$poutcome <- as.factor(train$poutcome)
train$y <- as.factor(train$y)
colnames(train)[c(8:10,12)] <- c("emp_var_rate","cons_price_idx","cons_conf_idx","nr_employed") #rename some columns to prevent coding error cause by labels.

#normalise the training data
min_max_scale <- function(x){
  (x - min(x)) / (max(x)-min(x))
}
train$emp_var_rate <- min_max_scale(train$emp_var_rate)
train$cons_price_idx <- min_max_scale(train$cons_price_idx)
train$cons_conf_idx <- min_max_scale(train$cons_conf_idx)
train$euribor3m <- min_max_scale(train$euribor3m)
train$nr_employed <- min_max_scale(train$nr_employed)

#Step 2
#convert categorical variables in training data into "dummy" variables
sub_matrix <- model.matrix(~job+marital+education+housing+loan+contact
                           +poutcome+emp_var_rate+cons_price_idx+cons_conf_idx
                           +euribor3m+nr_employed, data = train)
sub_matrix_final <- sub_matrix[,c(-1,-17)] #exclude 'illiterate' from the matrix since it only appears in the train dataset 

head(sub_matrix_final,4) #sanity check of the matrix

train$y <- as.integer(train$y)-1

colnames(sub_matrix_final)[c(1,6)] <- c("jobblue_collar","jobself_employed")
predictor_list <- paste(colnames(sub_matrix_final),collapse = "+")
predictor_list
f <- paste(c("train$y~",predictor_list),collapse = "")
f

##Step 3
#Build a neural network with a single hidden layer and 3 double hidden layers model, 
#any number of hidden nodes, and the logistic function as the activation function. 
set.seed(19)
nn_sub_one_layer <- neuralnet(f, data=sub_matrix_final, hidden=c(5), err.fct="ce", linear.output=FALSE, 
                              likelihood=TRUE,threshold = 0.1)
nn_sub_two_layers_1 <- neuralnet(f, data=sub_matrix_final, hidden=c(4,1), err.fct="ce", linear.output=FALSE, 
                                 likelihood=TRUE,threshold = 0.1)

nn_sub_two_layers_2 <- neuralnet(f, data=sub_matrix_final, hidden=c(1,4), err.fct="ce", linear.output=FALSE, 
                                likelihood=TRUE,threshold = 0.1)
  
nn_sub_two_layers_3 <- neuralnet(f, data=sub_matrix_final, hidden=c(5,3), err.fct="ce", linear.output=FALSE, 
                                likelihood=TRUE,threshold = 0.1)

#Produce a bar plot comparing all built models.
Class_NN_ICs <- tibble('Network' = rep(c("NN_1L","NN_2L_1",
                                         "NN_2L_2", "NN_2L_3"), each = 3),
                       'Metric' = rep(c('AIC', 'BIC', 'CE Loss'), length.out=12),
                       'Value' = c(nn_sub_one_layer$result.matrix[4,1],
                                   nn_sub_one_layer$result.matrix[5,1],
                                   nn_sub_one_layer$result.matrix[1,1],
                                   nn_sub_two_layers_1$result.matrix[4,1],
                                   nn_sub_two_layers_1$result.matrix[5,1],
                                   nn_sub_two_layers_1$result.matrix[1,1],
                                   nn_sub_two_layers_2$result.matrix[4,1],
                                   nn_sub_two_layers_2$result.matrix[5,1],
                                   nn_sub_two_layers_2$result.matrix[1,1],
                                   nn_sub_two_layers_3$result.matrix[4,1],
                                   nn_sub_two_layers_3$result.matrix[5,1],
                                   nn_sub_two_layers_3$result.matrix[1,1]))
nn_ggplot <- Class_NN_ICs %>%
  ggplot(aes(Network, Value, fill=Metric)) +
  geom_col(position = 'dodge') +
  ggtitle("AIC, BIC, and cross entropy loss of the neural networks")
nn_ggplot

plot(nn_sub_two_layers_3)

##Double check the previous comments, and find out which neural network is the one 
#with smallest value for AIC, BIC and the cross entropy loss function, by using the which.min command.
which.min(c(nn_sub_one_layer$result.matrix[4,1],
            nn_sub_two_layers_1$result.matrix[4,1],
            nn_sub_two_layers_2$result.matrix[4,1],
            nn_sub_two_layers_3$result.matrix[4,1])) #aic 4
which.min(c(nn_sub_one_layer$result.matrix[5,1],
            nn_sub_two_layers_1$result.matrix[5,1],
            nn_sub_two_layers_2$result.matrix[5,1],
            nn_sub_two_layers_3$result.matrix[5,1])) #bic 3
which.min(c(nn_sub_one_layer$result.matrix[1,1],
            nn_sub_two_layers_1$result.matrix[1,1],
            nn_sub_two_layers_2$result.matrix[1,1],
            nn_sub_two_layers_3$result.matrix[1,1])) #error 4

#Step 4
#---------------------------------------------#
#To predict on the test data, we need to first 
#clean the data as for the training data.
#---------------------------------------------#
test <- test[,c(-1,-5,-9,-10,-11,-12,-13,-14)]
str(test)
test$job <- as.factor(test$job)
test$marital <- as.factor(test$marital)
test$education <- as.factor(test$education)
test$housing <- as.factor(test$housing)
test$loan <- as.factor(test$loan)
test$contact <- as.factor(test$contact)
test$poutcome <- as.factor(test$poutcome)
test$y <- as.factor(test$y)
colnames(test)[c(8:10,12)] <- c("emp_var_rate","cons_price_idx","cons_conf_idx","nr_employed")

#Normalise the test data
test$emp_var_rate <- min_max_scale(test$emp_var_rate)
test$cons_price_idx <- min_max_scale(test$cons_price_idx)
test$cons_conf_idx <- min_max_scale(test$cons_conf_idx)
test$euribor3m <- min_max_scale(test$euribor3m)
test$nr_employed <- min_max_scale(test$nr_employed)

#convert categorical variables in test data into "dummy" variables
test_sub_matrix <- model.matrix(~job+marital+education+housing+loan+contact
                           +poutcome+emp_var_rate+cons_price_idx+cons_conf_idx
                           +euribor3m+nr_employed, data = test)
test_final_sub_matrix <- test_sub_matrix[,-1]

head(test_final_sub_matrix,4) #sanity check of the matrix

test$y <- as.integer(test$y)-1

colnames(test_final_sub_matrix)[c(1,6)] <- c("jobblue_collar","jobself_employed")

#Prediction
test_pred <- predict(nn_sub_two_layers_3,test_final_sub_matrix)
table(test$y,test_pred>0.5)




