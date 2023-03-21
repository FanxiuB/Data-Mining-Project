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

##1.Read in the data and perform exploratory analysis. 
#read in the data and delete the row with empty values
data <- read.csv("https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv", na.strings = "")
data <- na.omit(data)
str(data)

#split data and data cleaning
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
train$y <- revalue(train$y, c("0"="no","1"="yes"))
colnames(train)[c(8:10,12)] <- c("emp_var_rate","cons_price_idx","cons_conf_idx","nr_employed")

#Normalise the training data
min_max_scale <- function(x){
  (x - min(x)) / (max(x)-min(x))
}
train$emp_var_rate <- min_max_scale(train$emp_var_rate)
train$cons_price_idx <- min_max_scale(train$cons_price_idx)
train$cons_conf_idx <- min_max_scale(train$cons_conf_idx)
train$euribor3m <- min_max_scale(train$euribor3m)
train$nr_employed <- min_max_scale(train$nr_employed)

#convert categorical variables into "dummy" variables
sub_matrix <- model.matrix(~job+marital+education+housing+loan+contact
                           +poutcome+emp_var_rate+cons_price_idx+cons_conf_idx
                           +euribor3m+nr_employed, data = train)
sub_matrix_final <- sub_matrix[,-1]

head(sub_matrix_final,4) #sanity check of the matrix

train$y <- as.integer(train$y)-1

colnames(sub_matrix_final)[c(1,6)] <- c("jobblue_collar","jobself_employed")
predictor_list <- paste(colnames(sub_matrix_final),collapse = "+")
predictor_list
f <- paste(c("train$y~",predictor_list),collapse = "")
f

##3.Build a neural network with a single hidden layer, any number of hidden nodes, and the logistic 
#function as the activation function. Interpret the relative importance of variables using the garson function.
set.seed(19)
nn_sub_one_layer <- neuralnet(f, data=sub_matrix_final, hidden=c(5), err.fct="ce", linear.output=FALSE, 
                              likelihood=TRUE,threshold = 0.1)
nn_sub_two_layers_1 <- neuralnet(f, data=sub_matrix_final, hidden=c(4,1), err.fct="ce", linear.output=FALSE, 
                                 likelihood=TRUE,threshold = 0.1)

nn_sub_two_layer_2 <- neuralnet(f, data=sub_matrix_final, hidden=c(1,4), err.fct="ce", linear.output=FALSE, 
                                likelihood=TRUE,threshold = 0.1)

nn_sub_two_layer_3 <- neuralnet(f, data=sub_matrix_final, hidden=c(5,3), err.fct="ce", linear.output=FALSE, 
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
                                   nn_sub_two_layer_2$result.matrix[4,1],
                                   nn_sub_two_layer_2$result.matrix[5,1],
                                   nn_sub_two_layer_2$result.matrix[1,1],
                                   nn_sub_two_layer_3$result.matrix[4,1],
                                   nn_sub_two_layer_3$result.matrix[5,1],
                                   nn_sub_two_layer_3$result.matrix[1,1]))
nn_ggplot <- Class_NN_ICs %>%
  ggplot(aes(Network, Value, fill=Metric)) +
  geom_col(position = 'dodge') +
  ggtitle("AIC, BIC, and cross entropy loss of the neural networks")
nn_ggplot

#smaller value of AIC and BIC indicates a better model.
#In terms of AIC the forth neural network with 2 hidden layer and 5 and 3
#nodes in each layer seemed to be best, while BIC preferred the third
#neural network with 2 hidden layers and 1 and 4 nodes in each layer. The 
#cross entropy loss agreed with AIC so we would probably choose the fourth 
#neural network. This is an example of one of the many times where information
#criteria give different answer since they penalise complexity in different ways.


##Double check the preious comments, and find out which neural network is the one 
#with smallest value for the cross entropy loss function, by using the which.min command.
which.min(c(nn_sub_one_layer$result.matrix[4,1],
            nn_sub_two_layers_1$result.matrix[4,1],
            nn_sub_two_layer_2$result.matrix[4,1],
            nn_sub_two_layer_3$result.matrix[4,1]))
##aic 4
which.min(c(nn_sub_one_layer$result.matrix[5,1],
            nn_sub_two_layers_1$result.matrix[5,1],
            nn_sub_two_layer_2$result.matrix[5,1],
            nn_sub_two_layer_3$result.matrix[5,1]))
##bic 3
which.min(c(nn_sub_one_layer$result.matrix[1,1],
            nn_sub_two_layers_1$result.matrix[1,1],
            nn_sub_two_layer_2$result.matrix[1,1],
            nn_sub_two_layer_3$result.matrix[1,1]))
##error 4






garson(nn_subscribe)

#
set.seed(20)
nn_subscribe <- neuralnet(f, data=sub_matrix_final, hidden=c(5), err.fct="ce", linear.output=FALSE, 
                          rep = 5, likelihood=TRUE,threshold = 0.1)

plot(nn_subscribe, rep = "best")

#plots (plot(nn_di)), we can see that the optimisation algorithm stops at different iterations (from Steps) 
#and lead to different cross-entropy loss (from Error). In general, training the network longer decreases 
#the cross-entropy loss. However, this decrease takes place on the training data set and may not generalise
#to the test. In other words, training the model longer may increase the risk of overfitting.

#To select the optimal model, we could look at the AIC and BIC values.
which.min(nn_subscribe$result.matrix[4,]) #AIC
which.min(nn_subscribe$result.matrix[5,]) #BIC
#AIC and BIC agree in this case and they both choose the first repetition.



