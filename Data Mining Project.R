##Import library
#install.packages("splitTools")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("GGally")
library(splitTools)
library(corrplot)
library(ggplot2)
library(GGally)

##Import data
data <- read.csv('https://raw.githubusercontent.com/FanxiuB/Data-Mining-Project/main/DM-group_12.csv')
dim(data)

##Split data
set.seed(2023)
split <- partition(data$age, p = c(train = 0.6, valid = 0.2, test = 0.2))
train <- data[split$train, ]
valid <- data[split$valid, ]
test <- data[split$test, ]

##Exploratory Analysis on the training data set
dim(train)
summary(train)

train_num <- train[,c(1,11:14,16:21)]
train_cate <- train[,c(2:10,15,21)]

#Categorical Variables
barplot(table(train_cate$y))
barplot(table(train_cate$job))
pie(table(train_cate$marital))
barplot(table(train_cate$education))
pie(table(train_cate$default))
pie(table(train_cate$housing))
pie(table(train_cate$loan))
pie(table(train_cate$contact))
barplot(table(train_cate$month))
barplot(table(train_cate$day_of_week))
pie(table(train_cate$poutcome))

#ggplot2
y.freq <- table(train_cate$y)
y.freq <- as.data.frame(y.freq)
colnames(y.freq) <- c("y", "number")
ggplot(data = y.freq, mapping = aes(x = y, y = number, fill=y)) +
  geom_col()

job.y <- as.data.frame(table(train_cate$y, train_cate$job))
colnames(job.y) <- c("y","job","number")
ggplot(data = job.y, mapping = aes(x = job, y = number, fill=y)) +
  geom_col()

marital.y <- as.data.frame(table(train_cate$y, train_cate$marital))
colnames(marital.y) <- c("y","marital","number")
ggplot(data = marital.y, mapping = aes(x = marital, y = number, fill=y)) +
  geom_col()

education.y <- as.data.frame(table(train_cate$y, train_cate$education))
colnames(education.y) <- c("y","education","number")
ggplot(data = education.y, mapping = aes(x = education, y = number, fill=y)) +
  geom_col()

default.y <- as.data.frame(table(train_cate$y, train_cate$default))
colnames(default.y) <- c("y","default","number")
ggplot(data = default.y, mapping = aes(x = default, y = number, fill=y)) +
  geom_col()

housing.y <- as.data.frame(table(train_cate$y, train_cate$housing))
colnames(housing.y) <- c("y","housing","number")
ggplot(data = housing.y, mapping = aes(x = housing, y = number, fill=y)) +
  geom_col()

loan.y <- as.data.frame(table(train_cate$y, train_cate$loan))
colnames(loan.y) <- c("y","loan","number")
ggplot(data = loan.y, mapping = aes(x = loan, y = number, fill=y)) +
  geom_col()

contact.y <- as.data.frame(table(train_cate$y, train_cate$contact))
colnames(contact.y) <- c("y","contact","number")
ggplot(data = contact.y, mapping = aes(x = contact, y = number, fill=y)) +
  geom_col()

month.y <- as.data.frame(table(train_cate$y, train_cate$month))
colnames(month.y) <- c("y","month","number")
ggplot(data = month.y, mapping = aes(x = month, y = number, fill=y)) +
  geom_col()

day_of_week.y <- as.data.frame(table(train_cate$y, train_cate$day_of_week))
colnames(day_of_week.y) <- c("y","day_of_week","number")
ggplot(data = day_of_week.y, mapping = aes(x = day_of_week, y = number, fill=y)) +
  geom_col()

poutcome.y <- as.data.frame(table(train_cate$y, train_cate$poutcome))
colnames(poutcome.y) <- c("y","poutcome","number")
ggplot(data = poutcome.y, mapping = aes(x = poutcome, y = number, fill=y)) +
  geom_col()


#Numerical Variables
boxplot(train_num)

ggplot(data = train_num, mapping = aes(x = age, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = age, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "age") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = duration, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = duration, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "duration") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = campaign, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = campaign, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "campaign") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = pdays, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = pdays, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "pdays") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = previous, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = previous, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "previous") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = emp.var.rate, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = emp.var.rate, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "emp.var.rate") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = cons.price.idx, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = cons.price.idx, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "cons.price.idx") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = cons.conf.idx, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = cons.conf.idx, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "cons.conf.idx") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = euribor3m, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = euribor3m, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "euribor3m") +
  theme(legend.position = "none")

ggplot(data = train_num, mapping = aes(x = nr.employed, fill=y)) +
  geom_histogram(bins = 60, color = "white")
ggplot(data = train_num, aes(x = y, y = nr.employed, fill = y)) +
  geom_boxplot() +
  labs(x = "y", y = "nr.employed") +
  theme(legend.position = "none")

#Correlation between numerical variables
corr <- cor(train_num[,-11])
corrplot(corr, tl.col='black', type='upper')

ggpairs(train_num)
ggpairs(train_cate)








