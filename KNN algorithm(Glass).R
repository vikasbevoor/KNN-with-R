gls <- read.csv("D:/Data science/Assignments docs/KNN/glass.csv")
View(gls)
attach(gls)

# Data exploration
summary(gls)
str(gls)
anyNA(gls)

# Converting interger to factor
gls$Type <- as.factor(gls$Type)
str(gls$Type)

# table of diagnosis
table(Type)

# Table or proportions with more informative labels
round(prop.table(table(Type)) * 100, digits = 1)

# create normalization function
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the data
gls_n <- as.data.frame(lapply(gls[1:9], normalize))

gls_n <- cbind(gls_n,Type)
View(gls_n)

# create training and test data
data <- sample(1:nrow(gls_n), size = nrow(gls_n)*0.7, replace = FALSE)
gls_train <- gls_n[data, ]
gls_test <- gls_n[-data, ]

# create labels for training and test data
gls_train_labels <- gls_n[data,10]
gls_test_labels <- gls_n[-data, 10]

str(gls_train_labels)
str(gls_test_labels)


#Training a model on the data (load the "class" library)
install.packages("class")
library(class)
library(caret)

NROW(gls_train_labels)

# Building models for k=12
gls_test_pred <- knn(train = gls_train, test = gls_test,cl = gls_train_labels, k=12)
gls_test_pred

# Accuracy
Acc <- 100 * sum(gls_test_labels == gls_test_pred)/NROW(gls_test_labels)
Acc

table(gls_test_pred,gls_test_labels)

# Confusion matrix
library(caret)
confusionMatrix(table(gls_test_pred,gls_test_labels))


# Building models for k=13
gls_test_pred1 <- knn(train = gls_train, test = gls_test,cl = gls_train_labels, k=13)
gls_test_pred1

# Accuracy
Acc1 <- 100 * sum(gls_test_labels == gls_test_pred1)/NROW(gls_test_labels)
Acc1

table(gls_test_pred1,gls_test_labels)

# Confusion matrix
library(caret)
confusionMatrix(table(gls_test_pred1,gls_test_labels))

# Creating a loop to check the accuracy for different values of "k"
i=1
k.optm=1
for (i in 1:28) {
  knn.mod <- knn(train = gls_train, test = gls_test, cl = gls_train_labels, k=i)
  k.optm[i] <- 100 * sum(gls_test_labels == knn.mod)/NROW(gls_test_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
}

plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")


# Building models for k=10
gls_test_pred2 <- knn(train = gls_train, test = gls_test,cl = gls_train_labels, k=10)
gls_test_pred2

# Accuracy
Acc1 <- 100 * sum(gls_test_labels == gls_test_pred2)/NROW(gls_test_labels)
Acc1

table(gls_test_pred2,gls_test_labels)

# Confusion matrix
library(caret)
confusionMatrix(table(gls_test_pred2,gls_test_labels))
