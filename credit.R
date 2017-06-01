setwd("C:/Users/Ajeet Singh/Desktop/minor2/CreditCardFraud").
library(data.table)
library(caret)
library(ggplot2)
library(DMwR)
library(ROSE)
library(e1071)
library(class)
library(gmodels)
library(randomForest)
library(plotly)
library(stats)
library(caTools)
library(C50)
library(gmodels)
library(cluster)
library(fpc)
###  Importing the dataset.
creditcard <- read.csv("creditcard.csv")
### Get the feel of the data
creditcard$Class <- factor(creditcard$Class, levels = c("1", "0"), labels = c("Yes", "No"))
creditcard <- creditcard[sample(nrow(creditcard)),]
table(creditcard$Class)
#Plot the data
ggplot(creditcard,aes(x = creditcard$Class, fill="red")) + 
  geom_bar(position = "dodge", alpha = 0.5, col ="black") +
  scale_x_discrete( name = "Is it Fraud?") +
  scale_y_continuous() + 
  ggtitle("Fraud Case Classes") +
  theme(plot.title = element_text(hjust = 0.5))

### Dividing data into fraud and non fraud classes
fraudData = creditcard[creditcard$Class=="Yes",]
NofraudData = creditcard[creditcard$Class=="No",]

#########################################################################
### New dataset with 25% fraud data
fraud25 = fraudData[1:123,]
dataWith25Fraud = rbind(NofraudData,fraud25)

#Randomize the data for 10% fraud
dataFor25 <- dataWith25Fraud[sample(nrow(dataWith25Fraud)),]
table(dataFor25$Class)

###removing unbalanced classification problemand making data balanced
set.seed(4356)
smote_data <- SMOTE(Class ~ ., data  = dataFor25, perc.over = 300, perc.under = 150, k=5)
new.data <- sample(2, nrow(smote_data), replace = TRUE, prob = c(0.7, 0.3))
trainSplit2 <-smote_data[new.data==1,]
testSplit2 <-smote_data[new.data==2,]
table(trainSplit2$Class)
table(testSplit2$Class)

### Applying SVM algorithm for 25% fraud data
svm.model <- svm(Class ~ ., data = trainSplit2, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, testSplit2)
confusionMatrix(testSplit2$Class, svm.predict)
roc.curve(svm.predict, testSplit2$Class, plotit = T)

## Applying KNN Algorithm
knn.model <- knn(train = trainSplit2[,1:30],
                 test = testSplit2[,1:30],
                 cl = trainSplit2$Class)
plot(knn.model)
table(knn.model, testSplit2$Class)

confusionMatrix(knn.model, testSplit2[,31])
roc.curve(knn.model, testSplit2$Class, plotit = T)

### Applying Random Forest algorithm for 25% fraud data
model.rf <- randomForest(Class ~ ., data =trainSplit2 , ntree = 1000, importance = TRUE)
plot(model.rf)
cv.tree.pred2 <- predict(model.rf, testSplit2)

# Making table of Confusion matrix
CrossTable(cv.tree.pred2, testSplit2$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

confusionMatrix(cv.tree.pred2, testSplit2$Class)
### ploting results
roc.curve(cv.tree.pred2, testSplit2$Class, plotit = T)
x<-rchisq(1000,5,0)
plot_ly(x=cv.tree.pred2,type = 'histogram')

### Applying Logistic Regression Algorithm for 25% fraud data
logist <- glm(Class ~ ., data = trainSplit2, family = "binomial")
summary(logist)
logist_pred <- predict(logist, newdata=testSplit2,type = "response")
plot(logist_pred)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

############################################################################
### New dataset with 50% fraud data
fraud50 = fraudData[1:246,]
dataWith50Fraud = rbind(NofraudData,fraud50)

#Randomize the data for 50% fraud
dataFor50 <- dataWith50Fraud[sample(nrow(dataWith50Fraud)),]
table(dataFor50$Class)

###removing unbalanced classification problemand making data balanced
set.seed(4356)
smote_data <- SMOTE(Class ~ ., data  = dataFor50, perc.over = 300, perc.under = 150, k=5)
new.data <- sample(2, nrow(smote_data), replace = TRUE, prob = c(0.7, 0.3))
trainSplit2 <-smote_data[new.data==1,]
testSplit2 <-smote_data[new.data==2,]
table(trainSplit2$Class)
table(testSplit2$Class)

### Applying SVM algorithm for 50% fraud data
svm.model <- svm(Class ~ ., data = trainSplit2, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, testSplit2)
confusionMatrix(testSplit2$Class, svm.predict)
roc.curve(svm.predict, testSplit2$Class, plotit = T)

## Applying KNN Algorithm
knn.model <- knn(train = trainSplit2[,1:30],
                 test = testSplit2[,1:30],
                 cl = trainSplit2$Class)
plot(knn.model)
table(knn.model, testSplit2$Class)

confusionMatrix(knn.model, testSplit2[,31])
roc.curve(knn.model, testSplit2$Class, plotit = T)

### Applying Random Forest algorithm for 50% fraud data
model.rf <- randomForest(Class ~ ., data =trainSplit2 , ntree = 1000, importance = TRUE)
plot(model.rf)
cv.tree.pred2 <- predict(model.rf, testSplit2)

# Making table of Confusion matrix
CrossTable(cv.tree.pred2, testSplit2$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))
confusionMatrix(cv.tree.pred2, testSplit2$Class)
roc.curve(cv.tree.pred2, testSplit2$Class, plotit = T)
x<-rchisq(1000,5,0)
plot_ly(x=cv.tree.pred2,type = 'histogram')

### Applying Logistic Regression Algorithm for 50% fraud data
logist <- glm(Class ~ ., data = trainSplit2, family = "binomial")
summary(logist)
logist_pred <- predict(logist, newdata=testSplit2,type = "response")
plot(logist_pred)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

############################################################################
### New dataset with 75% fraud data
fraud75 = fraudData[1:369,]
dataWith75Fraud = rbind(NofraudData,fraud75)

#Randomize the data for 75% fraud
dataFor75 <- dataWith75Fraud[sample(nrow(dataWith75Fraud)),]
table(dataFor75$Class)

###removing unbalanced classification problemand making data balanced
set.seed(4356)
smote_data <- SMOTE(Class ~ ., data  = dataFor75, perc.over = 300, perc.under = 150, k=5)
new.data <- sample(2, nrow(smote_data), replace = TRUE, prob = c(0.7, 0.3))
trainSplit2 <-smote_data[new.data==1,]
testSplit2 <-smote_data[new.data==2,]
table(trainSplit2$Class)
table(testSplit2$Class)

### Applying SVM algorithm for 75% fraud data
svm.model <- svm(Class ~ ., data = trainSplit2, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, testSplit2)
confusionMatrix(testSplit2$Class, svm.predict)
roc.curve(svm.predict, testSplit2$Class, plotit = T)

## Applying KNN Algorithm
knn.model <- knn(train = trainSplit2[,1:30],
                 test = testSplit2[,1:30],
                 cl = trainSplit2$Class)
plot(knn.model)
table(knn.model, testSplit2$Class)

confusionMatrix(knn.model, testSplit2[,31])
roc.curve(knn.model, testSplit2$Class, plotit = T)

### Applying Random Forest algorithm for 75% fraud data
model.rf <- randomForest(Class ~ ., data =trainSplit2 , ntree = 1000, importance = TRUE)
plot(model.rf)
cv.tree.pred2 <- predict(model.rf, testSplit2)

# Making table of Confusion matrix
CrossTable(cv.tree.pred2, testSplit2$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))
confusionMatrix(cv.tree.pred2, testSplit2$Class)
roc.curve(cv.tree.pred2, testSplit2$Class, plotit = T)
x<-rchisq(1000,5,0)
plot_ly(x=cv.tree.pred2,type = 'histogram')

### Applying Logistic Regression Algorithm for 75% fraud data
logist <- glm(Class ~ ., data = trainSplit2, family = "binomial")
summary(logist)
logist_pred <- predict(logist, newdata=testSplit2,type = "response")
plot(logist_pred)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

###########################################################################
### New dataset with 100% fraud data
fraud100 = fraudData[1:492,]

dataWith100Fraud = rbind(NofraudData,fraud100)

#Randomize the data for 100% fraud
dataFor100 <- dataWith100Fraud[sample(nrow(dataWith100Fraud)),]
table(dataFor100$Class)

###removing unbalanced classification problemand making data balanced
set.seed(4356)
smote_data <- SMOTE(Class ~ ., data  = dataFor100, perc.over = 300, perc.under = 150, k=5)
new.data <- sample(2, nrow(smote_data), replace = TRUE, prob = c(0.7, 0.3))
trainSplit2 <-smote_data[new.data==1,]
testSplit2 <-smote_data[new.data==2,]
table(trainSplit2$Class)
table(testSplit2$Class)

### Applying SVM algorithm for 100% fraud data
svm.model <- svm(Class ~ ., data = trainSplit2, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, testSplit2)
confusionMatrix(testSplit2$Class, svm.predict)
roc.curve(svm.predict, testSplit2$Class, plotit = T)

## Applying KNN Algorithm
knn.model <- knn(train = trainSplit2[,1:30],
                 test = testSplit2[,1:30],
                 cl = trainSplit2$Class)
plot(knn.model)
table(knn.model, testSplit2$Class)

confusionMatrix(knn.model, testSplit2[,31])
roc.curve(knn.model, testSplit2$Class, plotit = T)

### Applying Random Forest algorithm for 100% fraud data
model.rf <- randomForest(Class ~ ., data =trainSplit2 , ntree = 1000, importance = TRUE)
plot(model.rf)
cv.tree.pred2 <- predict(model.rf, testSplit2)

# Making table of Confusion matrix
CrossTable(cv.tree.pred2, testSplit2$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))
confusionMatrix(cv.tree.pred2, testSplit2$Class)
roc.curve(cv.tree.pred2, testSplit2$Class, plotit = T)
x<-rchisq(1000,5,0)
plot_ly(x=cv.tree.pred2,type = 'histogram')

### Applying Logistic Regression Algorithm for 100% fraud data
logist <- glm(Class ~ ., data = trainSplit2, family = "binomial")
summary(logist)
logist_pred <- predict(logist, newdata=testSplit2,type = "response")
plot(logist_pred)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

#---------------------- RESULTS ----------------------#

#KNN Algorithm results for different fraud percent 
mat <- matrix(c(0.6262,0.5959,0.6527,0.6000,0.6240,0.6311,0.5894,0.6687,0.6159,0.6300,0.6564,0.6222,0.6879,0.6481,0.6560,0.6862,0.6349,0.7339,0.6893,0.6870),ncol=5,byrow = TRUE)
colnames(mat) <- c(" Accuracy"," Sensitivity"," Specificity"," Precision","  AUC")
rownames(mat) <- c(" 25% Fraud"," 50% Fraud"," 75% Fraud","100% Fraud")
res <- as.table(mat)
res

dat <- matrix(res,ncol=4) # make data
matplot(dat, type = c("o"),pch=1,col = 1:4,xlab = "Columns", ylab = "Result", 
        main = "KNN") #plot
legend("bottomright", legend = 1:4, col=1:4, pch=1)

#SVM Algorithm results for different fraud percent 
mat <- matrix(c(0.9617,0.9467,0.9755,0.9726,0.9610,0.9686,0.9700,0.9674,0.9636,0.9690,0.9658,0.9816,0.9523,0.9467,0.9670,0.9612,0.9713,0.9522,0.9474,0.9620),ncol=5,byrow = TRUE)
colnames(mat) <- c(" Accuracy"," Sensitivity"," Specificity"," Precision","  AUC")
rownames(mat) <- c(" 25% Fraud"," 50% Fraud"," 75% Fraud","100% Fraud")
res <- as.table(mat)
res

dat <- matrix(res,ncol=4) # make data
matplot(dat, type = c("o"),pch=1,col = 1:4,xlab = "Columns", ylab = "Result", 
        main = "Support Vector Machine") #plot
legend("bottomright", legend = 1:4, col=1:4, pch=1)


#Random Forest Algorithm results for different fraud percent
mat <-matrix(c(0.9585,0.9452,0.9701,0.9650,0.9590,0.9827,0.9702,0.9940,0.9932,0.9830,0.9755,0.9511,0.9979,0.9977,0.9770,0.9715,0.9498,0.9924,0.9914,0.9730),ncol=5,byrow = TRUE)
colnames(mat) <- c(" Accuracy"," Sensitivity"," Specificity"," Precision","  AUC")
rownames(mat) <- c(" 25% Fraud"," 50% Fraud"," 75% Fraud","100% Fraud")
res <- as.table(mat)
res

dat <- matrix(res,ncol=4) # make data
matplot(dat, type = c("o"),pch=1,col = 1:4,xlab = "Columns", ylab = "Result", 
        main = "Random Forest") #plot
legend("bottomright", legend = 1:4, col=1:4, pch=1)

#Logistic Regression Algorithm results for different fraud percent
mat <-matrix(c(0.9521,0.9810,0.9226,0.9281,0.9520,0.9686,0.9539,0.9862,0.9881,0.9700,0.9370,0.9180,0.9600,0.9651,0.9390,0.9509,0.9315,0.9740,0.9771,0.9530),ncol=5,byrow = TRUE)
colnames(mat) <- c(" Accuracy"," Sensitivity"," Specificity"," Precision","  AUC")
rownames(mat) <- c(" 25% Fraud"," 50% Fraud"," 75% Fraud","100% Fraud")
res <- as.table(mat)
res

dat <- matrix(res,ncol=4) # make data
matplot(dat, type = c("o"),pch=1,col = 1:4,xlab = "Columns", ylab = "Result", 
        main = "Logistic Regration") #plot
legend("bottomright", legend = 1:4, col=1:4, pch=1)

