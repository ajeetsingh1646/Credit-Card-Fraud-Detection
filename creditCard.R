library(data.table)
library(caret)
library(ggplot2)
library(DMwR)
library(ROSE)
library(e1071)
library(ROSE)
library(randomForest)
library(plotly)
library(stats)
library(cluster)
library(caTools)
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

### New dataset with 25% fraud data
fraud25 = fraudData[1:123,]
dataWith25Fraud = rbind(NofraudData,fraud25)
summary(dataWith25Fraud)

#Randomize the data for 10% fraud
dataFor25 <- dataWith25Fraud[sample(nrow(dataWith25Fraud)),]

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
logist_pred <- predict(model2, newdata=testSplit2,type = "response")
plot(logist)
plot(logist_pred)
summary(logist)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

##################################################################
### New dataset with 50% fraud data
fraud50 = fraudData[1:246,]
fraud50
dataWith50Fraud = rbind(NofraudData,fraud50)
summary(dataWith50Fraud)
#Randomize the data for 50% fraud
dataFor50 <- dataWith50Fraud[sample(nrow(dataWith50Fraud)),]

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
logist_pred <- predict(model2, newdata=testSplit2,type = "response")
plot(logist)
plot(logist_pred)
summary(logist)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

############################################################################
### New dataset with 75% fraud data
fraud75 = fraudData[1:369,]
dataWith75Fraud = rbind(NofraudData,fraud75)
summary(dataWith75Fraud)

#Randomize the data for 75% fraud
dataFor75 <- dataWith75Fraud[sample(nrow(dataWith75Fraud)),]

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
logist_pred <- predict(model2, newdata=testSplit2,type = "response")
plot(logist)
plot(logist_pred)
summary(logist)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

################################################
### New dataset with 100% fraud data
fraud100 = fraudData[1:492,]

dataWith100Fraud = rbind(NofraudData,fraud100)
summary(dataWith100Fraud)

#Randomize the data for 100% fraud
dataFor100 <- dataWith100Fraud[sample(nrow(dataWith100Fraud)),]

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
logist_pred <- predict(model2, newdata=testSplit2,type = "response")
plot(logist)
plot(logist_pred)
summary(logist)
pred=rep("Yes",length(logist_pred))
pred[logist_pred > 0.5] = "No"
confusionMatrix(testSplit2$Class, pred)
roc.curve(pred, testSplit2$Class, plotit = T)

table(c("25% Fraud","50% Fraud","75% Fraud","100% Fraud"),
      c("Accuracy","0.9744","0.9796","0.9669","0.9683"))
