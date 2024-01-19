rm(list=ls())
cat("\014")
#=============================================================================
#Reading CSV file
df <- read.csv("dcc.csv", stringsAsFactors = FALSE)
summary(df)
#Coverting to dataframe
df <- as.data.frame(df)
#Converting to factor and labelling Sex column
df$SEX <- factor(df$SEX,
                 level = c(1,2),
                 labels = c("Male", 
                            "Female"))
#Data Cleaning Education column
df$EDUCATION[df$EDUCATION == 0] <- 4
df$EDUCATION[df$EDUCATION == 5] <- 4
df$EDUCATION[df$EDUCATION == 6] <- 4
#Converting to factor and labelling Education column
df$EDUCATION <- factor(df$EDUCATION,
                       level = c(1,2,3,4),
                       labels = c("Graduate School",
                                  "University",
                                  "High School",
                                  "Others"))  
#Data cleaning Marriage column
df$MARRIAGE[df$MARRIAGE == 0] <- 3 
#Converting to factor and labelling Marriage column
df$MARRIAGE <- factor(df$MARRIAGE,
                      level = c(1,2,3),
                      labels = c("Married",
                                 "Single",
                                 "Others"))
#Converting default payment column to factor
df$default.payment.next.month <- as.factor(df$default.payment.next.month)
#Converting the Pay_N columns to factor
df[,c(7:12)] <- lapply(df[,c(7:12)],factor)
#==========================================================================
#==============================================================================
#Exploratory Data Analysis
library(ggplot2)

#1.	 Number of Customers Grouped by Education (Bar Plot - Count)
g1 <- ggplot(df, aes(EDUCATION, fill=EDUCATION))
g1+geom_bar(position='dodge', )+ geom_text(stat='count',
                                           aes(label = ..count..),
                                           position = position_dodge(width = 1.0), 
                                           vjust = -0.5)
#2. Limit Balance of Customers by Education (Box Plot)
g2<- ggplot(df, aes(EDUCATION, LIMIT_BAL))
g2 +  geom_boxplot(notch = TRUE, fill = c("light green", "steel blue", "pink", "orange"))
#3. Number of Customers who will default ot not depending on Education
ggplot(df, aes(x= EDUCATION,  group=default.payment.next.month)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="EDUCATION") +facet_grid(~default.payment.next.month) +scale_y_continuous(labels = scales::percent)


#4.Marriage Group BY
g4 <- ggplot(df, aes(MARRIAGE, fill=MARRIAGE)) 
g4 + geom_bar(position="dodge") + geom_text(stat='count', aes(label = ..count..),position = position_dodge(width = 1.0), vjust = -0.5)
#5.Marriage Default Payment
g5 <- ggplot(df, aes(MARRIAGE,fill=default.payment.next.month)) 
g5 + geom_bar(position="dodge", ) + geom_text(stat='count', aes(label = ..count..),position = position_dodge(width = 1.0), vjust = -0.5)
#6.Marriage percentage based on default
ggplot(df, aes(x= MARRIAGE,  group=default.payment.next.month)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Marriage") +facet_grid(~default.payment.next.month) +scale_y_continuous(labels = scales::percent)

#7. Sex vs Default
g7 <- ggplot(df,aes(SEX,fill=SEX))
g7 + geom_bar()  + geom_text(stat='count', aes(label = ..count..),position = position_dodge(width = 1.0), vjust = -0.5)
#8. Sex vsLimit Balance
ggplot(df, aes(x=df$LIMIT_BAL, fill=SEX, facet=SEX)) + geom_histogram(position="identity", alpha=0.5)+
  facet_grid(~df$SEX)+scale_x_continuous(breaks = seq(0, 1000000, by=50000))+theme(axis.text.x = element_text(angle = 45, hjust=1))
#9. Sex vs Default
ggplot(df, aes(x= default.payment.next.month,  group=SEX)) + 
  geom_bar(aes(y = ..prop.., fill = (SEX)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="SEX") +
  facet_grid(~SEX) +
  scale_y_continuous(labels = scales::percent)

#10.Age vs Limit Balance
g10 <- ggplot(df, aes(AGE, LIMIT_BAL))
g10 + geom_point(aes(color=default.payment.next.month)) + geom_smooth(method=lm)
#11. Age vs Default
ggplot(df, aes(x=df$AGE, fill=default.payment.next.month)) + geom_histogram(position="identity", alpha=0.5)+
  scale_x_continuous(breaks = seq(0, 80, by=10)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))



#=========================================================
#Classification
#===============================================================================
#KNN
set.seed(123)   # for reproducible results
train <- sample(1:nrow(df), (0.6)*nrow(df))
train.df <- df[train,]
test.df <- df[-train,]


library(caret)
# Checking distribution of outcome classes -> very few class = "1"
prop.table(table(train.df$default.payment.next.month)) * 100
prop.table(table(test.df$default.payment.next.month)) * 100
prop.table(table(df$default.payment.next.month)) * 100

#ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
ctrl <- trainControl(method="cv", number=10)
# use preProcess to to normalize the predictors
# "center" subtracts the mean; "scale" divides by the standard deviation
knnFit <- train(default.payment.next.month ~ .,
                data = train.df, method = "knn",
                trControl = ctrl, preProcess = c("center","scale"),
                tuneGrid = expand.grid(k = 1:10))

plot(knnFit)

actual <- test.df$default.payment.next.month
knnPredict <- predict(knnFit, test.df)
cm_knn <- table(knnPredict, actual)
cm_knn
# alternative way to get a comprehensive set of statistics
confusionMatrix(knnPredict, actual, positive="1")
# or
Confusion_Matrix_knn <- confusionMatrix(cm_knn, positive = "1") # "yes", "default"
KNN_Accuracy <- Confusion_Matrix_knn$overall['Accuracy']

#====================================================================================
#Logistic Regression

logit.reg <- glm(default.payment.next.month ~ ., 
                 data = train.df, family = "binomial") 
summary(logit.reg)

logitPredict <- predict(logit.reg, test.df, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)

actual_logit <- test.df$default.payment.next.month
predict_logit <- logitPredictClass
cm_logit <- table(predict_logit, actual_logit)
cm_logit
Confusion_Matrix_Logit <- confusionMatrix(as.factor(predict_logit), as.factor(actual_logit))

Logit_Accuracy <- Confusion_Matrix_Logit$overall['Accuracy']
Logit_Accuracy

############ 3. Naive Bayes Classifier ########### 
# install the following package for building naive Bayes classifier
#install.packages("e1071")
library(e1071)

# run naive bayes
fit.nb <- naiveBayes(default.payment.next.month ~ ., 
                     data = train.df)
fit.nb

# Evaluate Performance using Confusion Matrix
actual_nb <- test.df$default.payment.next.month
# predict class probability
Predict_nb <- predict(fit.nb, test.df, type = "raw")
# predict class membership
nbPredictClass <- predict(fit.nb, test.df, type = "class")
cm_nb <- table(nbPredictClass, actual_nb)
cm_nb
# alternative way to get confusion matrix
Confusion_matrix_NB <- confusionMatrix(nbPredictClass, actual_nb, positive="1")


NB_Accuracy <- Confusion_matrix_NB$overall['Accuracy']
NB_Accuracy

#============================================================================

library(ROCit)
library(ROCR)
#ROC KNN

valid_pred_knn <- predict(knnFit,test.df,type = 'prob')
pred_val_knn <- prediction(valid_pred_knn[,2],test.df$default.payment.next.month)
perf_val_knn <- performance(pred_val_knn,'auc')
perf_val_knn
perf_val_knn <- performance(pred_val_knn,'tpr', 'fpr')
plot(perf_val_knn, col = 'blue', lwd = 1.5)


#ROC Logit

valid_pred_logit <- predict(logit.reg,test.df,type = 'response')
pred_val_logit <- prediction(valid_pred_logit,test.df$default.payment.next.month)
perf_val_logit <- performance(pred_val_logit,'auc')
perf_val_logit
perf_val_logit <- performance(pred_val_logit,'tpr', 'fpr')
plot(perf_val_logit, col = 'red', lwd = 1.5, add = 1)


#ROC Naives

predScore_nb <- predict(fit.nb, test.df, type = "raw")
predActual_nb <- test.df$default.payment.next.month
pred_nb <- prediction(Predict_nb[,2], test.df$default.payment.next.month)
perf_nb <- performance(pred_nb, measure='tpr', x.measure='fpr')
plot(perf_nb, col = 'green',colorize=FALSE, add = TRUE)

legend("bottomright", legend = c("KNN", "Logistic", "Naive Bayes"),
       col = c("blue", "red" , "green"),lty=1, cex=1)

#Best ROC

predScore_roc <- predict(logit.reg, test.df, type = "response")
predActual_roc <- test.df$default.payment.next.month

# create ROC curve
# negref: the reference group (either numeric or character type)
roc_empirical <- rocit(score = predScore_roc, class = predActual_roc, negref = 0) 

# check AUC, Cutoff, TPR, FPR(=1-Specificity)
result = data.frame(cbind(AUC=roc_empirical$AUC, Cutoff=roc_empirical$Cutoff, 
                          TPR=roc_empirical$TPR, FPR=roc_empirical$FPR))
head(result)
tail(result)

# find the optimal point (Youden Index point)
result$diff = result$TPR - result$FPR
result[which.max(result[, c("diff")]), ]

# plot ROC 
plot(roc_empirical, values = F, add = TRUE, col = c(2,4))  # default


#====================================================================

Accuracy_df <- c(KNN_Accuracy,Logit_Accuracy,NB_Accuracy)
Accuracy_df <- as.data.frame(Accuracy_df)
Accuracy_df
row.names(Accuracy_df) <- c("KNN Accuracy", "Logistic Accuracy", "Naive Bayes Accuracy")
Accuracy_df