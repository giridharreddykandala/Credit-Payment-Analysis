# Credit-Payment-Analysis
# Readme File for Analysis of Credit Card Default Prediction

## Overview

This R code focuses on the analysis of a credit card default prediction dataset. The code includes data cleaning, exploratory data analysis (EDA), and classification using K-Nearest Neighbors (KNN), Logistic Regression, and Naive Bayes algorithms. Additionally, Receiver Operating Characteristic (ROC) curves are generated for each classifier, and accuracy metrics are reported.

## Usage

1. **Data Loading:**
   - Ensure that the dataset file `dcc.csv` is in the same directory as the R script.
   - Use the `read.csv` function to load the dataset.

```
df <- read.csv("dcc.csv", stringsAsFactors = FALSE)
```

2. **Data Cleaning and Preprocessing:**
   - Various data cleaning steps are performed, such as normalizing education levels, handling missing values, and converting variables to factors.

```
# Example: Converting Education column to factor and labeling
df$EDUCATION <- factor(df$EDUCATION, level = c(1,2,3,4), labels = c("Graduate School", "University", "High School", "Others"))
```

3. **Exploratory Data Analysis (EDA):**
   - The code utilizes the `ggplot2` library to create informative visualizations based on different aspects of the dataset, such as education, marriage, sex, and default status.

```
# Example: Number of Customers Grouped by Education (Bar Plot - Count)
g1 <- ggplot(df, aes(EDUCATION, fill=EDUCATION))
g1 + geom_bar(position='dodge') + geom_text(stat='count', aes(label = ..count..), position = position_dodge(width = 1.0), vjust = -0.5)
```

4. **Classification:**
   - The code includes implementation of KNN, Logistic Regression, and Naive Bayes classifiers for predicting credit card default.

```
# Example: KNN Classification
knnFit <- train(default.payment.next.month ~ ., data = train.df, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneGrid = expand.grid(k = 1:10))
```

5. **ROC Curve Generation:**
   - ROC curves are created for each classifier, showcasing their performance.

```
# Example: ROC Curve for KNN
valid_pred_knn <- predict(knnFit, test.df, type = 'prob')
pred_val_knn <- prediction(valid_pred_knn[,2], test.df$default.payment.next.month)
perf_val_knn <- performance(pred_val_knn, 'auc')
plot(perf_val_knn, col = 'blue', lwd = 1.5)
```

6. **Accuracy Metrics:**
   - The code computes and presents accuracy metrics for each classifier.

```
# Example: Displaying Accuracy Metrics
Accuracy_df <- c(KNN_Accuracy, Logit_Accuracy, NB_Accuracy)
Accuracy_df <- as.data.frame(Accuracy_df)
```

## Dependencies

- R version 3.5.0 or higher
- Required R libraries: ggplot2, caret, e1071, ROCR, ROCit
