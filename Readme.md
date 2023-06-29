# Predict the default payment result
The dataset is from a public dataset.

## Characteristics

This project is completed using MATLAB with R

This project uses **SMOTE** oversampling method to deal with the imbalance of the dataset. 
Various data mining algorithms, from knn to lightGBM algorithms, are also utilized to complete the project.

## Description
Credit Card applicants’ qualification decision is crucial for lending institutions to 
reduce their credit card default risk. In this research, our group tries to analyze 
some cases of default payments in Taiwan by some features including the amount 
of given credit, gender, education level, marital status, etc. During the training 
process, some of the algorithms utilize **SMOTE** method to balance the number of 
cases whether the repayment is in default or not. The multilayer perceptron (MLP) 
and Bagging methods showed relatively good prediction ability, and the MLP with 
a hidden layer structure of 5-3 achieved the highest F1 score (0.5335). Except for 
these algorithms, **lightGBM**, **Naïve Bayes** classification, **SVM**, **Random Forest**, 
**Decision Tree**, and **KNN** have also been tried using 10-fold cross-validation 
evaluation. The experimental results show that MLP achieved the best performance 
under the F1 score and the training time required for lightGBM to achieve similar 
predictions is shorter and the F1 value is only 1.4% lower than that of MLP.

