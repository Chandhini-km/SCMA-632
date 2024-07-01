library(DataExplorer)
library(dplyr)
#pima diabetics 
dia<-read.csv("C:\\Users\\Chand\\Downloads\\pima-diabetes.csv")
dia
summary(dia)
plot_missing(dia)
sum(is.na(dia))
head(dia)
tail(dia)
names(dia)
str(dia)

# Replace missing values with the mean for numeric columns
dia <- dia %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#Checking missing values after filling it with mean values of the column 
missing_info <- colSums(is.na(dia))
cat("Missing Values Information:\n")
print(missing_info)

#performing logistic regression , validate assumptions , and evaluating the performance with a confusion matrix
#and ROC curve and interpreting the results 

cor_matrix<-cor(dia)
print(cor_matrix)
heatmap(cor_matrix)
boxplot(Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age~Outcome,data=dia)
install.packages("caTools")
install.packages("MLmetrics")

library(dplyr)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(MLmetrics)

# Logistic Regression
set.seed(123)
split<-sample.split(dia$Outcome,SplitRatio = 0.7)
train<-subset(dia,split==TRUE)
test<-subset(dia,split==FALSE)
model<-glm(Outcome~.,data=train,family=binomial)
predicted_probs<-predict(model,newdata=test,type="response")
predicted_class<-ifelse(predicted_probs>=0.5,1,0)

# Confusion Matrix 
CM<-ConfusionMatrix(factor(predicted_class),factor(test$Outcome))
print(CM)
roc_obj<-roc(test$Outcome,predicted_probs)
auc<-auc(roc_obj)
print(paste("AUC-ROC:",auc))
plot(roc_obj,main="ROC Curve",print.auc=TRUE)

#decision tree analysis for the data in part A and compare the results of the 
#Logistic regression and Decision tree
library(stats)
install.packages("rpart")
library(rpart)
install.packages("caTools")
library(caTools)



set.seed(123)
split<-sample.split(dia$Outcome,SplitRatio = 0.7)
train<-subset(dia,split == TRUE)
test<-subset(dia,split == FALSE)
model<-rpart(Outcome~.,data=train,method="class")
predicted_probs<-predict(model,newdata=test,type="prob")
predicted_class<-ifelse(predicted_probs[,2]>=0.5,1,0)

ConfM<-ConfusionMatrix(factor(predicted_class),factor(test$Outcome))
print(ConfM)-
roc_obj<-roc(test$Outcome,predicted_probs[,2])
auc<-auc(roc_obj)
print(paste("AUC-ROC:",auc))
plot(roc_obj,main="ROC Curve",print.auc=TRUE)

