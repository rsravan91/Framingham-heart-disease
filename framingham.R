setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/Framingham Heart Study/")
framingham=read.csv("framingham.csv")
str(framingham)
# check distribution of target
table(framingham$TenYearCHD)

# split dataset into training and test set
library(caTools)
set.seed(1000)
split=sample.split(framingham$TenYearCHD,SplitRatio = 0.65)
framinghamTrain=subset(framingham,split==TRUE)
framinghamTest=subset(framingham,split==FALSE)

# Build logistic regression using glm function
framinghamLog=glm(formula = TenYearCHD~.,data=framinghamTrain,family = "binomial")
summary(framinghamLog)

# Test the model on test set, with threshold as 0.5
predictTest=predict(framinghamLog,newdata = framinghamTest,type="response")
table(framinghamTest$TenYearCHD,predictTest>0.5)
# This model predicts the CHD very rarely
# Accuracy of this model is (1069+11)/(1069+11+187+6)=0.8483896
# Note that 211 NA's are generated in predictTest
# Accuracy of baseline model by taking output as frequent occuring target
# We get baseline accuracy as (1069+6)/(1069+11+187+6)=0.8444619
# This shows that our model is no better than baseline model
# Let us vary the threshold by checking the ROC curve
library(ROCR)
ROCRPred=prediction(predictions = predictTest,labels = framinghamTest$TenYearCHD)
ROCRPerf=performance(prediction.obj = ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=0.0)
abline(coef = c(0,1),col="red") # Draw line which signifies random prediction
# Let us check the model wwith threshold of 0.2
table(framinghamTest$TenYearCHD,predictTest>0.2)
# Though the precision is about 30% , but the sensitivity is high about 55%, 
# we are less prone to missing out on heart disease patients compared to earlier model


# Finally we calculate AUC
as.numeric(performance(prediction.obj = ROCRPred,"auc")@y.values)

# Summarising the model coefficients also gave an insight that 
# higher smoking,cholestrol,sysBP and glucose causes higher risk of CHD (Cornonary heart disease)
