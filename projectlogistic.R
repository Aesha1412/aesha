#Set working directory
setwd("D:/aesha/DATA SCIENCE/project")

#Read csv files.
lptrain <- read.csv("projecttrain.csv")
lptest <- read.csv("projecttest.csv")
lpvalidate <- read.csv("projectvalidate.csv")

#Explore Training datasets.
str(lptrain)
View(lptrain)
table(is.na(lptrain))
summary(lptrain)
#In this dataset 86NA'S fill it by values.
#Fill missing values in catagorical variable
levels(lptrain$Gender)[1]<- "Male"
summary(lptrain)
levels(lptrain$Married)[1]<- "Yes"
summary(lptrain)
levels(lptrain$Dependents)[1]<- "0"
summary(lptrain)
levels(lptrain$Self_Employed)[1]<- "No"
summary(lptrain)
#Fill missing valus in continuous variable.
lptrain$LoanAmount[is.na(lptrain$LoanAmount)] <- median(lptrain$LoanAmount, na.rm = TRUE)
summary(lptrain)
lptrain$Loan_Amount_Term[is.na(lptrain$Loan_Amount_Term)] <- median(lptrain$Loan_Amount_Term, na.rm = TRUE)
summary(lptrain)
lptrain$Credit_History = as.factor(lptrain$Credit_History)
table(lptrain$Credit_History)
lptrain$Credit_History[is.na(lptrain$Credit_History)] <- "1"
summary(lptrain)

#Explore Test Dataset
str(lptest)
table(is.na(lptest))
summary(lptest)
#In this dataset 40NA'S fill it by values.
#Filling missing values in categorical variable.
levels(lptest$Gender)[1]<- "Male"
summary(lptest)
levels(lptest$Dependents)[1]<- "0"
summary(lptest)
levels(lptest$Self_Employed)[1]<- "No"
summary(lptest)
#Filling missing values in continuous variable.
lptest$Credit_History =as.factor(lptest$Credit_History)
table(lptest$Credit_History)
lptest$Credit_History[is.na(lptest$Credit_History)] <- "1"
summary(lptest)
#Through kNN method.
library(VIM)
data = kNN(lptest)
summary(data)
#Remove extra variable created by kNN imputation.
names(data)
head(data)
data = subset(data, select = Loan_ID:Property_Area)
summary(data)
#Data Cleaning Ends.

#Modeling using logistic regression.
library(caTools)
set.seed(100)
splt = sample.split(lptrain$Loan_Status, 0.7)
train = subset(lptrain, splt==TRUE)
test = subset(lptrain, splt==FALSE)
prop.table(table(lptrain$Loan_Status))
#N         Y 
#0.3127036 0.6872964 
prop.table(table(train$Loan_Status))
#N         Y 
#0.3123543 0.6876457 
prop.table(table(test$Loan_Status))
#N         Y 
#0.3135135 0.6864865 
train$Loan_Status <- ifelse(train$Loan_Status == "Y",1,0)
test$Loan_Status <- ifelse(test$Loan_Status == "Y",1,0)
View(train)
View(test)

#Building the model and checking the summary of model
modellog = glm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed +
                 ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term +
                 Credit_History + Property_Area, data = train, family = "binomial")
summary(modellog)
#AIC: 430.02
cor(train$ApplicantIncome, train$LoanAmount)# 0.5310805
cor(train$CoapplicantIncome, train$LoanAmount) #0.2194625
install.packages("car")
library(car)
vif(modellog)
#For test model.
test$prob = predict(modellog, newdata = test, type = "response")
table(test$Loan_Status)
127/(nrow(test)) # 68%
nrow(test)

table(test$Loan_Status, as.numeric(test$prob >= 0.5))
#Overall accuracy
(31+124)/nrow(test) # 83%

#Sensitivity
(124)/(3+124) # 97%

#Specificity
(31)/(31+27) # 53%

#Area under the curve
install.packages("ROCR")
library(ROCR)
install.packages("gplots")
library(gplots)

pred= prediction(test$prob, test$Loan_Status)
as.numeric(performance(pred, "auc")@y.values) # 78%

#Train data prediction.
predtrain = predict(modellog, type = "response")
rocrpred = prediction(predtrain, train$Loan_Status)
rocrpref = performance(rocrpred, "tpr", "fpr")

#ROC curve.
plot(rocrpref, colorize=TRUE)
plot(rocrpref, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) 
table(test$Loan_Status, as.numeric(test$prob >= 0.35))
#Overall accuracy
(30+125)/nrow(test) # 83%
#Sensityvity
(125)/(2+125) # 97%
#Specificity
(30)/(30+28) # 51%

#MODEL2 FOR TRAIN DATASET.
modeltrain = glm(Loan_Status ~ Gender  + Dependents  + Self_Employed +
                   ApplicantIncome  + LoanAmount + Loan_Amount_Term +
                   Credit_History + Property_Area, data = train, family = "binomial")
summary(modellog) 
#AIC: 430.02
cor(train$ApplicantIncome, train$LoanAmount)# 0.5310805
cor(train$CoapplicantIncome, train$LoanAmount) #0.2194625
vif(modeltrain)
test$prob = predict(modeltrain, newdata = test, type = "response")
table(test$Loan_Status)
127/nrow(test) #68%
nrow(test) #185
table(test$Loan_Status, as.numeric(test$prob >= 0.5))
(30+124)/nrow(test) #Overall accuracy 82%
(124)/(30+124) #Sensitivity 80%
(30)/(30+28) #Specificity 51%
library(ROCR)
library(gplots)
pred= prediction(test$prob, test$Loan_Status)
as.numeric(performance(pred, "auc")@y.values) #78%

#MODEL3 FOR TRAINING DATSET
modeltrain1 = glm(Loan_Status ~ Gender  + Education + Self_Employed +
                 ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term +
                 Credit_History  , data = train, family = "binomial")
summary(modeltrain1) #AIC: 434
vif(modeltrain1)
test$prob = predict(modeltrain1, newdata = test, type = "response")
table(test$Loan_Status)
127/nrow(test) #68%
nrow(test) #185%
table(test$Loan_Status, as.numeric(test$prob >= 0.5))
(30+125)/nrow(test) #Overall accuracy 83%
(125)/(30+125) #Sensitivity 80%
(30)/(30+28) #Specificity 51%
pred= prediction(test$prob, test$Loan_Status)
as.numeric(performance(pred, "auc")@y.values)

#in model1 overall acuuracy:83%, Sensitivity:97%, Specificity:53%, AUC:78%
#in modeltrain overall acuuracy:82%, Sensitivity:80%, Specificity:51%, AUC:78%
#in modeltrain1 overall acuuracy:83%, Sensitivity:80%, Specificity:51%, AUC:73%
#After running this three model model1 is the best model.



# LPTEST DATASET.
modellog1 = glm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + 
               ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + 
               Credit_History + Property_Area, data = test, family="binomial")
summary(modellog1)
cor(test$ApplicantIncome, test$LoanAmount) # 68%
cor(test$CoapplicantIncome, test$LoanAmount) # 10%

library(car)
vif(modellog1)

test$prob = predict(modellog1, newdata=test, type="response")
table(test$Loan_Status)
(127)/nrow(test) # 68%
nrow(test)
table(test$Loan_Status, as.numeric(test$prob >= 0.5))
#Overall acuuracy.
(33+124)/nrow(test) # 84%
#Sensityvity
(124)/(3+124) # 97%
#Specificity
(33)/(33+25) # 56%

#AUC curve.
pred = prediction(test$prob, test$Loan_Status)
as.numeric(performance(pred, "auc")@y.values) # 87%

#Test Data prediction.
predtest = predict(modellog1, type = "response")
rocrpred = prediction(predtest, test$Loan_Status)
rocrpref = performance(rocrpred, "tpr", "fpr")

plot(rocrpref, colorize=TRUE)
plot(rocrpref, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) 
table(test$Loan_Status, as.numeric(test$prob >= 0.35))

#Overall accuracy
(31+125)/nrow(test) # 84%
#Sensityvity
(125)/(2+125) # 98%
#Specificity
(31)/(31+27) # 53%