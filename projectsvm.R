#Set working directory.
setwd("D:/aesha/DATA SCIENCE/project")

#Read csv files.
train <- read.csv("projecttrain.csv")
test <- read.csv("projecttest.csv")
validate <- read.csv("projectvalidate.csv")

dim(train)
dim(test)
dim(validate)

#Join dataframes by rbind function.
test$Loan_Status = NA
x_all = rbind(train,test)
summary(x_all)

#Filling the missing values.
table(x_all$Gender)
levels(x_all$Gender)[levels(x_all$Gender)==""]= "Male"
summary(x_all)

table(x_all$Married)
levels(x_all$Married)[levels(x_all$Married)==""] = "Yes"
summary(x_all)

table(x_all$Dependents)
levels(x_all$Dependents)[levels(x_all$Dependents)==""] = "0"
summary(x_all)

table(x_all$Self_Employed)
levels(x_all$Self_Employed)[levels(x_all$Self_Employed)==""] = "No"
summary(x_all)

#Remove outliers.
table(is.na(x_all$ApplicantIncome))
boxplot(x_all$ApplicantIncome)
summary(x_all$ApplicantIncome)
x=(5516+(1.5*IQR(x_all$ApplicantIncome))) # 9477.5
x
x_all$ApplicantIncome[x_all$ApplicantIncome>=x] = mean(x_all$ApplicantIncome)
boxplot(x_all$ApplicantIncome)

table(is.na(x_all$CoapplicantIncome))
boxplot(x_all$CoapplicantIncome) 
summary(x_all$CoapplicantIncome)
x1 = (2365+(1.5*IQR(x_all$CoapplicantIncome))) # 5912.5 
x1
x_all$CoapplicantIncome[x_all$CoapplicantIncome>=x1] = mean(x_all$CoapplicantIncome)
boxplot(x_all$CoapplicantIncome)

table(is.na(x_all$LoanAmount))
summary(x_all$LoanAmount)
x_all$LoanAmount[is.na(x_all$LoanAmount)] = median(x_all$LoanAmount,na.rm = T)
boxplot(x_all$LoanAmount)
summary(x_all$LoanAmount)
x3 = (160+(1.5*IQR(x_all$LoanAmount))) # 248.5
x3
x_all$LoanAmount[x_all$LoanAmount>=x3] = median(x_all$LoanAmount)
boxplot(x_all$LoanAmount)

summary(x_all$Loan_Amount_Term)
table(x_all$Loan_Amount_Term)
x_all$Loan_Amount_Term[is.na(x_all$Loan_Amount_Term)] = 360
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term %in%c("6","12","36","60")]="1-5 yrs"
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term %in%c("300","350","360")]="360"
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term %in%c("120","84")]="120"
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term %in%c("180")]="180"
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term %in%c("240")]="240"
x_all$Loan_Amount_Term[x_all$Loan_Amount_Term %in%c("480")]="480"
x_all$Loan_Amount_Term=as.factor(x_all$Loan_Amount_Term)

table(x_all$Credit_History)
x_all$Credit_History[is.na(x_all$Credit_History)] = 1
x_all$Credit_History= is.factor(x_all$Credit_History)
summary(x_all$Credit_History)

table(x_all$Property_Area)

#Create train and test data from the merge data.
x_train = x_all[1:nrow(train),]
x_test = x_all[-(1:nrow(train)),]

x_train$Loan_ID= NULL
x_test$Loan_ID=NULL
x_test$Loan_Status=NULL

#START SUPPORT VECTOR MACHINE.

install.packages("e1071")
library(rpart)
library(caret)
library(e1071)
install.packages("heuristica")
library(heuristica)

#Build the model using kernal radial.
model1 = svm(x_train$Loan_Status ~ . , data = x_train, kernel= "radial")
summary(model1) #gamma: 0.05263158, supportvectors: 430 (238,192), levels: yes,no.
y_pred = predict(model1, newdata = x_test)
confusionMatrix(y_pred, validate$outcome)

#Confusion Matrix and Statistics

#Reference
#Prediction   N   Y
#N   0   0
#Y  77 290

#Accuracy : 0.7902          
#95% CI : (0.7449, 0.8307)
#No Information Rate : 0.7902          
#P-Value [Acc > NIR] : 0.5305          

#Kappa : 0               
#Mcnemar's Test P-Value : <2e-16          

#Sensitivity : 0.0000          
#Specificity : 1.0000          
#Pos Pred Value :    NaN          
#Neg Pred Value : 0.7902          
#Prevalence : 0.2098          
#Detection Rate : 0.0000          
#Detection Prevalence : 0.0000          
#Balanced Accuracy : 0.5000          

#'Positive' Class : N               

attributes(model1)
model1$fitted # levels: Y N
table(model1$fitted) # n:0 Yes:614.
confusionMatrix(model1$fitted, x_train$Loan_Status)

#Confusion Matrix and Statistics

#Reference
#Prediction   N   Y
#N   0   0
#Y 192 422

#Accuracy : 0.6873         
#95% CI : (0.649, 0.7238)
#No Information Rate : 0.6873         
#P-Value [Acc > NIR] : 0.5195         

#Kappa : 0              
#Mcnemar's Test P-Value : <2e-16         

#Sensitivity : 0.0000         
#Specificity : 1.0000         
#Pos Pred Value :    NaN         
#Neg Pred Value : 0.6873         
#Prevalence : 0.3127         
#Detection Rate : 0.0000         
#Detection Prevalence : 0.0000         
#Balanced Accuracy : 0.5000         

#'Positive' Class : N     

#Build other model using kernal linear.
model2 = tune(svm, Loan_Status ~ ., data = x_train, kernel = "linear", 
              ranges=list(cost=c(2,4,20,10,100)),
              gamma=c(0.2,0.25,0.05,0.07))
summary(model2)
#Parameter tuning of 'svm':
  
#  - sampling method: 10-fold cross validation 

#- best parameters:
  #cost
#2

#- best performance: 0.3126917 

#- Detailed performance results:
  #cost     error dispersion
#1    2 0.3126917 0.06790632
#2    4 0.3126917 0.06790632
#3   20 0.3126917 0.06790632
#4   10 0.3126917 0.06790632
#5  100 0.3126917 0.06790632

attributes(model2)
model2$best.model
#Parameters:
  #SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  2 
#gamma:  0.2 0.25 0.05 0.07 

#Number of Support Vectors:  447
confusionMatrix(model2$best.model$fitted, x_train$Loan_Status)
#Confusion Matrix and Statistics

#Reference
#Prediction   N   Y
#N   9   6
#Y 183 416

#Accuracy : 0.6922         
#95% CI : (0.654, 0.7285)
#No Information Rate : 0.6873         
#P-Value [Acc > NIR] : 0.4159         

#Kappa : 0.0436         
#Mcnemar's Test P-Value : <2e-16         

#Sensitivity : 0.04688        
#Specificity : 0.98578        
#Pos Pred Value : 0.60000        
#Neg Pred Value : 0.69449        
#Prevalence : 0.31270        
#Detection Rate : 0.01466        
#Detection Prevalence : 0.02443        
#Balanced Accuracy : 0.51633        

#'Positive' Class : N   

#Model3

model3 = tune(svm, Loan_Status ~ . , data = x_train, kernal = "radial",
              ranges=list(cost=c(2^(2:9)),gamma=seq(0,1,0.1)))
model3$best.model
model3$best.model$fitted #levels: Y N
table(model3$best.model$fitted) # N:0 Y:614

confusionMatrix(model3$best.model$fitted,x_train$Loan_Status)
#Confusion Matrix and Statistics

#Reference
#Prediction   N   Y
#N   0   0
#Y 192 422

#Accuracy : 0.6873         
#95% CI : (0.649, 0.7238)
#No Information Rate : 0.6873         
#P-Value [Acc > NIR] : 0.5195         

#Kappa : 0              
#Mcnemar's Test P-Value : <2e-16         

#Sensitivity : 0.0000         
#Specificity : 1.0000         
#Pos Pred Value :    NaN         
#Neg Pred Value : 0.6873         
#Prevalence : 0.3127         
#Detection Rate : 0.0000         
#Detection Prevalence : 0.0000         
#Balanced Accuracy : 0.5000         

#'Positive' Class : N      
              