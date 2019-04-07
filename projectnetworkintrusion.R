#set working directory.
setwd("D:/aesha/DATA SCIENCE/project")

#Read csv files.
ntrain <- read.csv("projectnettrain.csv")
ntest <- read.csv("projectnettest.csv")
nvalidate <- read.csv("projectnetvalidate.csv")

dim(ntrain)
dim(ntest)
dim(nvalidate)
#in test data 41 coloumns so we add one coloumn in ntest data.
ntest$class = NA
dim(ntest)

#now combine the both data set.
xall = rbind(ntrain, ntest)

#check NA values.
table(is.na(xall))
summary(xall) #no NA in dataset.

#verify incomplete row through command.
nrow(ntrain[!complete.cases(ntrain),]) #0
nrow(ntest[!complete.cases(ntest),]) #22544
nrow(nvalidate[!complete.cases(nvalidate),]) #0
nrow(xall[!complete.cases(xall),]) #22544

#Put variable which convert into factor in vector column.
cols=c("land","wrong_fragment","urgent","num_failed_logins",
       "logged_in","root_shell","su_attempted",
       "num_shells","is_guest_login")
#using loop covert into factor
for (i in cols) {
  xall[,i]=as.factor(xall[,i])
}

#Removing column which have 0 values
table(xall$num_outbound_cmds) # 0, 47736
xall$num_outbound_cmds=NULL

levels(xall$service)
#67 variable and merge same variable to reduce the size.
levels(xall$service)[levels(xall$service)%in%c("domain","domain_u")]="domain"
levels(xall$service)[levels(xall$service)%in%c("echo","eco_i")]="echo"
levels(xall$service)[levels(xall$service)%in%c("ftp","ftp_data","tftp_u")]="ftp"
levels(xall$service)[levels(xall$service)%in%c("http","http_443","http_8001")]="http"
levels(xall$service)[levels(xall$service)%in%c("netbios_dgm","netbios_ns","netbios_ssn")]="netbios"
levels(xall$service)[levels(xall$service)%in%c("pop_2","pop_3")]="pop"
levels(xall$service)[levels(xall$service)%in%c("tim_i","time")]="time"
levels(xall$service)[levels(xall$service)%in%c("uucp","uucp_path")]="uucp"
levels(xall$service)[levels(xall$service)%in%c("shell","ssh","kshell")]="shell"
levels(xall$service)[levels(xall$service)%in%c("login","klogin")]="login"

levels(xall$service) #after reducing: 50 variables.

#Reduce count of factor.
table(xall$service)
levels(xall$service)[levels(xall$service)%in%c("red_i","urh_i")]="other"

#merging similar flag.
levels(xall$flag)
table(xall$flag)

levels(xall$flag)[levels(xall$flag)%in%c("S0","S1","S2","S3")]="S0"
levels(xall$flag)[levels(xall$flag)%in%c("RSTO","RSTOS0")]="RSTO"
levels(xall$flag)[levels(xall$flag)%in%c("OTH","SF")]="SF"

x_train=xall[1:nrow(ntrain),]
x_test=xall[-(1:nrow(ntrain)),]
x_test$class=NULL

#Building the model for decision tree.
install.packages("rpart")
library(rpart)
model=rpart(class~.,x_train,method = 'class')
summary(model)

library(caret)
pred1=predict(model,x_test,type = "class")
length(pred1) #22544
library(heuristica)
confusionMatrix(pred1, nvalidate$class)

#Confusion Matrix and Statistics

#Reference
#Prediction anomaly normal
#anomaly    9220    336
#normal     3613   9375

#Accuracy : 0.8248          
#95% CI : (0.8198, 0.8298)
#No Information Rate : 0.5692          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.6569          
#Mcnemar's Test P-Value : < 2.2e-16       

#Sensitivity : 0.7185          
#Specificity : 0.9654          
#Pos Pred Value : 0.9648          
#Neg Pred Value : 0.7218          
#Prevalence : 0.5692          
#Detection Rate : 0.4090          
#Detection Prevalence : 0.4239          
#Balanced Accuracy : 0.8419          

#'Positive' Class : anomaly      

plot(model,margin = 0.1)
text(model,use.n = TRUE,pretty = TRUE,cex =0.8)

#Random forest.
install.packages("randomForest")
library(randomForest)
model_random=randomForest(class~.,data = x_train,ntree=600)
model_random
#No. of variables tried at each split: 6

#OOB estimate of  error rate: 4.64%
#Confusion matrix:
#  anomaly normal class.error
#anomaly   11718     25 0.002128928
#normal     1145  12304 0.085136441

predRF=predict(model_random,x_test,type = "class")
confusionMatrix(predRF,nvalidate$class)
#Confusion Matrix and Statistics

#Reference
#Prediction anomaly normal
#anomaly    9584    664
#normal     3249   9047

#Accuracy : 0.8264          
#95% CI : (0.8214, 0.8314)
#No Information Rate : 0.5692          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.6572          
#Mcnemar's Test P-Value : < 2.2e-16       

#Sensitivity : 0.7468          
#Specificity : 0.9316          
#Pos Pred Value : 0.9352          
#Neg Pred Value : 0.7358          
#Prevalence : 0.5692          
#Detection Rate : 0.4251          
#Detection Prevalence : 0.4546          
#Balanced Accuracy : 0.8392          

#'Positive' Class : anomaly    

#MODEL 2
model_random1=randomForest(class~.,data = x_train,ntree=500)
model_random1  
#No. of variables tried at each split: 6

#OOB estimate of  error rate: 4.6%
#Confusion matrix:
#  anomaly normal class.error
#anomaly   11717     26 0.002214085
#normal     1133  12316 0.084244182

predRF=predict(model_random1,x_test,type = "class")
confusionMatrix(predRF,nvalidate$class)
#Confusion Matrix and Statistics

#Reference
#Prediction anomaly normal
#anomaly    9516    648
#normal     3317   9063

#Accuracy : 0.8241          
#95% CI : (0.8191, 0.8291)
#No Information Rate : 0.5692          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.653           
#Mcnemar's Test P-Value : < 2.2e-16       

#Sensitivity : 0.7415          
#Specificity : 0.9333          
#Pos Pred Value : 0.9362          
#Neg Pred Value : 0.7321          
#Prevalence : 0.5692          
#Detection Rate : 0.4221          
#Detection Prevalence : 0.4509          
#Balanced Accuracy : 0.8374          

#'Positive' Class : anomaly   

#MODEL 3
model_random2=randomForest(class~.,data = x_train,ntree=700)
model_random2
#No. of variables tried at each split: 6

#OOB estimate of  error rate: 5.6%
#Confusion matrix:
#  anomaly normal class.error
#anomaly   11717     26 0.002214085
#normal     1386  12063 0.103055989

predRF=predict(model_random2,x_test,type = "class")
confusionMatrix(predRF,nvalidate$class)

#Confusion Matrix and Statistics

#Reference
#Prediction anomaly normal
#anomaly    9692   1159
#normal     3141   8552

#Accuracy : 0.8093          
#95% CI : (0.8041, 0.8144)
#No Information Rate : 0.5692          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.6205          
#Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.7552          
#            Specificity : 0.8807          
#         Pos Pred Value : 0.8932          
#         Neg Pred Value : 0.7314          
#            Prevalence : 0.5692          
#         Detection Rate : 0.4299          
#   Detection Prevalence : 0.4813          
#      Balanced Accuracy : 0.8179          
                                          
#       'Positive' Class : anomaly 