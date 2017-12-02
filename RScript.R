#Libraries

library(dplyr)
library(ggplot2)
library(magrittr)
library(caTools)
library(caret)
library(ROCR)
#install.packages("Metrics")
library(Metrics)
library(bestglm)

#Loading the data

train = fread("train.csv", na.strings = c("", NA))
test = fread("test.csv", na.strings = c("", NA))
summary(train)
summary(test)

str(train)
str(test)
#check missing values
colSums(is.na(train))
colSums(is.na(test))

#Data exploration 
train[,.N/nrow(train), Gender]
test[,.N/nrow(test), Gender]

#Additional data exploration techniques

#prop.table(table(train$Gender, train$Loan_Status),2)
qplot(data = train, x = Gender,main = "Gender frequency distribution in train data") + geom_bar()


train[,.N/nrow(train), Married]
test[,.N/nrow(test), Married]

train[,.N/nrow(train), Dependents]
test[,.N/nrow(test), Dependents]

train[,.N/nrow(train), Education]
test[,.N/nrow(test), Education]

train[,.N/nrow(train), Self_Employed]
test[,.N/nrow(test), Self_Employed]

train[,.N/nrow(train), Loan_Amount_Term]
test[,.N/nrow(test), Loan_Amount_Term]

qplot(data = test, x = Loan_Amount_Term,main = "Loan Amount term distribution in test data") + geom_bar()

train[,.N/nrow(train), Credit_History]
test[,.N/nrow(test), Credit_History]


train[,.N/nrow(train), Property_Area]
test[,.N/nrow(test), Property_Area]

summary(train$ApplicantIncome)
summary(test$ApplicantIncome)

summary(train$CoapplicantIncome)
summary(test$CoapplicantIncome)

summary(train$LoanAmount)
summary(test$LoanAmount)

hist(train$LoanAmount,main = "Distribution of Loan Amount in Train data",xlab = "Rating",col = "red")

#Merging train and test prior to data cleaning
alldata = rbind(train,test, fill=TRUE)
View(alldata)


#Data cleaning

alldata[is.na(Gender), Gender := "Male"]

alldata[is.na(Married), Married := "Yes"]

alldata[is.na(Dependents), Dependents := 0]

alldata[is.na(Self_Employed), Self_Employed:= "No"]

alldata[is.na(Loan_Amount_Term), Loan_Amount_Term := 360]

alldata[Loan_Amount_Term == 6, Loan_Amount_Term := 12]
alldata[Loan_Amount_Term == 350 , Loan_Amount_Term := 360]

alldata[is.na(Credit_History), Credit_History:= 1]

for(i in "LoanAmount")
  set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$LoanAmount,na.rm = T))


#Understanding significant variables

#Bringing back the train and test data
train = alldata[!is.na(Loan_Status)]
train[,Loan_Status :=as.factor(Loan_Status)]

test <- alldata[is.na(Loan_Status)]
test [,Loan_Status := NULL]


#train_final = subset(train, select = -c(Loan_ID))

model = glm(Loan_Status ~., family = binomial(link='logit'), data = train[,-c("Loan_ID")])
summary(model)

#Different accuracy matrices

anova(model, test = "Chisq")

#Creating a new model for trying to bring up accuracy

model2 = glm(Loan_Status ~ Married + Education + Credit_History + Property_Area, family = binomial(link='logit'), data = train)
summary(model2)

#comparing two models

anova(model, model2, test = "Chisq")

#splitting the model and creating the model
#set.seed(88)
install.packages("caret")
split <- createDataPartition(y = train$Loan_Status,p = 0.6,list = FALSE)

loantrain = train[split]
loantest = train[-split]
#split = sample.split(train$Loan_Status, SplitRatio = 0.75)
#loantrain = subset(train, split == TRUE)
#loantest = subset(train, split == FALSE)
loantrain = subset(loantrain, select = -c(Loan_ID))
log_model = glm(Loan_Status ~ Married + Education + Credit_History + Property_Area, family = binomial(link='logit'), data = loantrain)
summary(log_model)

log_predict <- predict(log_model,newdata = loantest,type = "response")


#Plot ROC


pr <- prediction(log_predict,loantest$Loan_Status)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(loantest$Loan_Status,log_predict)

auc(loantest$Loan_Status, log_predict)
auc #0.766

summary(log_model)

log_predict <- ifelse(log_predict > 0.5,"Y","N")
misClasificError = mean(log_predict != loantest$Loan_Status)
#Accuracy value
print(paste('Accuracy', 1-misClasificError))
#Confusion Matrix
table(loantest$Loan_Status, log_predict>0.5)

#Applying on test data

log_predict_test <- predict(log_model,newdata = test,type = "response")
View(log_predict_test)


#McFadden R2 index

library(pscl)
pR2(model)
pR2(model_3)



#apply best glm
glm_model = bestglm(Xy= glm_model, family=binomial, IC="AIC", method = "exhaustive")

glm_model$BestModels
summary(glm_model$BestModel)


# install.packages("HH")
# library(HH)
# vif(log_model)
# install.packages("VIF")
# library(VIF)
# vif_func(in_frame = subset(train, c("Married", "Education", "Credit_History", "Property_Area")), thresh= 5, trace = T)

pR2(log_model)

