rm(list=ls(all=TRUE))
setwd("G:\\projects\\analytics vidhya\\smart recruits")

train<- read.csv("train.csv",header=T,na.string="")
sum(is.na(train))
test<- read.csv("test.csv",header=T)
names(test)
names(train)
str(train)
str(test)
summary(train)
summary(test)

levels(train$Applicant_Qualification)
levels(train$Applicant_Gender)
levels(train$Manager_Status)
sum(is.na(train))

train$Application_Receipt_Date<-as.Date(train$Application_Receipt_Date,"%m/%d/%Y")
train$Applicant_BirthDate<-as.Date(train$Applicant_BirthDate,"%m/%d/%Y")
train$Manager_DoB<-as.Date(train$Manager_DoB,"%m/%d/%Y")  
train$Manager_DOJ<-as.Date(train$Manager_DOJ,"%m/%d/%Y")
train$Business_Sourced<-as.factor(train$Business_Sourced)

test$Application_Receipt_Date<-as.Date(test$Application_Receipt_Date,"%m/%d/%Y")
test$Applicant_BirthDate<-as.Date(test$Applicant_BirthDate,"%m/%d/%Y")
test$Manager_DoB<-as.Date(test$Manager_DoB,"%m/%d/%Y")  
test$Manager_DOJ<-as.Date(test$Manager_DOJ,"%m/%d/%Y")

today <- as.Date(Sys.Date(), format='%d/%m/%y')
applicant_age<-difftime(today,train$Applicant_BirthDate,units="weeks")/52.25
train$applicant_age<-round(applicant_age)

#to find manager experience
Manager_exp<-difftime(today,train$Manager_DOJ,units="weeks")/52.25
train$Manager_exp<-round(Manager_exp)
train$Manager_exp<-as.integer(train$Manager_exp)


#is employee home city is the working city.
train$Work_Home_sameCity<-ifelse(train$Office_PIN==train$Applicant_City_PIN,yes = 'Y' ,no = 'N')

train1$applicant_age<-as.numeric(train1$applicant_age)
train1$Work_Home_sameCity<-as.factor(train1$Work_Home_sameCity)

names(train)
#remove columns
train1<-train[,-c(2,3,4,6,10,16)]
names(train1)
train1<-train1[,c(1:16,18,19,20,17)]
str(train1)


str(train1)
summary(train1)

train1[train1==" "]<-NA
train1[train1$Applicant_Occupation==""]<-NA
levels(train1$Applicant_Occupation)
table(train1$Applicant_Occupation)
#NA handling
#knn imputation
train2<-knnImputation(train1)

###################
str(train2)
summary(train2)
names(train2)
train3<-train2[,-c(2:9)]

levels(train2$Manager_Gender)
table(train2$Manager_Current_Designation)

train2[train2$Applicant_Occupation==""]<-NA
#model building
model<-C5.0(Business_Sourced~.,train2[,-c(1)], rules=T)
summary(model)
model_C50_predict <- predict.C5.0(model, test)
tb<-table(test[,"Business_Sourced"],model_C50_predict)
tb
error.C5.0_1 <- 1-(sum(diag(tb))/sum(tb))
error.C5.0_1

final<- predict.C5.0(model, validation)
final
capture.output(summary(model), file="summary1.txt")
