mydata <- read.csv("F:/Master's/Fall 2020/Data Mining/Project/WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
library(dplyr)
library(ggplot2)
library(ggthemes)
colnames(mydata)[1] <- c("Age")
str(mydata)
dim(mydata)
numeric_mydata <- mydata[,c(1,4,6,7,10,11,13,14,15,17,19,20,21,24,25,26,28:35)]
numeric_Attrition = as.numeric(mydata$Attrition)- 1
numeric_mydata = cbind(numeric_mydata, numeric_Attrition)
str(numeric_mydata)
library(corrplot)

M <- cor(numeric_mydata)
corrplot(M, method="circle")

library(caTools)
library(e1071)
library(glmnet)

mydatanew = mydata[,-c(6,9,22)]
str(mydatanew)

l <- ggplot(mydata, aes(OverTime,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$OverTime,mean)

### MaritalStatus vs Attiriton
l <- ggplot(mydata, aes(MaritalStatus,fill = Attrition))
l <- l + geom_histogram(stat="count")
IE 7275 Data Mining in Engineering
14
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$MaritalStatus,mean)
l <- ggplot(mydata, aes(JobRole,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$JobRole,mean)
mean(as.numeric(mydata$Attrition) - 1)

###Gender vs Attrition
l <- ggplot(mydata, aes(Gender,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$Gender,mean)
l <- ggplot(mydata, aes(EducationField,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$EducationField,mean)

###Department vs Attrition
l <- ggplot(mydata, aes(Department,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$Department,mean)
l <- ggplot(mydata, aes(BusinessTravel,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$BusinessTravel,mean)
### x=Overtime, y= Age, z = MaritalStatus , t = Attrition
ggplot(mydata, aes(OverTime, Age)) +
facet_grid(.~MaritalStatus) +
geom_jitter(aes(color = Attrition),alpha = 0.4) +
ggtitle("x=Overtime, y= Age, z = MaritalStatus , t = Attrition") +
theme_light()
split <- sample.split(mydatanew$Attrition, SplitRatio = 0.80)
train <- subset(mydatanew, split == T)
test <- subset(mydatanew, split == F)
library(caret)
model_glm <- glm(Attrition ~ ., data = train, family='binomial')
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)
confusionMatrix(factor(predicted_glm),factor(as.numeric(test$Attrition)-1))
train_labels = train$Attrition
test_labels = test$Attrition
knn_train = subset(train,select = -c(Attrition))
knn_test = subset(test,select = -c(Attrition))
knn_data = mydatanew
knn_data[, c("Age","DailyRate", "Education","EmployeeNumber","EnvironmentSatisfaction","HourlyRate","JobInvolvem
IE 7275 Data Mining in Engineering
15
ent","JobLevel","JobSatisfaction","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","PerformanceRating","RelationshipSatisfaction","StandardHours","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")] <- scale(knn_data[, c("Age","DailyRate", "Education","EmployeeNumber","EnvironmentSatisfaction","HourlyRate","JobInvolvement","JobLevel","JobSatisfaction","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","PerformanceRating","RelationshipSatisfaction","StandardHours","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")])
knn_data = subset(knn_data, select = -c(StandardHours))
knn_data$Gender = ifelse(knn_data$Gender == "Male", 1, 0)
knn_data$OverTime = ifelse(knn_data$OverTime == "Yes", 1, 0)
library(psych)
BusinessTravel <- as.data.frame(dummy.code(knn_data$BusinessTravel))
Department <- as.data.frame(dummy.code(knn_data$Department))
EducationField <- as.data.frame(dummy.code(knn_data$EducationField))
JobRole <- as.data.frame(dummy.code(knn_data$JobRole))
MaritalStatus <- as.data.frame(dummy.code(knn_data$MaritalStatus))
JobRole <- rename(JobRole, HR_JobRole = `Human Resources`)
EducationField <- rename(EducationField, HR_Edu = `Human Resources`)
Department <- rename(Department, HR_Dep = `Human Resources`)
library(dplyr)
knn_data = subset(knn_data, select = -c(BusinessTravel,Department,EducationField,JobRole,MaritalStatus))
knn_data = cbind(knn_data, BusinessTravel, Department, EducationField, JobRole, MaritalStatus)
knn_train <- subset(knn_data, split == T)
knn_test <- subset(knn_data, split == F)
train_labels = knn_train$Attrition
knn_train = subset(knn_train,select = -c(Attrition))
test_labels = knn_test$Attrition
knn_test = subset(knn_test,select = -c(Attrition))