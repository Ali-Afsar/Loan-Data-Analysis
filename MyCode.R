

# Install package 

install.packages("caret")

library(caret)

install.packages("ellipse")

library(ellipse)

install.packages("mlr")
library(mlr)

# Attaching data to the R environment

dataset <- read.csv("LoanData.csv", na.strings = c("", " ", "NA"))

# Creat Training and Test Dataset

index <- createDataPartition(dataset$Loan_Status, p=0.8, list = FALSE)

# Select 80% data for the training

train <- dataset[index,]

# Select 20% data for Test

test <- dataset[-index,]


summarizeColumns(train)

summarizeColumns(test)

# Dimension of the dataset

dim(train)

# Types of Attributes
  
sapply(train,class)

# Level of the Class (target variable)

levels(train$Loan_Status)

# Target Class distribution

PercentageofData <- prop.table(table(train$Loan_Status)) * 100

CountofData <- table(train$Loan_Status)

cbind(PercentageofData,CountofData)

# Statical Summary
  summary(train)

  
# Visualisation of Dataset

# Bar Plot of target Variable
  
  par(mfrow = c(1,1))
  
  barplot(table(train$Loan_Status), main = names(train)[13])
  
# To check the percentage of Approved and Not Approved Loan
  prop.table(table(train$Loan_Status)) *100
  
# Checking the levels of Gender
  
  levels(train$Gender)
  
# Gender has two levels with na's
# Bar Plot for Gender
  
  par(mfrow = c(1,2))
  barplot(table(train$Gender), main = "Train Dataset")
  barplot(table(test$Gender), main = "Test Dataset")
  
  
# Percentage of gender in both the dataset
  
 TrainGender <- prop.table(table(train$Gender)) * 100
 TestGender <- prop.table(table(test$Gender)) * 100
 
 cbind(TrainGender, TestGender)
 
# Checking the level of Married variable
  
  levels(train$Married)
  any(is.na(train$Married))
  
  any(is.na(test$Married))
  
# Married variable has two level with na's
# Bar Plot of Married Variable
  
  par(mfrow = c(1,2))
  barplot(table(train$Married), main = "Train Dataset")
  barplot(table(test$Married), main = "Test Dataset")
  
# Percentage of Married status in both dataset
  
  TrainMarred <- prop.table(table(train$Married)) * 100
  TestMarried <- prop.table(table(test$Married)) * 100
  
  cbind(TrainMarred, TestMarried)
  
# Checking the levels of Dependents
  
  levels(train$Dependents)
  
  any(is.na(train$Dependents))

# Dependents variable has 4 levels with ns's

# Bar Plot of Dependents for both dataset

  par(mfrow = c(1,2))
  barplot(table(train$Dependents), main = "Train Dataset")
  barplot(table(test$Dependents), main = "Test Dataset")
  
# Percentage of Dependents status in both dataset
  
  table(train$Dependents)
  table(test$Dependents)
  
  cbind(table(train$Dependents),table(test$Dependents))
  
  TrainDependents <- prop.table(table(train$Dependents)) * 100
  TestDependets <- prop.table(table(test$Dependents)) * 100
  
  cbind(TrainDependents,TestDependets)
  
# Checking levels and na's for Education
  
  levels(train$Education)
  any(is.na(train$Education))
  
# Education has 2 levels and no na's
  
# Barplot of Education for both datasets
  
  par(mfrow = c(1,2))
  barplot(table(train$Education), main = "Train Dataset")
  barplot(table(test$Education), main = "Test dataset")
  
# Percentage of Education variables
  
  CountofEducTrain <- table(train$Education)
  CountofEducTest <- table(test$Education)
  
  EducaTrain <- prop.table(CountofEducTrain) * 100
  EducaTest <- prop.table(CountofEducTest) * 100
  
  cbind(EducaTrain,EducaTest)
  
# Checking factor of Self-Employed variable
  
  levels(train$Self_Employed)
  any(is.na(train$Self_Employed))
  
# Self Employed has two levels with na's
  
# Barplot of Self Employed
  
  par(mfrow = c(1,2))
  barplot(table(train$Self_Employed), main = "Train Dataset")
  barplot(table(test$Self_Employed), main = "Test Dataset")
  
# Percentage of Self Employed variable
  
 CountofSelfEmpTrain <- table(train$Self_Employed)
 CountofSelfEmpTest <- table(test$Self_Employed)
 
 SelfEmpTrain <- prop.table(CountofSelfEmpTrain) * 100
 SelfEmpTest <- prop.table(CountofSelfEmpTest) * 100
 
 cbind(SelfEmpTrain,SelfEmpTest)
 
# Applicant Income and CoApplicant Income both are numeric type with no na's
 
 class(train$ApplicantIncome)
 class(train$CoapplicantIncome)
 
 any(is.na(train$ApplicantIncome))
 any(is.na(train$CoapplicantIncome))
 
# Boxplot of ApplicantIncome and CoapplicantIncome
 par(mfrow = c(1,2))
 boxplot(train$ApplicantIncome,train$CoapplicantIncome, names = c("App Income","CoApp Income"), main = "Train Dataset")
 boxplot(test$ApplicantIncome, test$CoapplicantIncome, names = c("App Income","CoApp Income"),main = "Test Dataset")

 
# Loan Amount is numeric and has na's
 
 class(train$LoanAmount)
 any(is.na(train$LoanAmount))
 
# Boxplot of LoanAmount for both dataset (Distributions are right-asymetric)
 
 par(mfrow = c(1,2))
 boxplot(train$LoanAmount, main = "Train Dataset")
 boxplot(test$LoanAmount, main = "Test Dataset")
 
# Loan Amount term is numeric and has na's
 
 class(train$Loan_Amount_Term)
 any(is.na(train$Loan_Amount_Term))
 
# Histogram of LoanAmountTerm for both the dataset
 
 par(mfrow = c(1,2))
 hist(train$Loan_Amount_Term, breaks = 500, main = "Train Dataset")
 hist(test$Loan_Amount_Term, breaks = 500, main = "Test Dataset")
 
# Summary of Loan Amount Term
 
  summary(train$Loan_Amount_Term)
  summary(test$Loan_Amount_Term)
  
# Credit_History is a integer type but it should have factor type
  
  train$Credit_History
  class(train$Credit_History)
  
  train$Credit_History <- as.factor(train$Credit_History)
  test$Credit_History <- as.factor(test$Credit_History)
  
# Barplot of Credit_History
  
  par(mfrow = c(1,2))
  barplot(table(train$Credit_History), main = "Train Dataset")
  barplot(table(test$Credit_History), main = "Test Dataset")
  
# Distribution of credit History
  
  CntCreditHistoryTrain <- table(train$Credit_History)
  CntCreditHistoryTest <- table(test$Credit_History)
  
  CHTrain <- prop.table(CntCreditHistoryTrain) * 100
  CHTest <- prop.table(CntCreditHistoryTest) * 100
  
  cbind(CHTrain,CHTest)
  
# Property Area is factor and has 3 levels and no na's
  
  class(train$Property_Area)
  levels(train$Property_Area)
  any(is.na(train$Property_Area))
  
# Barplot of Property Area
  
  par(mfrow = c(1,2))
  barplot(table(train$Property_Area), main = "Train Dataset")
  barplot(table(test$Property_Area), main = "Test Dataset")
  
# Distribution of Property Area
  
  CntPAtrain <- table(train$Property_Area)
  CntPATest <- table(test$Property_Area)
  
  PATrain <- prop.table(CntPAtrain) * 100
  PATest <- prop.table(CntPATest) * 100
  
  cbind(PATrain, PATest)
   
# ggplot (Loan_Status by Gender)
  
install.packages("ggplot2")
library(ggplot2)

ggplot(train, aes(x=Loan_Status)) + 
  geom_bar() +
  facet_grid(.~Gender) +
  ggtitle("Loan Status by Gender of Applicant")

# ggplot (Loan Status by Marital Status of Applicant)

ggplot(train, aes(x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~Married) +
  ggtitle("Loan Status by Marital Status of Applicant")

# # ggplot (Loan Status by Dependents of Applicants)

ggplot(train, aes( x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~ Dependents) +
  ggtitle("Loan Status by Dependents of Applicant")

# ggplot (Loan Status by Education of Applicant)

ggplot(train, aes( x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~Education) +
  ggtitle("Loan Status by Education of Applicant")

# ggplot (Loan Status by Employemnt of Applicant)

ggplot(train, aes( x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~Self_Employed) +
  ggtitle("Loan Status by Employment of Applicant")

# ggplot (Loan Status by term of Loan)

ggplot(train, aes(x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~Loan_Amount_Term) +
  ggtitle("Loan Status by term of Loan")

# ggplot (Loan Status by Credit History of Applicant)

ggplot(train, aes(x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~Credit_History) +
  ggtitle("Loan Status by Credit History of Applicant")

#  ggplot (Loan status by property Area)

ggplot(train, aes(x = Loan_Status)) +
  geom_bar() +
  facet_grid(.~Property_Area) +
  ggtitle("Loan Status by Property Area")

# ggplot (Loan Status by Applicant Income)

ggplot(train, aes(x = Loan_Status, y = ApplicantIncome)) +
  geom_boxplot() +
  ggtitle("Loan Status by Applicant Income")

# ggplot (Loan Status by CoApplicant Income)

ggplot(train, aes(x = Loan_Status, y = CoapplicantIncome)) +
  geom_boxplot() +
  ggtitle("Loan Status by Co Applicant Income")

# ggplot (Loan Status by Loan Amount)

ggplot(train, aes(x = Loan_Status, y = LoanAmount)) +
  geom_boxplot() +
  ggtitle("Loan Status by Loan Amount")
 
# Tidying the Data - Filling in Missing Values

# Binding all the rows of train and test dataset from 2nd column to 12 column

alldata <- rbind(train[,2:12], test[,2:12])
dim(alldata)

# The first variable we will deal with Applicant Income and CoApplicant Income. Some of the Applicant are Male and presumably,
# CoApplicants are Female and Vice versa

# Applicant with income higher than 20000 have been truncated from the plot

# GGPLOT for Applicant Income vs Gender vs Marital Status  
ggplot(data = alldata[alldata$ApplicantIncome < 20000,],aes(ApplicantIncome,fill = Married)) +
  geom_bar(position = "dodge") +
  facet_grid(Gender~.)

# GGPLOt for CoApplicant income vs Gender vs Marital Status (Applicant Income <20000)

ggplot(data = alldata[alldata$ApplicantIncome <20000,], aes(CoapplicantIncome, fill = Married) )+
  geom_bar(position = "dodge") +
  facet_grid(Gender ~.)

# We are going to create one new variabl total income

install.packages("plyr") 
library(plyr)

alldata2 <- mutate(alldata, TotalIncome = ApplicantIncome + CoapplicantIncome)

install.packages("ggplot2")
library(ggplot2)
ggplot(data = alldata2, aes(TotalIncome, fill = Married)) +
  geom_bar(position = "dodge") +
  facet_grid(Gender ~.)

# Imputing Marital status "No" when the CoApplicant income is zero and Yes Otherwise

alldata2$Married[is.na(alldata2$Married) & alldata2$CoapplicantIncome == 0] <- "No"

alldata2$Married[is.na(alldata2$Married)] <- "Yes"

# Gender and Dependents

alldata2[is.na(alldata2$Gender) & is.na(alldata2$Dependents),]
alldata2$Gender[is.na(alldata2$Gender) & is.na(alldata2$Dependents)] <- "Male"

#  barplot between Dependents vs Gender vs Married
 ggplot(data = alldata2, aes(x = Dependents, fill = Gender))+
   geom_bar() +
   facet_grid(.~Married)
 
# Dependents0+MaritalStatusNO+DependensNA) set this to Dependents ==0
 
 alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Married == "No"] <- "0"
 
# I will use the rpart to predict the number of dependents for this population, using Applicant Income, CoApplicantIncome,
 # Loan Amount, Loan Term, and Property Area as Predictor.
 
 colnames(alldata2)
 
 mm <- alldata2[(alldata2$Gender == "Male" & alldata2$Married == "Yes"), c(3,6:9,11)]
 
 mmTrain <- mm[!is.na(mm$Dependents),]
 
 mmTest <- mm[is.na(mm$Dependents),]
 
 library(rpart)
 
 depFit <- rpart(data = mmTrain, Dependents ~., xval = 3)
 
 install.packages("rattle")
 library(rattle)
 fancyRpartPlot(depFit)