############Naive Bayes Model
library(naivebayes)
library(ggplot2)
library(mlbench)
library(caret)
library(psych)
?psych

#DATA(TRAIN)
train_sal <- read.csv(file.choose())
View(train_sal)
str(train_sal)

train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
View(train_sal$educationno)

#DATA(TEST)
test_sal <- read.csv(file.choose())
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
str(test_sal)


#VISUALIZATION
#PLOT AND GGPLOT

ggplot(data = train_sal,aes(x = train_sal$Salary, y = train_sal$age, fill = train_sal$Salary))+
  geom_boxplot()+
  ggtitle("BOXPLOT")

#plot
plot(train_sal$workclass,train_sal$Salary)
plot(train_sal$age,train_sal$Salary)
plot(train_sal$education,train_sal$Salary)
plot(train_sal$educationno,train_sal$Salary)
plot(train_sal$maritalstatus,train_sal$Salary)
plot(train_sal$occupation,train_sal$Salary)
plot(train_sal$relationship, train_sal$Salary)
plot(train_sal$race,train_sal$Salary)
plot(train_sal$sex,train_sal$Salary)
plot(train_sal$native,train_sal$Salary)
########

ggplot(data = train_sal,aes(x=train_sal$Salary,y=train_sal$capitalgain, fill = train_sal$Salary))+
  geom_boxplot()+
  ggtitle("boxplot")

######
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

####
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Age - Density Plot")

ggplot(data=train_sal,aes(x = train_sal$education, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train_sal,aes(x = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')



###########NAVIE BAYES MODEL

model <- naive_bayes(train_sal$Salary ~., data = train_sal)
model
#
model_pred <- predict(model,test_sal)
mean(model_pred == test_sal$Salary)   #82%

#CONFUSIONMATRIX
confusionMatrix(model_pred,test_sal$Salary)   #positive class : <=50K

###########################################################################
