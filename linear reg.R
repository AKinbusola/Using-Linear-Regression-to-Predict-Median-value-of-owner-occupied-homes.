library(MASS)
library(caret)
library(tidyverse)
library(ggvis)
library(lattice)
library(corrplot)
library(car)


data("Boston")
Boston
summary(Boston)
is.na(Boston)


?Boston

set.seed(2000)

sep<-createDataPartition(Boston$medv, p=0.7, list=F)
Boston_Train<- Boston[sep,]
Boston_Test<- Boston[-sep,]
dim(Boston_Train)
dim(Boston_Test)


#to calculate the correlation between  all the variables in a dataset
cr_Boston<-cor(Boston)
corrplot(cr_Boston)
corrplot(method="number",cr_Boston)

model_Boston<- lm(medv~.,data=Boston_Train)#
model_Boston
summary(model_Boston)
# we can see that some variables are not significant to the response variable
vif(model_Boston)

barplot(vif(model_Boston), main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

# Generally, VIF should be 5 or less, the VIF for rad and tax are extremely high.

model2_Boston<- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data=Boston_Train)#model after removing variable with low significant level
summary(model2_Boston)

pre_Boston<-predict(model2_Boston,Boston_Test)#to predict the test dataset
pre_Boston #predicted values
Boston_Test


#Model Evaluation
#to check the error on each test
errors<-pre_Boston-Boston_Test$medv
errors
mean(abs(errors)) # Mean absolute error
max(abs(errors))
min(abs(errors))

RMSE(pred = pre_Boston, obs = Boston_Test$medv) # root mean square value

plot(errors, type="l", col="red")#graph of error

#graph comparing the actual and the predicted values
plot(pre_Boston, type="l", col="red" )
lines(Boston_Test$medv, col="SteelBlue")
legend(x = "topright",          
       legend = c("Actual", "Predicted"),  
       lty = c(1, 1),           
       col = c(4, 2),           
       lwd = 2)                
title(main = 'Graph comparing the Actual and the Predicted values')
