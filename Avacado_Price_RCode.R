#calling the required libraries and packages
library(data.table)
library(mltools)
library(randomForest)
library(e1071)

# Import Data 
data = read.csv("C:/Users/91773/Desktop/MSc Data Analytics/DMML/Project/Avocado/avocado.csv")
# Check the structure of the data
str(data)

#Check the summary of the data
head(data)

#checking for outliers in dependent variable
boxplot(data$AveragePrice, col = "yellow",
        main = "Average Price Plot")

#removing outliers
medPrice <- quantile(data$AveragePrice, 0.5)
out <- quantile(data$AveragePrice, 0.95)

data$AveragePrice[data$AveragePrice > out ] <- medPrice

#checking box plot after removing the outliers
boxplot(data$AveragePrice, col = "pink",
        main = "Average Price Plot")

library(ggplot2)
ggplot(data = data, aes(x = year, fill = type)) + geom_bar()


#train-test split
#splitting the data into training and test datasets

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test_data  <- data[-sample, ]

#finding the columns containing character values
chr_col <- which(sapply(train, is.character))
object_cols <- names(chr_col)


#checking the columns containing categorical columns:
print(object_cols)

# creating a copy of train
train_copy = train

#performing one hot encoding on the modified train data
#this will help us in easy identification of categorical variables
train_copy$type = as.factor(train_copy$type)
train_copy$region = as.factor(train_copy$region)
train_1h <- one_hot(as.data.table(train_copy))

#extracting the response variable in 'actual' before the prediction begins
actual <- test_data$AveragePrice

#removing the response variable from the test dataset 
#as this value will be predicted by the model
test <- subset (test_data, select = -AveragePrice)

#performing one hot encoding on the modified test dataset
#this is done to maintain the same number of columns in train and test
test$type = as.factor(test$type)
test$region = as.factor(test$region)
test_1h <- one_hot(as.data.table(test))

#####################################################################################################
## Random Forest Model ## 
#####################################################################################################

set.seed(222)
rf_model <- randomForest(AveragePrice ~ .,
                         data = train_1h, 
                         ntree = 301,
                         replace = TRUE,
                         nodesize = 3,
                         importance = TRUE); 

print(rf_model)
plot(rf_model)


#Performing prediction using test data set
prediction <- predict(rf_model, test_1h)
#creating a dataframe containing prices from test data and the predicted values
df2<-data.frame(actual,prediction)

#Finding RMSE and MSE of the model
rmse_randomforest = rmse(actual,prediction)
sprintf("The RMSE of the Random Forest Model is %f", rmse_randomforest)
mse_randomforest = mse(actual,prediction)
sprintf("The MSE of the Random Forest Model is %f", mse_randomforest)

write.csv(df2, file = 'C:/Users/91773/Desktop/MSc Data Analytics/DMML/Project/Avocado/avacado_price_randomforest.csv'
          ,row.names = F)

plot(prediction,actual,
     main = "Random Forest: Actual vs Predicted Plot",
     xlab="predicted",ylab="actual")
abline(a=0,b=1)



#####################################################################################################
## SVM Regression Model ## 
#####################################################################################################


svm_model <- svm(AveragePrice ~ ., data = train_1h)

# Predict with test data
pred1 <- predict(svm_model, test_1h)
df4<-data.frame(actual,pred1)

#Finding RMSE and MSE of the SVM model
rmse_svm = rmse(actual,pred1)
sprintf("The RMSE of the SVM Model is %f", rmse_svm)
mse_svm = mse(actual,pred1)
sprintf("The MSE of the SVM Model is %f", mse_svm)

write.csv(df4, file = 'C:/Users/91773/Desktop/MSc Data Analytics/DMML/Project/Avocado/avacado_price_svm.csv'
          ,row.names = F)



#plotting the actual vs predicted plots
plot(pred1,actual,
     main = "SVM: Actual vs Predicted Plot",
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


