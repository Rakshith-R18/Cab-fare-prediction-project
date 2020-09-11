#Clean the environment
rm(list=ls())

#Set Working Directory
setwd("C:/Users/User/Desktop/edwisor/New folder")

#get Working directory
getwd()

#Load the librarires
libraries = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
              "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"tidyr")


install.packages(libraries)
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
test = read.csv("test.csv")

########################################EXPLORATARY DATA ANALYSIS########################################
# Summarizing  data

#Verify first five rows of data

head(train)

head(test)

#Dimensions of data
dim(train)

dim(test)

#Column names
names(train)

names(test)

#Structure of variables
str(train)

str(test)

#Verify  summary of data
summary(train)

summary(test)

# Changing the data types of variables
train$fare_amount = as.numeric(as.character(train$fare_amount))
train$passenger_count = round(train$passenger_count)

####Missing Value analysis
# Checking the Missing data
apply(train, 2, function(x) {sum(is.na(x))})

#Creating data frame of missing values present in each variable 
value = data.frame(apply(train, 2, function(x){sum(is.na(x))}))
value$Columns = row.names(value) 
names(value)[1] = "null_percentage"

# missing value percentage
value$null_percentage = (value$null_percentage/nrow(train)) * 100

# Sorting in Descending order
value = value[order(-value$null_percentage),] 
row.names(value) = NULL

# Reordering the columns 
value = value[,c(2,1)]


value

#We have seen that null values are very less in our data set i.e. less than 1%. 
#So we can delete the columns having missing values
# delete all the rows with missing values, 
train = drop_na(train)

# Verifying of missing value upon deletion 
sum(is.na(train))

#Splitting Date and time on train data
train$pickup_date = as.Date(as.character(train$pickup_datetime))
train$weekday = as.factor(format(train$pickup_date,"%u")) 
train$month = as.factor(format(train$pickup_date,"%m"))
train$year = as.factor(format(train$pickup_date,"%Y"))
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$hour = as.factor(format(pickup_time,"%H"))

#Splitting Date and time on test data
test$pickup_date = as.Date(as.character(test$pickup_datetime))
test$weekday = as.factor(format(test$pickup_date,"%u")) 
test$month = as.factor(format(test$pickup_date,"%m"))
test$year = as.factor(format(test$pickup_date,"%Y"))
pickup_time_test = strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$hour = as.factor(format(pickup_time_test,"%H"))

# Now drop the column pickup_datetime and pickup_date from train data
train = subset(train, select = -c(pickup_datetime))
train = subset(train, select = -c(pickup_date))

# Now drop the column pickup_datetime and pickup_date from test data
test = subset(test, select = -c(pickup_datetime))
test = subset(test, select = -c(pickup_date))




########### data cleaning  #############
#working on fare amount variable

# fare amount cannot be less than one 
# considering fare amount 453 as max and removing all the fare amount greater than 500
nrow(train[which(train$fare_amount < 1 ),]) 

train = train[-which(train$fare_amount < 1 ),]  

nrow(train[which(train$fare_amount >500 ),])
train = train[-which(train$fare_amount >500 ),]  

# passenger count cannot be Zero
# even if we consider suv max seat is 6, so removing passenger count greater than 6.
nrow(train[which(train$passenger_count < 1 ),]) 
train=train[-which(train$passenger_count < 1 ),]

nrow(train[which(train$passenger_count >6 ),]) 
train=train[-which(train$passenger_count >6 ),] 

# Latitudes range from -90 to 90.Longitudes range from -180 to 180.
# Removing which does not satisfy these ranges.
summary(train)
train = train[-which(train$pickup_latitude > 90),] 

# Also we will see if there are any values equal to 0.
nrow(train[which(train$pickup_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
nrow(train[which(train$dropoff_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])

# removing those data points.
train=train[-which(train$pickup_longitude == 0 ),]
train=train[-which(train$dropoff_longitude == 0),]

# checking for missing values.
sum(is.na(train))
sum(is.na(test))

train=na.omit(train) # we have removed the missing values...as they are less
sum(is.na(train))  

# Calculate the distance travelled using longitude and latitude
haversine = function(long1, lat1, long2, lat2) {
  rad = pi/180
  a1 = lat1*rad
  a2 = long1*rad
  b1 = lat2*rad
  b2 = long2*rad
  dlon = b2 - a2
  dlat = b1 - a1
  a = (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c = 2*atan2(sqrt(a), sqrt(1 - a))
  Radius = 6371
  distance = Radius*c
  return(distance)
}
# Using the haversine formula to calculate distance variable in both train and test data set
train$distance = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)

test$distance = haversine(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)

# We will remove the variables which were used to feature engineer the new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))


summary(train)

# creating boxplot of the continous variables to check outlier
ggplot(data = train, aes(x = "", y = distance)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150))

#Working on distance variable
nrow(train[which(train$distance ==0 ),])
nrow(test[which(test$distance==0 ),])
nrow(train[which(train$distance >30 ),])
nrow(test[which(test$distance >30 ),])

# considering the distance 30 as max and considering rest as outlier.
train=train[-which(train$distance ==0 ),]
train=train[-which(train$distance >30 ),]
test=test[-which(test$distance ==0 ),]

#working on fare amount variable
ggplot(data = train, aes(x = "", y = fare_amount)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150))
 
# considering the fare amount 100 as max and considering rest as outlier.
nrow(train[which(train$fare_amount >100 ),])
train = train[-which(train$fare_amount >100 ),] 

########## feature selection
#selecting only numeric
numeric_index = sapply(train,is.numeric) 
numeric_data = train[,numeric_index]
cnames = colnames(numeric_data)

#Correlation analysis for numeric variables
library(corrgram)
corrgram(train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")


########3# feature scaling ##
library(car)
library(MASS)
qqPlot(train$fare_amount)  
truehist(train$fare_amount) 
lines(density(train$fare_amount)) 

d=density(train$fare_amount)
plot(d,main="distribution")
polygon(d,col="purple",border="red")

D=density(train$distance)
plot(D,main="distribution")
polygon(D,col="purple",border="red")

A=density(test$distance)
plot(A,main="distribution")
polygon(A,col="green",border="red")

#make a copy
df_train1 = train
df_test1 = test

#Normalisation
# log transformation.
train$fare_amount=log1p(train$fare_amount)
train$distance=log1p(train$distance)

# checking back features after transformation.
d=density(train$fare_amount)
plot(d,main="distribution")
polygon(d,col="purple",border="red")

D=density(train$distance)
plot(D,main="distribution")
polygon(D,col="green",border="black")

A=density(test$distance)
plot(A,main="distribution")
polygon(A,col="purple",border="red")

############### Data Visualization ########################
# Scatter plot b/w passenger and fare on train data
ggplot(data = train, aes_string(x = train$passenger_count, y = train$fare_amount))+ 
  geom_point()

# Scatter plot b/w distance and fare on train data
ggplot(data = train, aes_string(x = train$distance, y = train$fare_amount))+ 
  geom_point()

# Scatter plot b/w hour and fare on train data
ggplot(data = train, aes_string(x = train$hour, y = train$fare_amount))+ 
  geom_point()

# Scatter plot b/w weekday and fare on train data
ggplot(data = train, aes_string(x = train$weekday, y = train$fare_amount))+ 
  geom_point()

#make a copy
df_train = train
df_test = test



######################### Model Development ###################

########## Split train data into train and test #################
set.seed(123)
split_index = createDataPartition(train$fare_amount, p = 0.8, list = FALSE) 
train_data = train[split_index,]
test_data = train[-split_index,]

#############  Linear regression Model  #################
lm_model = lm(fare_amount ~., data=train_data)

# summary of trained model
summary(lm_model)

# residual plot
plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare amount",
     ylab = "standardized residuals")

# prediction on test data
lm_predictions = predict(lm_model,test_data[,2:7])
# prediction on train data
lm_predictions_train = predict(lm_model,train_data[,2:7])
qplot(x = test_data[,1], y = lm_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],lm_predictions)
#        mae        mse       rmse       mape 
#    0.16237321 0.06233015 0.24966007 0.07057444 

regr.eval(train_data[,1],lm_predictions_train)
#       mae        mse       rmse       mape 
#   0.16189595 0.06081755 0.24661215 0.07072041

# compute r^2 on test data
r2_lr = sum((lm_predictions - test_data$fare_amount) ^ 2)
t2_lr = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_lr = 1 - r2_lr/t2_lr
rsq_lr
#    r^2 - 0.788882

# compute r^2 on train data
r2_lr1 = sum((lm_predictions_train - train_data$fare_amount) ^ 2)
t2_lr1 = sum((train_data$fare_amount - mean(train_data$fare_amount)) ^ 2)
rsq_lr1 = 1 - r2_lr1/t2_lr1
rsq_lr1
#    r^2 - 0.7928228

#################### Decision Tree Regressor Model ###################
DT_model = rpart(fare_amount ~ ., data=train_data, method = "anova" , minsplit=5)

# summary on train data
summary(DT_model)

#Prediction on test data
prediction_DT = predict(DT_model, test_data[,2:7])

#Prediction on train data
prediction_DT_train = predict(DT_model, train_data[,2:7])

qplot(x = test_data[,1], y = prediction_DT, data=test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1], prediction_DT)
#      mae        mse       rmse       mape 
#   0.19141056 0.07190210 0.26814567 0.08440643

regr.eval(train_data[,1], prediction_DT_train)
#   mae        mse       rmse       mape 
# 0.18934990 0.07080609 0.26609414 0.08347341

# compute r^2 on test data
r2_dt = sum((prediction_DT - test_data$fare_amount) ^ 2)
t2_dt = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_dt = 1 - r2_dt/t2_dt
rsq_dt
#    r^2 - 0.7587964

# compute r^2 on train data
r2_dt1 = sum((prediction_DT_train - train_data$fare_amount) ^ 2)
t2_dt1 = sum((train_data$fare_amount - mean(train_data$fare_amount)) ^ 2)
rsq_dt1 = 1 - r2_dt1/t2_dt1
rsq_dt1
#    r^2 - 0.7564609

################  Random forest Regressor Model #####################
rf_model = randomForest(fare_amount ~., data=train_data ,  ntree = 100 , importance=TRUE)

# summary on trained model
summary(rf_model)

# prediction of test data
rf_prediction = predict(rf_model, test_data[,2:7])

# prediction of train data
rf_prediction_train = predict(rf_model, train_data[,2:7])

qplot(x = test_data[,1], y = rf_prediction, data=test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1], rf_prediction)
#           mae        mse       rmse       mape 
#       0.17062625 0.05869819 0.24227709 0.07632930

regr.eval(train_data[,1], rf_prediction_train)
#     mae        mse       rmse       mape 
# 0.08646949 0.01528412 0.12362895 0.03859882 

# compute r^2 on test data
r2_rf = sum((rf_prediction - test_data$fare_amount) ^ 2)
t2_rf = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_rf = 1 - r2_rf/t2_rf
rsq_rf
#    r^2 - 0.8011838

# compute r^2 on train data
r2_rf1 = sum((rf_prediction_train - train_data$fare_amount) ^ 2)
t2_rf1 = sum((train_data$fare_amount - mean(train_data$fare_amount)) ^ 2)
rsq_rf1 = 1 - r2_rf1/t2_rf1
rsq_rf1
#    r^2 - 0.9479341


#######################Selection of model########

#Predict for test data with best fit model - Random forest
# we have already clean the test data
# we use whole training Dataset to predict the fare on test dataset
train=df_train1
rf_model1 = randomForest(fare_amount ~., data=train ,  ntree = 100 , importance=TRUE)
pred_RF_test = predict(rf_model1,df_test)
pred_RF_test_R_DF = data.frame(df_test$passenger_count, df_test$distance,"fare_amount" = pred_RF_test )
write.csv(pred_RF_test_R_DF,"Test_Prediction_R.csv",row.names = FALSE)

