#Clean the environment
rm(list = ls())

#Loading Libraries
#libraries = c("plyr", "ggplot2","rpart","dplyr","DMwR","randomForest",
              "usdm","corrgram","DataCombine","caret","tidyr","lubridate","tidyverse",
              "VIF","purrr","geosphere","rlist")
#install.packages(libraries)

#lapply(X = libraries,require, character.only = TRUE)

#rm(libraries)
library(rpart)
library(randomForest)
library(DataCombine)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(datetime)
library(tidyverse)
library(VIF)
library(purrr)
library(geosphere)
library(rlist)
library(usdm)
#Loding the dataset
train_data = read.csv('train_cab.csv')
test_data = read.csv('test.csv')

#Checking Data
str(train_data)
head(train_data,4)

#Shape of the data
dim(train_data)

#------EDA----------------------#

#Converting data type of fare amount to float
train_data$fare_amount = as.numeric(as.character(train_data$fare_amount))

#-------Missing Value Analysis-----------#

missing_val = data.frame(sapply(train_data, function(x){sum(is.na(x))}))
missing_val$Variables = row.names(missing_val)
names(missing_val)[1] =  "Missing_Values_Count"
missing_val = missing_val[,c(2,1)]
missing_val$Missing_percentage = (missing_val$Missing_Values_Count/nrow(train_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val

#deleting the rows containing NA
train_data = train_data %>% drop_na()

#Checking the Data
dim(train_data)
str(train_data)    

#----------- Feature Engineering ------------#

#Extracting Year, Month, dayOfweek, hour, day from pickup_datetime Column for Train data
train_data <- mutate(train_data,
                     pickup_datetime = ymd_hms(pickup_datetime),
                     year = as.factor(year(pickup_datetime)),
                     month = as.factor(month(pickup_datetime)),
                     day = as.factor(day(pickup_datetime)),
                     dayOfWeek = as.factor(wday(pickup_datetime)),
                     hour = hour(pickup_datetime),
                     hour = as.factor(hour(pickup_datetime))
)

#Extracting Year, Month, dayOfweek, hour, day from pickup_datetime Column for Test data
test_data <- mutate(test_data,
                    pickup_datetime = ymd_hms(pickup_datetime),
                    year = as.factor(year(pickup_datetime)),
                    month = as.factor(month(pickup_datetime)),
                    day = as.factor(day(pickup_datetime)),
                    dayOfWeek = as.factor(wday(pickup_datetime)),
                    hour = hour(pickup_datetime),
                    hour = as.factor(hour(pickup_datetime))
)

#Check the data
str(test_data)
head(test_data,5)

str(train_data)
head(train_data,5)

#----- Outlier Analysis -------------#
#Box plot of continuous data

continous_var = c("dropoff_longitude","dropoff_latitude","pickup_longitude","pickup_latitude","fare_amount")

for (i in 1:length(continous_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = continous_var[i]), data = train_data)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=continous_var[i])+
           ggtitle(paste("Box plot for",continous_var[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol=2)

#-------------Removing Outliers based on the Test data feature conditions ---------------------#

#Analysing train_data for selecting feature conditions
summary(train_data)

#Analysing Test_data for extracting feature conditions
summary(test_data)

#feature 1: Finding min and max of longitude
lon_min = min(min(test_data$pickup_longitude),min(test_data$dropoff_longitude))
lon_max = max(max(test_data$pickup_longitude),max(test_data$dropoff_longitude))

#feature 2: Finding min and max of latitude
lat_min = min(min(test_data$pickup_latitude),min(test_data$dropoff_latitude))
lat_max = max(max(test_data$pickup_latitude),max(test_data$dropoff_latitude))

cat("Longitude min is : ",lon_min, "and max is: ",lon_max, "")
cat("Latitude min is : ",lat_min, "and max is: ",lat_max, "")

#feature 3: Finding min and max of fareamount and Removing -ve values from the fare_amount variable and
#max range selected based on the data distribution we found that max is 65
fare_max = 65
fare_min = 1

#feature 4: Finding min and max of passenger count

pass_min = min(test_data$passenger_count)
pass_max = max(test_data$passenger_count)
cat("Passemger count min is : ",pass_min, "and max is: ",pass_max, "")

#Applying all the feature conditions on train data

#Taking copy of the data
new_train_data = train_data

#train_data = new_train_data

train_data <- filter(train_data,
                     
                     train_data$pickup_longitude >= lon_min & 
                       train_data$pickup_longitude <= lon_max &
                       
                       train_data$dropoff_longitude >= lon_min &
                       train_data$dropoff_longitude <= lon_max &
                       
                       train_data$pickup_latitude >= lat_min & 
                       train_data$pickup_latitude <= lat_max &
                       
                       train_data$dropoff_latitude >= lat_min &
                       train_data$dropoff_latitude <= lat_max &
                       
                       train_data$fare_amount >0 &
                       train_data$fare_amount < 65 &
                       
                       train_data$passenger_count >=1 &
                       train_data$passenger_count <= 6
)

#Checking the data                    
summary(train_data)
head(train_data,5)


#Deriving New variable distance for train data and test data

#Here pickup and drop locations are related to fare_amont of the data,
#So we need to find the distance using pickup and drop location coordinates by using "Haversine distance formula" 

#Loading the libararies purrr,geosphere,rlist for calculating ditance using latitudes and logitudes,
#library(purrr)
#library(geosphere)
#library(rlist)

#Haversine distance formula for finding the distance
#Function for adding a new feature distance

get_geo_distance = function(long1, lat1, long2, lat2) {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  #if (units == "km") {
  distance = distance_m / 1000.0;
  
  distance
}

#Applying distance formula for train data
for(i in (1:nrow(train_data)))
{
  train_data$distance[i]= get_geo_distance(train_data$pickup_longitude[i],train_data$pickup_latitude[i],train_data$dropoff_longitude[i],train_data$dropoff_latitude[i])
}

#Applying distance formula for test data
for(i in (1:nrow(test_data)))
{
  test_data$distance[i]= get_geo_distance(test_data$pickup_longitude[i],test_data$pickup_latitude[i],test_data$dropoff_longitude[i],test_data$dropoff_latitude[i])
}

#Removing observations those have distance of  0 Km in train data
train_data_d = train_data
train_data <- filter(train_data,
                     train_data$distance>0)


#Checking the train data
summary(train_data)
head(train_data,4)

#Checking the test data
summary(test_data)
head(test_data,4)

#Removing generated null values
train_data = train_data %>% drop_na()

#checking the train data
summary(train_data)

#-------Checking the Distribution of the data ---------------------#
continuous_variables = c('year','month','fare_amount','passenger_count','pickup_longitude',
                         'pickup_latitude','dropoff_longitude','dropoff_latitude','distance')

#histogram of continuous variables
#fare amount
ggplot(train_data, aes_string(x = train_data$fare_amount)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Fare amount") + ylab("Frequency")

#pickup latitude
ggplot(train_data, aes_string(x = train_data$pickup_latitude)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Pickup latitude") + ylab("Frequency")

#pickup longitude
ggplot(train_data, aes_string(x = train_data$pickup_longitude)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Pickup longitude") + ylab("Frequency")

#dropoff latitude
ggplot(train_data, aes_string(x = train_data$dropoff_latitude)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Dropoff latitude") + ylab("Frequency")

#dropoff longitude
ggplot(train_data, aes_string(x = train_data$dropoff_longitude)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Dropoff longitude") + ylab("Frequency")

#distance
ggplot(train_data, aes_string(x = train_data$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Distance") + ylab("Frequency")

#Scatter plot between distance and fare
ggplot(train_data, aes(x = fare_amount, y =distance))+geom_point(color='red')+xlab('Fare Amount')+ylab('Distance')

#Hypothesis Assumptions
#1 - Check the pickup date and time affect the fare or not

ggplot(train_data, aes(x = day, y =fare_amount))+geom_point(color='green')+xlab('Day')+ylab('Fare Amount')
ggplot(train_data, aes(x = hour, y =fare_amount))+geom_point(color='green')+xlab('Hour')+ylab('Fare Amount')

#2 - Number of Passengers vs Fare
ggplot(train_data, aes(x = passenger_count, y =fare_amount))+xlim(1,6)+geom_point(color='green')+xlab('Passenger Count')+ylab('Fare Amount')

#3 - Does the day of the week affect the fare?
ggplot(train_data, aes(x = dayOfWeek, y =fare_amount))+geom_point(color='green')+xlab('Day of the Week')+ylab('Fare Amount')

#-------Feature Selection/Feature Scaling-------------#
#Checking Correlation

con= c('fare_amount', 'pickup_longitude', 'pickup_latitude',
       'dropoff_longitude', 'dropoff_latitude','distance')


#Correlation Plot 
corrgram(train_data[,continuous_variables], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Removing pickupdate variable as it is no longer required

#Checking multicollinearity using VIF
con= c('fare_amount', 'pickup_longitude', 'pickup_latitude',
       'dropoff_longitude', 'dropoff_latitude','distance')

df_vif = train_data[,con]
vifcor(df_vif)

#Selected variables for Model Building
continuous_variables

#--------------- Model Development ----------------#
#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = sample(1:nrow(train_data), 0.8 * nrow(train_data))
train = train_data[ train.index,-2]
test  = train_data[-train.index,-2]


#------Linear Regression------------#
#Building the Mdel
LR = lm(fare_amount ~ ., data = train)

#predicting for test data
y_pred = predict(LR,test[,names(test) != "fare_amount"])

# For testing data 
print(postResample(pred = y_pred, obs =test$fare_amount))

# RMSE      Rsquared       MAE 
# 5.1525245 0.7325277 2.8235090 

#------- Decision tree---------#
#Develop Model on training data
DT = rpart(fare_amount ~., data = train, method = "anova")

#predicting for test data
y_pred = predict(DT,test[,names(test) != "fare_amount"])

# For testing data 
print(postResample(pred = y_pred, obs =test$fare_amount))

#     RMSE    Rsquared       MAE 
# 4.4755224   0.7938271   2.5698910 

#---------Random Forest----------#
#Developing model on train data
RF = randomForest(fare_amount~., data = train,ntree=200)

#predicting for test data
y_pred = predict(RF,test[,names(test) != "fare_amount"])

# For testing data
print(postResample(pred = y_pred, obs = test$fare_amount))

#   RMSE      Rsquared       MAE 
# 3.9960223 0.8590887   2.2500168 

#-------HyperParameter Tuning for Random Forest Using repeatedCV-------

# Tuning Random Forest

control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')
#create tunegrid
tunegrid <- expand.grid(.mtry = c(sqrt(ncol(train))))
modellist <- list()

#train with different ntree parameters
for (ntree in c(300,500,600,800)){
  set.seed(123)
  fit <- train(fare_amount~.,
               data = train,
               method = 'rf',
               metric = 'rmse',
               tuneGrid = tunegrid,
               trControl = control,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

#Compare results
results <- resamples(modellist)
summary(results)

#Modelling with Hyperparameters
#---------Random Forest----------#
#Developing model on train data
RF = randomForest(fare_amount~., data = train,ntree=800)

#predicting for test data
y_pred = predict(RF,test_data[,names(test_data) != "pickup_datetime"])

# Salpme predicted data
print(y_pred)