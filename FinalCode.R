library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)

train <- read_csv("Acctrain.csv")
train <- na.omit(train)
test <- read_csv("AcctestNoY.csv")

train$Start_Time <- as.Date(train$Start_Time)
train$Year <- format(as.Date(train$Start_Time), "%Y")
test$Year <- format(as.Date(test$Start_Time), "%Y")

blocked_i<-grep(c("blocked."), train$Description)
blocked_boolean_train <- rep(0, dim(train)[1])
blocked_boolean_train[blocked_i] <- 1
train$blocked_boolean <- blocked_boolean_train

closed_i <- grep(c("Closed"), train$Description)
closed_boolean_train <- rep(0, dim(train)[1])
closed_boolean_train[closed_i] <- 1
train$closed_boolean <- closed_boolean_train 

caution_i<-grep(c("caution"), train$Description)
caution_boolean_train <- rep(0, dim(train)[1])
caution_boolean_train[caution_i] <- 1
train$caution_boolean <- caution_boolean_train 

blocked_i_test<-grep(c("blocked."), test$Description)
blocked_boolean_test <- rep(0, dim(test)[1])
blocked_boolean_test[blocked_i_test] <- 1
test$blocked_boolean <- blocked_boolean_test

closed_i_test<-grep(c("Closed"), test$Description)
closed_boolean_test <- rep(0, dim(test)[1])
closed_boolean_test[closed_i_test] <- 1
test$closed_boolean <- closed_boolean_test

caution_i_test<-grep(c("caution"), test$Description)
caution_boolean_test <- rep(0, dim(test)[1])
caution_boolean_test[caution_i_test] <- 1
test$caution_boolean <- caution_boolean_test #adding caution to test

train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)
train[sapply(train, is.logical)] <- lapply(train[sapply(train, is.logical)], as.factor)

test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], as.factor)
test[sapply(test, is.logical)] <- lapply(test[sapply(test, is.logical)], as.factor)

names <- c("Start_Time", "End_Time", "Description", "Street", "City", "County", "Zipcode", "Airport_Code", "Weather_Timestamp", "Weather_Condition", "Starting_Date", "Country", "Turning_Loop", "Wind_Direction", "Timezone", "Traffic_Calming", "Roundabout", "Bump", "Railway", "No_Exit", "Give_Way", "Amenity", "Station", "Junction", "Stop", "Astronomical_Twilight", "Nautical_Twilight", "Sunrise_Sunset", "Civil_Twilight", "Traffic_Signal", "Side", "Crossing", "Wind_Speed.mph", "Visibility.mi.")

train.sub <- train[, !(names(train) %in% names)]
test.sub <- test[, !(names(test) %in% names)]

#imputing NAs
test.sub$Temperature.F.[is.na(test.sub$Temperature.F.)] <- mean(test.sub$Temperature.F., na.rm = TRUE)
test.sub$Wind_Chill.F.[is.na(test.sub$Wind_Chill.F.)] <- mean(test.sub$Wind_Chill.F., na.rm = TRUE)
test.sub$Humidity...[is.na(test.sub$Humidity...)] <- mean(test.sub$Humidity..., na.rm = TRUE)
test.sub$Pressure.in.[is.na(test.sub$Pressure.in.)] <- mean(test.sub$Pressure.in., na.rm = TRUE)

library(randomForest)
RFmodel <- randomForest(Severity~. , data=train.sub, mtry = 3, ntree = 128, importance = TRUE)
yhat = predict(RFmodel, newdata = test.sub, type = "class")
df <- data.frame("Ob" = seq(1:15000), "SEVERITY" = yhat)
#write.csv(df, file = 'finalmod.csv', row.names = FALSE)

