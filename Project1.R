library(tidyverse)
library(haven)
library(sjmisc)
library(plyr)
library(ggplot2)
library(dplyr)

OrigFlight <- read.csv("OrigFlight.csv")

names(OrigFlight)
#DataWrangling

#removed unsignificant varibales
OrigFlight<-select(OrigFlight, -c(ActualElapsedTime, UniqueCarrier, CRSDepTime))
OrigFlight$NASDelay <- NULL
OrigFlight$Diverted <- NULL
OrigFlight$TaxiOut <- NULL
OrigFlight$Dest <- NULL
OrigFlight$ArrDelay <- NULL
OrigFlight$DayOfWeek <- NULL
OrigFlight$Year <- NULL
OrigFlight$DayofMonth <- NULL
OrigFlight$X <- NULL
OrigFlight$ArrTime <- NULL
OrigFlight$FlightNum <- NULL
OrigFlight$CRSElapsedTime <- NULL
OrigFlight$DepDelay <- NULL
OrigFlight$Distance <- NULL
OrigFlight$Cancelled <- NULL
OrigFlight$CarrierDelay <- NULL
OrigFlight$Month <- NULL
OrigFlight$DepTime <- NULL
OrigFlight$CRSArrTime <- NULL
OrigFlight$TailNum <- NULL
OrigFlight$AirTime <- NULL
OrigFlight$CancellationCode <- NULL
OrigFlight$TaxiIn <- NULL

#rename variables
OrigFlight <- rename(OrigFlight, c("WeatherDelay"="Weather", "SecurityDelay"= "Security", "LateAircraftDelay" = "LateAircraft"))

#remove na
OrigFlight1 <- na.omit(OrigFlight)
View(OrigFlight1)

#Create subset data
OrigFlight1 <- subset(OrigFlight1, Origin == "ATL"| Origin == "LAX")


write.csv(OrigFlight1, file = "OrigFlight1.csv")

name_rows(OrigFlight1)

#Creating training and datasets 80-20


# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(OrigFlight1$LateAircraft, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- OrigFlight1[-validation_index,]
# use the remaining 80% of data to training and testing the models
OrigFlight1<- OrigFlight1[validation_index,]

#Exploratory Data Anaylsis
dim(OrigFlight1)

sapply(OrigFlight1, class)

head(OrigFlight1)

levels(OrigFlight1$LateAircraft)


# summarize the class distribution
percentage <- prop.table(table(OrigFlight1$LateAircraft)) * 100
cbind(freq=table(OrigFlight1$LateAircraft), percentage=percentage)

summary(OrigFlight1)


#stats Ana-ScatterPlots
ggplot(OrigFlight1, aes(x = Origin, y = LateAircraft, color = Origin)) + geom_jitter() + ggtitle("LateAircraft vs Origin")


3       
ggplot(OrigFlight1, aes(x = Origin, y = Weather, color = Origin)) + geom_jitter() +ggtitle("Weather vs Origin")

#Machine Learning
OrigFlight1 = read.csv("OrigFlight1.csv")
str(OrigFlight1)

summary(OrigFlight1)

#MachineLearning models with data
modell = lm(LateAircraft ~ Origin, data=OrigFlight1)
summary(modell)

modell$residuals

SSE =sum(modell$residuals^2)
SSE

model2 =lm(LateAircraft ~ Origin + Security + Weather, data=OrigFlight1)
summary(model2)


model3 =lm(LateAircraft ~ Security, data=OrigFlight1)
summary(model3)


control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(LateAircraft ~ Origin + Security + Weather, data=OrigFlight1, method="lm", metric="RMSE", trControl=control)
# display results
print(fit)
summary(fit)

model4 =lm(LateAircraft ~ Origin + Security + Weather, data=validation)
summary(model4)


