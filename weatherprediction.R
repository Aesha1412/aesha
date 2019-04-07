# Weather Prediction.
setwd("D:/aesha/weather") #("set working directory as shown in the class")
weather <- read.csv("nyc.csv")

#Explore the dataset 
str(weather)
View(weather)
table(is.na(weather))
summary(weather)

#Remove null values

weather$Humidity[is.na(weather$Humidity)] <- median(weather$Humidity, na.rm = TRUE)
summary(weather)
weather$Pressure[is.na(weather$Pressure)] <- median(weather$Pressure, na.rm = TRUE)
summary(weather)
weather$Temperature[is.na(weather$Temperature)] <- median(weather$Temperature, na.rm = TRUE)
summary(weather)

# Modeling - Logistic regression
str(weather)
summary(weather)

#Data Partitioning

library(caTools)
set.seed(100)
spl = sample.split(weather$Weather_Description, 0.7)
wtrain = subset(weather, spl==TRUE)
wtest = subset(weather, spl==FALSE)
prop.table(table(weather$Weather_Description))
prop.table(table(wtrain$Weather_Description))
prop.table(table(wtest$Weather_Description))
wtrain$Weather_Description <- ifelse(wtrain$Weather_Description == "sky is clear", 1,0)
View(wtrain)
View(wtest)

#Building the Logistic Model and checking the model summary
modLog = glm(Weather_Description ~ Humidity + Pressure + Temperature + Wind_Direction + Wind_Speed,
             data = wtrain, family = "binomial")

summary(modLog)

cor(wtrain$Humidity, wtrain$Pressure)
cor(wtrain$Temperature, wtrain$Pressure)
cor(wtrain$Wind_Direction, wtrain$Pressure)
cor(wtrain$Wind_Speed, wtrain$Pressure)

library(car)
vif(modLog)

table(wtrain$Weather_Description)


