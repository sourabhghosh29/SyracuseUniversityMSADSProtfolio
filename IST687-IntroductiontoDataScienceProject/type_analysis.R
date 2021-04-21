library(jsonlite)
library(tidyverse)
airplaneData <- jsonlite::fromJSON('E:/Disk partition/Syracuse ADS/Syracuse ADS/1st Semester/IST 687 Introduction to Data Science/project/fall2019-survey-M07.json')
View(airplaneData)
summary(airplaneData)
str(airplaneData)
#'data.frame':	10282 obs. of  32 variables:
colSums(is.na(airplaneData))
#na values :
#Departure.Delay.in.Minutes:225     Arrival.Delay.in.Minutes:259
#Flight.time.in.minutes: 259      freeText: 10000

library(mice)
md.pattern(airplaneData)

library(VIM)
mice_plot <- aggr(airplaneData, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(airplaneData), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

install.packages('DMwR')
library(DMwR)
View(airplaneData)
help(factor)
#factor(airplaneData$Gender)
#View(airplaneData$Gender)
airplaneData$Gender <- factor(airplaneData$Gender, labels = "")
airplaneData$Airline.Status <- factor(airplaneData$Airline.Status, labels = "")
airplaneData$Type.of.Travel <- factor(airplaneData$Type.of.Travel, labels = "")
airplaneData$Class <- factor(airplaneData$Class, labels = "")
airplaneData$Partner.Code <- factor(airplaneData$Partner.Code, labels = "")
airplaneData$Partner.Name<- factor(airplaneData$Partner.Name, labels = "")
airplaneData$Origin.City<- factor(airplaneData$Origin.City, labels = "")
airplaneData$Origin.State<- factor(airplaneData$Origin.State, labels = "")
airplaneData$Destination.City<- factor(airplaneData$Destination.City, labels = "")
airplaneData$Destination.State<- factor(airplaneData$Destination.State, labels = "")
airplaneData$Destination.State<- factor(airplaneData$Destination.State, labels = "")
airplaneData$Destination.State<- factor(airplaneData$Destination.State, labels = "")


colSums(is.na(airplaneData))
View(airplaneData)
df = subset(airplaneData, select = -c(freeText) )
View(df)
########################implementing mice imputation
#tempData <- mice(predictorFeatures,m=2,maxit=3,meth='pmm',seed=100)
#tempData
#summary(tempData)

#######################################################
#nrow(predictorFeatures)
#predictorFeatures %>% drop_na()

#nrow(predictorFeatures)
#colSums(is.na(predictorFeatures))
new_cleaned <- na.omit(df, cols=c("Arrival.Delay.in.Minutes", "Departure.Delay.in.Minutes","Flight.time.in.minutes"))
colSums(is.na(new_cleaned))
nrow(new_cleaned)


df$Likelihood.to.recommend <- (df$Likelihood.to.recommend > 8)
df$Likelihood.to.recommend[df$Likelihood.to.recommend== TRUE]<-1

df$Likelihood.to.recommend[df$Likelihood.to.recommend== FALSE]<-0
View(df)


######################logistic regression

library(data.table)
library(plyr)
library(stringr)
colnames(df)
model2 <- glm(Likelihood.to.recommend ~ Destination.City+Origin.City+Airline.Status+    
            Age+Gender+Price.Sensitivity, data = df,family = binomial(link="logit"))
summary(model2)
kk
