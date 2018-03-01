library(tidyverse)
library(mice)
library(ggplot2)
library(VIM)
library(leaflet)

Death15_16 <- read_csv("2015_2016.csv")

# aggr(Death15_16, prop=F, numbers=T) 
# complete lat long with city, state

# Factors
Death15_16$raceethnicity <- as.factor(Death15_16$raceethnicity)
Death15_16$gender <- as.factor(Death15_16$gender)
Death15_16$armed <- as.factor(Death15_16$armed)
Death15_16$age <- as.numeric(Death15_16$age)
Death15_16$month <- as.factor(Death15_16$month)
Death15_16$state <- as.factor(Death15_16$state)
Death15_16$month <- as.factor(Death15_16$month)
Death15_16$mannerofdeath <- as.factor(Death15_16$mannerofdeath)

#color coded

summary(Death15_16)

# Summary Counts


## Summary Statistics 2015-2016

#Race
summaryrace1516 <-  data.frame(table(Death15_16$raceethnicity)/length(Death15_16$raceethnicity)*100) 
colnames(summaryrace1516) <- c("Race/Ethnicity", "Percent")

#Average Age
summaryage1516 <- data.frame(mean(Death15_16$age, na.rm= TRUE))
colnames(summaryage1516) <- c("AverageAge")

#Gender
summarygender1516 <-data.frame(table(Death15_16$gender)/length(Death15_16$gender)*100) 
colnames(summarygender1516) <- c("Gender", "Percent")

#Month
summarymonth1516 <- data.frame(table(Death15_16$month)/length(Death15_16$month)*100) 
colnames(summarymonth1516) <- c("Month", "Percent")

#Armed/Not
summaryarmed1516 <- data.frame(table(Death15_16$armed)/length(Death15_16$armed)*100)
colnames(summaryarmed1516) <- c("Armed", "Percent")


#Percentages of people killed by gender, race = Filter by raceethnicity / total # rows
#Counts of people killed. # unique race 
# I.e. spreadsheet of counts and of percentages
# this code should determine how pull demographics as inputs go in


# Leaflet tes
#leaflet(data = Death15_16) %>% 
# addTiles() %>%
#addCircles(~longitude, ~latitude)

