library(tidyverse)
library(mice)
library(ggplot2)
library(VIM)
library(leaflet)
library(scales)
library(ggrepel)
library(plotly)

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

#Month
summarymonth1516 <- data.frame(table(Death15_16$month)/length(Death15_16$month)*100) 
colnames(summarymonth1516) <- c("Month", "Percent")

#Armed/Not
summaryarmed1516 <- data.frame(table(Death15_16$armed)/length(Death15_16$armed)*100)
colnames(summaryarmed1516) <- c("Armed", "Percent")

#Gender
summarygender1516 <-data.frame(table(Death15_16$gender)/length(Death15_16$gender)) 
colnames(summarygender1516) <- c("Gender", "prop")

#plot_ly(summarygender1516, labels = ~Gender, values = ~prop, type = 'pie',
    #    textposition = 'auto',
   #     insidetextfont = list(color = "#FFFFFF"),

#marker = list(colors = c("#556677", "#AA3344", "#772200"),line = list(color = '#FFFFFF', width = 1)),
  #      textinfo='label+percent') %>%
  #layout(autosize=TRUE)

  #  title = "Title",
    #xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
   # yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
    #    insidetextfont = list(color = '#FFFFFF'),
 #marker = list(colors = c("#556677", "#AA3344", "#772200"),line = list(color = '#FFFFFF', width = 1))) 



# Maps of Percentages, etc

#label <- paste0(summarygender1516$Gender, " ", round(summarygender1516$prop *100), "%")
#ggplot(summarygender1516, aes(x="", y = prop, fill=Gender)) +
 # geom_bar(width=1, stat = "identity") +
  #coord_polar("y", start = 0) + 
  #theme_void() +
  #geom_text(aes(x = 1, y = cumsum(prop)-prop/2, label = label), size =3)


