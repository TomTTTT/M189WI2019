# Load working directory
# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Documents/GitHub/M189WI2019/HW3")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW3'))

#Load dependencies
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(stargazer)
library(tree)
library(rattle)
library(rpart)
library(rpart.plot)
library(party)
library(ggsci)
library(Hmisc)

#Load Data
df<-read.csv("Data1.txt")

#Plot with bin size = 4000
ggplot(df, aes(x=location)) + geom_histogram(binwidth = 4000)

#Create column that give distance between Palindrome
df$location_lag <- Lag(df$location, 1)
df$distance <- df$location-df$location_lag
df$location_lag<-NULL
