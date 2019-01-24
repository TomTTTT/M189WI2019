#Load working directory
#Change this to match your own directory and make sure that your data file is in your directory
setwd("C:/Users/buwen/OneDrive/Desktop/MATH 189")

#Load dependencies

#example of how to install: install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#Load dataset
df<-read.csv("babies23.txt", header = TRUE, sep="")

#Data managment
#Clean certain extreme values


#Estimations for smokers
df_smoker<- df%>%filter(smoke==1)
summary(df_smoker)

df_smoker$age[df_smoker$age == 99] <- NA
df_smoker$gestation[df_smoker$gestation == 999] <- NA
#check gestation for 999 values
#check race for 999 values
#age 99
#ed 9
#ht 99
#wt.1 999
#drace 99
#dage 99
#ded 9
#dht 99
#dwt 999
#marital 
#inc goes to 9
#smoke 0-3
#time 0-9
#number 0-9

#Histogram
ggplot(df_smoker, aes(wt)) + 
  geom_histogram() + ggtitle("Weights of Smoker") + theme(plot.title = element_text(hjust = 0.5))
  


#Estimations for nonsmokers
df_nonsmoker<- df%>%filter(smoke==0)
summary(df_nonsmoker)

#Histogram
ggplot(df_nonsmoker, aes(wt)) + 
  geom_histogram() + ggtitle("Weights of Nonsmoker") + theme(plot.title = element_text(hjust = 0.5))
