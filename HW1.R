#Load working directory
#Change this to match your own directory and make sure that your data file is in your directory
setwd("C:/Users/buwen/OneDrive/Desktop/MATH 189")

#Load dependencies
library(dplyr)

#Load dataset
df<-read.csv("babies23.txt", header = TRUE, sep="")

#Data managment
#Clean certain extreme values


#Estimations for smokers
df_smoker<- df%>%filter(smoke==1)
summary(df_smoker)


#Estimations for nonsmokers
df_nonsmoker<- df%>%filter(smoke==0)
summary(df_nonsmoker)