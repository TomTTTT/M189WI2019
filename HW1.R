#Load working directory
#Enter your own path in the quotes in the path variable
path<-""
setwd(paste0(path,'Documents/Git/M189WI2019'))


#Load dependencies
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
df_smoker$race[df_smoker$race == 99] <- NA
#check race for 999 values
df_smoker$age[df_smoker$age == 99] <- NA
#age 99
df_smoker$ed[df_smoker$ed == 9] <-NA
#ed 9
df_smoker$ht[df_smoker$ht == 99] <-NA
#ht 99
df_smoker$wt.1[df_smoker$wt.1 == 999] <-NA
#wt.1 999
df_smoker$drace[df_smoker$drace == 99] <-NA
#drace 99
df_smoker$dage[df_smoker$dage == 99] <-NA
#dage 99
df_smoker$ded[df_smoker$ded == 9] <-NA
#ded 9
df_smoker$dht[df_smoker$dht == 99] <-NA
#dht 99
df_smoker$dwt[df_smoker$dwt == 999] <-NA
#dwt 999
#marital 
df_smoker$inc[df_smoker$inc == 98] <-NA
#inc goes to 9
#smoke 0-3
#time 0-9
#number 0-9

#Nonsmoker Check
df_nonsmoker$gestation[df_nonsmoker$gestation == 999] <-NA
#check gestation for 999 values
# we should check parities because having 13 previous pregnancies includinge deaths seems a bit weird
df_nonsmoker$race[df_nonsmoker$race == 10] <-NA
#check race for values greater than 9
df_nonsmoker$age[df_nonsmoker$age == 99] <-NA
#check for mothers age of termination 
df_nonsmoker$ht[df_nonsmoker$ht == 99] <-NA
#check for heights of mother for 99 values
df_nonsmoker$wt.1[df_nonsmoker$wt.1 == 999] <-NA
#check for heights of mother for 999 values
df_nonsmoker$drace[df_nonsmoker$drace == 99] <-NA
#check drace
df_nonsmoker$dage[df_nonsmoker$dage == 99] <-NA
#check dage
df_nonsmoker$ded[df_nonsmoker$ded == 9] <-NA
#ded
df_nonsmoker$dht[df_nonsmoker$dht == 99] <-NA
#dht
df_nonsmoker$dwt[df_nonsmoker$dwt == 999] <-NA
#dwt
df_nonsmoker$inc[df_nonsmoker$inc == 98] <-NA

summary(df_nonsmoker)

#Histogram
ggplot(df_smoker, aes(wt)) + 
  geom_histogram() + ggtitle("Weights of Smoker") + theme(plot.title = element_text(hjust = 0.5))
  


#Estimations for nonsmokers
df_nonsmoker<- df%>%filter(smoke==0)
summary(df_nonsmoker)

#Histogram
ggplot(df_nonsmoker, aes(wt)) + 
  geom_histogram() + ggtitle("Weights of Nonsmoker") + theme(plot.title = element_text(hjust = 0.5))
