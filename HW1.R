#Load working directory
#Enter your own path in the quotes in the path variable
path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019'))


#Load dependencies
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

#Load dataset
df<-read.csv("babies23.txt", header = TRUE, sep="")

######################
####Data managment####
######################


df$age[df$age == 99] <- NA
df$gestation[df$gestation == 999] <- NA
df$race[df$race == 99] <- NA
df$age[df$age == 99] <- NA
df$ed[df$ed == 9] <-NA
df$ht[df$ht == 99] <-NA
df$wt.1[df$wt.1 == 999] <-NA
df$drace[df$drace == 99] <-NA
df$dage[df$dage == 99] <-NA
df$ded[df$ded == 9] <-NA
df$dht[df$dht == 99] <-NA
df$dwt[df$dwt == 999] <-NA
df$inc[df$inc == 98] <-NA


#Separate data into smokers and non smokers
df_smoker<- df%>%filter(smoke==1)
summary(df_smoker)

#Estimations for nonsmokers
df_nonsmoker<- df%>%filter(smoke==0)
summary(df_nonsmoker)

#Find a way to print publishable summary statistics tables for our desired variables

#################
#####Smokers#####
#################
#Histogram of weight nonsmokers 
s_weight<-ggplot(df_smoker, aes(wt)) + 
  geom_histogram(bins=20) + ggtitle("Smokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)")

#Histogram of gestation smokers 
s_gestation<-ggplot(df_smoker, aes(gestation)) + 
  geom_histogram(bins=20) + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Days")


#################
###Nonsmokers####
#################
#Histogram of weight for nonsmokers
ns_weight<-ggplot(df_nonsmoker, aes(wt)) + 
  geom_histogram(bins=20) + ggtitle("Nonsmokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)")

#Histogram of gestation nonsmokers 
ns_gestation<-ggplot(df_nonsmoker, aes(gestation)) + 
  geom_histogram(bins=20) + ggtitle("Nonsmokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Days")


##Maybe add a count legend that changes color with higher counts in the bins
#Side by side plot of Birth weights 
grid.arrange(s_weight, ns_weight, ncol=2, top = textGrob("Birth Weights",gp=gpar(fontsize=20,font=3)))

#Side by side plot of gestation
grid.arrange(s_gestation, ns_gestation, ncol=2, top = textGrob("Birth Weights",gp=gpar(fontsize=20,font=3)))
