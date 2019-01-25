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
#Histogram of weight
s_weight_hist<-ggplot(df_smoker, aes(wt)) + 
  geom_histogram(bins=20) + ggtitle("Smokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)")

#Histogram of gestation 
s_gestation_hist<-ggplot(df_smoker, aes(gestation)) + 
  geom_histogram(bins=20) + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Days")

#QQ plot of weight
s_weight_qq<-ggplot(df_smoker, aes(sample = wt)) +
  stat_qq() + stat_qq_line() + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) 

#QQ plot of gestation
s_gestation_qq<-ggplot(df_smoker, aes(sample = gestation)) +
  stat_qq() + stat_qq_line() + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) 


#################
###Nonsmokers####
#################
#Histogram of weight
ns_weight_hist<-ggplot(df_nonsmoker, aes(wt)) + 
  geom_histogram(bins=20) + ggtitle("Nonsmokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)")

#Histogram of gestation  
ns_gestation_hist<-ggplot(df_nonsmoker, aes(gestation)) + 
  geom_histogram(bins=20) + ggtitle("Nonsmokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Days")

#QQ plot of weight
ns_weight_qq<-ggplot(df_nonsmoker, aes(sample = wt)) +
  stat_qq() + stat_qq_line() + ggtitle("Nonsmokers") +
  theme(plot.title = element_text(hjust = 0.5)) 

#QQ plot of gestation
ns_gestation_qq<-ggplot(df_nonsmoker, aes(sample = gestation)) +
  stat_qq() + stat_qq_line() + ggtitle("Nonsmokers") +
  theme(plot.title = element_text(hjust = 0.5)) 


#Histograms
##Maybe add a count legend that changes color with higher counts in the bins
#Side by side plot of Birth weights 
grid.arrange(s_weight_hist, ns_weight_hist, ncol=2, top = textGrob("Birth Weights",gp=gpar(fontsize=20,font=3)))

#Side by side plot of gestation
grid.arrange(s_gestation_hist, ns_gestation_hist, ncol=2, top = textGrob("Gestation",gp=gpar(fontsize=20,font=3)))

#QQ plots
#Side by side plot of Birth weights 
grid.arrange(s_weight_qq, ns_weight_qq, ncol=2, top = textGrob("Birth Weights",gp=gpar(fontsize=20,font=3)))

#Side by side plot of gestation
grid.arrange(s_gestation_qq, ns_gestation_qq, ncol=2, top = textGrob("Gestation",gp=gpar(fontsize=20,font=3)))
