#Load working directory
#Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# path<-"C:/Users/Aiden/"
# path<-"C:/Users/buwen/"
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
df$smoke[df$smoke==9]<-NA


#Separate data into smokers and non smokers
df_smoker<- df%>%filter(smoke== c(1,2,3))
summary(df_smoker)
summary(df_smoker$wt)
summary(df_smoker$outcome)

#Estimations for nonsmokers
df_nonsmoker<- df%>%filter(smoke==0)
summary(df_nonsmoker)
summary(df_nonsmoker$wt)
summary(df_nonsmoker$outcome)

#Find a way to print publishable summary statistics tables for our desired variables

#################
#####Plots#######
#################

###Histograms###
#weight
s_weight_hist<-ggplot(df_smoker, aes(wt)) + 
  geom_histogram(bins=20) + ggtitle("Smokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)") + xlim(50, 180)

ns_weight_hist<-ggplot(df_nonsmoker, aes(wt)) + 
  geom_histogram(bins=20) + ggtitle("Nonsmokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)") + xlim(50, 180)

#Maybe add a count legend that changes color with higher counts in the bins
#Side by side plot of Birth weights 
grid.arrange(s_weight_hist, ns_weight_hist, ncol=2, top = textGrob("Birth Weights",gp=gpar(fontsize=20,font=3)))

#gestation 
s_gestation_hist<-ggplot(df_smoker, aes(gestation)) + 
  geom_histogram(bins=20) + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Days") + xlim(200, 350)

ns_gestation_hist<-ggplot(df_nonsmoker, aes(gestation)) + 
  geom_histogram(bins=20) + ggtitle("Nonsmokers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Days") + xlim(200, 350)

grid.arrange(s_gestation_hist, ns_gestation_hist, ncol=2, top = textGrob("Gestation",gp=gpar(fontsize=20,font=3)))


###QQ plots###
#weight
s_weight_qq<-ggplot(df_smoker, aes(sample = wt)) +
  stat_qq() + stat_qq_line() + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) 

ns_weight_qq<-ggplot(df_nonsmoker, aes(sample = wt)) +
  stat_qq() + stat_qq_line() + ggtitle("Nonsmokers") +
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(s_weight_qq, ns_weight_qq, ncol=2, top = textGrob("Birth Weights",gp=gpar(fontsize=20,font=3)))

#gestation
s_gestation_qq<-ggplot(df_smoker, aes(sample = gestation)) +
  stat_qq() + stat_qq_line() + ggtitle("Smokers") +
  theme(plot.title = element_text(hjust = 0.5)) 

ns_gestation_qq<-ggplot(df_nonsmoker, aes(sample = gestation)) +
  stat_qq() + stat_qq_line() + ggtitle("Nonsmokers") +
  theme(plot.title = element_text(hjust = 0.5)) 


###Box plots###
#Factorize smoke in order to plot correctly, and then make sure it's unfactorized after the boxplots
#Need to rename X axis for the graphs make sense
df$smoke<-factor(df$smoke)

ggplot(df, aes(x=smoke, y=wt)) + 
  geom_boxplot() + ggtitle("Birth Weight") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Smoking Status", y = "Weight (oz.)")

ggplot(df, aes(x=smoke, y=gestation)) + 
  geom_boxplot() + ggtitle("Gestation time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Smoking Status", y = "Days")

grid.arrange(s_gestation_qq, ns_gestation_qq, ncol=2, top = textGrob("Gestation",gp=gpar(fontsize=20,font=3)))

#Make smoke continuous again
df$smoke<-as.numeric(df$smoke)




















##################
### Question 3 ###
##################

#converted 2500g to ounces
low_birth_weight <- 88.1849

#get frequency of smokers
smoker_freq <- nrow(df_smoker %>% filter(wt < low_birth_weight)) / nrow(df_smoker)

#get frequency of nonsmokers
nonsmoker_freq <- nrow(df_nonsmoker %>% filter(wt < low_birth_weight)) / nrow(df_nonsmoker)

#Finding Frequencies with low-weight babies added/subtracted
smoker_freq_addded <- (nrow(df_smoker %>% filter(wt < low_birth_weight)) + 5 )/ nrow(df_smoker)
smoker_freq_subtracted <- (nrow(df_smoker %>% filter(wt < low_birth_weight)) - 5 )/ nrow(df_smoker)
nonsmoker_freq_added <- (nrow(df_nonsmoker %>% filter(wt < low_birth_weight)) + 5 )/ nrow(df_nonsmoker)
nonsmoker_freq_subtracted <- (nrow(df_nonsmoker %>% filter(wt < low_birth_weight)) - 5 )/ nrow(df_nonsmoker)



##########################
### MORTALITY DATA #######
##########################
data <- read.csv("linkco2012us_num.csv") # for motality
data2 <- read.csv("linkco2012us_den.csv") # data for birth to get weight and compare with data$aged below

# details on google doc
summary(data$aged) # how long the infant survived for in days
summary(data$BRTHWGT)
