#Load working directory
#Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Desktop/UCSD/homework/math_189/case1/M189WI2019/")
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019'))


# # packages install
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("readxl")
# install.packages("moments")


#Load dependencies
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(readxl)
library(moments)

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

#Create indicator for smoking mothers; 1 = smoke, 0 else 
df<- df%>%mutate(smoke_binary = ifelse(smoke==c(1,2,3), 1, 0))

# Separate data into smokers and non smokers
df_smoker<- df%>%filter(smoke== c(1,2,3))
summary(df_smoker)
summary(df_smoker$wt)
summary(df_smoker$outcome)

# Estimations for nonsmokers
df_nonsmoker<- df%>%filter(smoke==0)
summary(df_nonsmoker)
summary(df_nonsmoker$wt)
summary(df_nonsmoker$outcome)

# kurtosis of normal = 3
kurtosis(rnorm(1000))
kurtosis(df_smoker)
kurtosis(df_nonsmoker)

# skewness of normal = 0
skewness(rnorm(1000))
skewness(df_smoker)
skewness(df_nonsmoker)


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

#Try overlaying histograms 
df$smoke_binary<-factor(df$smoke_binary)
# Find the mean of each group
library(plyr)
df$smoke_binary<-factor(df$smoke_binary)
cdf <- ddply(df, "smoke_binary", summarise, wt.mean=mean(wt, na.rm=T))
cdf<-cdf[-3,]

ggplot(data=subset(df, !is.na(smoke_binary)), aes(x=wt, fill=smoke_binary)) +
  geom_histogram(aes(y = ..density..),binwidth=5, alpha=.5, position="identity") +
  geom_vline(data=cdf, aes(xintercept=wt.mean,  colour=smoke_binary),
             linetype="dashed", size=1)+ ggtitle("Birth Weights") + 
  geom_density(alpha=0.3, fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)") 

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

#Try overlaying histograms 
# Find the mean of each group
gdf <- ddply(df, "smoke_binary", summarise, gestation.mean=mean(gestation, na.rm=T))
gdf<-gdf[-3,]

ggplot(data=subset(df, !is.na(smoke_binary)), aes(x=gestation, fill=smoke_binary)) +
  geom_histogram(aes(y = ..density..),binwidth=5, alpha=.5, position="identity") +
  geom_vline(data=gdf, aes(xintercept=gestation.mean,  colour=smoke_binary),
             linetype="dashed", size=1)+ ggtitle("Gestation") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Weight (oz.)") 







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

# smoking time
df_smoke_during_preg <- df%>%filter(time == c(1,2))
df_smoke_1to4yr_before_preg <- df%>%filter(time == c(3,4,5,6))
df_smoke_been_awhile <- df%>%filter(time == c(7,8))

df_smoke_during_preg$time[df_smoke_during_preg$time %in% c(1,2)] <- 1
df_smoke_1to4yr_before_preg$time[df_smoke_1to4yr_before_preg$time %in% c(3,4,5,6)] <- 2
df_smoke_been_awhile$time[df_smoke_been_awhile$time %in% c(7,8)] <- 3

summary(df_smoke_during_preg$wt)
summary(df_smoke_1to4yr_before_preg$wt)
summary(df_smoke_been_awhile$wt)

x <- ggplot(df_smoke_during_preg, aes(time, y = wt)) +
  geom_boxplot() + ggtitle("Smoked during pregnancy")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "smoked during pregnancy", y = "Weight (oz.)") +
  ylim(80, 180)

y <- ggplot(df_smoke_1to4yr_before_preg, aes(x = time, y = wt)) +
  geom_boxplot() + ggtitle("Smoked 1 to 4 years before pregnancy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "smoked 1 to 4 years prior", y = "weight (oz.)") +
  ylim(80, 180)

z <- ggplot(df_smoke_been_awhile, aes(x = time, y = wt)) +
  geom_boxplot() + ggtitle("Smoked 5+ years before pregnancy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "smoked 5+ years prior", y = "weight (oz.)") +
  ylim(80, 180)

grid.arrange(x,y,z, ncol = 3, top = textGrob("Smoking Time", gp = gpar(fontsize=20, font=3)))



#Make smoke continuous again
df$smoke<-as.numeric(df$smoke)


###################
#### BOOTSTRAP ####
###################

# finds confidence interval
ciBoot <- function(df, B, conf_lvl)
{
  mean.df = mean(df)
  sd.df = sd(df)
  t = numeric(B)
  n = length(df)
  
  
  for (i in 1:B)
  {
    boot <- sample(df, n, replace = TRUE)
    mean.b <- mean(boot)
    sd.b <- sd(boot)
    t[i] <- (mean.b - mean.df)/(sd.b/sqrt(n))
  }
  
  ci <- mean.df + (sd.df/sqrt(n)) * quantile(t, c((1-conf_lvl)/2, 1-(1-conf_lvl)/2))
  return(ci)
}

B = 10000
conf_lvl = 0.90   # confidence level

# wt variable
ci.smoker_wt = ciBoot(df_smoker$wt, B, conf_lvl)
ci.nonsmoker_wt = ciBoot(df_nonsmoker$wt, B, conf_lvl)

# gestation variable
ci.smoker_gest = ciBoot(df_smoker$gestation, B, conf_lvl)
ci.nonsmoker_gest = ciBoost(df_nonsmoker$gestation, B, conf_lvl)

# moments








##################
### Question 3 ###
##################

#converted 2500g to ounces
low_birth_weight <- 88.1849

#Create indicator for low birth weight; 1 = low, 0 else 
df<- df%>%mutate(low_birth_weight = ifelse(wt<low_birth_weight, 1, 0))

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

# information about bwtr14
# 01) 227- 499 grams 
# 02) 500 - 749 grams 
# 03) 750 - 999 grams 
# 04) 1000 - 1249 grams 
# 05) 1250 - 1499 grams 
# 06) 1500 - 1999 grams 
# 07) 2000 - 2499 grams 
# 08) 2500 - 2999 grams 
# 09) 3000 - 3499 grams 
# 10) 3500 - 3999 grams 
# 11) 4000 - 4499 grams 
# 12) 4500 - 4999 grams 
# 13) 5000 - 8165 grams 
# 14) Not Stated





