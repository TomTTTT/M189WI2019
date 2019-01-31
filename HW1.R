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
mortality_df <- read_excel('mort2012.xlsx')

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
df$time[df$time==98]<-NA

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

##some variables might be usefull
numSmokers= length(df_smoker$id)
numNonSmokers= length(df_nonsmoker$id)
numUnderweightSmokers = 0
for(i in 1:numSmokers){
  if (df_smoker$wt[i]< 88.1){ numUnderweightSmokers= numUnderweightSmokers+1}
}
numUnderweightNonSmokers = 0
for(i in 1:numNonSmokers){
  if(df_nonsmoker$wt[i]<88.1){ numUnderweightNonSmokers= numUnderweightNonSmokers+1}
}

numHealthyWeightSmokers = numSmokers - numUnderweightSmokers
numHealthyWeightNonSmokers = numNonSmokers- numUnderweightNonSmokers


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
  mean.df = mean(df, na.rm = TRUE)
  sd.df = sd(df, na.rm = TRUE)
  t = numeric(B)
  n = length(df)
  
  
  for (i in 1:B)
  {
    boot <- sample(df, n, replace = TRUE)
    mean.b <- mean(boot)
    sd.b <- sd(boot)
    
    # t-test statistic
    t[i] <- (mean.b - mean.df)/(sd.b/sqrt(n))
  }
  
  ci <- mean.df + (sd.df/sqrt(n)) * quantile(t, c((1-conf_lvl)/2, 1-(1-conf_lvl)/2), na.rm = TRUE)
  return(ci)
}

B <- 10000
conf_lvl <- 0.90   # confidence level

# wt variable
ci.smoker_wt <- ciBoot(df_smoker$wt, B, conf_lvl)
ci.nonsmoker_wt <- ciBoot(df_nonsmoker$wt, B, conf_lvl)

# gestation variable
ci.smoker_gest <- ciBoot(df_smoker$gestation, B, conf_lvl)
ci.nonsmoker_gest <- ciBoot(df_nonsmoker$gestation, B, conf_lvl)

ci.smoker_wt
ci.nonsmoker_wt

ci.smoker_gest
ci.nonsmoker_gest

#######################
##### MONTE CARLO #####
#######################

mc <- function(df, B, conf_lvl, mu, sigma)
{
  mean.df <- mean(df, na.rm = TRUE)
  sd.df <- sd(df, na.rm = TRUE)
  t <- numeric(B)
  n <- length(df)
  
  for(i in 1:B)
  {
    boot <- rnorm(n, mu, sigma)
    mean.mc <- mean(boot)
    sd.mc <- sd(boot)
    
    # t-test statistic
    t[i] <- (mean.mc - mean.df)/(sd.mc/sqrt(n))
  }
  
  ci <- mean.df + (sd.df/sqrt(n)) * quantile(t, c((1-conf_lvl)/2, 1-(1-conf_lvl)/2), na.rm = TRUE)
  return(ci)
}

B <- 10000
conf_lvl <- 0.90

# HAVE TO CHANGE THE RNORM VALUES SINCE THE DATA ISN'T STANDARD NORMAL
# wt
mc.smoker_wt <- mc(df_smoker$wt, B, conf_lvl, 0, 1)
mc.nonsmoker_wt <- mc(df_nonsmoker$wt, B, conf_lvl, 0, 1)

# gestation
mc.smoker_gest <- mc(df_smoker$gestation, B, conf_lvl, 0, 1)
mc.nonsmoker_gest <- mc(df_nonsmoker$gestation, B, conf_lvl, 0, 1)

mc.smoker_wt
mc.nonsmoker_wt

mc.smoker_gest
mc.nonsmoker_gest




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

#########################
####Testting for independence of smoking and nonsmoking on difference in probability of early bith####
#########################

testingTable = matrix(ncol=2,nrow =2)
  
testingTable[1,1]= numUnderweightNonSmokers
testingTable[1,2]= numUnderweightSmokers
testingTable[2,1]= numHealthyWeightNonSmokers
testingTable[2,2]= numHealthyWeightSmokers


chisq.test(testingTable)
#very low p-value there is a dependency in smoking and low baby birth weights

######################################
###Plots gestation v birthweight######
###T test for mean diff in gestation##
######################################
plot(df_nonsmoker$gestation,df_nonsmoker$wt)
plot(df_smoker$gestation, df_smoker$wt)

meanGestationSmoker = mean(df_smoker$gestation, na.rm = TRUE)
meanGestationNonSmoker = mean(df_nonsmoker$gestation, na.rm = TRUE)
t.test(df_nonsmoker$gestation, df_smoker$gestation, alternative = "two.sided", var.equal = FALSE)
#T test shows the mean gestation period is the same so the low baby birthweights is not 
#a result of premature babies

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


## Data manipulation to find frequencies of mortality data ##
age_weight_df <- mortality_df %>% select(aged, bwtr14)

early_death_df <- age_weight_df %>% filter(aged < 28) %>%
  group_by(bwtr14) %>%
  summarise(n2 = n()) %>%
  ungroup()

all_death_df <- age_weight_df %>% 
                group_by(bwtr14) %>%
                summarise(n1 = n()) %>%
                ungroup()


## Two Proportion Z - Test ##
prop.test(c(nrow(df_smoker %>% filter(wt < low_birth_weight)), nrow(df_nonsmoker %>% filter(wt < low_birth_weight))),
          c(nrow(df_smoker), nrow(df_nonsmoker)), alternative = "greater",
          conf.level = .95)

## Frequency of Weights ##
frequency_df <- early_death_df %>% merge(all_death_df, by = "bwtr14") %>% 
                  mutate(frequency = n2/n1)

## Plot of Frequency of Babies mortality < 28) 
ggplot(frequency_df, aes(bwtr14, frequency)) + geom_bar(stat = "identity")
