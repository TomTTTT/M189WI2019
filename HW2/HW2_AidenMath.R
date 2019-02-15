# Load working directory
# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Documents/GitHub/M189WI2019/HW2")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW2'))

library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(gridExtra)
library(readxl)
library(moments)
library(bootstrap)
# library(resample)

# Load Data
videodf1<-read.table("videodata.txt", header = T, sep = "", dec = ".")

videodf2<-read.table("videodata2.txt", header = T, sep="", dec=".")

# Rename time column in videodf2 to time2
names(videodf2)[names(videodf2) == 'time'] <- 'time2'

df <- cbind(videodf1, videodf2)

# added a new variable named like_binary; 1 = like to play, 0 = like to or never played
df <- df%>%mutate(df, like_binary = ifelse(like == 2| like == 3, 1, 0))
# added a variable named played_prior; 1 = played before survey, 0 = didn't play
df <- df%>%mutate(df, played_prior = ifelse(time > 0, 1, 0))

######################
####Data managment####
######################

# Assign NA to all 99 values in R
df[df == 99] <- NA




#######################
##### Scenario 1 ######
#######################


freq1.like = nrow(df %>% filter(like_binary == 1))
freq0.like = nrow(df %>% filter(like_binary == 0))

freq1.play = nrow(df %>% filter(played_prior == 1))
freq0.play = nrow(df %>% filter(played_prior == 0))

# total population
N <- dim(df)[1]
p.hat = freq1.play/N
# sampled error
p.se = sqrt((p.hat * (1-p.hat)) / N)

# 1.96 for 5% of people who played week prior
ci.play = c(freq1.play/N - (p.se * 1.96),
            freq1.play/N + (p.se * 1.96))

ci.play


####################
#### JACKKNIFE #####
####################

# failed code
# jk <- function(x, B, conf_lvl)
# {
#   mean.x <- mean(x, na.rm=T)
#   sd.x <- sd(x, na.rm=T)
#   t <- numeric(B)
#   n <- length(x)
#   x.hat <- numeric(n)
# 
#   
#   for (i in 1:B)
#   {
#     sd.jk = 0
#     
#     for (j in 1:n)
#     {
#       x.hat[j]<- (1/n-1) * (sum(x)-x[i])
#     }
#     
#     mean.jk <- sum(x.hat)/n
#     
#     for (k in 1:n)
#     {
#       sd.jk <- sqrt((n-1)/n * sum( (x.hat[k] - mean.jk) ^ 2))
#     }
#     
#     t <- c(mean.jk, sd.jk)
#   }
#   
#   return(t)
# }
# 
# B = 10000
# conf_lvl = 0.90
# 
# jk(df$like_binary, B, conf_lvl)









# jackknife result from like_binary mean
jk.like <- jackknife(boot.population.like, mean)
jk.like
# table from jackknife of pepole who like to play
# |Observed (mean)| SE | Mean | Bias|
# |:------:|:--:|:----:|:----|
# |0.7582418| 0.04513082| 0.7582418|0|

# jackknife result from played_prior mean
jk.play <- jackknife(boot.population.play, mean)
jk.play
# table from jackknife of people who played prior
# |Observed | SE | Mean | Bias|
# |:------:|:----:|:----:|:----:|
# |0.3736264|0.05099343|0.05099343|0|




#######################
###### BOOTY POP ######
#######################

B <- 10000
n <- 365

boot.population.like <- rep(df$like_binary, length.out = n)
boot.population.play <- rep(df$played_prior, length.out = n)


#####################
##### BOOTSTRAP #####
#####################

### MEAN ###
ci.mean.boot = function(data, B, conf_lvl)
{
  mean.data = mean(data, na.rm = TRUE)
  sd.data = sd(data, na.rm = TRUE)
  t = numeric(B)
  n = length(B)
  boot.population <- rep(data, length.out = 365)
  
  for (i in 1:B)
  {
    # boot <- sample(B, n, replace = TRUE)
    boot.sample <- sample(boot.population, size = 91, replace = F)

    mean.b <- mean(boot.sample)
    sd.b <- sd(boot.sample)
  
    # t-test statistic
    t[i] <- (mean.b - mean.data)/(sd.b/sqrt(n))
  }
  
  ci <- mean.data  + (sd.data/sqrt(n)) * quantile(t, c((1-conf_lvl)/2, 1-(1-conf_lvl)/2))
  return(ci)
}

B = 1000
conf_lvl = 0.90

# error because boot.sample is filled with NA values
ci.mean.boot(df$like_binary, B, conf_lvl)
ci.mean.boot(df$played_prior, B, conf_lvl)



### Std Dev ###
ci.var.Boot = function(data, B, n, conf_lvl)
{
  mean.data = mean(data, na.rm=T)
  sd.data = sd(data, na.rm=T)
  t = numeric(B)
  n = length(data)
  
  for (i in 1:B)
  {
    boot.sample <- sample(boot.population, size = n, replace = F)
    
    mean.b <- mean(boot.sample)
    sd.b <- sd(boot.sample)
    
    t[i] <- ((n-1) * sd.b) / qchisq(conf_lvl/2, n-1)
  }
  
  
}

n = dim(df)[1]
ci.var.boot(df$like_binary, B, n, conf_lvl)