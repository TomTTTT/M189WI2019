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
library(lattice)

#Load Data
df<-read.csv("Data1.txt")
nbins = 57
lambda.hat = dim(df)[1]/nbins

#Plot with bin size = 4000
a<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 3000)
b<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 4500)
c<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 6000)
grid.arrange(a,b,c,ncol=3)

#These are the MLE for lambda with different bin sizes 
cut_3000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=3000), dig.lab =7)))
mean(cut_3000$Freq)
#3.87

cut_4500<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=4500), dig.lab =7)))
mean(cut_4500$Freq)
#5.78

cut_6000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=6000), dig.lab =7)))
mean(cut_6000$Freq)
#7.74

stripplot(df$location)



#Create column that give distance between Palindrome
df$location_lag <- Lag(df$location, 1)
df$distance <- df$location-df$location_lag
df$location_lag<-NULL
ggplot(df, aes(x=distance)) + geom_histogram(bins=30)

#Test is this follows exponential distribution
#This is the estimated parameter for the estimated distribution  
294/sum(df$distance, na.rm = T)
#Now apply chi square 



################
## simulation ##
################
library(OneR)

bin=bin(df, 57, label = c(1:57))


boot = function(x, B, nbins)
{
  p = numeric(B)
  mean.x = mean(x)
  sd.x = sd(x)
  bin = bin(x, nbins)
  
  # max # of palindromes out of all bins
  max.bin = 0
  
  for (i in 1:nbins)
  {
    if(max.bin < length(which(bin == i)))
    {
      max.bin = length(which(bin == i))
    }
  }

  p[i] = 1 - pexp(max.bin, 1/lambda.hat)^57
  
  pval = sum(p)/B
  return(pval)
}

B = 1000
boot(df$location, B, nbins)




