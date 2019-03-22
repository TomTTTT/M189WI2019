########Final Project########

# This file export runs a chi square test to check for normality of acoustic
# signal distribution 

#Set Working Directory 
setwd("C:/Users/ADRC/Documents/MATH 189")


#Load dependencies
library(data.table)
library(ggplot2)
library(matrixStats)

# Load  first dataset 
df1<-fread("train_X_1.csv")

# Plot histogram 
ggplot(df1, aes(x=acoustic_data)) + geom_histogram(binwidth=1) +xlim(-20,20)



# Chi square test
#Put into proportion table first 

#Create vector of culmulative proababilities for quantile
probability<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#Obtain quantiles 
a<-quantile(df1$acoustic_data, probability)
b<-as.numeric(a[1:length(a)])

#Create proportion table
proportion_table<-as.table(as.numeric(table(cut(df1$acoustic_data, breaks=c(-Inf,0,2,3,4,5,6,7,9, Inf)))))

#Perform Chi Square Goodness of Fit
chisq.test(proportion_table, p=c(0.1, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1))

#The data does not follow a normal distribtion 



# K-S Test 
#Generate Normal distributed numbers
normal_generates<-rnorm(nrow(df1), mean(df1$acoustic_data), sd(df1$acoustic_data))

#Perform KS test 
ks.test(df1$acoustic_data, normal_generates)
#P Value is low, so not normal




#Shapiro Wilks test for normality
shapiro.test(df1$acoustic_data)

t.test(df1$acoustic_data,mu=0)


#rnorm(length(df1), mean(df1$acoustic_data), )

quantile(df1$acoustic_data, c(.05, 0.95))
