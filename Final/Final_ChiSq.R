########Final Project########

# This file export runs a chi square test to check for normality of acoustic
# signal distribution 

#Set Working Directory 
setwd("C:/Users/ADRC/Documents/MATH 189")


#Load dependencies
library(data.table)
library(ggplot2)
library(matrixStats)
library(gridExtra)

# Load  first dataset 
df1<-fread("train_X_1.csv")

# Plot histogram 
ggplot(df1, aes(x=acoustic_data)) + geom_histogram(binwidth=1) +xlim(-20,20)


######################
#Tests for Normality##
######################

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




# Shapiro Wilks test for normality
#Pull 5000 samples to allow the test to work 
ad_sample<-sample(df1$acoustic_data, replace=F, 5000)
shapiro.test(ad_sample)
# Not normal becuase p-values are very small 






##################################
#Flag the values that are extreme#
##################################

#Use 3*sD to filter out extreme vaues 
sd<-sd(df1$acoustic_data)
df1$spike<-ifelse(df1$acoustic_data >= 3*sd |df1$acoustic_data <= -3*sd, 1, 0)

subset_nospikes<-subset(df1$acoustic_data,df1$spike==0)
# Plot without spikes
ggplot(subset(df1, df1$spike==0), aes(x=acoustic_data)) + geom_histogram(binwidth=1) +xlim(-100,100)





#Tests for Normality#


# Chi square test
#Put into proportion table first 

#Create vector of culmulative proababilities for quantile
probability<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#Obtain quantiles 
a<-quantile(subset_nospikes, probability)
b<-as.numeric(a[1:length(a)])

#Create proportion table
proportion_table<-as.table(as.numeric(table(cut(subset_nospikes, breaks=c(-Inf,0,2,3,4,5,6,7,9, Inf)))))

#Perform Chi Square Goodness of Fit
chisq.test(proportion_table, p=c(0.1, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1))

#The data does not follow a normal distribtion 



# K-S Test 
#Generate Normal distributed numbers
normal_generates<-rnorm(length(subset_nospikes), mean(subset_nospikes), sd(subset_nospikes))

#Perform KS test 
ks.test(subset_nospikes, normal_generates)
#P Value is low, so not normal




# Shapiro Wilks test for normality
#Pull 5000 samples to allow the test to work 
ad_sample<-sample(subset_nospikes, replace=F, 5000)
shapiro.test(ad_sample)
# Not normal becuase p-values are very small 







############################################################
#Let's look at the corresponding times for the flagged ones#
############################################################


#Bind Corresponding Y data frame
df2<-fread("train_Y_1.csv")
df3<-cbind(df1, df2)
rm("df2")
df2<-df3
rm("df3")


# Plot of times that are spiked
spiked_times_p<-ggplot(subset(df2, df2$spike==1), aes(x=time_to_failure)) + geom_histogram()

# Plot of times that are not spiked
unspiked_times_p<-ggplot(subset(df2, df2$spike==0), aes(x=time_to_failure)) + geom_histogram()

grid