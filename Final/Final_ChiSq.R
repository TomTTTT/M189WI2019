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
rm("df1")

# Plot of times that are spiked
spiked_times_p<-ggplot(subset(df2, df2$spike==1), aes(x=time_to_failure)) + geom_histogram(binwidth=0.1) +xlim(0, 1)

# Plot of times that are not spiked
unspiked_times_p<-ggplot(subset(df2, df2$spike==0), aes(x=time_to_failure)) + geom_histogram()

grid.arrange(spiked_plot, unspiked_plot)




########################################
#Let's create a new data frame of the first n values and store it in df3
df3<-df2[1:10000000]

#n is the number of rows in df3
n<-nrow(df3)

#Initialize dataframe in order to use it for regression 
#So we make j + 1 columns such that j is the window size (50 in this case) and + 1 for the time to failure value
#The number of rows is n-(j-1) = n-j+1
j<-50
df_4 <- data.frame(matrix(ncol = j+1, nrow = n-50+1))


#Fill dataframe as desired
#for i in 1:j
for(i in 1:j){
  df_4[,i]<-df3$acoustic_data[i:(n-(j-i))]
}

#rename variable columns to be Xj
for(i in 1:j){eval(parse(text=paste0("
                                     colnames(df_4)[i] <- paste0('X','",i,"')
                                     ")))
}

#Add y column from j:n
df_4[j+1]<-df3$time_to_failure[j:n]


#Create formula for i:j
formula<-""
for(i in 1:j){eval(parse(text=paste0("
                                     formula<-paste0(formula, ' X','",i," +')
                                     ")))
}

#Remove unecessary characters
formula<-substring(formula, 2, nchar(formula)-2)

#Add dependent variable to formula X(j+1)
formula <- paste0("X51 ~ ", formula)

#Rename columns in dataframe before running regression 
lm<-lm(formula, data=df_4)
sm<-summary(lm(formula, data=df_4))




##### Create dataframe for prediction #####
#Initialize dataframe
prediction_df <- data.frame(matrix(ncol = j, nrow = 1000000-j+1))

#Fill dataframe as desired
#Let's try n=3000
df3<-df2[1000001:2000000]
n=1000000
for(i in 1:50){
  prediction_df[,i]<-df3$acoustic_data[i:(n-(50-i))]
}


#rename variable columns
for(i in 1:50){eval(parse(text=paste0("
                                      colnames(prediction_df)[i] <- paste0('X','",i,"')
                                      ")))
}



#Now predict on this subset and get the MSE
#the range for the thing you're subtracting is n+50:2*n
sum((predict(lm, prediction_df) - df2$time_to_failure[(n+50):(2*n)])^2)
#0.001904822

#Try 10,000, 25,000, 50,000
#Once we try these different window sizes, record the MSE, then see what the optimal window size it
rm('df4')
rm('df3')
rm('lm')
rm('prediction_df')





########################################
#Window size = 10000

#Memory management
remove(list = ls())
df1<-fread("train_X_1.csv")
df2<-fread("train_Y_1.csv")
df3<-cbind(df1, df2)
rm("df2")
df2<-df3
rm("df3")
rm("df1")

#Let's create a new data frame of the first n values and store it in df3
df3<-df2[1:10000000]

#Remove df2
rm("df2")
#Sample every 500
df3<-df3[seq(1, nrow(df3), 500)]

#n is the number of rows in df3
n<-nrow(df3)

#Initialize dataframe in order to use it for regression 
#So we make j + 1 columns such that j is the window size (10000 in this case) and + 1 for the time to failure value
#The number of rows is n-(j-1) = n-j+1
j<-10000
df_4 <- data.frame(matrix(ncol = (j+1), nrow =(n-j+1)))


#Fill dataframe as desired
#for i in 1:j
for(i in 1:j){
  df_4[,i]<-df3$acoustic_data[i:(n-(j-i))]
}

#rename variable columns to be Xj
for(i in 1:j){eval(parse(text=paste0("
                                     colnames(df_4)[i] <- paste0('X','",i,"')
                                     ")))
}

#Add y column from j:n
df_4[j+1]<-df3$time_to_failure[j:n]


#Create formula for i:j
formula<-""
for(i in 1:j){eval(parse(text=paste0("
                                     formula<-paste0(formula, ' X','",i," +')
                                     ")))
}

#Remove unecessary characters
formula<-substring(formula, 2, nchar(formula)-2)

#Add dependent variable to formula X(j+1)
formula <- paste0("X51 ~ ", formula)

#Rename columns in dataframe before running regression 
lm<-lm(formula, data=df_4)
sm<-summary(lm(formula, data=df_4))




##### Create dataframe for prediction #####
#Initialize dataframe
prediction_df <- data.frame(matrix(ncol = j, nrow = 1000000-j+1))

#Memory management
remove(list = ls())
df1<-fread("train_X_1.csv")
df2<-fread("train_Y_1.csv")
df3<-cbind(df1, df2)
rm("df2")
df2<-df3
rm("df3")
rm("df1")

#Fill dataframe as desired
#Let's try n=3000
df3<-df2[1000001:2000000]
n=1000000
for(i in 1:10000){
  prediction_df[,i]<-df3$acoustic_data[i:(n-(10000-i))]
}


#rename variable columns
for(i in 1:10000){eval(parse(text=paste0("
                                      colnames(prediction_df)[i] <- paste0('X','",i,"')
                                      ")))
}



#Now predict on this subset and get the MSE
#the range for the thing you're subtracting is n+10000:2*n
sum((predict(lm, prediction_df) - df2$time_to_failure[(n+10000):(2*n)])^2)