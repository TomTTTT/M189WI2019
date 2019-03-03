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
## simulation (part 4) ##
################
library(OneR)

nbins <- 57
lambda.hat <- dim(df)[1]/nbins
bin<-bin(df$location, nbins, label = c(1:57))


boot = function(x, nbins)
{
  p <- numeric(nbins)

  # max # of palindromes out of all bins
  max.bin = 0
  where.cluster.is = 1
  for (i in 1:nbins)
  {
    if(max.bin < length(which(bin == i)))
    {
      max.bin <- length(which(bin == i))
      where.cluster.is <- i
    }
    
    
  }

  p <- 1 - ppois(max.bin, lambda.hat)^57
  
  
  

  return(c(p, where.cluster.is, max.bin))
}

boot(df$location, nbins)

# type 2 error
set.seed(24)
t2err = 0
for(j in 1:1000)
{
  test <- runif(296, 0, 1)
  tbins <- 57
  tbin <- bin(test, tbins, label = c(1:57))
  max.test = 0
  
  for(k in 1:tbins)
  {
    if(max.test < length(which(tbin == k))) {max.test <- length(which(tbin == k))}
  }
  t.lambda.hat <- 296/57
  if( ((1-ppois(max.test, t.lambda.hat)^57) > 0.05)) {t2err <- t2err + 1}
}
# power
1- (t2err/1000)



# create a new df without bin 24 
# then try to use the new max
# to determine if that is also a large cluster





#####################################
########### Type 2 Error ###########
####################################

# going to compare the cmv level between males and females and then check type 2 error for that

df2 <- read.csv("Data2.csv")
df2.m <- df2 %>% filter(df2$ï..sex == "Male")
df2.f <- df2 %>% filter(df2$ï..sex == "Female")

mean(df2.m$cmv)
mean(df2.f$cmv)
t.test(df2.m$cmv, df2.f$cmv)

df$location_lag <- Lag(df$location, 1)
df$distance_pair<- df$location-df$location_lag
df$location_lag<-NULL
df_pair<-ggplot(df, aes(x=distance_pair)) +
  geom_histogram(color='black', fill='red',position='dodge',alpha=0.5,bins=30) +
  theme(axis.title.x=element_blank()) + labs(y="Original")
#Triplet
df$location_lag <- Lag(df$location, 2)
df$distance_triplet <- df$location-df$location_lag
df$location_lag<-NULL
df_triplet<-ggplot(df, aes(x=distance_triplet)) + 
  geom_histogram(color='black', fill='blue',position='dodge',alpha=0.5,bins=30) + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())
#Quintuples
df$location_lag <- Lag(df$location, 5)
df$distance_quint <- df$location-df$location_lag
df$location_lag<-NULL
df_quint<-ggplot(df, aes(x=distance_quint)) + 
  geom_histogram(color='black', fill='green',position='dodge',alpha=0.5,bins=30) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

#List of dataframes to loop through for the simluations
dataframelist<-c("unifMC_1","unifMC_2","unifMC_3")

for(i in dataframelist){
  eval(parse(text=paste0("
                         #Pair
                         ",i,"$location_lag <- Lag(",i,"$location, 1)
                         ",i,"$distance_pair<- ",i,"$location-",i,"$location_lag
                         ",i,"$location_lag<-NULL
                         ",i,"_pair<-ggplot(",i,", aes(x=distance_pair)) +
                         geom_histogram(color='black', fill='red',position='dodge',alpha=0.5,bins=30) +
                         theme(axis.title.x=element_blank()) + labs(y='Simulation')
                         #Triplet
                         ",i,"$location_lag <- Lag(",i,"$location, 2)
                         ",i,"$distance_triplet <- ",i,"$location-",i,"$location_lag
                         ",i,"$location_lag<-NULL
                         ",i,"_triplet<-ggplot(",i,", aes(x=distance_triplet)) + 
                         geom_histogram(color='black', fill='blue',position='dodge',alpha=0.5,bins=30) + 
                         theme(axis.title.x=element_blank(),axis.title.y=element_blank())
                         #Quintuples
                         ",i,"$location_lag <- Lag(",i,"$location, 5)
                         ",i,"$distance_quint <- ",i,"$location-",i,"$location_lag
                         ",i,"$location_lag<-NULL
                         ",i,"_quint<-ggplot(",i,", aes(x=distance_quint)) + 
                         geom_histogram(color='black', fill='green',position='dodge',alpha=0.5,bins=30) +
                         theme(axis.title.x=element_blank(),axis.title.y=element_blank())
                         ")))
}


Exp_MLE_df <- data.frame(data=c("Original", "Simulation 1","Simulation 2","Simulation 3"),
                         value = c(0.001289471, 0.001312996, 0.001307781, 0.001300281))
dataframlist <- c("df", "unifMC_1", "unifMC_2", "unifMC_3")
tuplet <- c("_pair", "_triplet", "_quint")

N <- 1000

for(d in dataframelist)
{
  for(t in tuplet)
  {
    eval(parse(text=paste0("t2err_",d,"",t," <- 0")))
  }
}

# need to make sure that t2err calculates for individual df and tuplets
for(d in dataframelist)
{
  for(t in tuplet)
  {
    for(i in 1:N)
    {
      for(j in 1:4)
      {
        test <- rexp(100, Exp_MLE_df$value[j])
        eval(parse(text=paste0("if((t.test(",d,"",t,", mu = Exp_MLE_df$value[j])$p.value, is.na = T) > 0.05) (t2err_",d,"",t," = t2err_",d,"",t," + 1)")))
      }
      
    }
  }
}







#           data       value
# 1     Original 0.001289471
# 2 Simulation 1 0.001312996
# 3 Simulation 2 0.001307781
# 4 Simulation 3 0.001300281

failing to rejecct null = Type II error




