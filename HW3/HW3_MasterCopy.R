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


#####PART 1######

#Some histograms with various bin widths for our data
#Plot with bin size = 4000
a<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 3000)
b<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 4500)
c<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 6000)
d<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 10000)
grid.arrange(a,b,c,d, ncol=4)


N<-229354
n<-296

#Random Scatter
set.seed(n)
df$scatter_rand<-sort(sample.int(N, n), decreasing = FALSE)
stripplot(df$scatter) 
random_plot<-ggplot(df, aes(x=scatter_rand)) + geom_histogram(binwidth=10000)

#Uniform
set.seed(n)
unif_quant <- seq(-3,3, length.out=N)
df$scatter_unif<-sample.int(N, size=n, prob=dunif(unif_quant, min = -3, max=3))
uniform_plot<-ggplot(df, aes(x=scatter_unif)) + geom_histogram(binwidth=10000)


# #Poisson
# set.seed(n)
# poisson_quant <-seq(0, 100, length.out = N)
# df$scatter_poisson <- sample.int(N, size=n, prob=dpois(poisson_quant,5.78))
# 
# df$scatter_poisson<-rpois(10, 10)
# poisson_plot<-ggplot(df, aes(x=scatter_poisson)) + geom_histogram(binwidth=10000)
# 
# grid.arrange(random_plot, uniform_plot, poisson_plot)




####Part 2 Locations and spacings 

#Create column that gives distance between pairs of Palindromes
df$location_lag <- Lag(df$location, 1)
df$distance_pair<- df$location-df$location_lag
df$location_lag<-NULL
pair<-ggplot(df, aes(x=distance_pair)) + geom_histogram(bins=30)

#Create column that gives distance between triplets of Palindromes
df$location_lag <- Lag(df$location, 2)
df$distance_triplet <- df$location-df$location_lag
df$location_lag<-NULL
triplet<-ggplot(df, aes(x=distance_triplet)) + geom_histogram(bins=30)

#Create column that gives distance between quintuples of Palindromes 
df$location_lag <- Lag(df$location, 5)
df$distance_quint <- df$location-df$location_lag
df$location_lag<-NULL
quint<-ggplot(df, aes(x=distance_quint)) + geom_histogram(bins=30)

grid.arrange(pair,triplet,quint)




####Part 3 Counts
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



#Test is this follows exponential distribution
#This is the estimated parameter for the estimated distribution  
294/sum(df$distance, na.rm = T)
#Now apply chi square 





#use for later 
set.seed(1)
unif_scatter<-sample(c(0,1), size=229354, replace=TRUE, prob=c(229058/229354,296/229354))
unif_scatter_location<-as.data.frame((which(unif_scatter==1)))
colnames(unif_scatter_location)<-"location"
ggplot(unif_scatter_location, aes(x=location)) +  geom_histogram(binwidth = 4500)