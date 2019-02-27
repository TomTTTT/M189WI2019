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
library(knitr)
library(purrr)
library(OneR)
library(nnet)

#Load Data
df<-read.csv("Data1.txt")


########PART 1#########

#Some histograms with various bin widths for our data
a<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 3000)
b<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 4500)
c<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 6000)
d<-ggplot(df, aes(x=location)) + geom_histogram(binwidth = 10000)
grid.arrange(a,b,c,d, ncol=4)

#Strip plot of original location of palindromes
stripplot(df$location)

####Generation process###
N<-229354 #Length of CMV
n<-296 #Number of palindromes

#Random Scatter
#Another version of Monte Carlo Simulation 
set.seed(1)
unifMC<-rep(0,296)
for(i in 1:296){
  unifMC[i]<-rdunif(1, N, 1)
}

#Sort values 
unifMC<-sort(unifMC, decreasing=FALSE)
unifMC<-as.data.frame(unifMC)
unifMC_p1<-ggplot(unifMC, aes(x=unifMC)) + geom_histogram(binwidth = 4500)

set.seed(11)
unifMC<-rep(0,296)
for(i in 1:296){
  unifMC[i]<-rdunif(1, N, 1)
}

#Sort values 
unifMC<-sort(unifMC, decreasing=FALSE)
unifMC<-as.data.frame(unifMC)
unifMC_p2<-ggplot(unifMC, aes(x=unifMC)) + geom_histogram(binwidth = 4500)

set.seed(20)
unifMC<-rep(0,296)
for(i in 1:296){
  unifMC[i]<-rdunif(1, N, 1)
}

#Sort values 
unifMC<-sort(unifMC, decreasing=FALSE)
unifMC<-as.data.frame(unifMC)
unifMC_p3<-ggplot(unifMC, aes(x=unifMC)) + geom_histogram(binwidth = 4500)

#Add title later, but this is orginal compared to 3 samples
#Change colors and add titles later
grid.arrange(b,unifMC_p1,unifMC_p2,unifMC_p3, ncol=4)


# 
# 
# 
# 
# df$scatter_rand<-sort(sample.int(N, n), decreasing = FALSE)
# 
# #Strip plot of scattered palindromes
# stripplot(df$scatter) 
# random_plot<-ggplot(df, aes(x=scatter_rand)) + geom_histogram(binwidth=10000)
# 
# #Monte Carlo Uniform Simulation
# 
# B <- 1000 # 1000 MC uniform samples
# PalindromeMC <- data.frame(matrix(data = NA, ncol = B, nrow = n))
# PalindromeMC_mean <- rep(0,B)
# 
# bin_df<-data.frame(matrix(data=NA, ncol=))
# for (i in 1:B) {
#   t = numeric(B)
#   #Fill ith column with n samples and sort them from lowest to highest
#   PalindromeMC[,i] <- sort(rdunif(n, N, 1), decreasing=FALSE)
#   PalindromeMC_mean[i] <- mean(PalindromeMC[,i])
#                               
# }
# 
# nbins = 57
# #Average of palindromes in the bin
# freq<-0
# for(i in 1:B){
#   freq <- freq + as.data.frame(table(bin(PalindromeMC[,i], nbins, label = c(1:nbins))))$Freq
# }
# 
# Palindrome_freq_mean <- freq/B
# 
# 
# #Change mean vectorinto df
# PalindromeMC_mean<-as.data.frame(PalindromeMC_mean)
# 
# #Plot the means
# ggplot(PalindromeMC_mean, aes(x=PalindromeMC_mean)) + geom_histogram(binwidth = 1000)
# 
# #Another version of Monte Carlo Simulation 
# unifMC<-rep(0,296)
# for(i in 1:296){
#   unifMC[i]<-rdunif(1, N, 1)
# }
# 
# #Sort values 
# unifMC<-sort(unifMC, decreasing=FALSE)
# unifMC<-as.data.frame(unifMC)
# ggplot(unifMC, aes(x=unifMC)) + geom_histogram(binwidth = 4500)







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

#Do the same thing for the MC samples, then arrange these into a 3x3 grid possibly usingfacet wrap?





####Part 4 clusters
#These are the MLE for lambda with different bin sizes 
cut_3000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=3000), dig.lab =7)))
c3m<-round(mean(cut_3000$Freq),2)
#3.87

cut_4500<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=4500), dig.lab =7)))
c4m<-round(mean(cut_4500$Freq),2)
#5.78

cut_6000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=6000), dig.lab =7)))
c6m<-round(mean(cut_6000$Freq),2)
#7.74

cut_10000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=10000), dig.lab =7)))
c10m<-round(mean(cut_10000$Freq),2)
#12.73

#Table that organizes these MLE results
MLE_df <- data.frame(binwidth = c(3000, 4500, 6000, 10000),
                     value = c(c3m, c4m, c6m, c10m))

knitr::kable(MLE_df,
             row.names = FALSE,
             col.names = c("Bin Width", "$\\hat{\\lambda}$"), booktabs = TRUE,
             caption = "MLE of $\\hat{\\lambda}$", escape = FALSE)






#Test is this follows exponential distribution
#This is the estimated parameter for the estimated distribution  
294/sum(df$distance_pair, na.rm = T)
294/sum(df$distance_triplet, na.rm = T)
294/sum(df$distance_quint, na.rm = T)

#Now apply chi square 

test<-as.data.frame(table(bin(df$location, 12, label=c(1:12))))$Freq

chisq.test(test)




#####Addtional hypothesis (need to fix)
df2<-read.csv("Data2.csv")

#Create binaries
df2$hiv_binary <- ifelse(df2$hiv == "positive", 1, 0)
df2$sex<-ifelse(df2$"ï..sex" =="Male",1,0) #Male 1; Female 0
df2$sex <- factor(df2$sex)

df2$cmv_tertile_4cat <- ifelse(df2$cmv_tertile == "Negative", 0,
                               ifelse(df2$cmv_tertile == "Low", 1,
                                      ifelse(df2$cmv_tertile == "Medium", 2, 3)))
df2$cmv_tertile_4cat<-factor(df2$cmv_tertile_4cat)
test21<-glm(hiv_binary~cmv_tertile_4cat, data=df2, family=binomial())
test21predict<-as.data.frame(predict(test21))

HCMV IgG OD (as the dependent variable) and sex, HIV and TB status 
test9<-glm(cmv~, data=df2) 
             #sex + hiv_binary, data=df2)
test9<-predict(test9)
df3<-cbind(df2, test9)


# #use for later 
# set.seed(1)
# unif_scatter<-sample(c(0,1), size=229354, replace=TRUE, prob=c(229058/229354,296/229354))
# unif_scatter_location<-as.data.frame((which(unif_scatter==1)))
# colnames(unif_scatter_location)<-"location"
# ggplot(unif_scatter_location, aes(x=location)) +  geom_histogram(binwidth = 4500)