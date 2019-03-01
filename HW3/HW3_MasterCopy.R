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
binlist<-c(3000,4500,6000,10000)
for(i in binlist){
  eval(parse(text=paste0("
  cmv_p_",i,"<-ggplot(df, aes(x=location)) +
  geom_histogram(color='black', fill='red', binwidth = ",i,", position = 'dodge', alpha=0.5) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
  labs(x = 'DNA Base Pair Index', subtitle='Interval Size = ",i," base pairs') +
  scale_x_continuous(breaks = seq(0, 229354, 25000))
                         "))) 
}

#Compile into grid
grid.arrange(cmv_p_3000,cmv_p_4500,cmv_p_6000,cmv_p_10000, bottom=textGrob("Letter Index", gp=gpar(fontsize=15)),
             left = textGrob("Count", rot = 90, vjust = 0.4, gp=gpar(fontsize=15)), 
             top=textGrob("CMV Palindrome Locations with Different Interval Sizes",gp=gpar(fontsize=20)))


#Strip plot of original location of palindromes
strip_df<-stripplot(df$location, jitter=0.1, offset=1/3,xlab="", main="Original Data")

#Let's generate a few Monte Carlo Samples
####Generation process###
N<-229354 #Length of CMV
n<-296 #Number of palindromes

##Random Scatter
#Loop that does random scatter and generates plot
for(j in 1:3){

  #Set seed
  set.seed(j)

  #Create empty vector
  eval(parse(text=paste0("unifMC_",j,"<-rep(0,296)")))
  
  #Uniform MC generation
  for(i in 1:296){
    eval(parse(text=paste0("unifMC_",j,"[i]<-rdunif(1, N, 1)")))
  }

#Sort values and put it into dataframe
eval(parse(text=paste0("unifMC_",j,"<-sort(unifMC_",j,", decreasing=FALSE)")))
eval(parse(text=paste0("unifMC_",j,"<-as.data.frame(unifMC_",j,")")))

#Rename the location column 
eval(parse(text=paste0("colnames(unifMC_",j,")[1] <- 'location'")))

#Create plots of simulated palindrome locations
eval(parse(text=paste0("
unifMC_p",j,"<-ggplot(unifMC_",j,", aes(x=location)) +
  geom_histogram(color='black', fill='blue', binwidth = 4500, position = 'dodge', alpha=0.5) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 60, hjust = 1), axis.title.y=element_blank(), axis.title.x=element_blank()) +
  labs(title = 'CMV Palindrome Location',
       subtitle='Uniform Scatter: Iteration ",j,"') +
  scale_x_continuous(breaks = seq(0, 229354, 25000)) 

")))
eval(parse(text=paste0("strip_",j,"<-stripplot(df$location, jitter=0.1, offset=1/3, xlab='', main = 'Iteration ",j,"')")))
}

grid.arrange(strip_df, strip_1, strip_2, strip_3)

#Use histograms from original data, but remove x/y axis titles from the plots
binlist<-c(3000,4500,6000,10000)
for(i in binlist){
  eval(parse(text=paste0("
                         cmv_p_",i,"<-ggplot(df, aes(x=location)) +
                         geom_histogram(color='black', fill='red', binwidth = ",i,", position = 'dodge', alpha=0.5) +
                         theme(plot.title = element_text(hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5), 
                         axis.text.x = element_text(angle = 60, hjust = 1),
                         axis.title.x=element_blank(), axis.title.y=element_blank()) +
                         labs(x = 'DNA Base Pair Index', title = 'CMV Palindrome Location',
                         subtitle='Bin Width = ",i,"') +
                         scale_x_continuous(breaks = seq(0, 229354, 25000))
                         "))) 
}

#Use this graph in markdown 
grid.arrange(cmv_p_4500,unifMC_p1,unifMC_p2,unifMC_p3, bottom=textGrob("DNA Base Pair Index", gp=gpar(fontsize=15)),
             left = textGrob("Count", rot = 90, vjust = 0.4, gp=gpar(fontsize=15)), ncol=2,nrow=2)



######################################
####Part 2 Locations and spacings#####
######################################

#Create column that gives distance between pairs, triplets, quintuples of Palindromes

#Original
#Pair
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


#Arrange all plots into 4x3 matrix
grid.arrange(arrangeGrob(df_pair, unifMC_1_pair, unifMC_2_pair, unifMC_3_pair,nrow=4, top="Pair"),
arrangeGrob(df_triplet, unifMC_2_triplet, unifMC_2_triplet, unifMC_3_triplet,nrow=4, top="Triplet"),
arrangeGrob(df_quint, unifMC_1_quint, unifMC_2_quint, unifMC_3_quint,nrow=4, top="Decuple"), 
bottom=textGrob("Letter Index", gp=gpar(fontsize=15)),
left = textGrob("Count", rot = 90, vjust = 0.2, gp=gpar(fontsize=15)), ncol=3,as.table = FALSE)


#Estimated Lambda for exponential distributions and generate random deviates and store in respective  dataframe
set.seed(45)
dataframelist<-c("df","unifMC_1","unifMC_2","unifMC_3")
for(i in dataframelist){
  eval(parse(text=paste0(" 
  print((296-sum(is.na(",i,"$distance_pair)))/sum(",i,"$distance_pair, na.rm = T))
  ",i,"$rexp<-rexp(296, (296-sum(is.na(",i,"$distance_pair)))/sum(",i,"$distance_pair, na.rm = T))
  ")))
}

#Table that organizes these Poisson MLE results
Exp_MLE_df <- data.frame(data=c("Original", "Simulation 1","Simulation 2","Simulation 3"), value = c(0.001289471, 0.001312996, 0.001307781, 0.001300281))

knitr::kable(Exp_MLE_df,
             row.names = FALSE,
             col.names = c("Data", "$\\hat{\\lambda}$"), booktabs = TRUE,
             caption = "Exponential MLE of $\\hat{\\lambda}$", escape = FALSE)


#Change original df_pair to reflect correct title 
df_pair<-ggplot(df, aes(x=distance_pair)) +
  geom_histogram(color='black', fill='red',position='dodge',alpha=0.5, bin=20) +
  theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Original Paired Distance Histogram")

rexp_df<-ggplot(df, aes(x=rexp)) +
  geom_histogram(color='black', fill='blue',position='dodge',alpha=0.5,bins=20) +
  theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5)) + 
  ggtitle(expression(paste("Original w/ ", lambda," = 0.001289471")))

rexp_unifMC_1<-ggplot(unifMC_1, aes(x=rexp)) +
  geom_histogram(color='black', fill='green',position='dodge',alpha=0.5,bins=20) +
  theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression(paste("Sim. 1 w/ ", lambda," =  0.001312996")))

rexp_unifMC_2<-ggplot(unifMC_1, aes(x=rexp)) +
  geom_histogram(color='black', fill='green',position='dodge',alpha=0.5,bins=20) +
  theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression(paste("Sim. 2 w/ ", lambda," =  0.001307781")))

rexp_unifMC_3<-ggplot(unifMC_1, aes(x=rexp)) +
  geom_histogram(color='black', fill='green',position='dodge',alpha=0.5,bins=20) +
  theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression(paste("Sim. 3 w/ ", lambda," =  0.001300281")))

#Compile into Grid
grid.arrange(df_pair, rexp_df, rexp_unifMC_1,rexp_unifMC_2,rexp_unifMC_3,
             bottom=textGrob("Distance Between Pairs", gp=gpar(fontsize=15)),
             left = textGrob("Count", rot = 90, vjust = 0.2, gp=gpar(fontsize=15)),
             top = textGrob("Distributions of Paired Locations",gp=gpar(fontsize=24)), ncol=3,as.table = FALSE)


######################################
####Part 3############################
######################################


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

#Table that organizes these Poisson MLE results
Poisson_MLE_df <- data.frame(binwidth = c(3000, 4500, 6000, 10000),
                     value = c(c3m, c4m, c6m, c10m))

knitr::kable(MLE_df,
             row.names = FALSE,
             col.names = c("Bin Width", "$\\hat{\\lambda}$"), booktabs = TRUE,
             caption = "Poisson MLE of $\\hat{\\lambda}$", escape = FALSE)








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

