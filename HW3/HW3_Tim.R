# Load working directory
# Enter your own path in the quotes in the path variable
#path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Documents/GitHub/M189WI2019/HW3")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW3'))
options(scipen = 999)

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
  labs(x = 'DNA Base Pair Index', subtitle='Bin Width = ",i," base pairs') +
  scale_x_continuous(breaks = seq(0, 229354, 25000))
                         "))) 
}
grid.arrange(cmv_p_3000,cmv_p_4500,cmv_p_6000,cmv_p_10000, bottom=textGrob("Letter Index", gp=gpar(fontsize=15)),
             left = textGrob("Count", rot = 90, vjust = 0.4, gp=gpar(fontsize=15)), 
             top=textGrob("CMV Palindrome Locations with Different Bin Withs ",gp=gpar(fontsize=20)))


#Strip plot of original location of palindromes
strip_df<-stripplot(df$location, jitter=0.1, offset=1/3,xlab="", main="Original Data")

#Let's generate a few Monte Carlo Samples
####Generation process###vv
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




####Part 2 Locations and spacings 

#Create column that gives distance between pairs, triplets, quintuples of Palindromes

#List of dataframes to loop through
dataframelist<-c("df", "unifMC_1","unifMC_2","unifMC_3")

for(i in dataframelist){
  eval(parse(text=paste0("
  #Pair
  ",i,"$location_lag <- Lag(",i,"$location, 1)
  ",i,"$distance_pair<- ",i,"$location-",i,"$location_lag
  ",i,"$location_lag<-NULL
  ",i,"_pair<-ggplot(",i,", aes(x=distance_pair)) +
                    geom_histogram(color='black', fill='red',position='dodge',alpha=0.5,bins=30) +
                    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) 
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
arrangeGrob(df_quint, unifMC_1_quint, unifMC_2_quint, unifMC_3_quint,nrow=4, top="Quintuple"), 
bottom=textGrob("Letter Index", gp=gpar(fontsize=15)),
left = textGrob("Count", rot = 90, vjust = 0.2, gp=gpar(fontsize=15)), ncol=3,as.table = FALSE)


####Part 3
#These are the MLE for lambda with different bin sizes 
cut_3000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=3000), dig.lab =7)))
c3m<-round(mean(cut_3000$Freq),2)
bin_val_3000 <- trunc(229354/3000)
freq_3000 <- as.data.frame(table(cut_3000$Freq))
#Generating Frequency Chart for 3000
expected <- c()
for(i in 0:30){
  expected <- append(expected,dpois(i, c3m) * bin_val_3000)
}

freq_3000$Var1 <- as.numeric(freq_3000$Var1)

Poison_3_table <- (data.frame("Var" = 0:30,
                              "Expected" = expected)) %>% left_join(freq_3000, by = c("Var" = "Var1"))
Poison_3_table[is.na(Poison_3_table)] <- 0

#Poisson 3000
Poison_3_table$Var <- as.character(Poison_3_table$Var)
cleaned_poisson_3 <- rbind(data.frame("Var" = "0-1", "Expected" = 7.7201, "Freq" = 1),
                           Poison_3_table[3:7,],
                           data.frame("Var" = "7+", "Expected" = 7.415565, "Freq" = 16))

c3m_chi_square_val <- 0 
for (i in 1:nrow(cleaned_poisson_3)){
  expected_val <- cleaned_poisson_3[i,2]
  diff <- cleaned_poisson_3[i,3] - expected_val
  c3m_chi_square_val <- c3m_chi_square_val + (diff^2)/expected_val
}


#4500
cut_4500<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=4500), dig.lab =7)))
c4m<-round(mean(cut_4500$Freq),2)
freq_4500 <- as.data.frame(table(cut_4500$Freq))
bin_val_4500 <- trunc(229354/4500)

#Generating Frequency Chart for 4500
expected <- c()
for(i in 0:30){
  expected <- append(expected,dpois(i, c4m) * bin_val_4500)
}

freq_4500$Var1 <- as.numeric(freq_4500$Var1)

Poison_4_table <- (data.frame("Var" = 0:30,
                              "Expected" = expected)) %>% left_join(freq_4500, by = c("Var" = "Var1"))
Poison_4_table[is.na(Poison_4_table)] <- 0

#Poisson 4500
Poison_4_table$Var <- as.character(Poison_4_table$Var)
cleaned_poisson_4 <- rbind(data.frame("Var" = "0-3", "Expected" = 8.597073, "Freq" = 12 ),
                           Poison_4_table[5:8,],
                           data.frame("Var" = "8+", "Expected" = 11.31634, "Freq" = 12))

c4m_chi_square_val <- 0 
for (i in 1:nrow(cleaned_poisson_4)){
  expected_val <- cleaned_poisson_4[i,2]
  diff <- cleaned_poisson_4[i,3] - expected_val
  c4m_chi_square_val <- c4m_chi_square_val + (diff^2)/expected_val
}



#5.78

cut_6000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=6000), dig.lab =7)))
c6m<-round(mean(cut_6000$Freq),2)
freq_6000 <- as.data.frame(table(cut_6000$Freq))
bin_val_6000 <- trunc(229354/6000)

#Generating Frequency Chart for 6000
expected <- c()
for(i in 0:30){
  expected <- append(expected,dpois(i, c6m) * bin_val_6000)
}

freq_6000$Var1 <- as.numeric(freq_6000$Var1)

Poison_6_table <- (data.frame("Var" = 0:30,
                              "Expected" = expected)) %>% left_join(freq_6000, by = c("Var" = "Var1"))
Poison_6_table[is.na(Poison_6_table)] <- 0

#Poisson 6000
Poison_6_table$Var <- as.character(Poison_6_table$Var)
cleaned_poisson_6 <- rbind(data.frame("Var" = "0-5", "Expected" = 13.15367, "Freq" = 20),
                           Poison_6_table[8:9,],
                           data.frame("Var" = "9-10", "Expected" = 8.05756, "Freq" = 5),
                           data.frame("Var" = "11+", "Expected" = 6.048498, "Freq" = 5))

c6m_chi_square_val <- 0 
for (i in 1:nrow(cleaned_poisson_6)){
  expected_val <- cleaned_poisson_6[i,2]
  diff <- cleaned_poisson_6[i,3] - expected_val
  c6m_chi_square_val <- c6m_chi_square_val + (diff^2)/expected_val
}

#7.74

cut_10000<-as.data.frame(table(cut(df$location, breaks=seq(0,229354, by=10000), dig.lab =7)))
c10m<-round(mean(cut_10000$Freq),2)
freq_10000 <- as.data.frame(table(cut_10000$Freq))
bin_val_10000 <- trunc(229354/10000)

#Generating Frequency Chart for 10000
expected <- c()
for(i in 0:50){
  expected <- append(expected,dpois(i, c10m) * bin_val_10000)
}

freq_10000$Var1 <- as.numeric(freq_10000$Var1)

Poison_10_table <- (data.frame("Var" = 0:30,
                               "Expected" = expected)) %>% left_join(freq_10000, by = c("Var" = "Var1"))
Poison_10_table[is.na(Poison_10_table)] <- 0



##### CALCULATING CHI SQUARE VALUES #####
pchisq(c3m_chi_square_val, nrow(cleaned_freq_3000) - 2, lower.tail = FALSE)
pchisq(c4m_chi_square_val, nrow(cleaned_freq_4500) - 2, lower.tail = FALSE)
pchisq(c6m_chi_square_val, nrow(cleaned_freq_6000) - 2, lower.tail = FALSE)



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
df2$sex<-ifelse(df2$"?..sex" =="Male",1,0) #Male 1; Female 0
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
