# Load working directory

# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Desktop/UCSD/homework/math_189/case1/M189WI2019/")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW2'))

# Load Dependencies 
library(dplyr)
library(ggplot2)
# Load Data
videodf1<-videodata

videodf2<-videodata2
# Rename time column in videodf2 to time2
names(videodf2)[names(videodf2) == 'time'] <- 'time2'

#Bind the two dataframes
df<-cbind(videodf1, videodf2)

# added a new variable named like_binary; 1 = like to play, 0 = like to or never played
df <- df%>%mutate(like_binary = ifelse(like == 2| like == 3, 1, 0))
# added a variable named played_prior; 1 = played before survey, 0 = didn't play
df <- df%>%mutate(played_prior = ifelse(time > 0, 1, 0))

######################
####Data managment####
######################

# Assign NA to all 99 values in R
df[df == 99] <- NA



#######################
##### Scenario 1 ######
#######################

freq1.like = nrow(df %>% filter(like_binary == 1))
freq2.like = nrow(df %>% filter(like_binary == 0))

freq1.play = nrow(df %>% filter(played_prior == 1))
freq2.play = nrow(df %>% filter(played_prior == 0))

p.hat = freq1.play/dim(df)[1]
p.se = sqrt((p.hat * (1-p.hat)) / dim(df)[1])

# 1.96 for 5%
ci.play = c(freq1.play/dim(df)[1] - (p.se * 1.96),
            freq1.play/dim(df)[1] + (p.se * 1.96))

########################
##### Scenario 2 #######
########################

ggplot(df, aes(x=freq)) + geom_histogram()
#note 13 observations are not available for frequency of play
ggplot(df, aes(x=time)) + geom_histogram()

#proportion that plays games at least every week
numPlayGames= 0
for(i in 1:length(df$freq)){
  if(df$freq[i] < 3 & !is.na(df$freq[i])){numPlayGames = numPlayGames+1}
}

numObsPlayedGames =0
for(i in 1:length(df$time)){
  if(df$time[i]>0){numObsPlayedGames= numObsPlayedGames+1}
}

propPlayGames= numPlayGames/(91-13)
propPlayWeekBefore= numObsPlayedGames/(91)

obsDaily=0
obsWeekly=0
obsMonthly=0
obsSemesterly= 0
for(i in 1:length(df$time)){
  
  if(df$time[i]>3.5){obsDaily= obsDaily+1}
  
  else if(df$time[i]<=3.5 & df$time[i]>.5){obsWeekly= obsWeekly+1}
  
  else if(df$time[i]<=.5 & df$time[i]>0){obsMonthly= obsMonthly+1}
  
  else{obsSemesterly= obsSemesterly+1}
  
}

expDaily=0
expWeekly= 0
expMonthly=0
expSemesterly= 0
for(i in 1:length(df$freq)){
  
  if(df$freq[i] == 4 | is.na(df$freq[i])){expSemesterly= expSemesterly+1}
  
  else if(df$freq[i] == 3 & !is.na(df$freq[i])){expMonthly= expMonthly+1}
  
  else if(df$freq[i] == 2 & !is.na(df$freq[i])){expWeekly= expWeekly+1}
  
  else{expDaily= expDaily+1}
}

Q1table = matrix(c(obsDaily,expDaily,obsWeekly, expWeekly,obsMonthly,expMonthly,
                   obsSemesterly,expSemesterly),ncol=2, byrow = TRUE)
colnames(Q1table)= c("observed", "expected")
rownames(Q1table)= c("Daily","Weekly", "Monthly","Semesterly/Never")

Q1table = as.table(Q1table)

Q1table

chisq.test(Q1table)

#####################
#####Question 3######
#####################





