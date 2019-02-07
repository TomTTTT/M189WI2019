# Load working directory

# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Desktop/UCSD/homework/math_189/case1/M189WI2019/")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW2'))

# Load Dependencies 
library(dplyr)
# Load Data
videodf1<-read.table("videodata.txt", header = T, sep = "", dec = ".")

videodf2<-read.table("videodata2.txt", header = T, sep="", dec=".")
# Rename time column in videodf2 to time2
names(videodf2)[names(videodf2) == 'time'] <- 'time2'

df <-cbind(videodf1, videodf2)

# added a new variable named like_binary; 1 = like to play, 0 = like to or never played
df <- df%>%mutate(df, like_binary = ifelse(like == 2| like == 3, 1, 0))
# added a variable named played_prior; 1 = played before survey, 0 = didn't play
df <- df%>%mutate(df, played_prior = ifelse(time > 0, 1, 0))



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


