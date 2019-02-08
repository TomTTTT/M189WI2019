# Load working directory
# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Desktop/UCSD/homework/math_189/case1/M189WI2019/")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW2'))

#Load dependencies
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

#Load Data
df1<-read.table("videodata.txt", header = T, sep = "", dec = ".")
df2<-read.table("videodata2.txt", header = T, sep="", dec=".")

df<-cbind(df1, df2)

# added a new variable named like_binary; 1 = like to play, 0 = like to or never played
df <- df%>%mutate(df, like_binary = ifelse(like == 2| like == 3, 1, 0))
# added a variable named played_prior; 1 = played before survey, 0 = didn't play
df <- df%>%mutate(df, played_prior = ifelse(time > 0, 1, 0))

######################
####Data managment####
######################

# Assign NA to all 99 values in R
df[df == 99] <- NA


#Question 5

#Separate data by people who like video games and people who don't
df_gamer<- df %>% filter(like == 2 | like == 3)
df_notgamer<- df %>% filter(like == 1 | like == 4 | like == 5)

#Create indicator for like and don't like in df
df$likesgames<- ifelse(df$like == 2 | df$like == 3, 1, 0)

#Find which person didn't fill in the like question
which(is.na(df$like))
#Person 49

# Grouped



df_sex <- df %>% select("sex", "likesgames")
df_sex %>% na.omit(likesgames) %>% 
  ggplot(aes(factor(likesgames),..count..)) + 
  geom_bar(aes(fill = factor(sex)), position = "dodge") +
  scale_x_discrete(labels=c("0" = "No", "1" = "No")) + xlab("Likes Games")

#Now loop through them 

varlist<-c("sex", "home")
for(i in varlist){
  eval(parse(text=paste0("df_",i," <- df %>% select('",i,"', 'likesgames')
  plot_likesgames_",i,"<-df_",i," %>% na.omit(likesgames) %>% 
    ggplot(aes(factor(likesgames),..count..)) + 
    geom_bar(aes(fill = factor(",i,")), position = 'dodge') +
    scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) + xlab('Likes Games')")))
}

grid.arrange(plot_likesgames_home, plot_likesgames_sex, ncol=2)










