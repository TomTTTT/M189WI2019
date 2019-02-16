# Load working directory
# Enter your own path in the quotes in the path variable
path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Desktop/UCSD/homework/math_189/case1/M189WI2019/")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW2'))

install.packages("rattle")
install.packages("party")

library(dplyr)
library(tree)
library(rattle)
library(rpart)
library(rpart.plot)
library(party)

#Load Data
videodf1<-read.table("videodata.txt", header = T, sep = "", dec = ".")
videodf2<-read.table("videodata2.txt", header = T, sep="", dec=".")

colnames(videodf2)[13] <- "time2"

df<-cbind(videodf1, videodf2)

######################
####Data managment####
######################

# Assign NA to all 99 values in R
df[df == 99] <- NA



##################
####Question 4####
##################

q4_df <- videodf %>% select()


#Separate data by people who like video games and people who don't
df_gamer<- df %>% filter(like == 2 | like == 3)
df_notgamer<- df %>% filter(like == 1 | like == 4 | like == 5)

#Create indicator for like and don't like in df
df$likesgames<- ifelse(df$like == 2 | df$like == 3, 1, 0)


data.tree <- tree(likesgames~educ+sex+age+home+math+work+own+cdrom+grade, data=df)
plot(data.tree, type="uniform")
text(data.tree)

tree1 <- rpart(likesgames~educ+sex+age+home+math+work+own+cdrom+grade, data=df)
prp(tree1)

tree2 <- ctree(likesgames~educ+sex+age+home+math+work+own+cdrom+grade, data=df, na.action = na.omit())

#Description of Tree:


