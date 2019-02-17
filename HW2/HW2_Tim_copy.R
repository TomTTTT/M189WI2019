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



#Second attempt at tree

df_gamer$likesgames<-ifelse(df_gamer$like==2, 1, 0)

attribute<-c("action", "adv", "sim", "sport", "strategy")
for(i in attribute){
  eval(parse(text=paste0("
                          tree_",i,"<-tree(",i,"~relax + coord + master + bored + graphic, data=df_gamer)
                          plot(tree_",i,", type='uniform')
                          text(tree_",i,")
                         ")))
}



for(i in attribute){
eval(parse(text=paste0("
                          regression_",i,"<-glm(",i,"~relax + coord + master + bored + graphic, data=df_gamer, family=binomial(), na.action=na.omit)
                       ")))

}
library(stargazer)
stargazer(regression_action, regression_adv, regression_sim, regression_sport, regression_strategy, type="text", out="Attributes.txt")




#out of the people who like games, the ones who did not put action as their top choice, were
#most prevalent in the categories for relaxation = 0 and master = 0 and graphic = 0

#out of the people who liked adventure games, the people liked it because it helped them relax and the games 
#like the graphics

#out of the people 