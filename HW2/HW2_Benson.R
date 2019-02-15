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
library(ggsci)

#Load Data
videodf1<-read.table("videodata.txt", header = T, sep = "", dec = ".")
videodf2<-read.table("videodata2.txt", header = T, sep="", dec=".")

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
  scale_x_discrete(labels=c("0" = "No", "1" = "No")) + xlab("Likes Games") +
  guides(fill=guide_legend(title="sex"))

#Now loop through them 

varlist<-c("sex", "home")
for(i in varlist){
  eval(parse(text=paste0("df_",i," <- df %>% select('",i,"', 'likesgames')
  plot_likesgames_",i,"<-df_",i," %>% na.omit(likesgames) %>% 
    ggplot(aes(factor(likesgames),y=..count.. / sum(..count..))) + 
    geom_bar(aes(fill = factor(",i,")), position = 'dodge') +
    scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) + xlab('Likes Games')")))
}




for(i in varlist){
  eval(parse(text=paste0("df_",i," <- df %>% select('",i,"', 'likesgames')
                          df_",i,"<-as.data.frame(prop.table(table(df_",i,"$likesgames, df_",i,"$",i,"),2))
                         plot_likesgames_",i,"<-df_",i," %>% na.omit(likesgames) %>% 
                         ggplot(aes(factor(likesgames),y=..count.. / sum(..count..))) + 
                         geom_bar(aes(fill = factor(",i,")), position = 'dodge') +
                         scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) + xlab('Likes Games')")))
}



ggplot(test, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") + scale_y_continuous(label=scales::percent)



##Correct 
df_sex <- df %>% select("sex", "likesgames")
df_sex<-data.frame(prop.table(table(df_sex$likesgames, df_sex$sex),2))
df_sex$Freq<-round(df_sex$Freq, 3)
ggplot(df_sex, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
  xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
  scale_y_continuous(label=scales::percent, limit=c(0,1)) +
  # scale_fill_discrete() +
  geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
  theme(plot.title = element_text(hjust = 0.5, size=20))+
  ggtitle("Game Preferencece by Sex") + scale_fill_jama(name="Sex",
                                                        breaks=c("0", "1"),
                                                        labels=c("Female", "Male"), alpha=0.9)

geom_text(aes(label=prob),position=position_dodge(0.9), vjust = .5, hjust = -.15, angle = 90, size=5)
  








plot_likesgames_home <-plot_likesgames_home +
  scale_fill_discrete(name="Has computer\nat home",
                      breaks=c("0", "1"),
                      labels=c("No", "Yes"))
plot_likes

ggplot(prop.table(table(df_sex$likesgames,df_sex$sex),margin=1)) 

grid.arrange(plot_likesgames_home, plot_likesgames_sex, ncol=2)


tab <- with(df_sex, table(likesgames, sex))
test<-as.data.frame(prop.table(tab, margin = 1))

ggplot(test, aes(factor(likesgames), y=)) + geom_bar(aes(fill=factor(sex)), position='dodge')



mydf <- data.frame(prop.table(table(diamonds$clarity, diamonds$cut),2))
ggplot(mydf, aes(Var1, Freq, fill = Var2)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels=scales::percent)




##Correct 
test<-data.frame(prop.table(table(df_sex$likesgames, df_sex$sex),2))
ggplot(test, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") + scale_y_continuous(label=scales::percent)

