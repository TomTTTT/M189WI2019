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


#Plots

#By sex
df_sex <- df %>% select("sex", "likesgames")
df_sex<-data.frame(prop.table(table(df_sex$likesgames, df_sex$sex),2))
df_sex$Freq<-round(df_sex$Freq, 3)
ggplot(df_sex, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
  xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
  scale_y_continuous(label=scales::percent, limit=c(0,1)) +
  # scale_fill_discrete() +
  geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size=18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size=18), 
        legend.title=element_text(size=14)) +
  ggtitle("Game preference by sex") + scale_fill_jama(name="Sex",
                                                        breaks=c("0", "1"),
                                                        labels=c("Female", "Male"), alpha=0.9) + 
  guides(colour = guide_legend(override.aes = list(size=15)))





#By work 
#initialize binary work variable 
df$works <- ifelse(df$work == 0, 0, 1)
df_works <- df %>% select("works", "likesgames")
df_works<-data.frame(prop.table(table(df_works$likesgames, df_works$works),2))
df_works$Freq<-round(df_works$Freq, 3)
ggplot(df_works, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
  xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
  scale_y_continuous(label=scales::percent, limit=c(0,1)) +
  # scale_fill_discrete() +
  geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size=18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size=18), 
        legend.title=element_text(size=14)) +
  ggtitle("Game preference by whether someone works") + scale_fill_jama(name="Works",
                                                       breaks=c("0", "1"),
                                                       labels=c("No", "Yes"), alpha=0.9) + 
  guides(colour = guide_legend(override.aes = list(size=15)))


#By is there a computer at home
df_home <- df %>% select("home", "likesgames")
df_home<-data.frame(prop.table(table(df_home$likesgames, df_home$home),2))
df_home$Freq<-round(df_home$Freq, 3)
ggplot(df_home, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
  xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
  scale_y_continuous(label=scales::percent, limit=c(0,1)) +
  # scale_fill_discrete() +
  geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size=18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size=18), 
        legend.title=element_text(size=14)) +
  ggtitle("Game Preference by whether someone owns computer at home") + scale_fill_jama(name="Computer\n at home",
                                                       breaks=c("0", "1"),
                                                       labels=c("No", "Yes"), alpha=0.9) + 
  guides(colour = guide_legend(override.aes = list(size=15)))

#By own a pc
df_own <- df %>% select("own", "likesgames")
df_own<-data.frame(prop.table(table(df_own$likesgames, df_own$own),2))
df_own$Freq<-round(df_own$Freq, 3)
ggplot(df_own, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
  xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
  scale_y_continuous(label=scales::percent, limit=c(0,1)) +
  # scale_fill_discrete() +
  geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size=18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size=18), 
        legend.title=element_text(size=14))+
  ggtitle("Game Preference by whether someone owns a PC") + scale_fill_jama(name="Own",
                                                      breaks=c("0", "1"),
                                                      labels=c("No", "Yes"), alpha=0.9) + 
  guides(colour = guide_legend(override.aes = list(size=15)))


#Arcade preference
#Create arcade binary variable
df$arcade <- ifelse(df$where == 1 | df$where == 4 |  df$where == 6, 1, 0)
df_arcade <- df %>% select("arcade", "likesgames")
df_arcade<-data.frame(prop.table(table(df_arcade$likesgames, df_arcade$arcade),2))
df_arcade$Freq<-round(df_arcade$Freq, 3)
ggplot(df_arcade, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
  xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
  scale_y_continuous(label=scales::percent, limit=c(0,1)) +
  # scale_fill_discrete() +
  geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size=18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size=18), 
        legend.title=element_text(size=14))+
  ggtitle("Game Preference by whether someone plays at the arcade") + scale_fill_jama(name="Plays at\n arcade",
                                                       breaks=c("0", "1"),
                                                       labels=c("No", "Yes"), alpha=0.9) + 
  guides(colour = guide_legend(override.aes = list(size=15)))




library(nnet)
multinom(factor(like) ~factor(action) + factor(adv) + factor(sim) + factor(sport) + factor(strategy) + factor(relax) + factor(coord) + factor(challenge) , data=df)







# #Where
# df_where <- df %>% select("where", "likesgames")
# df_where<-data.frame(prop.table(table(df_where$likesgames, df_where$where),2))
# df_where$Freq<-round(df_where$Freq, 3)
# ggplot(df_where, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
#   xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
#   scale_y_continuous(label=scales::percent, limit=c(0,1)) +
#   # scale_fill_discrete() +
#   geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
#   theme(plot.title = element_text(hjust = 0.5, size=20))+
#   ggtitle("Game Preference by Where") + scale_fill_jama(name="Where",
#                                                         breaks=c("1", "2", "3", "4", "5", "6"),
#                                                         labels=c("Arcade", "Home Sys", "Home Comp", "Arcade or Home Sys", "Home comp/ sys", "All 3"), alpha=0.9)



# #By Computer at home
# df_home <- df %>% select("home", "likesgames")
# df_home<-data.frame(prop.table(table(df_home$likesgames, df_home$home),2))
# df_home$Freq<-round(df_home$Freq, 3)
# ggplot(df_home, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
#   xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
#   scale_y_continuous(label=scales::percent, limit=c(0,1)) +
#   # scale_fill_discrete() +
#   geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
#   theme(plot.title = element_text(hjust = 0.5, size=20))+
#   ggtitle("Game Preference by Home") + scale_fill_jama(name="Home",
#                                                         breaks=c("0", "1"),
#                                                         labels=c("No", "Yes"), alpha=0.9)












# df_sex <- df %>% select("sex", "likesgames")
# df_sex %>% na.omit(likesgames) %>% 
#   ggplot(aes(factor(likesgames),..count..)) + 
#   geom_bar(aes(fill = factor(sex)), position = "dodge") +
#   scale_x_discrete(labels=c("0" = "No", "1" = "No")) + xlab("Likes Games") +
#   guides(fill=guide_legend(title="sex"))
# 
# #Now loop through them 
# 
# varlist<-c("sex", "home")
# for(i in varlist){
#   eval(parse(text=paste0("df_",i," <- df %>% select('",i,"', 'likesgames')
#   plot_likesgames_",i,"<-df_",i," %>% na.omit(likesgames) %>% 
#     ggplot(aes(factor(likesgames),y=..count.. / sum(..count..))) + 
#     geom_bar(aes(fill = factor(",i,")), position = 'dodge') +
#     scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) + xlab('Likes Games')")))
# }
# 
# 
# 
# 
# for(i in varlist){
#   eval(parse(text=paste0("df_",i," <- df %>% select('",i,"', 'likesgames')
#                           df_",i,"<-as.data.frame(prop.table(table(df_",i,"$likesgames, df_",i,"$",i,"),2))
#                          plot_likesgames_",i,"<-df_",i," %>% na.omit(likesgames) %>% 
#                          ggplot(aes(factor(likesgames),y=..count.. / sum(..count..))) + 
#                          geom_bar(aes(fill = factor(",i,")), position = 'dodge') +
#                          scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) + xlab('Likes Games')")))
# }
# 
# 
# 
# ggplot(test, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") + scale_y_continuous(label=scales::percent)
# 
# 
# 
# ##Correct 
# df_sex <- df %>% select("sex", "likesgames")
# df_sex<-data.frame(prop.table(table(df_sex$likesgames, df_sex$sex),2))
# df_sex$Freq<-round(df_sex$Freq, 3)
# ggplot(df_sex, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") +
#   xlab('Likes Games') + scale_x_discrete(labels=c('0' = 'No', '1' = 'Yes')) +
#   scale_y_continuous(label=scales::percent, limit=c(0,1)) +
#   # scale_fill_discrete() +
#   geom_text(aes(label=paste0(Freq*100,"%")), position=position_dodge(0.9), vjust =-1, size=5) +
#   theme(plot.title = element_text(hjust = 0.5, size=20))+
#   ggtitle("Game Preferencece by Sex") + scale_fill_jama(name="Sex",
#                                                         breaks=c("0", "1"),
#                                                         labels=c("Female", "Male"), alpha=0.9)
# 
# geom_text(aes(label=prob),position=position_dodge(0.9), vjust = .5, hjust = -.15, angle = 90, size=5)
#   
# 
# 
# 
# 
# 
# 
# 
# 
# plot_likesgames_home <-plot_likesgames_home +
#   scale_fill_discrete(name="Has computer\nat home",
#                       breaks=c("0", "1"),
#                       labels=c("No", "Yes"))
# plot_likes
# 
# ggplot(prop.table(table(df_sex$likesgames,df_sex$sex),margin=1)) 
# 
# grid.arrange(plot_likesgames_home, plot_likesgames_sex, ncol=2)
# 
# 
# tab <- with(df_sex, table(likesgames, sex))
# test<-as.data.frame(prop.table(tab, margin = 1))
# 
# ggplot(test, aes(factor(likesgames), y=)) + geom_bar(aes(fill=factor(sex)), position='dodge')
# 
# 
# 
# mydf <- data.frame(prop.table(table(diamonds$clarity, diamonds$cut),2))
# ggplot(mydf, aes(Var1, Freq, fill = Var2)) + 
#   geom_bar(position = "dodge", stat = "identity") +
#   scale_y_continuous(labels=scales::percent)
# 
# 
# 
# 
# ##Correct 
# test<-data.frame(prop.table(table(df_sex$likesgames, df_sex$sex),2))
# ggplot(test, aes(Var1, Freq, fill=Var2)) + geom_bar(position = "dodge", stat = "identity") + scale_y_continuous(label=scales::percent)
# 
