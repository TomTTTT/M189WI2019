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

#Load Data
df1<-read.table("videodata.txt", header = T, sep = "", dec = ".")
df2<-read.table("videodata2.txt", header = T, sep="", dec=".")

df<-cbind(df1, df2)


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


d<-structure(list(respectfromsuperior = structure(c(1L, 1L, 1L, 
                                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 
                                                 1L, 1L, 1L, NA, 2L, 1L, 1L, 1L, 1L, 2L), .Label = c("agree", 
                                                                                                     "disagree"), class = "factor"), respectideserve = structure(c(1L, 
                                                                                                                                                                   1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                   2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L), .Label = c("agree", 
                                                                                                                                                                                                                               "disagree"), class = "factor"), undesirablechange = structure(c(2L, 
                                                                                                                                                                                                                                                                                               2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, NA, 2L, 2L, 
                                                                                                                                                                                                                                                                                               2L, 2L, 2L, 1L, 1L, NA, 1L, 2L, 1L, 2L, 2L, 2L), .Label = c("agree", 
                                                                                                                                                                                                                                                                                                                                                           "disagree"), class = "factor"), jobsecuritypoor = structure(c(2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                         2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                         2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("agree", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "disagree"), class = "factor"), promotionprospectsadequate = structure(c(2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L), .Label = c("agree", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "disagree"), class = "factor"), salaryadequate = structure(c(2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("agree", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "disagree"), class = "factor"), branch = structure(c(1L, 1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Edinburgh", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Head Office", "Manchester"), class = "factor")), .Names = c("respectfromsuperior", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "respectideserve", "undesirablechange", "jobsecuritypoor", "promotionprospectsadequate", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "salaryadequate", "branch"), class = "data.frame", row.names = c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2L, 4L, 6L, 10L, 11L, 13L, 15L, 16L, 17L, 19L, 20L, 22L, 23L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              25L, 27L, 29L, 30L, 32L, 33L, 34L, 35L, 39L, 40L, 41L, 42L, 43L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              44L, 45L))
# get the stats using aggregate
res <- aggregate(d[,1:6], list(d$branch), function(x) sum(x=="agree", na.rm = T)/length(x))
res
Group.1   respectfromsuperior respectideserve undesirablechange jobsecuritypoor promotionprospectsadequate salaryadequate
1 Edinburgh                 1.0       0.8888889         0.1111111             0.0                  0.6666667      0.4444444
2 Head Office               0.7       0.3000000         0.4000000             0.2                  0.2000000      0.0000000
3 Manchester                0.8       0.8000000         0.2000000             0.1                  0.6000000      0.2000000
# to long format
library(reshape2)
res_long <- melt(res, id.vars='Group.1')
# plot
ggplot(data=res_long, aes(x=Group.1, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())


df_summary <- aggregate(df[,c("sex")], list(df$likesgames), function(x) sum(x=="1", na.rm=T)/length(x))

library(reshape2)

df_summary_long <- melt(df_summary, id.vars='Group.1')
ggplot(data=df_summary_long, aes(x=Group.1, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())

df$likesgames<-

df %>% filter(likesgames==1 | likesgames==0) %>% 
  filter(!is.na(likesgames)) %>%
  group_by(likesgames, sex) %>% summarise(Count=n()) %>%
  mutate(pct=prop.table(Count)*100) %>%
  ggplot(aes(x=reorder(sex, -pct), y=pct, fill=likesgames)) +
    geom_bar(stat='identity') + theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values=c('#D358F7', '#2E64FE')) +
        facet_wrap(~likesgames) +
          theme(strip.background = element_blank(), strip.text.x=element_blank()) +
        labs(x='Sex', y = 'Percent', title='Likes Games by Sex') +
          geom_text(aes(x=sex, y=0.01, label= sprintf("%.2f%%", pct)),
            hjust=0.5, vjust=-3, size=4, 
            colour="black", fontface="bold")

df$sex<-factor(df$sex)
ggplot(df) + geom_bar(aes(x=interaction(likesgames, sex)))
  


ggplot(bind_v1_c",i,"_conts$'Apnea/Hypopnea Index (0-141.667)') + 
  geom_bar(aes(x=interaction(solutions, LatentClass), y=est, color=LatentClass, group=LatentClass), stat='identity', alpha=0.5, size=2) +
  geom_text(aes(x=interaction(solutions, LatentClass), y=est, label=est), position=position_stack(vjust=0.5), size=5) +
  labs(y='Mean', x = 'Class')
test <- (df %>% group_by(likesgames, sex) %>% summarise(Count = n()) %>% mutate(pct = prop.table(Count)*100)) %>% select(likesgames, sex, pct)
test <- test[complete.cases(test),]

test$sex <- as.character(test$sex)


View(reshape(test, idvar = "likesgames", timevar = "sex" , direction = "wide"))



