# Load working directory
# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Documents/GitHub/M189WI2019/HW4")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW4'))

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
library(dplyr)

#Load Data
df<-read.csv("data1.txt", sep="")


###########################
######PART 1###############
###########################

#Take log of gain 
df$loggain<-log(df$gain)

#First do a scatter plot
#Linear
ggplot(df, aes(x=density, y=gain)) + geom_point() + geom_smooth(method = "glm", 
                                                                method.args = list(family = "gaussian"), se = TRUE)

#Log the gain                                                               
ggplot(df, aes(x=density, y=loggain)) + geom_point() + geom_smooth(method = "glm", 
                                                                method.args = list(family = "gaussian"), 
                                                                se = TRUE)



#REGRESSION
#Linear
glm<-glm(gain~density, data=df, family=gaussian())
#Bind residuals into data frame to plot
df$glm_residual<-resid(glm)
summary(glm)

#Log
glm_log<-glm(loggain~density, data=df, family=gaussian())
#Bind residuals into data frame to plot
df$glm_log_residual<-resid(glm_log)

summary(glm_log)
loggain_residual <- as.data.frame(summary(glm_log)$coefficients[,2])
loggain_mean <- mean(df$glm_log_residual)

#RESIDUAL PLOT
#Linear
ggplot(df, aes(x=density, y=glm_residual)) + geom_point()

#Log
ggplot(df, aes(x=density, y=glm_log_residual)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin= -loggain_residual[2,1],ymax=loggain_mean+loggain_residual[2,1]),alpha=0.2,fill="red")
#This residual plot tells you that the data is not normal

