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

# PREDICTION #

# log(38.6) = 3.653252
(-5.99714 + 3.653252)/-4.60315 # outputs x which is the predicted density
5.99714 + log(426.7) - 4.60315

(-5.99727 + log(38.6))/-4.60594 # outputs x which is the predicted density
1.298013 - 0.216203 * log(38.6)
0.5081689 
0.5089119
#######################
### Crossvalidation ###
#######################

# for prediction without density 0,508
df2 <- df%>%filter(density != 0.508)

ggplot(df2, aes(x=density, y=loggain)) + geom_point() + geom_smooth(method = "glm", 
                                                                    method.args = list(family = "gaussian"), 
                                                                    se = TRUE)

glm_log2<-glm(density~loggain, data=df2, family=gaussian())
summary(glm_log2)

# density prediction
y.hat2 <- 1.298422 - (0.216278 * log(38.6))

# residuals
df2$glm_log_residual<-resid(glm_log2)
glm_resid_sd2 <- sd(df2$glm_log_residual)
glm_resid_mean2 <- mean(df2$glm_log_residual)

# interval estimate assuming error is normally distributed
ci2 <- c(y.hat2 - (1.96 * glm_resid_sd2), y.hat2 + (1.96 * glm_resid_sd2))
ci2

# for prediction without density 0,001
df3 <- df%>%filter(density != 0.001)

ggplot(df3, aes(x=density, y=loggain)) + geom_point() + geom_smooth(method = "glm", 
                                                                    method.args = list(family = "gaussian"), 
                                                                    se = TRUE)
glm_log3<-glm(density~loggain, data=df3, family = gaussian())
summary(glm_log3)

# density prediction
y.hat3 <- 1.310114 - (0.219395 * log(38.6))

# residuals
df3$glm_log_residual <- resid(glm_log3)
glm_resid_sd3 <- sd(df3$glm_log_residual)
glm_resid_mean3 <- mean(df3$glm_log_residual)

# interval estimate assuming error is normally distributed
ci3 <- c(y.hat3 - (1.96 * glm_resid_sd3), y.hat3 + (1.96 * glm_resid_sd3))
ci3


ggplot(df2, aes(x=density, y=glm_residual)) + geom_point()
ggplot(df2, aes(x=density, y=glm_log_residual)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin= -loggain_residual2[2,1],ymax=loggain_mean+loggain_residual2[2,1]),alpha=0.2,fill="red")






