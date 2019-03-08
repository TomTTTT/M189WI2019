setwd("C:\\Users\\Mingcan\\Documents\\academics\\2019WI\\MATH 189\\HW 4")

df <- read.csv("data1.txt",sep="")

library(ggplot2)

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

#qqplot
plot(df$loggain, df$density)
qqnorm(df$glm_residual)
qqline(df$glm_log_residual,distribution = qnorm)

#density against loggain
plot(df$loggain, df$density)
loggain_686 <- df$loggain[df$density==0.686]

#deviations of loggains from the mean for each density (normal-like but not quite)
table(df$density)
density_bin <- c(0.001, 0.08, 0.148, 0.223, 0.318, 0.412, 0.508, 0.604, 0.686)
loggain_deviation <- c()
for (density in density_bin){
  loggain_density <- df$loggain[df$density == density];
  loggain_density <- loggain_density - mean(loggain_density)
  loggain_deviation <- c(loggain_deviation, loggain_density)
}
hist(loggain_deviation)
qqnorm(loggain_deviation)
qqline(loggain_deviation, distribution = qnorm)

#deviations of gains from the mean for each density (perfectly normal)
gain_deviation <- c()
for (density in density_bin){
  gain_density <- df$gain[df$density == density];
  gain_density <- gain_density - mean(gain_density)
  gain_deviation <- c(gain_deviation, gain_density)
}
hist(gain_deviation)
qqnorm(gain_deviation)
qqline(gain_deviation, distribution = qnorm)

#fit the mean gain (residual seems uniform on [-0.1, 0.1])
mean_gain <- 1:9
for (i in 1:9){
  mean_gain[i] <- mean(df$gain[df$density == density_bin[i]])
}
plot(mean_gain)
log_mean_gain <- log(mean_gain)
plot(log_mean_gain)
glm_meangain<-glm(log_mean_gain~density_bin, family=gaussian())
mean_gain_residual<-resid(glm_meangain)
hist(mean_gain_residual, breaks=20)
