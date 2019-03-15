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

#deviations of gains from the mean for each density (perfectly normal, dependent)
gain_deviation <- c()
for (density in density_bin){
  gain_density <- df$gain[df$density == density];
  gain_density <- gain_density - mean(gain_density)
  gain_deviation <- c(gain_deviation, gain_density)
}
hist(gain_deviation)
qqnorm(gain_deviation)
qqline(gain_deviation, distribution = qnorm)

#chi-square test for normality
bin_cutoff <- qnorm(c(0,0.2,0.4,0.6,0.8,1), mean=mean(gain_deviation), sd=sd(gain_deviation))
bin_gain_dev <- table(cut(gain_deviation, breaks=bin_cutoff))
chisq.test(bin_gain_dev)
#shapiro-wilk test for normality
shapiro.test(gain_deviation)
#k-s test for normality
ks.test(gain_deviation, rnorm(90,mean=mean(gain_deviation),sd=sd(gain_deviation)))

#scatter plot
plot(gain_deviation)
#k-s test for distribution across densities
ks.test(gain_deviation[1:30],gain_deviation[61:90])

#deviation max and min regression
gain_deviation_abs <- abs(gain_deviation)
gain_deviation_abs_max <- 1:9
for (i in 1:9){
  gain_deviation_abs_max[i] <- max(gain_deviation_abs)
}

#fit the log mean gain (residual seems uniform on [-0.1, 0.1])
mean_gain <- 1:9
for (i in 1:9){
  mean_gain[i] <- mean(df$gain[df$density == density_bin[i]])
}
plot(mean_gain)
log_mean_gain <- log(mean_gain)
plot(log_mean_gain)
glm_meangain<-glm(log_mean_gain~density_bin, family=gaussian())
mean_gain_residual<-resid(glm_meangain)
hist(mean_gain_residual, breaks=10)
#test uniform with chi square test
chisq.test(table(cut(mean_gain_residual, breaks=seq(-0.1, 0.1, length.out = 9)))) #exp < 5 not reliable
