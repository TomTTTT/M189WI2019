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
linear<-ggplot(df, aes(x=gain, y=density)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

#Log the gain                                                               
loggain_p1<-ggplot(df, aes(x=loggain, y=density)) + geom_point() + geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Scatter of Logged Gain Values and Densities") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Log(Gain)") + ylab("Density") +
  annotate("text", x=5, y=0.6, label= "OLS Line\n Density = -.216 + 1.298*Log(Gain)")
  



#REGRESSION
#No transformation
lm<-lm(density~gain, data=df)

#Bind residuals into data frame to plot
df$lm_residual<-resid(lm)
summary(lm)

#Log
lm_log<-lm(density~loggain, data=df)
#Bind residuals into data frame to plot
df$lm_log_residual<-resid(lm_log)
#Bind fitted values into df
df$lm_log_fitted<-predict(lm_log)

summary(lm_log)
loggain_residual <- as.data.frame(summary(lm_log)$coefficients[,2])
loggain_mean <- mean(df$lm_log_residual)

sd(df$lm_log_residual)
mean(df$gain)

ypredicted = function(x){
  y= 1.298422- .216278*(log(x))
  return(y)
}

interval = function(z,x){
  
  int.size= z*sd(df$lm_log_residual)*sqrt(1/length(df$density)+((log(x)-mean(df$loggain))^2)/
                                            ((length(df$density)-1)*var(df$loggain)))
  return(int.size)
  
}

#RESIDUAL PLOT
#Log
ggplot(df, aes(x=density, y=lm_log_residual)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin= -loggain_residual[2,1],ymax=loggain_mean+loggain_residual[2,1]),alpha=0.2,fill="red") +
  ggtitle("OLS Residual Plot") + ylab("Residuals") + xlab("Density") + theme(plot.title = element_text(hjust = 0.5))
  
#This residual plot tells you that the data is not normal

#QQ plot of the residuals 
qqnorm(df$lm_log_residual, main = "Q-Q Plot of Residuals")
qqline(df$lm_log_residual, distribution=qnorm)


######What if there is measurement error?
#Log regression with added white noise
df$whitenoise1<- rnorm(90, 0, 0.1)
df$whitenoise2<- rnorm(90,0, 0.5)
df$whitenoise3<- rnorm(90, 0, 1)

#Add white noise to each of the log gains and append to data frame 
df$loggain2<- df$loggain + df$whitenoise1
df$loggain3<- df$loggain + df$whitenoise2
df$loggain4<- df$loggain + df$whitenoise3

#Loop that extracts coefficients out of each new regression
coefficients <- c(as.numeric(lm_log$coefficients[2]))
for(i in 2:4){
  eval(parse(text=paste0("
  lm_log_",i,"<- lm(density~loggain",i,",data=df)
  coefficients<-append(coefficients,as.numeric(lm_log_",i,"$coefficients[2]))
")))
}

#Recall the original loggain model plot
loggain_p1<-ggplot(df, aes(x=loggain, y=density)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + 
  xlim(1.5,9.5) + ylim(0,0.8) + ggtitle("Scatter of Logged Gain Values and Densities\n No White Noise Added") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Log(Gain)") + ylab("Density") 

#Loop that creates plots for each of the new regressions
for(i in 2:4){
  eval(parse(text=paste0("
  loggain_p",i,"<-ggplot(df, aes(x=loggain",i,", y=density)) + geom_point() + geom_smooth(method = 'lm', se = TRUE) + xlim(1.5,9.5) +
  ylim(0,0.8) + theme(plot.title = element_text(hjust = 0.5)) + xlab('Log(Gain)') + ylab('Density') 

                         ")))
}

#Add special titles to graphs
loggain_p2 <- loggain_p2 + ggtitle("Scatter of Logged Gain Values and Densities\n White Noise ~ N(0,0.1)")
loggain_p3 <- loggain_p3+ ggtitle("Scatter of Logged Gain Values and Densities\n White Noise ~ N(0,0.5)")
loggain_p4 <- loggain_p4 + ggtitle("Scatter of Logged Gain Values and Densities\n White Noise ~ N(0,1)")

#Compile plots
grid.arrange(loggain_p1,loggain_p2,loggain_p3,loggain_p4)

#####Look at residuals for each of the 9 densities
#Make residual plots for each density
densitylist<-c(0.686, 0.604, 0.508, 0.412, 0.318, 0.223, 0.148, 0.080,0.001)
for(i in densitylist){
eval(parse(text=paste0("
  
  residual_",i,"<-ggplot((df %>% filter(density==",i,")), aes(x=density, y=lm_log_residual)) + geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  geom_ribbon(aes(ymin= -loggain_residual[2,1],ymax=loggain_mean+loggain_residual[2,1]),alpha=0.2,fill='red') + ylim(-.02, .05) +
  xlab('Density') +  ylab('Residuals') + ggtitle('Density = ",i," g/cm^3') + theme(plot.title = element_text(hjust = 0.5))
")))
}



grid.arrange(residual_0.001, residual_0.08, residual_0.148,
             residual_0.223, residual_0.318, residual_0.412, 
             residual_0.508, residual_0.604, residual_0.686, ncol=3, nrow=3)




################################################
# CROSS VALIDATION UPDATED #
############################
# for prediction without density 0,508
df.cv1 <- df%>%filter(density != 0.508)

ggplot(df.cv1, aes(x=density, y=loggain)) + geom_point() + geom_smooth(method = "glm", 
                                                                       method.args = list(family = "gaussian"), 
                                                                       se = TRUE)

glm_log2<-glm(density~loggain, data=df.cv1, family=gaussian())
summary(glm_log2)

# density prediction
y.hat2 <- 1.298422 - (0.216278 * log(38.6))

# residuals
df.cv1$glm_log_residual<-resid(glm_log2)
glm_resid_sd2 <- sd(df.cv1$glm_log_residual)
glm_resid_mean2 <- mean(df.cv1$glm_log_residual)

# interval estimate assuming error is normally distributed
ci2 <- c(y.hat2 - (1.96 * glm_resid_sd2), y.hat2 + (1.96 * glm_resid_sd2))
ci2

# for prediction without density 0,001
df.cv2 <- df%>%filter(density != 0.001)

ggplot(df.cv2, aes(x=density, y=loggain)) + geom_point() + geom_smooth(method = "glm", 
                                                                       method.args = list(family = "gaussian"), 
                                                                       se = TRUE)
glm_log3<-glm(density~loggain, data=df.cv2, family = gaussian())
summary(glm_log3)

# density prediction
y.hat3 <- 1.310114 - (0.219395 * log(38.6))

# residuals
df.cv2$glm_log_residual <- resid(glm_log3)
glm_resid_sd3 <- sd(df.cv2$glm_log_residual)
glm_resid_mean3 <- mean(df.cv2$glm_log_residual)

# interval estimate assuming error is normally distributed
ci3 <- c(y.hat3 - (1.96 * glm_resid_sd3), y.hat3 + (1.96 * glm_resid_sd3))
ci3



cv_p1<-ggplot(df.cv1, aes(x=density, y=lm_log_residual)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  +
  ggtitle("OLS Residual Plot\n with density = 0.508 removed") + ylab("Residuals") + xlab("Density") + theme(plot.title = element_text(hjust = 0.5))

cv_p2<-ggplot(df.cv2, aes(x=density, y=lm_log_residual)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("OLS Residual Plot\n with density = 0.001 removed") + ylab("Residuals") + xlab("Density") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(cv_p1, cv_p2)

#####Polynomial cross validation

#Plot of how the R^2 values changes. 
d.max = 30
R.square = rep(NA, d.max)
for (d in 1:d.max){
  fit = lm(density ~ poly(gain, d, raw=TRUE), data=df)
  R.square[d] = summary(fit)$r.squared
}
plot(1:d.max, R.square, xlab = "Degree", ylab = "R-squared", lwd = 2, col = "blue", pch = 5, main="Plot of R^2 as Degree Increases")
lines(1:d.max, R.square, type='l', lwd = 2, col = "blue")

#Plot of regression lines 
plot(df$gain, df$density, pch = 16)
pts <- seq(0, 600, length.out=100)
for (d in 1:10){
  fit <- lm(density ~ poly(gain, d, raw=TRUE), data=df)
  val <- predict(fit, data.frame(gain= pts))
  lines(pts, val, col=rainbow(10)[d], lwd = 2)
}


#It seems like two might be the best, but let's compare MSE
library(caret)
cv.degree.d <- function(k, n, d){
  flds<-createFolds(df$density,3)
  cv.mse <- rep(0, k)
  for (round in 1:3){
    test.idx<-flds[[round]]
    y <- df$density[-test.idx]
    x <- df$gain[-test.idx]
    fit <- lm(y ~ poly(x, d, raw=TRUE), data=df)
    y.hat <- predict(fit, data.frame(x = df$gain[test.idx]))
    cv.mse[round] <- sum((df$density[test.idx] - y.hat)^2)/length(test.idx)
  }
  return (mean(cv.mse))
}

k <- 3
n <- 90
d.max <- 9
mse <- rep(0, d.max)
for (d in 1:d.max){
  mse[d] <- cv.degree.d(k, n, d)
}
plot(1:d.max, mse, xlab = "Degree", ylab = "MSE", lwd = 2, col = "blue", pch = 5, main="MSE as Degree in\n Polynomial Regression Changes")
lines(1:d.max, mse, type='l', lwd = 2, col = "blue")

#6th and 8th degree is the best


##################################################################




#####
#Additional question
#####
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
