#Load dependencies
library(dplyr)
setwd("C:/Users/buwen/OneDrive/Desktop/ECON 178")
df<-read.csv("USMacro_Quarterly.csv")

#PCECPTP1 is the price index for personal consumption 
#For analysis use the sample period 1963:Q1-2017:Q4
df1<-df[which(df$X =="1963:01:00"):which(df$X =="2013:04:00"),]
df2<-df1$PCECTPI
#Plot the value of PCECT PI form 1963:Q1 through 2017:Q4. 
Index <- ts(df2, start=1963, frequency=4)
plot(Index)

#Compute the inflation 
# In f l = 400 · [ln(PCECT PIt)???ln(PCECT PIt???1)]
Index.L1<-stats::lag(Index, -1) 
cbind(Index, Index.L1)

Index.D1 <- diff(Index, differences = 1)  # creat first difference
plot(Index.D1, main="First difference of Index")
cbind(Index, Index.L1, Index.D1)