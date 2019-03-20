########Final Project########

# This file export the first and last 8th of the acoustic_data column to csv's

# Load working directory
setwd("C:/Users/ADRC/Downloads")

#Load dependencies
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(data.table)

#Load Data

#Load original data File
df<-fread("train.csv")
df2<-df[1:(nrow(df)/8),1]
df3<-df[(nrow(df)-nrow(df2)):nrow(df),1]
rm("df")
write.csv(df2, file="train_X_1.csv")
rm("df2")
write.csv(df3, file="train_X_2.csv")
rm("df3")
#ks.test(df2$acoustic_data, y)
