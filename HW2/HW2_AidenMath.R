# Load working directory
# Enter your own path in the quotes in the path variable
# path<-"/Users/Timlee/"
# setwd("C:/Users/Aiden/Desktop/UCSD/homework/math_189/case1/M189WI2019/")
# setwd('C:/Users/buwen/OneDrive/Desktop/MATH 189/HW2')
# path<-"C:/Users/buwen/"
setwd(paste0(path,'Documents/Git/M189WI2019/HW2'))

#Load Data
videodf1<-read.table("videodata.txt", header = T, sep = "", dec = ".")
videodf2<-read.table("videodata2.txt", header = T, sep="", dec=".")

videodf<-cbind(videodf1, videodf2)


######################
####Data managment####
######################

# Assign NA to all 99 values in R
df[df == 99] <- NA


#####################
##### BOOTSTRAP #####
#####################

