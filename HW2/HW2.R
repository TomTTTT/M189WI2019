setwd("C:\\Users\\Mingcan\\Documents\\academics\\2019WI\\MATH 189\\HW 2")
videodata = read.csv("videodata.txt", header=TRUE, sep="")
surveydata = read.csv("followupSurvey.txt", header=TRUE, sep="")

numRespondants <- 91
numNonrespondants <- 4

gradfreq <- table(videodata$grade)

# chi-squared test assuming no F
chisq.test(x=c(0,8,52,31), correct=FALSE, p=c(.1,.4,.3,.2))

# chi-squared test assuming nonrespondants are F
chisq.test(x=c(4,8,52,31), correct=FALSE, p=c(.1,.4,.3,.2))

# bootstrap for A,B,C,D/F separately assuming no D or F
fullsample <- rep(videodata$grade, length.out=314)
numtrials <- 100
gradeAdist <- 1:numtrials
gradeBdist <- 1:numtrials
gradeCdist <- 1:numtrials
gradeDFdist <- 1:numtrials
for (i in 1:numtrials){
  samplegrade <- sample(fullsample, 91, replace=FALSE)
  samplegradefreq <- table(samplegrade)/91
  gradeAdist[i] <- samplegradefreq["4"]
  gradeBdist[i] <- samplegradefreq["3"]
  gradeCdist[i] <- samplegradefreq["2"]
  gradeDFdist[i] <- 0
}
gradeAdist[is.na(gradeAdist)] <- 0
gradeBdist[is.na(gradeBdist)] <- 0
gradeCdist[is.na(gradeCdist)] <- 0
gradeDFdist[is.na(gradeDFdist)] <- 0

# grade A distribution
quantile(gradeAdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)
# grade B distribution
quantile(gradeBdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)
# grade C distribution
quantile(gradeCdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)
# grade D/F distribution
quantile(gradeDFdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)

# bootstrap for A,B,C,D/F separately assuming 4 Fs (nonrespondents)
amendedgrade <- append(videodata$grade, c(0,0,0,0), after=length(videodata$grade))
fullsample <- rep(amendedgrade, length.out=314)
numtrials <- 100
gradeAdist <- 1:numtrials
gradeBdist <- 1:numtrials
gradeCdist <- 1:numtrials
gradeDFdist <- 1:numtrials
for (i in 1:numtrials){
  samplegrade <- sample(fullsample, 95, replace=FALSE)
  samplegradefreq <- table(samplegrade)/95
  gradeAdist[i] <- samplegradefreq["4"]
  gradeBdist[i] <- samplegradefreq["3"]
  gradeCdist[i] <- samplegradefreq["2"]
  gradeDFdist[i] <- samplegradefreq["0"]
}
gradeAdist[is.na(gradeAdist)] <- 0
gradeBdist[is.na(gradeBdist)] <- 0
gradeCdist[is.na(gradeCdist)] <- 0
gradeDFdist[is.na(gradeDFdist)] <- 0

# grade A distribution
quantile(gradeAdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)
# grade B distribution
quantile(gradeBdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)
# grade C distribution
quantile(gradeCdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)
# grade D/F distribution
quantile(gradeDFdist, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1), names=TRUE)