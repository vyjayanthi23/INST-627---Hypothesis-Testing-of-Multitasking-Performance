#### INST 627 Project Team 10 ####
df<-read.csv("fulldata.csv")
View(df)
require(dplyr)
require(mvnormtest)
require(pwr)
require(sm)

#separating variables into different datasets
gender <- as.factor(df[,4])
time <- as.matrix(df[,12])
quality <- as.matrix(df[,13])
percent <- as.matrix(df[,16])
head(gender)

#create R objects for the residuals from each treatment level
TIMEresGEN=lm(df$Avg.time~df$Gender)$residuals
QUALresGEN=lm(df$Avg.quality~df$Gender)$residuals
PERCENTresGEN=lm(df$Avg.percent~df$Gender)$residuals

#checking for normality using qqplots
par(mfrow=c(1,3))
qqnorm(lm(df$Avg.time~df$Gender)$residuals, main="QQ Plot of Average Time Taken", col=4)
qqline(lm(df$Avg.time~df$Gender)$residuals, lwd=2,col='gray86')

qqnorm(lm(df$Avg.quality~df$Gender)$residuals, main="QQ Plot of Average Folding & Dribbling Quality",col=4)
qqline(lm(df$Avg.quality~df$Gender)$residuals, lwd=2,col='gray86')

qqnorm(lm(df$Avg.percent~df$Gender)$residuals, main="QQ Plot of Average Word Completion & Quiz Score",col=4)
qqline(lm(df$Avg.percent~df$Gender)$residuals, lwd=2,col='gray86')

#checking for normality graphically by using boxplots
par(mfrow=c(1,3))
boxplot(lm(df$Avg.time~df$Gender)$residuals~df$Gender, main="Boxplot of Average Time Taken")
boxplot(lm(df$Avg.quality~df$Gender)$residuals~df$Gender, main="Boxplot of Folding & Dribbling Quality")
boxplot(lm(df$Avg.percent~df$Gender)$residuals~df$Gender, main="Boxplot of Scores & Word Completion")

#manova test
x <- manova(cbind(df$Avg.time, df$Avg.quality,df$Avg.percent) ~ df$Gender, data = df)
#running wilks test
summary(x,test="Wilks")

#difference between the response vars
summary.aov(x)

#subsetting based on gender
male<-subset(df, Gender=="M")
female<-subset(df, Gender=="F")

#calculating power k is no. of groups, n is sample size, f is f statistic, sig level
pwr.anova.test(k=2, n=40, f=0.57, sig.level=0.05)


#trial error
# summary(df$Avg.time)
# sd(df$Avg.time)
# 
# summary(df$Avg.quality)
# sd(df$Avg.quality)
# 
# summary(df$Avg.percent)
# sd(df$Avg.percent)