#Question 2

#install.packages(ggpubr)
getwd()
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MCMCpack)
data <- read.csv('analysis.csv')
which(is.na(data))
install.packages("tidyverse")
#launch tidyverse
library(tidyverse)
install.packages("standardize")
install.packages("caret")
library(standardize)
library(caret)
#remove na values of independent variable

#Q2. A
#Feature Selection

chess <- subset(data[,4:15])

scaled_chess <- chess%>%mutate_if(is.numeric,scale)

scaled_chess$PreDeepBlue [scaled_chess$PreDeepBlue == "TRUE"] <- 1
scaled_chess$PreDeepBlue [scaled_chess$PreDeepBlue == "FALSE"] <- 0
scaled_chess


summary(scaled_chess)

scaled_chess <- scaled_chess[, -3] 
scaled_chess <- scaled_chess[, -5]

summary(scaled_chess)

correlationMatrix <- cor(scaled_chess)

correlationMatrix

apply(scaled_chess, 2, sd)

#round(cor(chess),2)


apply(scaled_chess, 2, mean)

dim(scaled_chess)
summary(scaled_chess)


apply(df_chess, 2, mean)


apply(df_chess, 2, sd)

apply(df_chess, 2, range)

#Histogram of Combined ACPL


hist(df_chess$Combined.ACPL)


pairs(subset(df_chess, select = c(Year, White.ACPL,Black.ACPL,Combined.ACPL,PreDeepBlue)))

cor(subset(df_chess, select = -c(Year, Combined.ACPL)))


plot(Combined.ACPL~., data = df_chess)


boxplot(Combined.ACPL ~ Year, data = df_chess) 
boxplot(Year ~ PreDeepBlue, data = df_chess) 



#Linear Regression

df_chess <- subset(data, select=c("Year","Game.Number","Combined.ACPL","PreDeepBlue"))


df_chess

df_chess$PreDeepBlue [df_chess$PreDeepBlue == "TRUE"] <- 1
df_chess$PreDeepBlue [df_chess$PreDeepBlue == "FALSE"] <- 0

lm1 <-lm(Combined.ACPL~., df_chess)


step_AIC_forward <- step(lm(Combined.ACPL~., data = df_chess), direction = "forward", scope = list(upper = lm1))
step_AIC_forward <- step(lm(Combined.ACPL~1, data = df_chess), direction = "forward", scope = list(upper = lm1))
step_AIC_forward
step_AIC2 <- step(lm(Combined.ACPL ~ Year^2+PreDeepBlue, data = df_chess)) ## interaction terms
step_AIC2
step_AIC_forward

summary(lm1)
plot(lm1, which = 2)+abline(lm1,col='blue')

yhat <- predict(lm1)
yhat

#choosing only two features from AIC values

df_chess <- subset(data, select=c("Year","PreDeepBlue"))


plot(yhat, df_chess$Combined.ACPL)

#QUESTION 2B 

df_chess <- data


df_chess$PreDeepBlue [df_chess$PreDeepBlue == "TRUE"] <- 1
df_chess$PreDeepBlue [df_chess$PreDeepBlue == "FALSE"] <- 0

df_chess

lm2 <-lm(Combined.ACPL~PreDeepBlue, df_chess)
summary(lm2)

cor(df_chess$Combined.ACPL,df_chess$PreDeepBlue)
plot(df_chess$Combined.ACPL,df_chess$PreDeepBlue)




