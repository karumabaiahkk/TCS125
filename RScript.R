setwd("/Users/keer/Desktop/PGDM/other courses/TCS")
getwd()


#calling the package

library(car)
library(lmtest)
library(psych)
library(readxl)
library(VGAM)
library(bestNormalize)
library(pracma)
library(MLmetrics)
library(sandwich)
library(rr2)
library(nlme)
library(robustbase)
library(MASS)
library(car)
library(lmtest)
library(psych)
library(readxl)
library(MASS)
library(broom)
library(VGAM)
library(bestNormalize)
library(pracma)
library(MLmetrics)
library(sandwich)
library(tidyverse)
library(lmtest)
library(nlme)


#DataIMPORT
trans=read_excel(file.choose(), sheet="Data")

attach(trans)

names(trans)



#building model

Sales1=lm(`Monthly Sales`~Name+Cost+Year, data=trans)

View(Sales1)

summary(Sales1)


######### testing assumption:####

#1.Average residual is zero.
mean(Sales1$residuals)
#here average is -9.964965e-15 which is negligle.

#2. Constant Variance
#h0. Constant variance assumption is satisfied
#h1. Constant variance assumption is not satisfied

#breusch pagan test is administered to test for heteroscedasticity.
bptest(Sales1)
#Here the p value is less than 5% i.e, p-value < 2.2e-16 thus the H0 is rejected and 
#Constant variance assumption is not satisfied thus H1 is accepted,

#3. Uncorrelated error.
#h0. alternate errors are uncorrelated 
#h1. alternate errors are Correlated error.

durbinWatsonTest(Sales1)
#here p value is 0.124 thus it satisfied h0 thus errors uncorrelated.

#4. Residual are normally distributed
#h0. Residual are normally distributed
#h1. Residual are not normally distributed

shapiro.test(Sales1$residuals)

(Sales1$residuals)


#p-value < 2.2e-16 thus it fails to satisfy the assumption. The residuals are not
#normally distributed.

#5.ALL THE REGRESSOR ARE STATISTICALLY INDEPENDENT
vif(Sales1)

#here VIF is less than 5 thus there is no problem of multi- coliniarity.

BCHR=boxcox(Sales1)
BCHR
lamb_HR=BCHR$x[which.max(BCHR$y)]
lamb_HR
Sales1$HRLAMBV=(`Monthly Sales`^lamb_HR-1)/lamb_HR
hist(Sales1$HRLAMBV)

hist(Sales1$residuals)

Sales2=lm(Sales1$HRLAMBV~Name+Cost+Year, data=trans) 
summary(Sales2)

Sales3=lm(Sales1$HRLAMBV~Cost+Year, data=trans)
summary(Sales3)

#This is the most feasiable model with Adjusted R-squared:  0.534 and
#P vlaue of  < 2.2e-16 where all significant.

######### testing assumption for Sales 3:####

#1.Average residual is zero.
mean(Sales3$residuals)
#here average is 1.152369e-15 which is negligle.

#2. Constant Variance
#h0. Constant variance assumption is satisfied
#h1. Constant variance assumption is not satisfied

#breusch pagan test is administered to test for heteroscedasticity.
bptest(Sales3)
#Here the p value is less than 5% i.e, p-value < 2.2e-16 thus the H0 is rejected and 
#Constant variance assumption is not satisfied thus H1 is accepted,

#3. Uncorrelated error.
#h0. alternate errors are uncorrelated 
#h1. alternate errors are Correlated error.

durbinWatsonTest(Sales3)
#here p value is 0.103 thus it satisfied h0 thus errors uncorrelated.

#4. Residual are normally distributed
#h0. Residual are normally distributed
#h1. Residual are not normally distributed

shapiro.test(Sales3$residuals)

(Sales3$residuals)


#p-value < 1.3e-10 thus it fails to satisfy the assumption. The residuals are not
#normally distributed.

#5.ALL THE REGRESSOR ARE STATISTICALLY INDEPENDENT
vif(Sales1)

#here VIF is less than 5 thus there is no problem of multi- coliniarity.