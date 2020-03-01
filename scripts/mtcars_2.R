#load libraries

library(Hmisc)
library(tidyverse)
library(tidyquant)
library(lubridate)

describe(mtcars)

#Step 1: Set up H1 and H0
#Hypothesis test: Example 1
#Kurkure claims that maximum saturated fat content in chip packet is 2 grams with std dev = 0.25
# A test on a sample of 35 packets reveals that the mean saturated fat is 2.1 grams. 
#Should this claim be rejected?
#Lets test the nul hypothesis at the 5% significance level.
#H0: mu<= 2
#H1: mu > 2, therefore this is an upper/right-tailed test
#level of significance:
alpha <- 0.05

#step 2: Compute test statistic

#Sample size is more than 30,so need to calculate z-statistic (std normal distribution)
#use t-distribution when sample size is less than 30
mu <- 2 #Population mean
xbar <- 2.1 #Sample mean
sigma <- 0.25 #Populaton std deviation
n <- 35 #sample size
SE <- sigma/sqrt(n)
z <- (xbar-mu)/SE
z  #z=2.366432 (result of z-statistic)

#Step 3: Compute critical value for significance level =5% or confidence interval=95%
zalpha = qnorm(1-alpha)
zalpha  #zalpha = 1.64485...

#step 4: Compare test stat to critical value and draw conclusions
#########################################################################
# If |z| < zalpha, z is not significant and we can accept the null hypothesis
# If |z| >= zalpha, z is significant and we reject the null hypothesis 
# 
#|z| >= zalpha: reject null hypothesis at 5% significance level