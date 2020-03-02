#load libraries

library(Hmisc)
library(tidyverse)
library(tidyquant)
library(lubridate)

describe(mtcars)

#Step 1: Set up H1 and H0
#Hypothesis test: Example 1 Upper/right tailed test
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

# Example 2: Hypo Test - Population Mean - Lower/ left Tailed Test

# An automatic machine fills an aerated drink in 2000 cc bottles.
# A tester needs to test Ho that the average amount being filled in a bottle is at least 2000 cc
# He selects a random sample of 40 bottles and records the exact content of the bottles and finds the sample mean to be 1999.6 cc
# Considers the population standard deviation as 1.30 cc
# Let's test the null hypothesis at the significance level of 5%.

# Solution

# Step 1:
# Set up null hypothesis and alternative hypothesis
#H0: mu >=2000
#H1: mu < 2000

#Step 2:
#Compute test statistic
sigma_2 <- 1.3
mu_2 <- 1999.6
n_2 <- 40
xbar <- 2000
SE_2 <- sigma_2/sqrt(n_2)
z_2 <- (xbar-mu_2)/SE_2 #-1.946

#Step 3: Compute critical value for significance level =5% or confidence interval=95%
zalpha_2 <- qnorm(1-alpha,lower.tail = F) #-1.644, if on the left-hand side of the graph you must say lower.tail = F

#step 4: Compare test stat to critical value and draw conclusions
#########################################################################
# If |z| > zalpha, z is not significant and we can accept the null hypothesis
# If |z| <= zalpha, z is significant and we reject the null hypothesis 
# 
#|z| <= zalpha: reject null hypothesis at 5% significance level

#Two-tailed test
##############################################################################
#Suppose the mean weight of king penguins last year was 15.4kg.
#In a sample of 35 penguins same time this year in the same colony, the mean penguiun weight is 14.6kg
#Assume the population standard deviation is 2.5kg
#At a 5% signficance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?
####################################################################################

# Solution

# Step 1:
# Set up null hypothesis and alternative hypothesis
#H0: mu = 15.4
#H1: mu != 15.4

#Step 2:
#Compute test statistic

mu_3 <- 15.4
xbar <- 14.6
sigma_3 <- 2.5
n_3 <- 35

SE_3 <- sigma_3/sqrt(n_3)
z_3 <- (xbar-mu_3)/SE_3 #test statistic -1.893 and 1.893

#Step 3: Compute critical value for significance level =5% or confidence interval=95%
zalpha_3 <- qnorm(1-alpha/2) #critcal value is -1.96 and 1.96
c(-zalpha_3,zalpha_3) #this is a two-tailed test

#You can also calculate the p-value
pval <- 2*pnorm(z_3) #pval = 0.058333, this is > than 5%, therefore we have insignificant evidence to reject the null hypothesis
#we need to multiply the pvalue with2 because it is a two-tailed test

#step 4: Compare test stat to critical value and draw conclusions
#z_3 > zalpha_3 or pval > 5%, therefore we have insignificant evidence at the 5% level to reject the null hypothesis

