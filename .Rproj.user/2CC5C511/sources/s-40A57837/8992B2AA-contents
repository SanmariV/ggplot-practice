#load libraries

library(Hmisc)
library(tidyverse)
library(tidyquant)
library(lubridate)

describe(mtcars)

# if the population mean and population standard deviation is known use z-statistic (standard normal distribution)
# if the population mean is known but the population standard deviation is not known and sample size < 30
# use t-statistic

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
SE <- sigma/sqrt(n)  # this is also known as the sample standard deviation
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
pval <- 2*pnorm(z_3) #pval = 0.058333, this is > than 5%, therefore we have insufficient evidence to reject the null hypothesis
#we need to multiply the pvalue with2 because it is a two-tailed test

#step 4: Compare test stat to critical value and draw conclusions
#z_3 > zalpha_3 or pval > 5%, therefore we have insufficient evidence at the 5% significance level to reject the null hypothesis


###############################################################################
#     t-statistics
# If the population standard deviation is not known, or the popultion mean < 30, use t-statistics for hypothesis test
# To calculate the critical value, you need to know the degrees of freedom (df) to use to look up with in the table
# Same rules apply regarding left-,right and two-tailed test
###############################################################################

# t-statistic example

# The high school athletic director is asked if football players are doing as well
# academically as the other student athletes.
# From a previous study the average GPA for student athletes was 3.10
# After an initiative to help improve the GPA of student athletes,
# the athletic director randomly samples 20 football players and finds that the
# average GPA of the sample is 3.18 with a sample standard deviation of  0.54
# Is there a significant improvement at the 5% significance level?

# Solution

# Step 1:
# Set up null hypothesis and alternative hypothesis
#H0: mu_4 = 3.10
#H1: mu != 3.10

#Step 2:
#Compute test statistic

xbar_4 <- 3.18
mu_4 <- 3.1
sigma_4 <- 0.54
n_4 <- 20
t <- (xbar_4-mu_4)/(sigma_4/sqrt(n_4))  #t = 0.6625

#Step 3: Compute critical value for significance level =5% or confidence interval=95%

# df = 19 (sample -1) i.e. 20-1 = 19
# because our alternative hypothesis is mu != 3.10, this is a two-tailed test
# so we need to look up 5%/2
# in the table we get -2.093 and 2.093 as our two t-critical values

#step 4: Compare test stat to critical value and draw conclusions

#since our t-test value is lower than our t-critical value, we have
#insufficient evidence at the the 5% significance level to reject the null hypothesis
# We can conclude that the average GPA of the football players is not significantly different
#than the other athletic students

##########################################################################
# CALCULATING A CONFIDNECE INTERVAL USING T-STATISTICS
#
# Duracell manufactures batteries that the CEO claims will last an average of 300 hours under
# normal use. A researcher randomly selected 20 batteries from the production line
# line and tested these batteries. The tested batteries had a mean life span of
# 270 hours with a standard deviation of 50 hours.
# Do we have enough evidence to suggest that the claim of an average lifetime 
# 300 hours is false?
############################################################################

#Step 1: Set up H0 and H1 and Calculate the SE, the t-stat and t-critical
# H0: mu = 300
# H1: mu != 300

mu_5 <- 300
sigma_5 <- 50
xbar_5 <- 270
n_5 <- 20

#Step 2:
#Compute test statistic

t_2 <- (xbar_5-mu_5)/(sigma_5/sqrt(n_5)) # t-stat is 2.6832
SE_t <- sigma_5/sqrt(20) #11.18


#Step 3: Compute critical value for significance level =5% or confidence interval=95%
# degree of freedom = 20 - 1 =19
# t-stat critical value is c(-2.093, 2.093)

# Margin of error = (t-stat critical value at 2.5%)*SE_t
ME <- SE_t * 2.093
CI_min <- xbar_5 - ME
CI_max <- xbar_5 + ME

# Confidence interval is therefore (246.6, 293.400)

#step 4: draw conclusions
# The mean of 300 is not included in the confidence interval, therefore we have sufficient 
# evidence at the 5% level to reject the null hypothesis