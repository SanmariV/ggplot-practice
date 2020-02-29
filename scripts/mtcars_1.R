#1. Load libraries
library(tidyverse)
library(lubridate)
library(sanzo)
library(tidyquant)

#2. Load data
mtcars

#3. Examine data
glimpse(mtcars)
#4. Join data

#5. Wrangle data
cyl <- as.factor(mtcars$cyl)
am <- as.factor(mtcars$am)
#6. Data insights

#scatterplot
theme_set(theme_bw())


ggplot(data = mtcars,aes(x = wt, y= mpg, color = factor(cyl), size=disp, shape=factor(am))) +
    geom_point()

#barplot
