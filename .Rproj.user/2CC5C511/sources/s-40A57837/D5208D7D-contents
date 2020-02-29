#1. Load libraries
library(tidyverse)
library(lubridate)
library(sanzo)
library(tidyquant)

#2. Load data
mtcars

#3. Examine data
glimpse(mtcars)
sanzo.demo.all()
sanzo.demo2()
#4. Join data

#5. Wrangle data

#6. Data insights

#scatterplot
theme_set(theme_bw())


ggplot(data = mtcars,aes(x = wt, y= mpg, color = factor(cyl), size=disp, shape=factor(am))) +
    geom_point()

#barplot
p <- ggplot(data = mtcars, aes(x = factor(cyl)))

ggplot(data = mtcars, aes(x = factor(cyl), fill = factor(am))) + 
    geom_bar() + 
    labs(
        title = "Automatic vs Manual transmission",
        subtitle = "Not sure what to say here",
        x_lab= "Size of cylinder",
        y_lab="Number of vehicles"
    )+
    theme_tq_dark()

#histogram
ggplot(data = mtcars, aes(x = mpg)) + 
    geom_histogram(binwidth = 5, color = "white", fill = "#AEB4A9") + 
    labs(
        title = "Miles per gallon Histogram",
        subtitle = "Not sure what to say here",
        x_lab= "Size of cylinder",
        y_lab="Number of vehicles"
    )

#density plot
ggplot(data = mtcars, aes(x = mpg)) + 
    geom_density(color = "#AEB4A9", fill = "white") + 
    labs(
        title = "Miles per gallon Histogram",
        subtitle = "Not sure what to say here",
        x_lab= "Size of cylinder",
        y_lab="Number of vehicles"
    )
