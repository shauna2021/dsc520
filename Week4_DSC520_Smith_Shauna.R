##Shauna Smith
##DSC520
##Week 4 Assignments
##Professor Fadi Aslaleem
##06/30/2022


score <- "/Users/Shaun/Downloads/scores.csv"
scores <- read.table(file=score, header=TRUE, sep=",")

head(scores)

sports_sections <- subset(scores, scores$Section=="Sports")

regular_sections <- subset(scores, scores$Section=="Regular")

sports_sections


install.packages("ggplot2")
library(ggplot2)

qplot(sample = sports_sections$Score) + 
  labs(title = "Sports Section Scores", x = "Sports sections", y = "Scores")

qplot(sample = regular_sections$Score) + 
  labs(title = "Regular Section Scores", x = "Regular sections", y = "Scores")

sports_story <- sum(sports_sections$Score)
sports_story

sports_mean <- mean(sports_sections$Score)
sports_mean

regular_story <- sum(regular_sections$Score) 
regular_story

regular_mean <- mean(regular_sections$Score)
regular_mean

summary(sports_sections)

summary(regular_sections)

shapiro.test(sports_sections$Score)

shapiro.test(regular_sections$Score)


install.packages(car)
library(car)

leveneTest(sports_sections$Score, regular_sections$Score, center = mean)

##Part 2 of Week 4 assignment "housing"

housing <- "/Users/Shaun/OneDrive/Documents/week-6-housing.csv"
housing <- read.table(file=housing, header=TRUE, sep=",")
str(housing)
housing

?apply
apply(X = housing, MARGIN = 2, FUN = max, na.rm = TRUE, simplify = TRUE)
apply(X = housing, MARGIN = 1, FUN = max, na.rm = TRUE, simplify = TRUE)

install.packages("dplyr")
library(dplyr)     

head(housing)
summary(housing)

aggregate(Sale.Price~year_built+square_feet_total_living, housing, mean)
aggregate(year_built~Sale.Price, housing, median)

install.packages("plyr")
library(plyr)

summary(housing)

housing %>% 
  select(Sale.Price, square_feet_total_living)%>%
  mutate(price.per.sq.ft = Sale.Price/square_feet_total_living)%>%
  summarise(Avg.price.per.sq.ft=mean(price.per.sq.ft))

install.packages("ggplot2")
library(ggplot2)

dnorm(housing$Sale.Price)

rnorm(housing)

testing <-rnorm(housing)
hist(testing)

boxplot(housing$Sale.Price)
hist(housing$Sale.Price)

hist.housing <- ggplot(housing, aes(Sale.Price))+
  geom_histogram(aes(y=..density..), binwidth = 20000)+
  geom_density(color= "red")
  
hist.housing

t.test(housing$Sale.Price)

boxplot(housing$Sale.Price)
outliers <- boxplot.stats(housing$Sale.Price)$out
outliers

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

## Creating 2 new variables (using lot size for Acreage & Sale Price for "classifications")
with_fancy <- housing %>%
  select(Sale.Price, sq_ft_lot)%>%
  mutate(High_End = Sale.Price >= '1000000', 
         High_End = if_else(High_End == TRUE, "Fancy", "Standard"))%>%
  mutate(Acres = sq_ft_lot >= '43560')
 
  
  

  
  

