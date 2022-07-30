# Assignment: ASSIGNMENT Week8 Housing
# Name: Smith, Shauna
# Date: 2022-27-07



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

fancy_fit <- lm(sq_ft_lot~Sale.Price, housing)
summary(fancy_fit)

Price_pin <- lm(Sale.Price~square_feet_total_living+year_built, housing)
summary(Price_pin)

install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(fancy_fit)
predicted <- data.frame(Sale.Price = c(125000,85000,54000))

lm.beta(Price_pin)

data.frame(predict(fancy_fit,predicted), interval="confidence", level = .95)

lmorig<-lm(formula=Sale.Price~sq_ft_lot,data=housing)
lmnew<-lm(formula=Sale.Price~sq_ft_lot, data=with_fancy)
anova(object=lmorig,lmnew)


plot(lmorig)
par(mfrow=c(3,3))
plot(lmorig)


plot(lmnew)

with_fancy$predicted<-fitted.values(lmnew)
with_fancy$residuals<-residuals(lmnew)
view(with_fancy)
summary(with_fancy)

sum(with_fancy$residuals>2)

plot(lmnew, pch=18, which=4)
plot(lmnew, pch=18, which=3)
plot(lmnew, pch=18, which=5)

anova(lmorig,lmnew)
pairs(with_fancy[,1:2])

install.packages("car")
library(car)
mlcol<-cor(with_fancy)
mlcol
vif(mlcol)
ivar<- with_fancy[,1:2]
vif(ivar)
vif(lmorig)
vif(lmnew)
vif(with_fancy)
vif(fancy_fit)
