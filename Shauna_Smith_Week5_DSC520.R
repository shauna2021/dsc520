##Shauna Smith
##DSC520
##Week 5 Assignments
##Professor Fadi Aslaleem
##07/7/2022

install.packages("dplyr")
library(dplyr)


housing <- "/Users/Shaun/OneDrive/Documents/week-6-housing.csv"
housing <- read.table(file=housing, header=TRUE, sep=",")
head(housing)

story_zip <- housing %>%
  group_by(zip5)%>%
  summarise(story = max(bedrooms))

story_zip

story_deal <- housing %>%
  select(square_feet_total_living,Sale.Price,sq_ft_lot)%>%
  mutate(price_per_ft=Sale.Price/square_feet_total_living)
  
story_deal
  
best_buys <- story_deal %>%
  filter(sq_ft_lot >= 43560)%>%
  arrange(price_per_ft)

best_buys

install.packages("purrr") 
library(purrr)

?keep

dalist <- list(bobo=1:10, cancan=1:5, daada=1:3)

dalist1<-dalist

dalist1 %>% 
  keep(~mean(.x)>2)

dalist2 <-dalist

dalist2 %>% 
  map_if(is.integer, mean)%>%
  compact()

story_deal%>%
  compact()

story_deal %>%
  keep(~is.integer(.x))

cbind(story_zip)
rbind(story_zip)

install.packages("stringr")
library(stringr)

string4 <- "Today is the day the Lord has made"
string4
splitted<-str_split(string4,pattern = " ")
splitted

houseList <- housing [1:3,]
houseList

splitHl <- str_split(string = houseList$addr_full, pattern =" " )
splitHl

redress <- str_c("Address:", splitHl, sep = " ")
redress


