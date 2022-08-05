# Assignment: Assignment1 for Week 9 DSC520
# Name: Smith, Shauna
# Date: 2022-08-01

setwd("/Users/Shaun/OneDrive/Documents")

thoracic <- read.csv("Assignment1 week9 Thoracic SX data.csv")

summary(thoracic)

yay<-glm(X.15 ~ X.14+X.13+X.12+X.11+X.10+X.9+X.7+X.6+X.5+X.4+X.3,data=thoracic,family="binomial")

summary(yay)

yaypredict<-predict(yay,type = 'response')
yaypredict
head(yaypredict)
accuracy<- mean(yaypredict)
accuracy
