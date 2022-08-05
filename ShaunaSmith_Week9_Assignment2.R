# Assignment: Assignment2 for Week 9 DSC520
# Name: Smith, Shauna
# Date: 2022-08-01

setwd("/Users/Shaun/OneDrive/Documents")

binary<- read.csv("Assignment2_binary_data.csv")
str(binary)

chekitout<-glm(label~x+y,data=binary,family = "binomial"(link="logit"))
chekitout
player1<-data.frame(x=22:66,y=22:66)
pop<-predict(chekitout, player1, type = "response")
summary(pop)

accuracychek<-mean(pop)
accuracychek

