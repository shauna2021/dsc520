# Assignment: Week 9 Project DSC520
# Name: Smith, Shauna
# Date: 2022-08-01

setwd("/Users/Shaun/OneDrive/Documents")

VTT <- read.csv("VTT DataSet.csv")


install.packages("tidyverse")
library(tidyverse)

SimpleVTT<-VTT%>%
  select(Complication.Status,Recovery.Duration.in.days,Visual.Training.Technique.used)

VTT_Data <- data.frame(SimpleVTT)


colnames(VTT_Data)<- c("Complication", "Duration", "Technique")
head(VTT_Data)

install.packages("car")
install.packages("mlogit")
install.packages("Rcmdr")
library(car)
library(mlogit)
library(Rcmdr)

logrvtt<-glm(Complication~Technique,data=VTT_Data,family="binomial")
summary(logrvtt)
modelchi<-logrvtt$null.deviance-logrvtt$deviance
chidf<-logrvtt$df.null-logrvtt$df.residual
chipvtt<-1-pchisq(modelchi,chidf)
chipvtt
R2vtt<-modelchi/logrvtt$null.deviance
R2vtt
testerlog<-glm(Complication~.,data=VTT_Data,family = "binomial")
summary(testerlog)
