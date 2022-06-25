#DSC520 
#American Community Survey Exercise
#Shauna Smith
#Week 3 (6/23/22)
#Professor: Fadi Alsaleeem

## Load the ggplot2 package
install.packages("ggplot2")
library(ggplot2)



## Set the working directory to the root
setwd("/Users/Shaun/OneDrive/Attachments")

## Load the `2014 American Community Survey`
survey <- read.csv ("American Community Survey.csv")

##Use str(), nrow(), ncol():
str(survey)
nrow(survey)
ncol(survey)

##Create a Histogram of the HSDegree variable using the ggplot2 package
ggplot(survey, aes(HSDegree)) + geom_histogram()

##Set a bin size for the Histogram
ggplot(survey, aes(HSDegree)) + geom_histogram(bins = 30)

##Include a Title and appropriate X/Y axis labels on your Histogram Plot
ggplot(survey, aes(HSDegree)) + geom_histogram(bins = 30) + ggtitle("Community Count HSDegree") + xlab("HSDegree (completed)") + ylab("Community Count (of Survey)")

##Include a normal curve to the Histogram that you plotted
ggplot(survey, aes(HSDegree)) + geom_histogram(aes(y = stat(density))) + geom_density(col = "red")

##Create a Probability Plot of the HSDegree variable
ggplot(survey, aes(sample = HSDegree)) + geom_qq() + geom_qq_line()

plots<-ggplot(survey, aes(sample = HSDegree)) + geom_qq() + geom_qq_line()
plots + stat_qq() + stat_qq_line()


##Now that you have looked at this data visually for normality, 
##you will now quantify normality with numbers using the stat.desc() function. 
##Include a screen capture of the results produced.
install.packages(pastecs)
library(pastecs)
stat.desc(survey$HSDegree)



