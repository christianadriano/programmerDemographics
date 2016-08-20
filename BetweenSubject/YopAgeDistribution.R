# Distribution of Age and Years of Programming Experience
setwd("C://firefly//BetweenSubject//")
sample <- read.csv("yopAgeDescriptive.csv",  header=T)
summary(sample)

boxplot(sample$YoP,
        sample$Age,
        ylab="Years", main="Workers Experience", 
        names = c("Years of Programming Experince", "Age"))
