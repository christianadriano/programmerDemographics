install.packages("clinfun")
install.packages("pgirmess")
install.packages("car")
install.packages("ggplot2")
install.packages("pastecs")

library(clinfun)
library(pgirmess)
library(car)
library(ggplot2)
library(pastecs)

#---------------------------------------------------------------------
# Compare Correct Answers among different scores in qualification test

sample <- read.csv("scorePrecision.csv",  header=T)
summary(sample)

#AVERAGE PRECISIONS

# SIGNIFICANT DIFFERENCES
wilcox.test(sample$score4, sample$score5, paired=FALSE)
# W = 11, p-value = 0.0156
wilcox.test(sample$score3, sample$score5, paired=FALSE)
#W = 6, p-value = 0.004071
wilcox.test(sample$score3, sample$above4, paired=FALSE)
#W = 12, p-value = 0.0362

# ALL NON-SIGNIFICANT DIFFERENCES
wilcox.test(sample$score4, sample$score3, paired=FALSE)
wilcox.test(sample$above4, sample$score5, paired=FALSE)

model <-wilcox.test(sample$score3, sample$score5, paired=FALSE, conf.int=TRUE)
N <-length(sample$score5)
rFromWilcox(model, N)
#sample$score3 and sample$score5 Effect size, r= -1.015622

model <-wilcox.test(sample$score4, sample$score5, paired=FALSE, conf.int=TRUE)
N <-length(sample$score5)
rFromWilcox(model, N)
#sample$score4 and sample$score5 Effect size, r= -0.8549587

model <-wilcox.test(sample$score3, sample$above4, paired=FALSE, conf.int=TRUE)
N <-length(sample$above4)
rFromWilcox(model, N)
#sample$score3 and sample$above4 Effect size, r= -0.7405657

#AVERAGE RECALLS
# SIGNIFICANT DIFFERENCES

wilcox.test(sample$recallScore3, sample$recallScore5, paired=FALSE)
#W = 12.5, p-value = 0.04377

## ALL NON-SIGNIFICANT DIFFERENCES
wilcox.test(sample$recallScore4, sample$recallScore5, paired=FALSE)
#W = 15, p-value = 0.07927
wilcox.test(sample$recallScore3, sample$recallAbove4, paired=FALSE)
#W = 15, p-value = 0.07995

#-------------------------------------------------------------------------
#PROFESSIONS
#---------------------------------------------------------------------
# Compare Correct Answers among different scores in qualification test

sample <- read.csv("professionPrecision.csv",  header=T)
summary(sample)

#AVERAGE PRECISIONS
#SIGNIFICANT
wilcox.test(sample$Student, sample$Non.Student)
#W = 8, p-value = 0.01065

#NON SIGNIFICANT
wilcox.test(sample$Graduate, sample$Professional)
#W = 20.5, p-value = 0.2388
wilcox.test(sample$Hobbyist, sample$Professional)
#W = 27.5, p-value = 0.6677

model <-wilcox.test(sample$Student, sample$Non.Student, paired=FALSE, conf.int=TRUE)
N <-length(sample$Non.Student)
rFromWilcox(model, N)

#RECALL
wilcox.test(sample$RecallStudent, sample$RecallNonStudent)
#W = 10.5, p-value = 0.02496

model <-wilcox.test(sample$RecallStudent, sample$RecallNonStudent, paired=FALSE, conf.int=TRUE)
N <-length(sample$RecallNonStudent)
rFromWilcox(model, N)
#sample$RecallStudent and sample$RecallNonStudent Effect size, r= -0.7926692

#-----------------------------------------------------------------------
# IDK
setwd("C://firefly//BetweenSubject//")
sample <- read.csv("IDKPrecision.csv",  header=T)
summary(sample)

#AVERAGE PRECISIONS
#NOT SIGNIFICANT!!
wilcox.test(sample$Precision_0, sample$Precision_1_34)
#W = 29, p-value = 0.7884


#-----------------------------------------------------------------------
# Years of Programming
setwd("C://firefly//BetweenSubject//")
sample <- read.csv("yopPrecision.csv",  header=T)
summary(sample)

#AVERAGE PRECISIONS
#NOT SIGNIFICANT!!
wilcox.test(sample$ YoP0_1, sample$YoP1_5)
#W = 23, p-value = 0.3571
wilcox.test(sample$ YoP5_10, sample$YoP1_5)
#W = 38.5, p-value = 0.5199
wilcox.test(sample$ YoP5_10, sample$YoP10_15)
#W = 35, p-value = 0.7899
wilcox.test(sample$ YoP15_50, sample$YoP10_15)
#W = 34.5, p-value = 0.8303

#----------------------------------------------------------------------

