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



#--------------------------------------------------------------------------
# TESTING THE NORMALITY ASSUMPTION

setwd("C://firefly//BetweenSubject//")
profession <- read.csv("profession.csv",  header=T)
mydataframe <- data.frame(profession)
summary(profession)
mean(profession$Hobbyist, na.rm=TRUE)

result <- shapiro.test(profession$NonStudentAllHITs)
result$p.value
#[1] 6.416916e-20

result <- shapiro.test(profession$StudentAllHITs)
result$p.value
#[1] 2.827004e-16

setwd("C://firefly//BetweenSubject//")
score <- read.csv("score.csv",  header=T)
summary(score)

result <- shapiro.test(score$X100.Percent)
result$p.value
#[1] 1.699727e-06

result <- shapiro.test(score$X80.Percent)
result$p.value
#[1] 1.440468e-07

result <- shapiro.test(score$X60.Percent)
result$p.value
#[1] 0.0001013187


#-------------------------------------------------------------------------
#YEARS OF PROGRAMMING 
setwd("C://firefly//BetweenSubject//")
YoP <- read.csv("YoP_Accuracy.csv",  header=T)
summary(YoP)


#tesing for normality
qplot(sample = YoP$LogYOP, stat="qq")
stat.desc(YoP$LogYOP, basic=FALSE, norm=TRUE)

result <- shapiro.test(YoP$Accuracy)
result$p.value
#[1] 3.372552e-21
shapiro.test(YoP$LogYOP)
result$p.value
#[1] 1.497787e-25

hist(YoP$Years.Programming)
hist(YoP$LogYOP)

set.seed(450)
x <- runif(50, min=2, max=4)
shapiro.test(x)

#-------------------------------------------------------------------------
#Worker sample 1 session from each worker

sample <- read.csv("OneSessionFromWorker.csv",  header=T)
#sample <- read.csv("Book7.csv",  header=T)

summary(sample)
hist(sample$Accuracy)
hist(sample$Corret.Answers)
hist(sample$TP+sample$TN)
hist(sample$log)

stat.desc(YoP$LogYOP, basic=FALSE, norm=TRUE)

shapiro.test(sample$Correct.Answers)

cor.test(sample$score, sample$Accuracy, method="kendall")
result$statistic
result$p.value
#--------------------------------------------------------------------------
sample <- read.csv("confidenceDifficulty.csv",  header=T)
cor.test(sample$c2, sample$d2, method="spearman") 

cor.test(sample$c5, sample$d5, method="kendall") 
#OK!
# > cor.test(sample$c1, sample$d1, method="spearman")
# 
# Spearman's rank correlation rho
# 
# data:  sample$c1 and sample$d1
# S = 164, p-value = 0.001141
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# -0.952381 

#cor.test(sample$c2, sample$d2, method="spearman")
#Spearman's rank correlation rho
#data:  sample$c2 and sample$d2
#S = 154, p-value = 0.01538
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.8333333 

#Could not find signficant correlation in the other pairs 
#of confidence and difficulty level.

#-------------------------------------------------------
sample <- read.csv("allOneHITCorrectAnswers.csv",  header=T)
summary(sample)

#Significan correlations
#z = 3.3967, p-value = 0.000682 tau=0.1317622 (negligible)
cor.test(sample$CorrectAnswers, sample$score, method="kendall") 

#z = -12.167, p-value < 2.2e-16 tau= -0.4842654 (strong)
cor.test(sample$CorrectAnswers, sample$IDKLevel, method="kendall")

#z = 5.2354, p-value = 1.646e-07 tau 0.1823747 (negligigle)
cor.test(sample$CorrectAnswers, sample$yearsOfProgramming, method="kendall")


#PRECISION DID NOT SHOW ANY SIGNIFICANT CORRELATION
cor.test(sample$Precision, sample$score, method="kendall")
cor.test(sample$Precision, sample$IDKLevel, method="kendall")


#-------------------------------------------------------------
# Compare Correct Answers among different scores in qualification test

sample <- read.csv("scoreCorrectAnswers.csv",  header=T)
summary(sample)
# ALL NON-SIGNIFICANT DIFFERENCES
wilcox.test(sample$score4, sample$score5, paired=FALSE)

#Significan W = 11098, p-value = 0.0003977
# Median for score 3=1, while Median for score 5 = 2.
wilcox.test(sample$score3, sample$score5, paired=FALSE)
#W = 20749.5, p-value = 0.0005312
wilcox.test(sample$score3, sample$above4, paired=FALSE)

#-- FUNCTION TO CALCULATE THE EFFECT SIZE IN WILCOX TEST
# references Andrew Field's book and http://www.uccs.edu/lbecker/effect-size.html

rFromWilcox <- function(wilcoxModel, N){
 z <- qnorm(wilcoxModel$p.value/2)
 r <- z/sqrt(N)
 cat(wilcoxModel$data.name, "Effect size, r=", r)
}

model <-wilcox.test(sample$score3, sample$score5, paired=FALSE, conf.int=TRUE)
N <-length(sample$score5)
rFromWilcox(model, N) #See formula page 665 book
#sample$score3 and sample$score5 Effect size, r= -0.1890368

model <-wilcox.test(sample$score3, sample$score4, paired=FALSE)
N <- length(sample$score4)
rFromWilcox(model, N) #See formula page 665 book
#sample$score3 and sample$score4 Effect size, r= -0.1311584


model <-wilcox.test(sample$score3, sample$above4, paired=FALSE)
N <- length(sample$above4)
rFromWilcox(model, N) #See formula page 665 book
#sample$score3 and sample$above4 Effect size, r= -0.1849208

#----------------------------------------------------------
# Correct answers by Profession 

sample <- read.csv("professionCorrectAnswers.csv",  header=T)
summary(sample)

#SIGNIFICANT
# W = 4117, p-value = 0.005823
wilcox.test(sample$Graduate, sample$Professional)
#W = 8027, p-value = 0.03393
wilcox.test(sample$Hobbyist, sample$Professional)
#W = 25101, p-value = 0.01862
wilcox.test(sample$Student, sample$Non.Student)


# NON-SIGNIFICANT
wilcox.test(sample$Graduate, sample$Undergraduate)
wilcox.test(sample$Other, sample$Undergraduate)
wilcox.test(sample$Hobbyist, sample$Undergraduate)
wilcox.test(sample$Hobbyist, sample$Graduate)
wilcox.test(sample$Other, sample$Professional)

model <-wilcox.test(sample$Graduate, sample$Professional, paired=FALSE)
N <- length(sample$Professional)
rFromWilcox(model, N) 
#sample$Graduate and sample$Professional Effect size, r= -0.1551276

model <-wilcox.test(sample$Hobbyist, sample$Professional, paired=FALSE)
N <- length(sample$Professional)
rFromWilcox(model, N) 
#sample$Hobbyist and sample$Professional Effect size, r= -0.1193085

model <-wilcox.test(sample$Student, sample$Non.Student, paired=FALSE)
N <- length(sample$Non.Student)
rFromWilcox(model, N)
#sample$Student and sample$Non.Student Effect size, r= -0.1323671

#------------------------------------------------------------------------
# YEARS OF PROGRAMMING

sample <- read.csv("YoP_CorrectAnswers.csv",  header=T)
summary(sample)

#SIGNIFICANT

wilcox.test(sample$UpTo_1, sample$X1_to_5)
#W = 5252.5, p-value = 0.01538

wilcox.test(sample$X1_to_5, sample$X5_to_10)
#W = 9847, p-value = 0.006362

wilcox.test(sample$X1_to_5, sample$above.10)
#W = 5658.5, p-value = 0.006073

wilcox.test(sample$UpTo_1, sample$above.10)
#W = 1111.5, p-value = 2.848e-05

#NON SIGNIFICANT
wilcox.test(sample$X5_to_10, sample$above.10)

#EFFECT SIZES
model <-wilcox.test(sample$UpTo_1, sample$X1_to_5, paired=FALSE)
N <- length(sample$X1_to_5)
rFromWilcox(model, N)
#sample$UpTo_1 and sample$X1_to_5 Effect size, r= -0.162645

model <-wilcox.test(sample$X1_to_5, sample$X5_to_10, paired=FALSE)
N <- length(sample$X5_to_10)
rFromWilcox(model, N)
#sample$X1_to_5 and sample$X5_to_10 Effect size, r= -0.1831251

model <-wilcox.test(sample$X5_to_10, sample$above.10, paired=FALSE)
N <- length(sample$X5_to_10)
rFromWilcox(model, N)
#sample$X5_to_10 and sample$above.10 Effect size, r= -0.02014275

model <-wilcox.test(sample$UpTo_1, sample$above.10, paired=FALSE)
N <- length(sample$X5_to_10)
rFromWilcox(model, N)
#sample$UpTo_1 and sample$above.10 Effect size, r= -0.2808993


#----------------------------------------------------------
# Correct answers by IDK LEVEL 

sample <- read.csv("IDKCorrectAnswers.csv",  header=T)
summary(sample)

#SIGNIFICANT
wilcox.test(sample$IDK_0, sample$IDK0_33)
#W = 24521, p-value < 2.2e-16



#NON SIGNIFICANT
wilcox.test(sample$IDK0_33, sample$IDK34_66)
wilcox.test(sample$IDK0, sample$IDK34_66)

model <-wilcox.test(sample$IDK_0, sample$IDK0_33, paired=FALSE)
N <- length(sample$IDK_0)
rFromWilcox(model, N)
#sample$IDK_0 and sample$IDK0_33 Effect size, r= -0.4557671
