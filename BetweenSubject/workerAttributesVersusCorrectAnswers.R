#-------------------------------------------------------
#CORRELATION AMONG WORKER ATTRIBUTES AND PROPORTION OF CORRECT ANSWERS
setwd("C://firefly//BetweenSubject")
sample <- read.csv("allOneHITCorrectAnswers.csv",  header=T)
summary(sample)

#Initially I ran the Kendall's tau test, however, if I don't need
#the strength of correlation, I noticed that I could 
#run a more common Chi-square. Below is the Kendall test results, 
##after it are the Chi-Square

#Significan correlations
#z = 3.3967, p-value = 0.000682 tau=0.1317622 (negligible)
cor.test(sample$CorrectAnswers, sample$score, method="kendall") 

#z = -12.167, p-value < 2.2e-16 tau= -0.4842654 (strong)
cor.test(sample$CorrectAnswers, sample$IDKLevel, method="kendall")

#z = 5.2354, p-value = 1.646e-07 tau 0.1823747 (negligigle)
cor.test(sample$CorrectAnswers, sample$yearsOfProgramming, method="kendall")

#------------------------------------------------
## Chi-square test

install.packages("gmodels")
library(gmodels)


#CHI Square
CrossTable(sample$CorrectAnswers, sample$score, fisher=FALSE, chisq=TRUE, 
           expected=TRUE, sresid=TRUE,format="SPSS")

CrossTable(sample$CorrectAnswers,  sample$IDKLevel, fisher=FALSE, chisq=TRUE, 
           expected=TRUE, sresid=TRUE,format="SPSS")

CrossTable(sample$CorrectAnswers,  sample$yearsOfProgramming, fisher=FALSE, chisq=TRUE, 
           expected=TRUE, sresid=TRUE,format="SPSS")


CrossTable(sample$CorrectAnswers,  sample$profession, fisher=FALSE, chisq=TRUE, 
           expected=TRUE, sresid=TRUE,format="SPSS")
