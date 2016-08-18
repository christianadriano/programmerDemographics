#Correlation between % quit and size of Java method

setwd("C://firefly//QuitAnalysis//")
sample <- read.csv("quitData.csv", header=T)
summary(sample)

#Normality Test, all series failed the Shapiro normality test (p-value<0.05)
shapiro.test(sample$NumberQuits)
shapiro.test(sample$PercentQuit)

cor.test(sample$NumberQuits, sample$PercentQuit, method="kendall")
#data:  sample$NumberQuits and sample$PercentQuit
#z = 2.8675, p-value = 0.004137
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#  tau = 0.8365019 #STRONG CORRELATION