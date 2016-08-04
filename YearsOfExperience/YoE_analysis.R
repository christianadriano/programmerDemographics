#-----------------------------------------------------------------
# Did the workers who passed the test had on average more YoE than workers who did not pass the test?
# YES, 
#Years of Experience by score in the qualification test

setwd("C://firefly//YearsOfExperience//")
sample <- read.csv("YoE_qualificationTest.txt", header=T)
summary(sample)

shapiro.test(sample$YoP_passed_test)
shapiro.test(sample$YoP_failed_test)

#Both failed the normality test

wilcox.test(sample$YoP_passed_test,sample$YoP_failed_test, alternative= "two.sided", paired=FALSE)
#  W = 477139, p-value < 2.2e-16

#-----------------------------------------------------------------
#Are professions distinct in terms of Years of Experience? 
#With exception of Hobbyist and Undergrads, and Other and Undergrads, all professions are.
#So, it would makes sense from YoE perspective, to consider Hobbyist, Undergrads and Others together.

setwd("C://firefly//YearsOfExperience//")
sample <- read.csv("YoE_Profession_data.csv", header=T)
summary(sample)
shapiro.test(sample$YoE_Professional_Developer)
shapiro.test(sample$YoE_Hobbyist)

#Pos hoc comparision
#Bonferroni of 5 groups = 5!/2*3! = 5*4/2 = 5*2 = 10
#significance level = 0.05 / 10 = 0.005

wilcox.test(sample$YoE_Professional_Developer,sample$YoE_Hobbyist, alternative= "two.sided", paired=FALSE)
#W = 211841.5, p-value < 2.2e-16
wilcox.test(sample$YoE_Professional_Developer,sample$YoE_Graduate_Student, alternative= "two.sided", paired=FALSE)
#W = 122293.5, p-value < 2.2e-16
wilcox.test(sample$YoE_Undergraduate_Student,sample$YoE_Graduate_Student, alternative= "two.sided", paired=FALSE)
#W = 80888.5, p-value = 2.221e-06
wilcox.test(sample$YoE_Professional_Developer,sample$YoE_Graduate_Student, alternative= "two.sided", paired=FALSE)
#W = 122293.5, p-value < 2.2e-16
wilcox.test(sample$YoE_Hobbyist,sample$YoE_Graduate_Student, alternative= "two.sided", paired=FALSE)
#W = 117157.5, p-value = 0.7261 NOT SIGNIFICANT
wilcox.test(sample$YoE_Hobbyist,sample$YoE_Undergraduate_Student, alternative= "two.sided", paired=FALSE)
#W = 213515.5, p-value = 1.621e-06
wilcox.test(sample$Other,sample$YoE_Undergraduate_Student, alternative= "two.sided", paired=FALSE)
#W = 50268.5, p-value = 0.03983 NOT SIGNIFICANT
wilcox.test(sample$Other,sample$YoE_Graduate_Student, alternative= "two.sided", paired=FALSE)
#W = 27439, p-value = 2.798e-05
wilcox.test(sample$YoE_students,sample$YoE_nonStudents, alternative= "two.sided", paired=FALSE)
#W = 441244, p-value < 2.2e-16



#-----------------------------------------------------------------
#Years of Experience distribution
setwd("C://firefly//YearsOfExperience//")
sample <- read.csv("YoEAgeData.csv", header=T)
summary(sample)
