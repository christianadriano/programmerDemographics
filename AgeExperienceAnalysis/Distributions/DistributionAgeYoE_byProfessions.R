#What is the distribution of ages and YoE among professions?

setwd("C://Users//chris//OneDrive//Documentos//GitHub//programmerDemographics//WorkerCategorizationAnalysis");

data_all <- read.csv("demographics_data.csv",header = TRUE,sep=",");

dataf = data.frame(data_all)
#summary(dataf)
#First I need to look at the outliers or invalid values
#Invalid age and invalid years of experience

#Remove rows with not available data
dataf <- dataf [(is.na(dataf$Worker.Age))==0,]

#Remove invalid values
dataf <- dataf [!dataf$Worker.Age <1,]
dataf <- dataf [!dataf$Worker.Age <1,]

#Assuming that the youngest age to start programming is 10 years old
#Remove inputs for which age-YoE<5 
dataf <- removeLinesColDiffSmallerThanValue(dataf,7,5,10)


#################################
#Subsetting for each profession
datafProfDeveloper <- dataf[(dataf$Worker.Experience=="Professional_Developer") ,] 
datafHobbyist <- dataf[(dataf$Worker.Experience=="Hobbyist") ,]
datafGraduate_Student <- dataf[(dataf$Worker.Experience=="Graduate_Student") ,]
datafUndergraduate_Student <- dataf[(dataf$Worker.Experience=="Undergraduate_Student") ,]
#Generate for Others
datafOther <- dataf[(dataf$Worker.Experience!="Professional_Developer") ,] 
datafOther <- datafOther[(datafOther$Worker.Experience!="Hobbyist") ,]
datafOther <- datafOther[(datafOther$Worker.Experience!="Graduate_Student") ,]
datafOther <- datafOther[(datafOther$Worker.Experience!="Undergraduate_Student") ,]

#numberWorkers = dim(datafProfDeveloper)[1] + dim(datafHobbyist)[1] + dim(datafGraduate_Student)[1]+ dim(datafUndergraduate_Student)[1] + dim(datafOther)[1]

################################
##Outliers removal
#Professional
cut = 3 * sd(datafProfDeveloper$Worker.Age) + mean(datafProfDeveloper$Worker.Age)
datafProfDeveloper<- removeLinesAboveValue(datafProfDeveloper,7,cut)

cut = 3 * sd(datafProfDeveloper$Years.Programming) + mean(datafProfDeveloper$Years.Programming)
datafProfDeveloper<- removeLinesAboveValue(datafProfDeveloper,5,cut)

#Hobbyist
cut = 3 * sd(datafHobbyist$Worker.Age) + mean(datafHobbyist$Worker.Age)
datafHobbyist<- removeLinesAboveValue(datafHobbyist,7,cut)

cut = 3 * sd(datafHobbyist$Years.Programming) + mean(datafHobbyist$Years.Programming)
datafHobbyist<- removeLinesAboveValue(datafHobbyist,5,cut)

#Graduate
cut = 3 * sd(datafGraduate_Student$Worker.Age) + mean(datafGraduate_Student$Worker.Age)
datafGraduate_Student<- removeLinesAboveValue(datafGraduate_Student,7,cut)

cut = 3 * sd(datafGraduate_Student$Years.Programming) + mean(datafGraduate_Student$Years.Programming)
datafGraduate_Student<- removeLinesAboveValue(datafGraduate_Student,5,cut)

#Undergraduate
cut = 3 * sd(datafUndergraduate_Student$Worker.Age) + mean(datafUndergraduate_Student$Worker.Age)
datafUndergraduate_Student<- removeLinesAboveValue(datafUndergraduate_Student,7,cut)

cut = 3 * sd(datafUndergraduate_Student$Years.Programming) + mean(datafUndergraduate_Student$Years.Programming)
datafUndergraduate_Student<- removeLinesAboveValue(datafUndergraduate_Student,5,cut)

#Other
cut = 3 * sd(datafOther$Worker.Age) + mean(datafOther$Worker.Age)
datafOther<- removeLinesAboveValue(datafOther,7,cut)
dim(datafOther)[1]
cut = 3 * sd(datafOther$Years.Programming) + mean(datafOther$Years.Programming)
datafOther<- removeLinesAboveValue(datafOther,5,cut)
dim(datafOther)[1]

############################ 
##SCATTER PLOT
#Looking at the patterns
plot(datafProfDeveloper$Worker.Age,datafProfDeveloper$Years.Programming) 
title("Professional Developers YoE x Age")
plot(datafHobbyist$Worker.Age,datafHobbyist$Years.Programming)
title("Hobbyist YoE x Age")
plot(datafGraduate_Student$Worker.Age,datafGraduate_Student$Years.Programming)
title("Graduate Student YoE x Age")
plot(datafUndergraduate_Student$Worker.Age,datafUndergraduate_Student$Years.Programming)
title("Undergraduate Student YoE x Age")
plot(datafOther$Worker.Age,datafOther$Years.Programming)
title("Other YoE x Age")
