## Demographics analysis

## Can we predict profession based on attributes of Age and YoE
## Cluster analysis. Do we have clusters in which most workers are from the same profession?

setwd("C://Users//chris//OneDrive//Documentos//GitHub//programmerDemographics//WorkerCategorizationAnalysis");
data_all <- read.csv("demographics_data.csv",header = TRUE,sep=",");

summary(data_all);
data <- cbind(data_all$Years.Programming,data_all$Worker.Age);
colnames(data)<-c("year_of_experience","age");
dataf = data.frame(data)
head(dataf)

#First I need to look at the outliers or invalid values
#Invalid age and invalid years of experience

#Remove rows with not available data
dataf <- dataf [rowSums(is.na(data))==0,]

dataf <- dataf [!dataf$year_of_experience <0,]
dataf <- dataf [!dataf$age <0,]

#Remove outliers
dataf <- dataf [!dataf$year_of_experience >40,]
dataf <- dataf [!dataf$age >80,]


summary(dataf)

## Clustering
(cl <- kmeans(dataf,5))
plot(dataf, col=cl$cluster)
## Found the following clusters:
#C1(age[18,25],experience[0,10])
#C2(age[26,38],experience[0,10])
#C3(age[26,38],experience[8,25])
#C4(age[40,65],experience[0,15])
#C5(age[40,65],experience[15,35])
Do these clusters map to 