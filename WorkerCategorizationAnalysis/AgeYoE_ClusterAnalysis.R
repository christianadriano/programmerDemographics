## Demographics analysis

## Cluster analysis. Do we have meaningful clusters in which most workers are from the same profession?

install.packages('knitr', dependencies = TRUE)
library(knitr)
library(ggplot2)


setwd("C://Users//chris//OneDrive//Documentos//GitHub//programmerDemographics//WorkerCategorizationAnalysis");
data_all <- read.csv("demographics_data.csv",header = TRUE,sep=",");

data <- cbind(data_all$Years.Programming,data_all$Worker.Age);
colnames(data)<-c("years_of_programming_experience","age");

#First I need to look at the outliers or invalid values
#Invalid age and invalid years of experience

dataf <- dataf [rowSums(is.na(data))==0,]

#Remove Invalid age and invalid years of experience
dataf <- dataf [!dataf$years_of_programming_experience <0,]
dataf <- dataf [!dataf$age <18,] ##minimum age to participate

#Remove people with no experience in programming
dataf <- dataf [!dataf$years_of_programming_experience ==0,]

#Remove people who whose age and experience 
dataf <- dataf [!(dataf$age - dataf$years_of_programming_experience) <5,]

#Remove outliers
dataf <- dataf [!dataf$years_of_programming_experience >40,]
dataf <- dataf [!dataf$age >80,]

summary(dataf);
cat("number of workers:",length(dataf[,1]));

## Plotting and regression curve
ggplot(dataf, aes(x=years_of_programming_experience, y=age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth() +            # Add a loess smoothed fit curve with confidence region
  ggtitle("Distribution of age by years of experience")

#People seemed to bucket their experiences in multiples of 5.


########## Clustering

# Function computes N clusters and plot a chart with them
plotKMeansClusters<- function(numberOfClusters){
  ## Clustering in three 
  (cl <- kmeans(dataf,numberOfClusters))
  
  dataf$cluster=factor(cl$cluster)
  centers=as.data.frame(cl$centers)
  
  #Looking at the clusters with size diameter 15.
  ggplot(dataf, aes(x=years_of_programming_experience, y=age, color=cluster)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth() + # Add a loess smoothed fit curve with confidence region
    geom_point(data=centers, aes(x=years_of_programming_experience,y=age, color='Center'), size=40, alpha=.3)+
    ggtitle("Clusters of years of experience and age")+
    guides(color=FALSE);
  
}

#What happens if we divide workers for 5 clusters, which is the number of professions?
plotKMeansClusters(5)
#We can see a lot of superposition of clusters. So, we look at fewer numbers

plotKMeansClusters(4)
plotKMeansClusters(3)
plotKMeansClusters(2)

(cl <- kmeans(dataf,2))

dataf$cluster=factor(cl$cluster)
centers=as.data.frame(cl$centers)

#Looking at the clusters with size diameter 15.
ggplot(dataf, aes(x=years_of_programming_experience, y=age, color=cluster)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth() + # Add a loess smoothed fit curve with confidence region
  geom_point(data=centers, aes(x=years_of_programming_experience,y=age, color='Center'), size=40, alpha=.3)+
  ggtitle("Clusters of years of experience and age")+
  guides(color=FALSE);


#Only with two clusters we circles do not superpose. 
#Interpreting that, we would have two large groups of workers. People above 35 years old with a wide spread of 
#programming experience. While people below 35 concentrated from 1 to 15 years of programming experience.

#Do these clusters map to professions?
## Can we predict profession based on attributes of Age and YoE: