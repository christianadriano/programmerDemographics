#Load demographics data
#Loads and cleans the data (remove invalid input and outliers)

loadDemographics<- function(fileName){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//programmerDemographics//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf = data.frame(data_all)
  #summary(dataf)
  #First I need to look at the outliers or invalid values
  #Invalid age and invalid years of experience
  
  #Remove rows with not available data
  dataf <- dataf [(is.na(dataf$Worker.Age))==0,]
  
  #Remove invalid values
  dataf <- dataf [!dataf$Worker.Age <1,]
  dataf <- dataf [!dataf$Worker.Age <1,]
  
  #Outliers
  #Assuming that the youngest age to start programming is 10 years old
  #Remove inputs for which age-YoE<5 
  dataf <- removeLinesColDiffSmallerThanValue(dataf,7,5,10)
  return(dataf)
}
