##Relation between number of programming languages and Years of Experience
## I expect that workers with more years of experience know on average more 
##programming languages than workers with fewer years

baseDir<- "C://Users//chris//OneDrive//Documentos//GitHub//";
dir<- paste(baseDir, "dataWrangling//dataframeUtil.R",sep="");
source(dir);
dir<- paste(baseDir, "programmerDemographics//loadDemographics.R",sep="");
source (dir);
dir<- paste(baseDir, "programmerDemographics//loadDemographics.R",sep="");
source (dir);
dir<- paste(baseDir, "programmerDemographics//ProgrammingLanguagesAnalysis//CategorizeLanguages.R",sep="");
source (dir);

######
datafLanguages <- cbind(dataf,totalLanguagesVector);

plot(datafLanguages$Years.Programming,datafLanguages$totalLanguagesVector)

####
## Validate data
# Workers with more than one year of programming and zero languages 