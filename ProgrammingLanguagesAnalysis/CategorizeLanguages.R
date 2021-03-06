
## Analysis of programming languages
#What is the proportion of programmers who know more than one programming language? 
#(Java, C/C++/C#, Javascript, Python)
#Number of different languages known by each person.

#Future challenges
#How to identify non-languages?
#How to filter same worker who filled the survey multiple times?
#How to identify misspelled language names?
#How many workers with none language?


#### MAIN ######################################################################

## Load scripts
baseDir<- "C://Users//chris//OneDrive//Documentos//GitHub//";
dir<- paste(baseDir, "dataWrangling//dataframeUtil.R",sep="");
source(dir);
dir<- paste(baseDir, "programmerDemographics//loadDemographics.R",sep="");
source(dir);

languageNames = 
          # c("java","C","jscript","python",
          #         "html","php","R","basic","sql","perl","ruby","other");

               c("cobol","dotNet","ada","assembly","matlab","lua","pascal","scala",
                "abap","a++","vhdl","xml","shell",
                 "groovy","autohotkey","swift","spss","clk","stata","cuda","drupal","gml","smalltalk",
                 "euphoria","ahk","sas","scratch","g-code","coldfusion","ccl","raptor","compass",
                 "labview");
#Load data
dataf<-loadDemographics("demographics_data.csv");

#Categorize
languageCountDF <- categorize(dataf,languageNames);
#Compute total different languages per worker
totalLanguagesVector <- rowSums(languageCountDF);
summarizeLanguagesPerWorker(totalLanguagesVector);


summarizeWorkersPerLanguage(languageCountDF); # INCLUDE "OTHER" lANGUAGES.
plotDistribution(totalLanguagesVector);

##############################################################################

## Function categorize data
categorize <- function(dataf, languageNames){
  
  #Extract language names from each worker
  #Count occurrences for all workers
  #Categorize each name under pre-defined classes
  #Append new columns to the original dataframe
  
  #Languages
  javaNames <- c("java","j2se","JRE","android"); 
  cNames <- c(" c ","cpp","c#","c\\+","objetive-c","mfc");
  jsNames <- c("script","ajax","js","asp"); #includes vbscript, actionscript,javascript
  pythonNames <- c("python");
  htmlNames <- c("html","css","hdml");
  phpNames <- c("php");
  rNames <- c("r "," r");
  basicNames <- c("basic","vb");
  sqlNames <- c("sql");
  perlNames <- c("perl");
  rubyNames <- c("ruby");
  cobolNames <- c("cobol","cobal");
  netNames <- c("\\.net","dotnet","dot net");
  #noneNames <- c("n//a","na","not","any","engineer","none","nil","i don't know",
   #              "0","1","2","3","4","5","6","yes","windows","dos","linux","excel",
    #             "english","tamil","df","-","unknown","all","byond",
     #            "gg","?","system languages","office","developing","software");
  adaNames <- c("ada");
  assemblyNames <- c("assembly","assembler");
  matlabNames <- c("matlab");
  luaNames <- c("lua");
  pascalNames <- c("pascal","powerbuilder");
  scalaNames <- c("scala");
  abapNames <- c("abap");
  aplusplusNames <- c("a\\+");
  vhdlNames <- c("vhdl");
  xmlNames <- c("xml");
  shellNames <- c("shell","prompt","bash");

  groovyNames <- c("groovy");
  swiftNames <- c("swift");
  autohotkeyNames <- c("auto"); #autohotkey Windows script to automate input (e.g., short keys)
  spssNames <- c("spss");
  clkNames <- c("clk"); #embeded system script
  stataNames <- c("stata"); #language of a statistical package
  cudaNames <- c("cuda"); #parallel programming language
  drupalNames <- c("drupal");
  gmlNames <- c("gml");
  smalltalkNames <- c("smalltalk");
  euphoriaNames <- c("euphoria");
  ahkNames <- c("ahk");
  sasNames <- c("sas");
  scratchNames <- c("scratch");
  gcodeNames <- c("g-code");
  coldFusionNames <- c("coldfusion");
  cclNames <- c("ccl");
  raptorNames <- c("raptor");
  compassNames <- c("compass");
  labviewNames <- c("labview");
  otherNames <- c("groovy","swift","spss","clk","stata","cuda","drupal","gml","smalltalk",
                  "euphoria","ahk","sas","scratch","g-code","coldfusion","ccl","raptor","compass",
                  "labview");
  
  
  #Convert all languages to lower case
  languagesVector <- tolower(dataf$Prog.Language)
  
  #Count occurrences
  # languageCountDF <-countWordsInVector(javaNames,languagesVector);
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(cNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(jsNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(pythonNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(htmlNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(phpNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(rNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(basicNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(sqlNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(perlNames,languagesVector));
  # languageCountDF<-cbind(languageCountDF,countWordsInVector(rubyNames,languagesVector));
  
  languageCountDF <-countWordsInVector(cobolNames,languagesVector);
   languageCountDF<-cbind(languageCountDF,countWordsInVector(netNames,languagesVector));
#   languageCountDF<-cbind(languageCountDF,countWordsInVector(noneNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(adaNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(assemblyNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(matlabNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(luaNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(pascalNames,languagesVector));
    languageCountDF<-cbind(languageCountDF,countWordsInVector(scalaNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(abapNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(aplusplusNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(vhdlNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(xmlNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(shellNames,languagesVector));

   languageCountDF<-cbind(languageCountDF,countWordsInVector(groovyNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(swiftNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(autohotkeyNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(spssNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(clkNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(stataNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(cudaNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(drupalNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(gmlNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(smalltalkNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(euphoriaNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(ahkNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(sasNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(scratchNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(gcodeNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(coldFusionNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(cclNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(raptorNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(compassNames,languagesVector));
   languageCountDF<-cbind(languageCountDF,countWordsInVector(labviewNames,languagesVector));

#  languageCountDF<-cbind(languageCountDF,countWordsInVector(otherNames,languagesVector));
  
  colnames(languageCountDF)[1] <- languageNames[1];
  colnames(languageCountDF)[2] <- languageNames[2];
  colnames(languageCountDF)[3] <- languageNames[3];
  colnames(languageCountDF)[4] <- languageNames[4];
  colnames(languageCountDF)[5] <- languageNames[5];
  colnames(languageCountDF)[6] <- languageNames[6];
  colnames(languageCountDF)[7] <- languageNames[7];
  colnames(languageCountDF)[8] <- languageNames[8];
  colnames(languageCountDF)[9] <- languageNames[9];
  colnames(languageCountDF)[10] <-  languageNames[10];
  colnames(languageCountDF)[11] <-  languageNames[11];
  colnames(languageCountDF)[12] <-  languageNames[12];
  
   colnames(languageCountDF)[13] <-  languageNames[13];
   colnames(languageCountDF)[14] <-  languageNames[14];
   colnames(languageCountDF)[15] <-  languageNames[15];
   colnames(languageCountDF)[16] <-  languageNames[16];
   colnames(languageCountDF)[17] <-  languageNames[17];
   colnames(languageCountDF)[18] <-  languageNames[18];
   colnames(languageCountDF)[19] <-  languageNames[19];
   colnames(languageCountDF)[20] <-  languageNames[20];
   colnames(languageCountDF)[21] <-  languageNames[21];
   colnames(languageCountDF)[22] <-  languageNames[22];
   colnames(languageCountDF)[23] <-  languageNames[23];
   colnames(languageCountDF)[24] <-  languageNames[24];
  # 
   colnames(languageCountDF)[25] <-  languageNames[25];
   colnames(languageCountDF)[26] <-  languageNames[26];
  # 
   colnames(languageCountDF)[27] <-  languageNames[27];
   colnames(languageCountDF)[28] <-  languageNames[28];
   colnames(languageCountDF)[29] <-  languageNames[29];
   colnames(languageCountDF)[30] <-  languageNames[30];
   colnames(languageCountDF)[31] <-  languageNames[31];
   colnames(languageCountDF)[32] <-  languageNames[32];
   colnames(languageCountDF)[33] <-  languageNames[33];
  # colnames(languageCountDF)[34] <-  languageNames[34];
  # colnames(languageCountDF)[35] <-  languageNames[35];
  # colnames(languageCountDF)[36] <-  languageNames[36];
  # colnames(languageCountDF)[37] <-  languageNames[37];
  # colnames(languageCountDF)[38] <-  languageNames[38];
  # colnames(languageCountDF)[39] <-  languageNames[39];
  # colnames(languageCountDF)[40] <-  languageNames[40];
  # colnames(languageCountDF)[41] <-  languageNames[41];
  # colnames(languageCountDF)[42] <-  languageNames[42];
  # colnames(languageCountDF)[43] <-  languageNames[43];
  # colnames(languageCountDF)[44] <-  languageNames[44];
  # colnames(languageCountDF)[45] <-  languageNames[45];
  # 
  # colnames(languageCountDF)[46] <-  languageNames[46];
  
  languageCountDF <- data.frame(languageCountDF);
  
  return (languageCountDF);
}

###############################################################
#Summary of total languages per worker
summarizeLanguagesPerWorker<- function(totalLanguagesVector){
  summary(totalLanguagesVector)
  df<- countNumbers(totalLanguagesVector)
  df<-data.frame(df)
  orderedDf <-df[order(df$percent),]
  colnames(orderedDf)[1]<-"Languages";
  print("Number of languages known by workers");
  print(orderedDf);
}

################################################################
#Summary of total workers per language
summarizeWorkersPerLanguage <- function(languageCountDF){
  workersPerLanguage <- colSums(languageCountDF);
  sortedColumnIndex <- order(-workersPerLanguage);
  workersPerLanguage <-workersPerLanguage[sortedColumnIndex];
  languageNames <- languageNames[sortedColumnIndex];
  
  par(cex.lab=0.55);
  par(cex.axis=0.65);
   barplot(workersPerLanguage, names.arg = languageNames, 
          xlab = "languages", ylab="workers", 
          main = "workers per language",
          col="lightgreen");
  
 
}

################################################################
#Function to plot the distribution of languages known by workers
plotDistribution <- function(totalLanguagesVector){
  hist(totalLanguagesVector, 
       main="Distribution MTurk workers' programming languages",
       breaks = 20,
       xlab="Programming Languages",
       ylab="Number of workers",
       xlim=c(0,10),
       ylim=c(0,1200),
       border="blue", col="green", 
       probability=FALSE);
  #line(density(totalLanguagesVector));
}



###############################################################
#Function count OTHER languages
workersPerOtherLanguages
OtherLanguagesPerWorker



###############################################################
#Test data
#NEED TO TEST THE DATA
test<-function(){
  column1<- c("perl","java, c, c++","Java, C","JAVA")
  column2<- c(0,1,2,3)
  df<- data.frame(column1,column2)
  names<-c("years","language")
  colnames (df) <- names
  
  count1 <-countWords("java",df,1)
  count2 <-countWords("JAVA",df,1)
}
