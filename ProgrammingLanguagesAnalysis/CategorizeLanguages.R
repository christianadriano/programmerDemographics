
## Analysis of programming languages
#What proportion of programmers know each programming language? 
#(Java, C/C++/C#, Javascript, Python)
#Number of different languages known by each person.
                                                                
# Function counts the number of words in a column of the data frame
countWords <- function(word, dataframe, columNumber){
  booleanVector <- grepl(word, dataframe[,columNumber]);
  booleanVector*1;
}

#Extract language names from each worker
#Categorize each name under pre-defined classes
# new columns = lapply(vector)
# append new columns to the original dataframe

#Languages
javaLang <- c("Java","java","JAVA","j2se");
cLang <- c("\\c\\>","\\C\\>","\\cpp\\>","\\c++\\>","\\C++\\>","\\c#\\>","\\C#\\>");
javaScript <- c("javascript","Javascript","JavaScript","AJAX","ajax","js");
pythonLang <- c("python","Python");
html <- c("html","HTML");
phpLang <- c("php","PHP");
rLang <- c("//R//>");
basic <- c("basic","Basic","VB");
sqlLang <- c("sql","SQL");
Perl <- c("perl","Perl");
Perl <- c("ruby","Ruby");




#Count Java
#for(language in )
countWords("java",df,)
countWords

#Test data
column1<- c("perl","java, c, c++","Java, C","JAVA")
column2<- c(0,1,2,3)
df<- data.frame(column1,column2)
names<-c("years","language")
colnames (df) <- names

