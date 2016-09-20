
#Decision Tree example

#Using rpart algorithm

library(rpart)
install.packages("rpart.plot")
library(rpart.plot) 

data("iris")
head(iris)
str(iris)
#iris

#Shuffle randomly the rows in the dataset
set.seed(9850)
g<- runif((nrow(iris))) #generates a random distribution
irisr <- iris[order(g),]

#rpart(Species ~ Sepal.length + Sepal.widht ) or just use . (see below)
#separates 100 to train and 50 to test
#class means that we want a categorization
#the . means that all the other columns are features.
model1 = rpart(Species ~ ., data = irisr[1:100,], method="class")
summary(model1)
rpart.plot(model1)
rpart.plot(model1, type=3, extra=101, fallen.leaves = T)

#Look at video for deatils https://www.youtube.com/watch?v=XLNsl1Da5MA

##TESTING THE MODEL NOW

p1 <- predict(model1, irisr[101:150,], type="class")
table(irisr[101:150,5],predicted=p1)

# predicted
#              setosa versicolor virginica
# setosa         16          0         0
# versicolor      0         13         2
# virginica       0          2        17

#All setosa were correctly predicted
#Two versicolors were incorectly predicted as virginica
#Two  virginicas were incorectly predicted as versicolor

#####################################################################

#USING C5.0 algorithm
install.packages("C50")
library(C50)
#-5 is to remove the column with the ground truth
model2 <- C5.0(irisr[1:100, -5], irisr[1:100, 5])
p2 <- predict(model2, irisr[101:150,], type="class")
table(irisr[101:150,5],predicted=p2)

# predicted
#               setosa versicolor virginica
# setosa         16          0         0
# versicolor      0         12         3
# virginica       0          0        19

#Performed better than rpart for the virginica.

