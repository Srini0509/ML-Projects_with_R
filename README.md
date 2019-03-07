## IRIS SPECIES PREDICTION WITH KNN ALGORITHM USING R-PROGRAM:

IRIS is the familiar small data set in Data science beginner's community for a nice start in Machine learning. It consists of 150 rows and 5 columns. As there is a relation discovered between flower's sepal and petal length with type of species, here I had predicted the type of Species using the relation stated earlier. It is classified using KNN Algorithm. The reason for choosing KNN is due to multiple classes in the response (Species). Here, for the easy reference for the visitor the result of the important execution is also loaded.

### Loading libraries

library(ISLR)

library(class)

library(caTools)

library(ggplot2)

library(gridExtra)

dim(iris)

  [1] 150   5

names(iris)

  [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"    

attach(iris)

### Checking for null values & datatypes 

mean(any(is.na(iris)))

[1] 0

str(iris)

'data.frame':	150 obs. of  5 variables:

$ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...

$ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...

$ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...

$ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...

$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

### Visualising the "Species" with other variables to identify median relation of the petal and sepal to species using box plot
data.frame(iris)

pl1<- ggplot(iris, aes(Species,Sepal.Length,colour=Species))+geom_boxplot(fill="orange",alpha=0.3)

pl2<- ggplot(iris, aes(Species,Sepal.Width,colour=Species))+geom_boxplot(fill="red",alpha=0.3)

pl3<- ggplot(iris, aes(Species,Petal.Length,colour=Species))+geom_boxplot(fill="pink",alpha=0.5)

pl4<- ggplot(iris, aes(Species,Petal.Width,colour=Species))+geom_boxplot(fill="darkblue",alpha=0.2)

grid.arrange(pl1,pl2,pl3,pl4,ncol=2)

 
### Splitting the data set into Train and Test set 

set.seed(123)

split<- sample.split(iris$Species,SplitRatio =0.75)

pre_train<- subset(iris,split==T)

pre_test<- subset(iris,split==F) 


dim(train.ir)

    [1] 114    4

dim(test.ir)

    [1] 36    4

### Applying KNN algorithm for identifying Species classification 
#### Choosing correct K value using the loop, funtion & plot 

species.pred<- NULL

error<- NULL

for(i in 1:20) {

set.seed(10)

species.pred<- knn(train.ir,test.ir,pre_train$Species,k=i)

error[i]<- mean(pre_test$Species!=species.pred)

}

print(error)

     [1] 0.13888889 0.13888889 0.08333333 0.08333333 0.05555556 0.05555556 0.05555556 0.05555556

     [9] 0.05555556 0.02777778 0.05555556 0.05555556 0.05555556 0.02777778 0.02777778 0.02777778

    [17] 0.05555556 0.05555556 0.05555556 0.05555556

spe.comp<- data.frame(pre_test$Species,species.pred)

spe.comp
   
#####     pre_test.Species   &   species.pred:
       1            setosa       setosa
       2            setosa       setosa
       3            setosa       setosa
       4            setosa       setosa
       5            setosa       setosa
       6            setosa       setosa
       7            setosa       setosa
       8            setosa       setosa
       9            setosa       setosa
       10           setosa       setosa
       11           setosa       setosa
       12           setosa       setosa
       13       versicolor   versicolor
       14       versicolor   versicolor
       15       versicolor   versicolor
       16       versicolor   versicolor
       17       versicolor   versicolor
       18       versicolor   versicolor
       19       versicolor   versicolor
       20       versicolor   versicolor
       21       versicolor   versicolor
       22       versicolor   versicolor
       23       versicolor   versicolor
       24       versicolor   versicolor
       25        virginica    virginica
       26        virginica    virginica
       27        virginica   versicolor  (misclassified data)
       28        virginica    virginica
       29        virginica    virginica
       30        virginica    virginica
       31        virginica    virginica
       32        virginica    virginica
       33        virginica    virginica
       34        virginica    virginica
       35        virginica    virginica
       36        virginica    virginica

kvalue<- 1:20

error.table<- data.frame(kvalue,error)

error.table

error.table

##### kvalue   &   error:
       1    0.13888889
       2    0.13888889
       3    0.08333333
       4    0.08333333
       5    0.05555556
       6    0.05555556
       7    0.05555556
       8    0.05555556
       9    0.05555556
       10   0.02777778      
       11   0.05555556


### This plot shows comparison of Error with K value for choosing correct K to reduce the misclassification 

ggplot(error.table,aes(kvalue,error))+geom_line(colour="blue")

![kvalue analysis](https://user-images.githubusercontent.com/43170364/53951908-8db31600-40f5-11e9-996e-e3abf64fd310.png)

 
### Using K=10 nearest neighbours for prediction 

species.prediction<- knn(train.ir,test.ir,pre_train$Species,k=10)

data.frame(pre_test$Species,species.prediction)

### Detecting the Error rate and Misclassified data in the 36 test observation 

E1<- mean(pre_test$Species!=species.prediction)

misclass_count<- 36*E1

misclass_count

    [1] 1

misclass_count/36
     
    [1] 0.02777778

### The error rate is low of value 0.027. One misclassified data out of 36 test observations found
