# K-NN
#jyce

library(class)

data <- read.csv("c:\\temp\\ArticleML\\knn.csv", header=TRUE, sep=",")
workclass <- levels(data$workclass)

# par rapport à la distance il vaut mieux que les variables aient le même ordre de grandeur pour ne pas se faire écrabouiller
data$fnlwgtk = data$fnlwgt/10000
# workclass 

n.points = nrow(data)
sampling.rate = 0.8

# Misclassification rate => ceux qui vont dans test
num.test.set.labels <- n.points * (1-sampling.rate)
set.seed(123) # for reproducibility we fix the seed for the sampling generator
training <-sample(1:n.points, sampling.rate*n.points)
train <- subset(data[training, ], select=c(age, fnlwgtk))
testing <- setdiff(1:n.points, training)
test <- subset(data[testing, ], select=c(age, fnlwgtk))

# labels

cl <-data$workclass[training]

# labels for testing
true.labels <-data$workclass[testing]

#choisir k
for (k in 1:100) {

  predicted.labels = knn (train, test, cl, k=3)
  num.incorrect.labels<- sum(predicted.labels!=true.labels)
  misclassification.rate <- num.incorrect.labels/num.test.set.labels
  res = sprintf("%d, %f", k,misclassification.rate)
  print(res)
 
}

