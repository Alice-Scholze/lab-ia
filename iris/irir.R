

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/iris")
dados <- iris
dados
View(dados)

set.seed(55)

irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)

irisCluster

resultado <- cbind(dados, irisCluster$cluster)

resultado

