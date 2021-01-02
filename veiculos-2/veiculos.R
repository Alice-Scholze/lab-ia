
library(klaR)

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/veiculos-2")
temp_dados <- read.csv("Material 02 - 5 - C - Veiculos - Dados.csv")

temp_dados$a <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

dados[,0:18]

set.seed(55)

cluster.results <- kmeans(dados, 5, nstart = 20)

cluster.results <- kmodes(dados[,0:18], 30, iter.max = 30, weighted = FALSE )

cluster.results

resultado <- cbind(dados, cluster.results$cluster)

resultado
