install.packages("klaR")

library(klaR)

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/moveis")
dados <- read.csv("Material 07 - 2 - Moveis - Dados.csv")

View(dados)

set.seed(55)

cluster.results <- kmodes(dados, 10, iter.max = 10, weighted = FALSE )

cluster.results

resultado <- cbind(dados, cluster.results$cluster)

resultado
