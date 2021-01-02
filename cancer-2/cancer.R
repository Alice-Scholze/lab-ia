
library(klaR)

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/cancer-2")
temp_dados <- read.csv("Material 02 - 2 - Cancer de Mama - Dados.csv")
# dados <- read.csv("Material 02 - 2 - Cancer de Mama - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
temp_dados$Id <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

dados[,0:10]

set.seed(55)

cluster.results <- kmeans(dados[,0:9], 3, nstart = 20)
cluster.results <- kmodes(dados[,0:9], 10, iter.max = 10, weighted = FALSE )

cluster.results

resultado <- cbind(dados, cluster.results$cluster)

resultado
