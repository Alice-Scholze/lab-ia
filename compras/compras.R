library(arules)

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/compras")
dados <- read.csv("Material 08 – 1 - Lista de Compras - Dados.csv")

View(dados)

dados <- read.transactions(file="Material 08 – 1 - Lista de Compras - Dados.csv",format="basket",sep=";")

inspect(dados[1:4])

set.seed(55)

rules <- apriori(dados, parameter = list(supp = 0.1, conf = 0.3, target = "rules"))

inspect(rules)

rules <- apriori(dados, parameter = list(supp = 0.3, conf = 0.6, target = "rules"))

inspect(rules)

rules <- apriori(dados, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))

inspect(rules)
