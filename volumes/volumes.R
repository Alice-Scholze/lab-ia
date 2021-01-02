install.packages("caret")
install.packages("e1071")
library(caret)
install.packages("mlbench")
install.packages("mice")
library(mlbench)
library(mice)
install.packages("Metrics")
library(Metrics)


### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/volumes")
dados <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados.csv")
dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")

View(dados)

set.seed(55)

indices <- createDataPartition(dados$Volume, p=0.80, list=FALSE)

treino <- dados[indices,]

teste <- dados[-indices,]

### Treino com Hold-Out

set.seed(55)

rna <- train(Volume~., data=treino, method="nnet", linout=T, trace=FALSE)

rna

predicoes.rna <- predict(rna, teste)

### metrics
predict(rna, teste)

cor(teste$Volume, predicoes.rna, use = "everything", method = "pearson")

mae(teste$Volume, predicoes.rna)

rse(teste$Volume, predicoes.rna)

rmse(teste$Volume, predicoes.rna)

syx(teste$Volume, predicoes.rna)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.rna)


### CV e parametrizacao da RNA

control <- trainControl(method = "cv", number = 10)

set.seed(55)

rna <- train(Volume~., data=treino, method="nnet", trainControl=control, linout=T,
             MaxNWts=10000, maxit=2000, trace=F)

rna

predicoes.rna <- predict(rna, teste)

mae(teste$Volume, predicoes.rna)

rse(teste$Volume, predicoes.rna)

cor(teste$Volume, predicoes.rna, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.rna)

r2(teste$Volume, predicoes.rna)

## melhor modelo

tuneGrid <- expand.grid(size = seq(from = 1, to = 10, by = 1), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(55)

rna <- train(Volume~., data=treino, method="nnet", trainControl=control, tuneGrid=tuneGrid, linout=T,
             MaxNWts=10000, maxit=2000, trace=F)

rna


tuneGrid <- expand.grid(size = 4, decay = 0.1)

set.seed(55)

rna <- train(Volume~., data=treino, method="nnet", trainControl=control, tuneGrid=tuneGrid, linout=T,
             MaxNWts=10000, maxit=2000, trace=F)

rna


predicoes.rna <- predict(rna, teste)

predicoes.rna 

plot(teste$Volume, predicoes.rna, 
     ylab="Volume") 
  abline(350, 350)                  # the horizon



mae(teste$Volume, predicoes.rna)

rse(teste$Volume, predicoes.rna)

cor(teste$Volume, predicoes.rna, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.rna)

r2(teste$Volume, predicoes.rna)


### PREDIÇÕES DE NOVOS CASOS

dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")

# View(dados_novos_casos)

dados_novos_casos$Volume <- NULL

predict.rna <- predict(rna, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.rna)

View(resultado)


##

### KNN

### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/volumes")
dados <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados.csv")

set.seed(55)

indices <- createDataPartition(dados$Volume, p=0.80, list=FALSE)

treino <- dados[indices,]

teste <- dados[-indices,]

### Treino com Hold-Out

set.seed(55)

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(55)

knn <- train(Volume ~ ., data = treino, method = "knn",
             
             tuneGrid=tuneGrid)

knn

### Aplica o modelo no arquivo de teste

predict.knn <- predict(knn, teste)

### Mostra as métricas

library(Metrics)

mae(teste$Volume, predict.knn)

rse(teste$Volume, predict.knn)

cor(teste$Volume, predict.knn, use = "everything", method = "pearson")

rmse(teste$Volume, predict.knn)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predict.knn)

### PREDIÇÕES DE NOVOS CASOS

dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")

predict.knn <- predict(knn, dados_novos_casos)

dados_novos_casos$Volume <- NULL

resultado <- cbind(dados_novos_casos, predict.knn)

View(resultado)


##

### SVM

### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/volumes")
dados <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados.csv")

set.seed(55)

indices <- createDataPartition(dados$Volume, p=0.80, list=FALSE)

treino <- dados[indices,]

teste <- dados[-indices,]

### Treino com Hold-Out
set.seed(55)

svm <- train(Volume~., data=treino, method="svmRadial")

svm
### 6. Aplicar modelos treinados na base de Teste

predicoes.svm <- predict(svm, teste)

### Calcular as métricas

mae(teste$Volume, predicoes.svm)

rse(teste$Volume, predicoes.svm)

cor(teste$Volume, predicoes.svm, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.svm)

#### Cross-validation SVM

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

svm <- train(Volume~., data=treino, method="svmRadial", trControl=ctrl)

svm

predicoes.svm <- predict(svm, teste)

mae(teste$Volume, predicoes.svm)

rse(teste$Volume, predicoes.svm)

cor(teste$Volume, predicoes.svm, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.svm)

#### Vários C e sigma

tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(55)

svm <- train(Volume~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm

tuneGrid = expand.grid(C=c(50), sigma=c(0.01))

set.seed(55)

svm <- train(Volume~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm
### 6. Aplicar modelos treinados na base de Teste

predicoes.svm <- predict(svm, teste)

### Calcular as métricas

mae(teste$Volume, predicoes.svm)

rse(teste$Volume, predicoes.svm)

cor(teste$Volume, predicoes.svm, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.svm)

### PREDIÇÕES DE NOVOS CASOS

dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")

dados_novos_casos$Volume <- NULL

predict.svm <- predict(svm, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.svm)

View(resultado)

##

### RF

### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/volumes")
dados <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados.csv")

set.seed(55)

indices <- createDataPartition(dados$Volume, p=0.80, list=FALSE)

treino <- dados[indices,]

teste <- dados[-indices,]

### Treino com Hold-Out
set.seed(55)

rf <- train(Volume~., data=treino, method="rf")

rf
### 6. Aplicar modelos treinados na base de Teste

predicoes.rf <- predict(rf, teste)

### Calcular as métricas

library(Metrics)

mae(teste$Volume, predicoes.rf)

rse(teste$Volume, predicoes.rf)

cor(teste$Volume, predicoes.rf, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.rf)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.rf)

#### Cross-validation RF

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

rf <- train(Volume~., data=treino, method="rf", trControl=ctrl)

rf

predicoes.rf <- predict(rf, teste)

### Calcular as métricas
mae(teste$Volume, predicoes.rf)

rse(teste$Volume, predicoes.rf)

cor(teste$Volume, predicoes.rf, use = "everything", method = "pearson")

rmse(teste$Volume, predicoes.rf)

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.rf)

#### Vários mtry
ctrl <- trainControl(method = "cv", number = 10)

tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))

set.seed(55)

rf <- train(Volume~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

tuneGrid = expand.grid(mtry=c(9))

set.seed(55)

rf <- train(Volume~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

predicoes.rf <- predict(rf, teste)

summary(rf)


### Calcular as métricas

rmse(teste$Volume, predicoes.rf)

mae(teste$Volume, predicoes.rf)

rse(teste$Volume, predicoes.rf)

cor(teste$Volume, predicoes.rf, use = "everything", method = "pearson")

r2 <- function(predito, observado) {
  
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
  
}

r2(teste$Volume, predicoes.rf)

### PREDIÇÕES DE NOVOS CASOS

dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")

dados_novos_casos$Volume <- NULL

predict.rf <- predict(rf, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.rf)

View(resultado)
