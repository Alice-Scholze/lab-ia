install.packages("caret")
install.packages("e1071")
library(caret)
install.packages("mlbench")
install.packages("mice")
library(mlbench)
library(mice)

### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/previsao-tempo")
dados <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")

View(dados)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$Chovera, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)
rna <- train(Chovera~., data=treino, method="nnet",trace=FALSE)
rna

### Predições dos valores do conjunto de teste
predict.rna <- predict(rna, teste)

### Matriz de confusão
confusionMatrix(predict.rna, as.factor(teste$Chovera))


# Usando Cross-validation

### indica o método cv e numero de folders 10

ctrl <- trainControl(method = "cv", number = 10)

### executa a RNA com esse ctrl

set.seed(55)

rna <- train(Chovera~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$Chovera))

rna


# Parametrização da RNA

### size, decay

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10),decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(55)

rna <- train(
  
  form = Chovera~. ,
  
  data = treino ,
  
  method = "nnet" ,
  
  tuneGrid = grid ,
  
  trControl = ctrl ,
  
  maxit = 2000,trace=FALSE)

rna

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$Chovera))



grid <- expand.grid(size = 1,decay = 0.1)

set.seed(55)

rna <- train(
  
  form = Chovera~. ,
  
  data = treino ,
  
  method = "nnet" ,
  
  tuneGrid = grid ,
  
  trControl = ctrl ,
  
  maxit = 2000,trace=FALSE)

rna

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$Chovera))

### PREDIÇÕES DE NOVOS CASOS

df <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")
dados_novos_casos <- df[1:3,]
dados_novos_casos$Chovera <- NULL
dados_novos_casos[1,]$Temperatura <- 'Baixa'
dados_novos_casos[2,]$Temperatura <- 'Media'
dados_novos_casos[2,]$Ceu <- 'Chuvoso'
dados_novos_casos[3,]$Ceu <- 'Ensolarado'
dados_novos_casos[1,]$Umidade <- 'Normal'
dados_novos_casos[3,]$Umidade <- 'Ensolarado'

View(dados_novos_casos)

predict.rna <- predict(rna, dados_novos_casos)

dados_novos_casos$Chovera <- NULL

resultado <- cbind(dados_novos_casos, predict.rna)

View(resultado)

###

###

### KNN
### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/previsao-tempo")
dados <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")


### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$Chovera, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(55)

knn <- train(Chovera ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)

knn

predict.knn <- predict(knn, teste)

confusionMatrix(predict.knn, as.factor(teste$Chovera))
###

### SVM

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/previsao-tempo")
dados <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")


### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$Chovera, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)
indices <- createDataPartition(dados$Chovera, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

svm <- train(Chovera~., data=treino, method="svmRadial")

svm

predicoes.svm <- predict(svm, teste)

confusionMatrix(predicoes.svm, as.factor(teste$Chovera))

#### Cross-validation SVM

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

svm <- train(Chovera~., data=treino, method="svmRadial", trControl=ctrl)

svm

predicoes.svm <- predict(svm, teste)

confusionMatrix(predicoes.svm, as.factor(teste$Chovera))

#### Vários C e sigma

tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(55)

svm <- train(Chovera~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm


tuneGrid = expand.grid(C=c(1), sigma=c(0.2))

set.seed(55)

svm <- train(Chovera~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm

predict.svm <- predict(svm, teste)

confusionMatrix(predict.svm, as.factor(teste$Chovera))

## Novos casos

dados <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")
dados_novos_casos <- df[1:3,]
dados_novos_casos$Chovera <- NULL
dados_novos_casos[1,]$Temperatura <- 'Baixa'
dados_novos_casos[2,]$Temperatura <- 'Media'
dados_novos_casos[2,]$Ceu <- 'Chuvoso'
dados_novos_casos[3,]$Ceu <- 'Ensolarado'
dados_novos_casos[1,]$Umidade <- 'Normal'

View(dados_novos_casos)

predict.svm <- predict(svm, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.svm)

dados_novos_casos$Chovera <- NULL

View(resultado)



###

### RF
### Obter os dados
### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/previsao-tempo")
dados <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")

View(dados)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$Chovera, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

rf <- train(Chovera~., data=treino, method="rf")

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$Chovera))

#### Cross-validation

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

rf <- train(Chovera~., data=treino, method="rf", trControl=ctrl)

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$Chovera))

#### Vários mtry

tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))

set.seed(55)

rf <- train(Chovera~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

tuneGrid = expand.grid(mtry=c(5))

set.seed(55)

rf <- train(Chovera~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$Chovera))


## Novos casos
df <- read.csv("Material 02 - 6 - C - Previsao do Tempo - Dados.csv")
dados_novos_casos <- df[1:3,]
dados_novos_casos$Chovera <- NULL
dados_novos_casos[1,]$Temperatura <- 'Baixa'
dados_novos_casos[2,]$Temperatura <- 'Media'
dados_novos_casos[2,]$Ceu <- 'Chuvoso'
dados_novos_casos[3,]$Ceu <- 'Ensolarado'
dados_novos_casos[1,]$Umidade <- 'Normal'

View(dados_novos_casos)

predict.rf <- predict(rf, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.rf)

resultado$Chovera <- NULL

View(resultado)
