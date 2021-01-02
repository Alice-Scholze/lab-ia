install.packages("caret")
install.packages("e1071")
library(caret)
install.packages("mlbench")
install.packages("mice")
library(mlbench)
library(mice)

### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/imposto")
dados <- read.csv("Material 02 - 7 – C - IR - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$sonegador, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)
rna <- train(sonegador~., data=treino, method="nnet",trace=FALSE)
rna

### Predições dos valores do conjunto de teste
predict.rna <- predict(rna, teste)

### Matriz de confusão
confusionMatrix(predict.rna, as.factor(teste$sonegador))


# Usando Cross-validation

### indica o método cv e numero de folders 10

ctrl <- trainControl(method = "cv", number = 10)

### executa a RNA com esse ctrl

set.seed(55)

rna <- train(sonegador~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$sonegador))

rna


# Parametrização da RNA

### size, decay

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10),decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(55)

rna <- train(
  
  form = sonegador~. ,
  
  data = treino ,
  
  method = "nnet" ,
  
  tuneGrid = grid ,
  
  trControl = ctrl ,
  
  maxit = 2000,trace=FALSE)

rna

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$sonegador))



grid <- expand.grid(size = 21,decay = 0.1)

set.seed(55)

rna <- train(
  
  form = sonegador~. ,
  
  data = treino ,
  
  method = "nnet" ,
  
  tuneGrid = grid ,
  
  trControl = ctrl ,
  
  maxit = 2000,trace=FALSE)

rna

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$sonegador))
### PREDIÇÕES DE NOVOS CASOS

dados_novos_casos <- read.csv("Material 02 - 7 – C - IR - Dados - Novos Casos.csv")

predict.rna <- predict(rna, dados_novos_casos)

dados_novos_casos$sonegador <- NULL

resultado <- cbind(dados_novos_casos, predict.rna)

View(resultado)

###

###

### KNN
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/imposto")
temp_dados <- read.csv("Material 02 - 7 – C - IR - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$sonegador, p=0.80,list=FALSE)

treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(55)

knn <- train(sonegador ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)

knn

predict.knn <- predict(knn, teste)

confusionMatrix(predict.knn, as.factor(teste$sonegador))
###

### SVM

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/imposto")
temp_dados <- read.csv("Material 02 - 7 – C - IR - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$sonegador, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

svm <- train(sonegador~., data=treino, method="svmRadial")

svm

predicoes.svm <- predict(svm, teste)

confusionMatrix(predicoes.svm, as.factor(teste$sonegador))

#### Cross-validation SVM

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

svm <- train(sonegador~., data=treino, method="svmRadial", trControl=ctrl)

svm

predicoes.svm <- predict(svm, teste)

confusionMatrix(predicoes.svm, as.factor(teste$sonegador))

#### Vários C e sigma

tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(55)

svm <- train(sonegador~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm


tuneGrid = expand.grid(C=c(1), sigma=c(0.2))

set.seed(55)

svm <- train(sonegador~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm

predict.svm <- predict(svm, teste)

confusionMatrix(predict.svm, as.factor(teste$sonegador))

## Novos casos

dados_novos_casos <- read.csv("Material 02 - 7 – C - IR - Dados - Novos Casos.csv")

predict.svm <- predict(svm, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.svm)

resultado$sonegador <- NULL

View(resultado)



###

### RF
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/imposto")
temp_dados <- read.csv("Material 02 - 7 – C - IR - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$sonegador, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

rf <- train(sonegador~., data=treino, method="rf")

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$sonegador))

#### Cross-validation

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

rf <- train(sonegador~., data=treino, method="rf", trControl=ctrl)

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$sonegador))

#### Vários mtry

tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))

set.seed(55)

rf <- train(sonegador~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

tuneGrid = expand.grid(mtry=c(5))

set.seed(55)

rf <- train(sonegador~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$sonegador))


## Novos casos

dados_novos_casos <- read.csv("Material 02 - 7 – C - IR - Dados - Novos Casos.csv")

predict.rf <- predict(rf, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.rf)

resultado$sonegador <- NULL

View(resultado)
