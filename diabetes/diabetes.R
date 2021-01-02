install.packages("caret")
install.packages("e1071")
library(caret)
install.packages("mlbench")
install.packages("mice")
library(mlbench)
library(mice)

### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/diabetes")
temp_dados <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
temp_dados$Num <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$diabetes, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)
rna <- train(diabetes~., data=treino, method="nnet",trace=FALSE)
rna

### Predições dos valores do conjunto de teste
predict.rna <- predict(rna, teste)

### Matriz de confusão
confusionMatrix(predict.rna, as.factor(teste$diabetes))


# Usando Cross-validation

### indica o método cv e numero de folders 10

ctrl <- trainControl(method = "cv", number = 10)

### executa a RNA com esse ctrl

set.seed(55)

rna <- train(diabetes~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$diabetes))

rna


# Parametrização da RNA

### size, decay

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10),decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(55)

rna <- train(
  
  form = diabetes~. ,
  
  data = treino ,
  
  method = "nnet" ,
  
  tuneGrid = grid ,
  
  trControl = ctrl ,
  
  maxit = 2000,trace=FALSE)

rna

grid <- expand.grid(size = 1,decay = 0.7)

set.seed(55)

rna <- train(
  
  form = diabetes~. ,
  
  data = treino ,
  
  method = "nnet" ,
  
  tuneGrid = grid ,
  
  trControl = ctrl ,
  
  maxit = 2000,trace=FALSE)

rna

predict.rna <- predict(rna, teste)

confusionMatrix(predict.rna, as.factor(teste$diabetes))


### PREDIÇÕES DE NOVOS CASOS

df <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")
dados_novos_casos <- df[1:3,]
dados_novos_casos$Num <- NULL
dados_novos_casos[1,]$preg0nt <- 2
dados_novos_casos[2,]$glucose <- 103
dados_novos_casos[3,]$pressure <- 72
dados_novos_casos[3,]$triceps <- 39
dados_novos_casos[1,]$insulin <- 202
dados_novos_casos[2,]$mass <- 25.5
dados_novos_casos[1,]$pedigree <- 0.567
dados_novos_casos[2,]$age <- 42
dados_novos_casos$diabetes <- NULL

predict.rna <- predict(rna, dados_novos_casos)

dados_novos_casos$diabetes <- NULL

resultado <- cbind(dados_novos_casos, predict.rna)

View(resultado)

###

###

### KNN
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/diabetes")
temp_dados <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
temp_dados$Num <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$diabetes, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(55)

knn <- train(diabetes ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)

knn

predict.knn <- predict(knn, teste)

confusionMatrix(predict.knn, as.factor(teste$diabetes))
###

### SVM

setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/diabetes")
temp_dados <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
temp_dados$Num <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$diabetes, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

svm <- train(diabetes~., data=treino, method="svmRadial")

svm

predicoes.svm <- predict(svm, teste)

confusionMatrix(predicoes.svm, as.factor(teste$diabetes))

#### Cross-validation SVM

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

svm <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl)

svm

predicoes.svm <- predict(svm, teste)

confusionMatrix(predicoes.svm, as.factor(teste$diabetes))

#### Vários C e sigma

tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(55)

svm <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm


tuneGrid = expand.grid(C=c(2), sigma=c(0.015))

set.seed(55)

svm <- train(diabetes~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)

svm

predict.svm <- predict(svm, teste)

confusionMatrix(predict.svm, as.factor(teste$diabetes))

## Novos casos
df <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")
dados_novos_casos <- df[1:3,]
dados_novos_casos$Num <- NULL
dados_novos_casos[1,]$preg0nt <- 2
dados_novos_casos[2,]$glucose <- 103
dados_novos_casos[3,]$pressure <- 72
dados_novos_casos[3,]$triceps <- 39
dados_novos_casos[1,]$insulin <- 202
dados_novos_casos[2,]$mass <- 25.5
dados_novos_casos[1,]$pedigree <- 0.567
dados_novos_casos[2,]$age <- 42
dados_novos_casos$diabetes <- NULL

predict.svm <- predict(svm, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.svm)

resultado$diabetes <- NULL

View(resultado)



###

### RF
### Obter os dados
setwd("/home/alice/Documents/Projects/studies/IAA/lab-ia/diabetes")
temp_dados <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")

View(temp_dados)

### Tratar o Id e Missing Values
temp_dados$Num <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

### Criar bases de Treino e Teste
set.seed(55)

indices <- createDataPartition(dados$diabetes, p=0.80,list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Treinar o modelo com Hold-out
set.seed(55)

rf <- train(diabetes~., data=treino, method="rf")

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$diabetes))

#### Cross-validation

ctrl <- trainControl(method = "cv", number = 10)

set.seed(55)

rf <- train(diabetes~., data=treino, method="rf", trControl=ctrl)

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$diabetes))

#### Vários mtry

tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))

set.seed(55)

rf <- train(diabetes~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

tuneGrid = expand.grid(mtry=c(5))

set.seed(55)

rf <- train(diabetes~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)

rf

predict.rf <- predict(rf, teste)

confusionMatrix(predict.rf, as.factor(teste$diabetes))


## Novos casos

df <- read.csv("Material 02 - 9 – C - Diabetes - Dados.csv")
dados_novos_casos <- df[1:3,]
dados_novos_casos$Num <- NULL
dados_novos_casos[1,]$preg0nt <- 2
dados_novos_casos[2,]$glucose <- 103
dados_novos_casos[3,]$pressure <- 72
dados_novos_casos[3,]$triceps <- 39
dados_novos_casos[1,]$insulin <- 202
dados_novos_casos[2,]$mass <- 25.5
dados_novos_casos[1,]$pedigree <- 0.567
dados_novos_casos[2,]$age <- 42
dados_novos_casos$diabetes <- NULL

View(dados_novos_casos)

predict.rf <- predict(rf, dados_novos_casos)

resultado <- cbind(dados_novos_casos, predict.rf)

resultado$diabetes <- NULL

View(resultado)
