library(ModelMetrics)
library(ggplot2)
library(caret)
library(dummies)
library(GGally)

porcentaje<-0.7
set.seed(123)

#Calculo de percentiles
data<-read.csv('train.csv')

data[is.na(data)] <- 0

View(data)
percentil <- quantile(data$SalePrice)


#Percentiles
estado<-c('Estado')
data$Estado<-estado


#Economica=0
#Intermedia=1
#Cara=2
data <- within(data, Estado[SalePrice<=129975] <- 0)

data$Estado[(data$SalePrice>129975 & data$SalePrice<=163000)] <- 1
data$Estado[data$SalePrice>163000] <- 2


#Modelo de Regresi?n log?stica
porcentaje<-0.7
datos<-data
set.seed(123)
#Variables dicotomicas
datos<-cbind(datos,dummy(data$Estado,verbose = T))
ncol(datos)

names (datos)[85] = "Cara"
names (datos)[84] = "Intermedia"
names (datos)[83] = "Economica"


corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

#Queremos saber si una casa es cara



modelo<-glm(Cara~., data = train[,c('SalePrice','GrLivArea','Cara','LotFrontage','LotArea','PoolArea')],family = binomial(), maxit=100)

#-------------------------------------------------
# Regresi?n Logistica 
#-------------------------------------------------

#Corelacion de las variables

ggpairs(datos[,c('SalePrice','GrLivArea','LotFrontage','LotArea','PoolArea')])

##Modelo con todas las variables
pred<-predict(modelo,newdata = test[,c('SalePrice','GrLivArea','LotFrontage','LotArea','PoolArea')], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$Cara),as.factor(prediccion))

#Modelo para verificar overfitting

trainPredict<-predict(modelo,newdata = train[,c('SalePrice','GrLivArea','LotFrontage','LotArea','PoolArea')], type = "response")
trainPred<- ifelse(trainPredict>0.5,1,0)
confusionMatrix(as.factor(train$Cara),as.factor(trainPred))

#Calculo de rmse para ver si tenemos overfittin, mientras mas cercano a 0 mayor overffiting.
rmse(train$Cara,trainPred)
rmse(test$Cara,prediccion)

train_numerico<-train[,c('SalePrice','GrLivArea','Cara','LotFrontage','LotArea','PoolArea')]

modeloCaret<-train(Cara~.,trControl=trainControl('none'),
                   train_numerico,
                   method='glm',family='binomial')

varImp(modeloCaret)
