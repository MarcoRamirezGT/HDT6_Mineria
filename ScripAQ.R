library(ModelMetrics)
library(ggplot2)
library(caret)
library(dummies)
library(GGally)

data<-read.csv('train.csv')


data[is.na(data)] <- 0
#Calculo de percentiles
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
#Modelo de Regresion logistica
porcentaje<-0.7
datos<-data
set.seed(123)
#Variables dicotomicas
datos<-cbind(datos,dummy(data$Estado,verbose = T))
names (datos)[85] = "Cara"
names (datos)[84] = "Intermedia"
names (datos)[83] = "Economica"

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

#Correlacion de varaibles 
# 1 Area de sotano total
# 2 Masonry veneer area in square feet
# 3 A;o de remodelacion
# 4 Above grade (ground) living area square feet
# 5 Wood deck area in square feet
ggpairs(datos[,c('TotalBsmtSF','MasVnrArea','YearRemodAdd','GrLivArea','WoodDeckSF')])

#Queremos saber si una casa es economica o no
modelo<-glm(Economica~., data = train[,c('TotalBsmtSF','MasVnrArea','Economica','YearRemodAdd','GrLivArea','WoodDeckSF')],family = binomial(), maxit=100)


# -------------------------------------------------
# Regresi?n Logistica 
#-------------------------------------------------


##Modelo con todas las variables
pred<-predict(modelo,newdata = test[,c('TotalBsmtSF','MasVnrArea','YearRemodAdd','GrLivArea','WoodDeckSF')], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$Economica),as.factor(prediccion))

#Modelo para verificar overfitting

trainPredict<-predict(modelo,newdata = train[,c('TotalBsmtSF','MasVnrArea','YearRemodAdd','GrLivArea','WoodDeckSF')], type = "response")
trainPred<- ifelse(trainPredict>0.5,1,0)
confusionMatrix(as.factor(train$Economica),as.factor(trainPred))

#Calculo de rmse para ver si tenemos overfittin, mientras mas cercano a 0 mayor overffiting.
rmse(train$Economica,trainPred)
rmse(test$Economica,prediccion)

train_numerico<-train[,c('TotalBsmtSF','MasVnrArea', 'Economica','YearRemodAdd','GrLivArea','WoodDeckSF')]

modeloCaret<-train(Economica~.,trControl=trainControl('none'),
                   train_numerico,
                   method='glm',family='binomial')

varImp(modeloCaret)
