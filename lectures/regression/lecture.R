# Prenesite datoteko "algae.txt" v lokalno mapo. To mapo nastavite kot delovno
# mapo okolja R. To lahko naredite s pomocjo ukaza "setwd" oziroma iz menuja 
# s klikom na File -> Change dir...
# 
# na primer:
# setwd("c:\\vaje\\data\\")

#####################################################################
#
# OCENJEVANJE UCENJA V REGRESIJI
#
#####################################################################

data <- read.csv("algae.txt", stringsAsFactors=T)
summary(data)

nrow(data)

# nakljucno razdelimo podatke na ucno in testni mnozico v razmerju 70:30

set.seed(0)
sel <- sample(1:nrow(data), as.integer(nrow(data) * 0.7), F)
train <- data[sel,]
test <- data[-sel,]


# linearna regresija
model <- lm(algae ~ ., train)
model

predicted <- predict(model, test)

observed <- test$algae

plot(observed)
points(predicted, col="red")



#
#
# Mere za ocenjevanje ucenja v regresiji
#
#

# srednja absolutna napaka
mae <- function(obs, pred)
{
	mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred)
{
	mean((obs - pred)^2)
}

mae(observed, predicted)
mse(observed, predicted)


#
# Trivialni model vedno napove povprecno vrednost ciljne spremenljivke
#

meanVal <- mean(train$algae)
meanVal

predTrivial <- rep(meanVal, nrow(test))

mae(observed, predTrivial)
mse(observed, predTrivial)



# Relativne mere ocenjujejo model v primerjavi s trivialno predikcijo

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{  
	sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{  
	sum((obs - pred)^2)/sum((obs - mean.val)^2)
}


rmae(observed, predicted, mean(train$algae))
rmse(observed, predicted, mean(train$algae))



#
# Precno preverjanje po principu izpusti enega (leave one out)
#

predicted <- vector()

for (i in 1:nrow(data))
{	
	# print(paste("Izlocen primer na indeksu", i))
	# flush.console()

	model <- lm(algae ~ ., data[-i,])
	predicted[i] <- predict(model, data[i,])
}

plot(data$algae)
points(predicted, col="red")

mae(data$algae, predicted)
mse(data$algae, predicted)

rmae(data$algae, predicted, mean(data$algae))
rmse(data$algae, predicted, mean(data$algae))


#
# Dobljene rezultate je mozno izboljsati z ustrezno predpripravo podatkov
#

# poglejmo porazdelitve vrednosti posameznih atributov

# nekateri atributi imajo priblizno normalno porazdelitev
hist(data$mxPH)
hist(data$mnO2)

# drugi nimajo niti priblizno normalne porazdelitve...
hist(data$Cl)

# z logaritmiranjem jih skusamo transformirati v normalne
hist(log(data$Cl))

# tezave nastanejo, ce je vsaj ena izmed vrednosti atributa enaka 0
summary(data$NO3)
summary(log(data$NO3))

# zaradi tega atribute transformiramo s funkcijo log1p(x), ki izracuna log(1+x)

# v izvirni obliki obdrzimo kategoricne atribute in tiste zvezne, ki imajo priblizno normalno porazdelitev.
# ostale pa transformiramo.

dataTrans <- data[,c("season", "size", "speed", "mxPH", "mnO2")]
dataTrans$logCl <- log1p(data$Cl)
dataTrans$logNO3 <- log1p(data$NO3)
dataTrans$logNH4 <- log1p(data$NH4)
dataTrans$logoPO4 <- log1p(data$oPO4)
dataTrans$logPO4 <- log1p(data$PO4)
dataTrans$logChla <- log1p(data$Chla)
dataTrans$logAlgae <- log1p(data$algae)
summary(dataTrans)

# sedaj izvedemo precno preverjanje po principu izpusti enega
logPredicted <- vector()

for (i in 1:nrow(dataTrans))
{	
	# print(paste("Izlocen primer na indeksu", i))
	# flush.console()

	model <- lm(logAlgae ~ ., dataTrans[-i,])
	logPredicted[i] <- predict(model, dataTrans[i,])
}

# dobljene napovedi je potrebno transformirati v originalni prostor
# uporabimo metodo expm1(x), ki izracuna exp(x)-1

predicted <- expm1(logPredicted)

plot(data$algae)
points(predicted, col="red")

mae(data$algae, predicted)
mse(data$algae, predicted)

rmae(data$algae, predicted, mean(data$algae))
rmse(data$algae, predicted, mean(data$algae))



#####################################################################
#
# PREGLED OSTALIH REGRESIJSKIH MODELOV
#
#####################################################################

wine <- read.table("winequality.txt", sep = ",", header = T)
summary(wine)

nrow(wine)

# nakljucno razdelimo primere na ucno in testni mnozico
set.seed(0)
sel <- sample(1:nrow(wine), as.integer(nrow(wine) * 0.7), F)
train <- wine[sel,]
test <- wine[-sel,]



#
# regresijsko drevo
#

library(rpart)
library(rpart.plot)

rt.model <- rpart(quality ~ ., data=train)
rpart.plot(rt.model)
predicted <- predict(rt.model, test)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))

# do boljsega rezultat lahko pridemo z usreznim rezanjem drevesa

# najprej zgradimo veliko drevo (nastavitev cp=0)
rt.model <- rpart(quality ~ ., data=train, cp=0)
rpart.plot(rt.model)

# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
tab <- printcp(rt.model)

# izberemo vrednost parametra cp, ki ustreza minimalni napaki internega presnega preverjanja
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th

# porezemo drevo z izbrano nastavitvijo
rt.model <- prune(rt.model, cp=th)
rpart.plot(rt.model)

predicted <- predict(rt.model, test)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))



#
# nakljucni gozd
#

library(randomForest)

rf.model <- randomForest(quality ~ ., train)
predicted <- predict(rf.model, test)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))



#
# svm
#

library(e1071)

svm.model <- svm(quality ~ ., train)
predicted <- predict(svm.model, test)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))



#
# k-najblizjih sosedov
#

library(kknn)

knn.model <- kknn(quality ~ ., train, test, k = 5)
predicted <- fitted(knn.model)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))




#
# nevronska mreža
#

library(nnet)

#
# pomembno!!! 
# za regresijo je potrebno nastaviti linout = T

# zaradi nakljucne izbire zacetnih utezi bo vsakic nekoliko drugacen rezultat
# ce zelimo ponovljiv model, nastavimo generator nakljucnih stevil

set.seed(0)
nn.model <- nnet(quality ~ ., train, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, test)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))


# pri ucenju nevronskih mrez je priporocljivo normalizirati ucne podatke

# zvezne atribute normaliziramo na interval [0,1]:

min_vals <- apply(train[,1:11], 2, min)
max_vals <- apply(train[,1:11], 2, max)

normTrain <- as.data.frame(scale(train[,1:11], center = min_vals, scale = max_vals - min_vals))
normTrain$quality <- train$quality


# na enak nacin (z istimi mejnimi vrednostmi!) normaliziramo tudi testno mnozico
normTest <- as.data.frame(scale(test[,1:11], center = min_vals, scale = max_vals - min_vals))
normTest$quality <- test$quality

set.seed(0)
nn.model <- nnet(quality ~ ., normTrain, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, normTest)
mae(test$quality, predicted)
rmae(test$quality, predicted, mean(train$quality))



#####################################################################
#
# OCENJEVANJE ATRIBUTOV
#
#####################################################################


student <- read.table("student.txt", sep=",", header=T, stringsAsFactors=T)
summary(student)

set.seed(0)
sel <- sample(1:nrow(student), as.integer(nrow(student) * 0.7), F)
train <- student[sel,]
test <- student[-sel,]


library(kknn)
modelFull <- train.kknn(G3 ~ ., train, ks=5)
predicted <- predict(modelFull, test)
rmse(test$G3, predicted, mean(train$G3))




#
# Izbira podmnozice atributov s filter metodo
#



# atribute ocenimo z neko mero

library(CORElearn)
sort(attrEval(G3 ~ ., train, "MSEofMean"), decreasing = TRUE)
sort(attrEval(G3 ~ ., train, "RReliefFexpRank"), decreasing = TRUE)


# model zgradimo s pomocjo nekaj najbolje ocenjenih atributov

modelReduced <- train.kknn(G3 ~ G2 + G1, train, ks=5)
predicted <- predict(modelReduced, test)
rmse(test$G3, predicted, mean(train$G3))


#
# Izbira podmnozice atributov z metodo ovojnice (wrapper)
#

source("wrapper.R")


# Funkcija za ucenje modela

myTrainFuncReg <- function(formula, traindata)
{
	train.kknn(formula, traindata, ks=5)
}


# Funkcija za pridobivanje napovedi modela

myPredictFuncReg <- function(model, testdata)
{
	predict(model, testdata)
}


# Funkcija za ocenjevanje kvalitete modela (v tem primeru RMSE)

myEvalFuncRMSE <- function(predicted, observed, trained)
{
	sum((observed - predicted)^2)/sum((observed - mean(trained))^2)	
}

set.seed(0)
wrapper(G3 ~ ., train, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=10)

# testirajmo na neodvisni testni mnozici
modelWrap <- train.kknn(G3 ~ G2 + absences, train, ks=5)
predicted <- predict(modelWrap, test)
rmse(test$G3, predicted, mean(train$G3))
