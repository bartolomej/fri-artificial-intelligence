# Prenesite datoteko "insurance.txt" v lokalno mapo. 
# To mapo nastavite kot delovno mapo okolja R. To lahko naredite s pomocjo 
# ukaza "setwd" oziroma iz menuja s klikom na File -> Change dir...

data <- read.table("insurance.txt", sep=",", header=T, stringsAsFactors=T)
summary(data)


#
# Ocenjevanje ucenja z neodvisno testno mnozico
#

nrow(data)

set.seed(0)
sel <- sample(1:nrow(data), as.integer(nrow(data) * 0.7), replace=F)
train <- data[sel,]
test <- data[-sel,]

library(rpart)
library(rpart.plot)

dt <- rpart(insurance ~ ., train)
rpart.plot(dt)

observed <- test$insurance
predicted <- predict(dt, test, type="class")

q <- observed == predicted
sum(q)/length(q)


# rezultat je lahko bistveno drugacen v odvisnosti od razbitja podatkov na ucni in testni del

set.seed(100)
sel <- sample(1:nrow(data), as.integer(nrow(data) * 0.7), replace=F)
train <- data[sel,]
test <- data[-sel,]
dt <- rpart(insurance ~ ., train)
observed <- test$insurance
predicted <- predict(dt, test, type="class")
q <- observed == predni mnozici
n <- nrow(data)

# parameter postopka
k <- 10

# primere nakljucno razbijemo v k priblizno enako mocnih podmnozic
set.seed(0)
fold.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
fold.id <- fold.id[s]

fold.id[1:100]

cv.dt <- vector()
for (i in 1:k)
{	
	print(paste("Iteracija", i))
	flush.console()

	sel <- fold.id == i

	dt <- rpart(insurance ~ ., data[!sel,])
	predicted <- predict(dt, data[sel,], type= "class")
	observed <- data[sel,]$insurance
	q <- observed == predicted
	cv.dt[i] <- sum(q)/length(q)
}

cv.dt
mean(cv.dt)
sd(cv.dt)/sqrt(length(cv.dt))



# lahko proceduro izvedemo tudi tako, da za vsak rimer hranimo indikator pravilne klasifikacije

set.seed(0)
fold.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
fold.id <- fold.id[s]
q <- vector()
ft(insurance ~ ., data[!sel,])
	predicted <- predict(dt, data[sel,], type= "class")
	observed <- data$insurance[sel]
	q[sel] <- predicted == obedicted <- factor(levels=levels(data$insurance))

for (i in 1:nrow(data))
{	
	print(paste("Izlocen primer na indeksu", i))
	flush.console()

	dt== predicted
mean(q)
sd(q)/sqrt(length(q))





#
# PREGLED KLASIFIKACIJSKIH MODELOV
#



# Prenesite datoteko "players.txt" v delovno mapon nnet. Poskrbite, da so vse knjiznice instalirane.
#
# Knjiznice instaliramo z ukazom install.packages(), npr.:
#
#     instal.packages(c("CORElearn", "e1071", "randomForest", "kernlab", "nnet"))
#


players <- read.table("players.txt", sep=",", header=T, stdstranimo ucne primere z manjkajocimi vrednostmi (atribut "weight")
players <- na.omit(players)

set.seed(0)
sel <- sample(1:nrow(ple atribut "position"

# indikator razreda (potrebujemo ga za ocenjevanje verjetnostnih napovedi)
library(nnet)
obsMat <- class.ind(test$pt$position

# funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
	mean(observed == predicted)
}

# fun- predictedMatrix) ^ 2) / nrow(predictedMatrix)
}



#
#
# ODLOCITVENO DREVO
#
#

# gradnja modela s pomocjo knjiznice "rpart"

library("class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)



# do boljsega rezultat lah=train, cp=0)
rpart.plot(dt)

# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
printcp(dt)
tab <- printcp(dt)

# izberemo vrednost- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th

# porezemo drevo z izbrano nastavitvijo
dt <- prune(dt, cp=th)
rpart.plot(dt)


predicsMat, predMat)





# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
dt <- CoreModel(position ~ ., data = train, modelob")
brier.score(obsMat, predMat)





#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(redMat <- predict(nb, test, type = "raw")
brier.score(obsMat, predMat)



# gradnja modela s pomocjo knjiznice "CORElearn"

libraryed, predicted)

predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)




#
#
# K-NAJBLIZJIH SOSEDOV
#
#

# gradnN = 5)
predicted <- predict(knn, test, type="class")
CA(observed, predicted)

predMat <- predict(knn, test, type = "prob")
brier.score("kInNN" lahko uporabimo precno preverjanje na ucni mnozici

n <- nrow(train)
k <- 10

set.seed(0)
fold.id <- rep(1:k, length.out=n)
s <inkInNN <- 1
maxkInNN <- 20
est <- vector()
for (val in minkInNN:maxkInNN)
{
	print(paste("Testiram za nastavitev kInNN", val))
	flu model="knn", kInNN = val)
		pred <- predict(knn, train[sel,], type= "class")
		obs <- train$position[sel]
		q[sel] <- pred == obs
 = train, model="knn", kInNN = 18)
predicted <- predict(knn, test, type="class")
CA(observed, predicted)

predMat <- predict(knn, mForest"

library(randomForest)

rf <- randomForest(position ~ ., data = train)
predicted <- predict(rf, testobserved, predicted)

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)



# gradnja modela s pomocjo knjiznicepe="class")
CA(observed, predicted)

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)




#
#
# SVM
#
#

# grast, type="class")
CA(observed, predicted)

sm <- svm(position ~ ., train, probability = T)
pred <- predict(sm, test, probability ugacen kot 
# je v matriki obsMat. 
colnames(obsMat)
colnames(predMat)

# popravimo ga
predMat <- predMat[,colnames(obsMat)]

# Iznice "kernlab"

library(kernlab)

sm <- ksvm(position ~ ., data = train, kernel = "rbfdot")
predicted <- predict(sm, test, type = "respdict(sm, test, type = "prob")
brier.score(obsMat, predMat)



#
#
# UMETNE NEVRONSKE MREZE
#
#

# gradnja modela s pomocjo knjiznd <- predict(nn, test, type = "class")
CA(observed, predicted)



# implementacija funkcije za ucenje nevronske mreze daje boljse re vsi atributi zvezni, razen ciljne spr. "position", ki je 3. stolpec)

summary(train)
names(train)
class <- which(names(train) = "position")
class

max_train <- apply(train[,-class], 2, max)
min_train <- apply(train[,-class], 2, min)

# normaliziramo podatke
train_scaled)
train_scaled$position <- train$position

# vse vrednosti atributov v ucni mnozici so na intervalu [0,1]
summary(train_scaleale = max_train - min_train)
test_scaled <- data.frame(test_scaled)
test_scaled$position <- test$position

# v testni mnozici  bodo vse vrednosti na intervalu [0,1]!
summary(test_scaled)

set.seed(0)
nn <- nnet(position ~ ., data = train_scaled, size = , decay = 0.0001, maxit = 10000)
predicted <- predict(nn, test_scaled, type = "class")
CA(observed, predicted)

predMat <- prct(nn, test_scaled, type = "raw")
brier.score(obsMat, predMat)


#
# POZOR!!!!!
#

# v primeru binarne klasifikacije bo funkcija "insurance.txt", sep=",", header=T, stringsAsFactors=T)
set.seed(0)
sel <- sample(1:nrow(data), as.integer(nrow(data) * 0.7), rplace=F)
train <- data[sel,]
test <- data[-sel,]

summary(train)

# normaliziramo zvezne atribute
contatt <- c(8:10, 12:17)
maxtrain <- apply(train[,contatt], 2, max)
min_train <- apply(train[,contatt], 2, min)

# normaliziramo podatke
train_scaled <- scle(train[,contatt], center = min_train, scale = max_train - min_train)
train_scaled <- data.frame(train_scaled)
train_scaled <- cbinx_train - min_train)
test_scaled <- data.frame(test_scaled)
test_scaled <- cbind(test_scaled, test[,-contatt])


observed <- test_0.0001, maxit = 10000)
predicted <- predict(nn, test_scaled, type = "class")
CA(observed, predicted)

pm <- predict(nn, test_scaled, 
