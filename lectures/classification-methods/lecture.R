#
# PREGLED KLASIFIKACIJSKIH MODELOV
#



# Prenesite datoteko "players.txt" v delovno mapo okolja R. 
#
# Uporabljali bomo funkcije iz naslednjih knjiznic: rpart, CORElearn, 
# e1071, randomForest, kernlab in nnet. Poskrbite, da so vse knjiznice instalirane.
#
# Knjiznice instaliramo z ukazom install.packages(), npr.:
#
#     install.packages(c("CORElearn", "e1071", "randomForest", "kernlab", "nnet"))
#


players <- read.table("players.txt", sep=",", header=T, stringsAsFactors=T)
summary(players)

# odstranimo atribut "id", ki ne omogoca generalizacije (ucenja)
players$id <- NULL

# odstranimo ucne primere z manjkajocimi vrednostmi (atribut "weight")
players <- na.omit(players)

set.seed(0)
sel <- sample(1:nrow(players), as.integer(nrow(players) * 0.7), replace=F)
train <- players[sel,]
test <- players[-sel,]


# ciljna spremenljivka je atribut "position"

# indikator razreda (potrebujemo ga za ocenjevanje verjetnostnih napovedi)
library(nnet)
obsMat <- class.ind(test$position)

# pravilni razredi testnih primerov (potrebujemo jih za ocenjevanje klasifikacijske tocnosti)
observed <- test$position

# funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
	mean(observed == predicted)
}

# funkcija za izracun povprecne Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}



#
#
# ODLOCITVENO DREVO
#
#

# gradnja modela s pomocjo knjiznice "rpart"

library(rpart)
dt <- rpart(position ~ ., data = train)

library(rpart.plot)
rpart.plot(dt)

predicted <- predict(dt, test, type="class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)



# do boljsega rezultat lahko pridemo z usreznim rezanjem drevesa

# najprej zgradimo veliko drevo (nastavitev cp=0)
dt <- rpart(position ~ ., data=train, cp=0)
rpart.plot(dt)

# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
printcp(dt)
tab <- printcp(dt)

# izberemo vrednost parametra cp, ki ustreza minimalni napaki internega precnega preverjanja
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th

# porezemo drevo z izbrano nastavitvijo
dt <- prune(dt, cp=th)
rpart.plot(dt)


predicted <- predict(dt, test, type="class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)





# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
dt <- CoreModel(position ~ ., data = train, model="tree")
predicted <- predict(dt, test, type="class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)





#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

nb <- naiveBayes(position ~ ., data = train)
predicted <- predict(nb, test, type="class")
CA(observed, predicted)

predMat <- predict(nb, test, type = "raw")
brier.score(obsMat, predMat)



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
nb <- CoreModel(position ~ ., data = train, model="bayes")
predicted <- predict(nb, test, type="class")
CA(observed, predicted)

predMat <- predict(nb, test, type = "prob")
brier.score(obsMat, predMat)




#
#
# K-NAJBLIZJIH SOSEDOV
#
#

# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
knn <- CoreModel(position ~ ., data = train, model="knn", kInNN = 5)
predicted <- predict(knn, test, type="class")
CA(observed, predicted)

predMat <- predict(knn, test, type = "prob")
brier.score(obsMat, predMat)


# model lahko dodatno izboljsamo z izbiro najbolj ustreznega parametra "kInNN"

# za izbiro parametra "kInNN" lahko uporabimo precno preverjanje na ucni mnozici

n <- nrow(train)
k <- 10

set.seed(0)
fold.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
fold.id <- fold.id[s]

# poskusili bomo vse vrednosti parametra "kInNN" na intervalu od 1 do 20
minkInNN <- 1
maxkInNN <- 20
est <- vector()
for (val in minkInNN:maxkInNN)
{
	print(paste("Testiram za nastavitev kInNN", val))
	flush.console()

	q <- vector()
	for (i in 1:k)
	{	
		sel <- fold.id == i
		knn <- CoreModel(position ~ ., data = train[!sel,], model="knn", kInNN = val)
		pred <- predict(knn, train[sel,], type= "class")
		obs <- train$position[sel]
		q[sel] <- pred == obs
	}

	est <- append(est, mean(q))
}

names(est) <- minkInNN:maxkInNN
est

which.max(est)


knn <- CoreModel(position ~ ., data = train, model="knn", kInNN = 18)
predicted <- predict(knn, test, type="class")
CA(observed, predicted)

predMat <- predict(knn, test, type = "prob")
brier.score(obsMat, predMat)







#
#
# NAKLJUCNI GOZD
#
#

# gradnja modela s pomocjo knjiznice "randomForest"

library(randomForest)

rf <- randomForest(position ~ ., data = train)
predicted <- predict(rf, test, type="class")
CA(observed, predicted)

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
rf <- CoreModel(position ~ ., data = train, model="rf")
predicted <- predict(rf, test, type="class")
CA(observed, predicted)

predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)




#
#
# SVM
#
#

# gradnja modela s pomocjo knjiznice "e1071"

library(e1071)

sm <- svm(position ~ ., data = train)
predicted <- predict(sm, test, type="class")
CA(observed, predicted)

sm <- svm(position ~ ., train, probability = T)
pred <- predict(sm, test, probability = T)
predMat <- attr(pred, "probabilities")

# v tem konkretnem primeru, je vrstni red razredov (stolpcev) v matriki predMat drugacen kot 
# je v matriki obsMat. 
colnames(obsMat)
colnames(predMat)

# popravimo ga
predMat <- predMat[,colnames(obsMat)]

# Iz tega razloga zamenjemo vrstni red stolpcev v matriki predMat
brier.score(obsMat, predMat)




# gradnja modela s pomocjo knjiznice "kernlab"

library(kernlab)

sm <- ksvm(position ~ ., data = train, kernel = "rbfdot")
predicted <- predict(sm, test, type = "response")
CA(observed, predicted)

sm <- ksvm(position ~ ., data = train, kernel = "rbfdot", prob.model = T)
predMat <- predict(sm, test, type = "prob")
brier.score(obsMat, predMat)



#
#
# UMETNE NEVRONSKE MREZE
#
#

# gradnja modela s pomocjo knjiznice "nnet"

library(nnet)

set.seed(0)
nn <- nnet(position ~ ., data = train, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, test, type = "class")
CA(observed, predicted)



# implementacija funkcije za ucenje nevronske mreze daje boljse rezultate v primeru,
# ko so ucni primeri normalizirani. 

# poiscemo zalogo vrednosti zveznih atributov
# (v nasem primeru so vsi atributi zvezni, razen ciljne spr. "position", ki je 3. stolpec)

summary(train)
names(train)
class <- which(names(train) == "position")
class

max_train <- apply(train[,-class], 2, max)
min_train <- apply(train[,-class], 2, min)

# normaliziramo podatke
train_scaled <- scale(train[,-class], center = min_train, scale = max_train - min_train)
train_scaled <- data.frame(train_scaled)
train_scaled$position <- train$position

# vse vrednosti atributov v ucni mnozici so na intervalu [0,1]
summary(train_scaled)


# testno mnozico skaliramo na zalogo vrednosti iz ucne mnozice!
test_scaled <- scale(test[,-class], center = min_train, scale = max_train - min_train)
test_scaled <- data.frame(test_scaled)
test_scaled$position <- test$position

# v testni mnozici ne bodo vse vrednosti na intervalu [0,1]!
summary(test_scaled)

set.seed(0)
nn <- nnet(position ~ ., data = train_scaled, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, test_scaled, type = "class")
CA(observed, predicted)

predMat <- predict(nn, test_scaled, type = "raw")
brier.score(obsMat, predMat)


#
# POZOR!!!!!
#

# v primeru binarne klasifikacije bo funkcija predict na modelu nnet vrnila verjetnosti samo enega razreda.
# celotno matriko moramo rekonstruirati sami

data <- read.table("insurance.txt", sep=",", header=T, stringsAsFactors=T)
set.seed(0)
sel <- sample(1:nrow(data), as.integer(nrow(data) * 0.7), replace=F)
train <- data[sel,]
test <- data[-sel,]

summary(train)

# normaliziramo zvezne atribute
contatt <- c(8:10, 12:17)
max_train <- apply(train[,contatt], 2, max)
min_train <- apply(train[,contatt], 2, min)

# normaliziramo podatke
train_scaled <- scale(train[,contatt], center = min_train, scale = max_train - min_train)
train_scaled <- data.frame(train_scaled)
train_scaled <- cbind(train_scaled, train[,-contatt])
summary(train_scaled)

test_scaled <- scale(test[,contatt], center = min_train, scale = max_train - min_train)
test_scaled <- data.frame(test_scaled)
test_scaled <- cbind(test_scaled, test[,-contatt])


observed <- test_scaled$insurance
obsMat <- class.ind(test_scaled$insurance)

nn <- nnet(insurance ~ ., data = train_scaled, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, test_scaled, type = "class")
CA(observed, predicted)

pm <- predict(nn, test_scaled, type = "raw")
head(pm)
predMat <- cbind(1-pm, pm)
brier.score(obsMat, predMat)

