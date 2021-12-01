################################################################
#
# Kombiniranje algoritmov strojnega ucenja
#
################################################################

vehicle <- read.table("vehicle.txt", sep=",", header = T, stringsAsFactors = T)
summary(vehicle)

# ce zelimo ponovljive eksperimente, moramo nastaviti generator nakljucnih stevil
set.seed(0)

sel <- sample(1:nrow(vehicle), size=as.integer(nrow(vehicle)*0.7), replace=F)
train <- vehicle[sel,]
test <- vehicle[-sel,]


table(train$Class)
table(test$Class)


#install.packages("CORElearn")
library(CORElearn)

CA <- function(observed, predicted)
{
	mean(observed == predicted)
}

#
# Glasovanje
#

modelDT <- CoreModel(Class ~ ., train, model="tree")
modelNB <- CoreModel(Class ~ ., train, model="bayes")
modelKNN <- CoreModel(Class ~ ., train, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$Class, predDT)
caDT

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$Class, predNB)
caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$Class, predKNN)
caKNN

# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predDT, predNB, predKNN)

head(pred)

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions)
{
	res <- vector()

  	for (i in 1 : nrow(predictions))  	
	{
		vec <- unlist(predictions[i,])
    	res[i] <- names(which.max(table(vec)))
  	}

  	res
}

predClass <- voting(pred)
head(predClass)

predicted <- factor(predClass, levels=levels(train$Class))
head(predicted)

CA(test$Class, predicted)




#
# Utezeno glasovanje
#

predDT.prob <- predict(modelDT, test, type="prob")
predNB.prob <- predict(modelNB, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")

# sestejemo napovedane verjetnosti s strani razlicnih modelov
predProb <- predDT.prob + predNB.prob + predKNN.prob

head(predProb)

head(max.col(predProb))

# izberemo razred z najvecjo verjetnostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(vehicle$Class))
head(predicted)

CA(test$Class, predicted)



#
# Pri utezenem glasovanju lahko upostevamo tudi tocnosti modelov
#

# Tocnost modela ocenimo npr. s precnim preverjanjem na ucni mnozici

library(ipred)

mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict <- function(object, newdata) {pred <- predict(object, newdata, type="class"); destroyModels(object); pred}

res <- errorest(Class ~ ., train, model=mymodel, predict=mypredict, target.model="tree")
caDT.cv <- 1 - res$error
caDT.cv

res <- errorest(Class ~ ., train, model=mymodel, predict=mypredict, target.model="bayes")
caNB.cv <- 1 - res$error
caNB.cv

mymodelKNN <- function(formula, data, valK){CoreModel(formula, data, model="knn", kInNN=valK)}
res <- errorest(Class ~ ., train, model=mymodelKNN, predict=mypredict, valK=5)
caKNN.cv <- 1 - res$error
caKNN.cv

# sedaj pri sestevanju napovedane verjetnosti utezimo s pricakovano tocnostjo modela
predProb <- caDT.cv * predDT.prob + caNB.cv * predNB.prob + caKNN.cv * predKNN.prob

predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(vehicle$Class))

CA(test$Class, predicted)




#
# Bagging
#

n <- nrow(train)
m <- 30

models <- list()
for (i in 1:m)
{
	# nakljucno izberemo n primerov z vracanjem
	sel <- sample(1:n, n, replace = T)
	bootstrap <- train[sel,]
	models[[i]] <- CoreModel(Class ~ ., bootstrap, model="tree", minNodeWeightTree=1)
}

pred <- NULL
for (i in 1:m)
	pred <- cbind(pred, as.character(predict(models[[i]], test, type="class")))

head(pred)

predClass <- voting(pred)
predicted <- factor(predClass, levels=levels(train$Class))
CA(test$Class, predicted)




#install.packages("ipred")
library(ipred)

bag <- bagging(Class ~ ., train, nbagg=30)
predicted <- predict(bag, test, type="class")
CA(test$Class, predicted)




#
# Nakljucni gozd je inacica bagginga
#

# install.packages("randomForest")
library(randomForest)

rf <- randomForest(Class ~ ., train)
predicted <- predict(rf, test, type = "class")
CA(test$Class, predicted)




#
# Boosting
#

models <- list()

n <- nrow(train)

# na zacetku imajo vsi primeri enako utez
w <- rep(1/n, n)

m <- 100

i <- 1
while (i <= m)
{
	# nakljucno izberemo primere glede na utezi
	sel <- sample(1:n, n, prob=w, replace=T)

	# zgradimo model na podlagi izbranih primerov
	hyp <- CoreModel(Class ~ ., train[sel,], model="tree")

	# uporabimo model za klasifikacijo vseh primerov v ucni mnozici
	pred <- predict(hyp, train, type="class")

	# kateri primeri so pravilno klasificirani
	correct <- pred == train$Class
	
	# napaka modela je vsota utezi napacno klasificiranih primerov
	err <- sum(w[!correct])

	# ce je napaka prevelika, ponovi iteracijo
	if (err > 0.5)
		next

	beta <- err/(1-err)
	
	# shranimo model in njegovo utez pri glasovanju (vecjo utez dobijo modeli z nizjo napako)
	models[[i]] <- list(model=hyp, quality=log(1/beta))

	# znizamo utez pravilno klasificiranim primerom
	w[correct] <- w[correct] * beta
	
	# normaliziramo utezi, da dobimo verj. porazdelitev
	w <- w / sum(w)

	print(paste("Zakljucil korak", i, "izmed", m))
	flush.console()

	# gremo na naslednji model
	i <- i + 1 
}

# izpisimo utez pri glasovanju posameznih modelov 
for (i in 1:length(models))
	print(models[[i]]$quality)


# pred glasovanjem pripravimo matriko (st. vrstic = st. testnih primerov, st. stolpcev = st. razredov)
predMat <- matrix(0, nrow=nrow(test), ncol=nlevels(train$Class))
colnames(predMat) <- levels(train$Class)
head(predMat)

# vsak model klasificira testne primere in glasuje za izbrane razrede v skladu s svojo kvaliteto
for (i in 1:length(models))
{
	pred <- as.character(predict(models[[i]]$model, test, type="class"))
	for (j in 1:length(pred))
		predMat[j, pred[j]] <- predMat[j, pred[j]] + models[[i]]$quality
}

head(predMat)

predClass <- colnames(predMat)[max.col(predMat)]
predicted <- factor(predClass, levels(vehicle$Class))

CA(test$Class, predicted)




# install.packages("adabag")
library(adabag)

bm <- boosting(Class ~ ., train, mfinal=100)
predictions <- predict(bm, test)
names(predictions)

predicted <- predictions$class
CA(test$Class, predicted)

