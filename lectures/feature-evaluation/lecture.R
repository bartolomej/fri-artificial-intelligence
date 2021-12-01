# Mere za ocenjevanje atributov so implementirane v paketu "CORElearn"

# Nalozimo knjiznico
#install.packages("CORElearn") 
library(CORElearn)


#
# Ocenjevanje atributov v klasifikacijskih problemih
#


zoo <- read.table(file="zoo.txt", sep=",", header=T)
summary(zoo)

for (i in 1:ncol(zoo))
	zoo[,i] <- as.factor(zoo[,i])

summary(zoo)



# Funkcija "attrEval" oceni kvaliteto atributov glede na ciljno spremenljivko, 
# kot je doloca formula (prvi argument). Najpreprostejsa formula doloca samo 
# ciljno spremenljivko, na primer "type ~ .". Znak "." v formuli pomeni, da 
# bo funkcija "attrEval" ocenila vse ostale atribute v podatkovni mnozici.

# Ocenjevanje atributov z informacijskim prispevkom
sort(attrEval(type ~ ., zoo, "InfGain"), decreasing = TRUE)



#
#
# Precenjevanje vecvrednostnih atributov
#
#


# v ucno mnozico bomo dodali neuporaben atribut "id", ki enolicno doloca ucne primere
id <- 1:nrow(zoo)
zoo$id <- as.factor(id)

head(zoo)

# Atribut "id" je precenjen, ceprav ne nosi nobene koristne informacije za 
# napovedovanje novih primerov
sort(attrEval(type ~ ., zoo, "InfGain"), decreasing = T)

# Atribut "id" razbije ucno mnozico na popolnoma ciste podmnozice, zato zavede oceno InfGain
# Informacijski prispevek atributa "id" je enak entropiji razreda
p0 <- table(zoo$type)/length(zoo$type)
-sum(p0*log2(p0))


# Odlocitveno drevo z uporabo informacijskega prispevka
dt <- CoreModel(type ~ ., zoo, model="tree", selectionEstimator="InfGain")
plot(dt, zoo)



# Gini prav tako precenjuje vecvrednostne atribute
sort(attrEval(type ~ ., zoo, "Gini"), decreasing = T)


# GainRatio omili precenjevanje atributa "id"
sort(attrEval(type ~ ., zoo, "GainRatio"), decreasing = TRUE)

# ReliefF in MDL pravilno ocenita "id" kot nepomemben atribut
sort(attrEval(type ~ ., zoo, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(type ~ ., zoo, "MDL"), decreasing = TRUE)

# Odlocitveno drevo z uporabo ReliefF
dt <- CoreModel(type ~ ., zoo, model="tree", selectionEstimator="ReliefFequalK")
plot(dt, zoo)



# Binarizacija vecvrednostnih atributov

sort(attrEval(type ~ ., zoo, "InfGain", binaryEvaluation=T), decreasing = T)
sort(attrEval(type ~ ., zoo, "Gini", binaryEvaluation=T), decreasing = T)


# Odlocitveno drevo z uporabo binarizacije + informacijskega prispevka
dt <- CoreModel(type ~ ., zoo, model="tree", selectionEstimator="InfGain", binaryEvaluation=T)
plot(dt, zoo)



# 
#
# Kratkovidnost ocen
#
#


quadrant <- read.table("quadrant.txt", sep=",", header=T)
summary(quadrant)

quadrant$Class <- as.factor(quadrant$Class)

plot(quadrant, col=quadrant$Class)
plot(quadrant$a1, quadrant$a2, col=quadrant$Class)


# Ocenjevanje atributov z informacijskim prispevkom
# Opomba: Informacijski prispevek je definiran samo za diskretne atribute. 
#         Funkcija attrEval samodejno binarizira zvezne atribute, ko uporabljamo oceno, ki zahteva diskretne atribute.
#
        			
sort(attrEval(Class ~ ., quadrant, "InfGain"), decreasing = TRUE)

# informacijski prispevek je kratkovidna ocena
# (predvideva, da so atributi pogojno neodvisni pri podani vrednosti ciljne spremenljivke)

# Odlocitveno drevo na podlagi informacijskega prispevka je zelo slab model 
dt <- CoreModel(Class ~ ., quadrant, model="tree", selectionEstimator="InfGain")
plot(dt, quadrant)


# Vse spodnje ocene so kratkovidne (ne morejo zaznati atributov v interakciji)
sort(attrEval(Class ~ ., quadrant, "GainRatio"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "Gini"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "MDL"), decreasing = TRUE)

# Ocene, ki niso kratkovidne (Relief in ReleifF)
sort(attrEval(Class ~ ., quadrant, "Relief"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "ReliefFexpRank"), decreasing = TRUE)

# Z ukazom ?attrEval lahko odprete dokumentacijo in pregledate seznam ostalih inacic 
# ocene ReliefF, ki so implementirane v knjiznici CORElearn

# Odlocitveno drevo na podlagi ocene Relief
set.seed(0)
dt <- CoreModel(Class ~ ., quadrant, model="tree", selectionEstimator = "ReliefFequalK")
plot(dt, quadrant)

# Opazimo, da je drevo zgrajeno z oceno, ki zazna interakcijo med atributi, pravilno 
# zajelo koncept odvisne spremenljivke






#
#
# Izbira podmnozice atributov
#
#

ins <- read.table("insurance.txt", sep=",", header=T, stringsAsFactors=T)
summary(ins)

nrow(ins)


# nastavimo generator nakljucnih stevil (zaradi ponovljivosti)
set.seed(0)

sel <- sample(1:nrow(ins), round(nrow(ins) * 0.7), replace=F)

train <- ins[sel,]
test <- ins[-sel,]

table(train$insurance)
table(test$insurance)


library(kernlab)

modelFull <- ksvm(insurance ~ ., train)
predicted <- predict(modelFull, test, type="response")
mean(test$insurance == predicted)



#
#
# Izbira podmnozice atributov s filter metodo
#
#

# atribute ocenimo z neko mero (v tem primeru ReliefF, saj imamo zvezne atribute)
set.seed(0)
sort(attrEval(insurance ~ ., train, "ReliefFequalK"), decreasing=T)

# model zgradimo s pomocjo nekaj najbolje ocenjenih atributov
modelReduced <- ksvm(insurance ~ num.of.doors + height + body.style + length, train) 
predicted <- predict(modelReduced, test, type="response")
mean(test$insurance == predicted)



#
#
# Izbira podmnozice atributov z metodo ovojnice (wrapper)
#
#

library(rpart)

modelFull <- rpart(insurance ~ ., train)
predicted <- predict(modelFull, test, type="class")
mean(test$insurance == predicted)


source("wrapper.R")

#
# Funkcija wrapper potrebuje:
# 	- formulo
#	- ucno mnozico 
#	- funkcijo za ucenje modela
#	- funkcijo za pridobivanje napovedi modela
#	- funkcijo za ocenjevanje kvalitete napovedi
#	- parameter precnega preverjanja
#


#
# Funkcija za ucenje modela (zaradi hitrosti bomo uporabili odlocitveno drevo)
#



myTrainFunc <- function(formula, traindata)
{
	rpart(formula, traindata)	
}


#
# Funkcija za pridobivanje napovedi modela (razredi)
#

myPredictFunc <- function(model, testdata)
{
	predict(model, testdata, type="class")
}


#
# Atribute lahko izberemo glede na klasifikacijsko tocnost modela
#

myEvalFunc <- function(predicted, observed, trained)
{
	# vracamo napako modela, saj wrapper minimizira vrednost ocene
	1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(insurance ~ ., train, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)

# testirajmo na neodvisni testni mnozici
modelWrap <- rpart(insurance ~ num.of.doors + height + fuel.type + peak.rpm + aspiration + length, train)
predicted <- predict(modelWrap, test, type="class")
mean(test$insurance == predicted)



#
# Podmnozico atributov lahko izberemo tudi glede na kvaliteto verjetnostnih napovedi modela
#


#
# Funkcija za pridobivanje verjetnostnih napovedi
#

myPredictFuncProb <- function(model, testdata)
{
	predict(model, testdata, type="prob")
}

#
# Model lahko ocenimo z Brierjevo mero
#

myEvalFuncBrier <- function(predicted, observed, trained)
{
	obsMat <- model.matrix(~observed-1)
	sum((obsMat - predicted) ^ 2) / nrow(predicted)	
}

set.seed(0)
wrapper(insurance ~ ., train, myTrainFunc, myPredictFuncProb, myEvalFuncBrier, cvfolds=10)

