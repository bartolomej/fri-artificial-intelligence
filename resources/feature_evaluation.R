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

# Atribut "id" je precenjen, ceprav ne nosi nobene koristne informacije mnozico na popolnoma ciste podmnozice, zato zavede oceno InfGain
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
sort(attrEval(type ~ ., zoo, "Relo ReliefF
dt <- CoreModel(type ~ ., zoo, model="tree", selectionEstimator="ReliefFequalK")
plot(dt, zoo)



# Binarizacija vecvrednostnih atributov

sort(attrEval(type ~ ., Gini", binaryEvaluation=T), decreasing = T)


# Odlocitveno drevo z uporabo binarizacije + informacijskega prispevka
dt <- CoreModel(type ~ ., zoo, model="tree", selection
#


quadrant <- read.table("quadrant.txt", sep=",", header=T)
summary(quadrant)

quadrant$Class <- as.factor(quadrant$Class)

plot(quadrant, col=quadrant$Class)
plot(quadrant$a1, quadrant$a2, col=quadrant$Class)
prispevkom
# Opomba: Informacijski prispevek je definiran samo za diskretne atribute. 
#         Funkcija attrEval samodejno binarizira zvezne atribute, ko uporabljamo oceno, ki zaInfGain"), decreasing = TRUE)

# informacijski prispevek je kratkovidna ocena
# (predvideva, da so atributi pogojno neodvisni pri podani vrednosti ciljne speModel(Class ~ ., quadrant, model="tree", selectionEstimator="InfGain")
plot(dt, quadrant)


# Vse spodnje ocene so kratkovidne (ne morejo zaznass ~ ., quadrant, "Gini"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "MDL"), decreasing = TRUE)

# Ocene, ki niso kratkovidne (Rt, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "ReliefFexpRank"), decreasing = TRUE)

# Z ukazom ?attrEval lahkoarn

# Odlocitveno drevo na podlagi ocene Relief
set.seed(0)
dt <- CoreModel(Class ~ ., quadrant, model="tree", selectionEstimator = "ReliefF# zajelo koncept odvisne spremenljivke






#
#
# Izbira podmnozice atributov
#
#

ins <- read.table("insurance.txt", sep=",", header=T, strin0)

sel <- sample(1:nrow(ins), round(nrow(ins) * 0.7), replace=F)

train <- ins[sel,]
test <- ins[-sel,]

table(train$insurance)
table(tesponse")
mean(test$insurance == predicted)



#
#
# Izbira podmnozice atributov s filter metodo
#
#

# atribute ocenimo z neko mero (vsing=T)

# model zgradimo s pomocjo nekaj najbolje ocenjenih atributov
modelReduced <- ksvm(insurance ~ num.of.doors + height + body.styl# Izbira podmnozice atributov z metodo ovojnice (wrapper)
#
#

library(rpart)

modelFull <- rpart(insurance ~ ., train)
predicted <- prediuje:
# 	- formulo
#	- ucno mnozico 
#	- funkcijo za ucenje modela
#	- funkcijo za pridobivanje napovedi modela
#	- funkcijo za ocenjli odlocitveno drevo)
#



myTrainFunc <- function(formula, traindata)
{
	rpart(formula, traindata)	
}


#
# Funkcija za vanje napovedi modela (razredi)
#

myPredictFunc <- function(model, testdata)
{
	predict(model, testdata, type="class")
}


#
# Atributecamo napako modela, saj wrapper minimizira vrednost ocene
	1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(insurance ~ ., trance ~ num.of.doors + height + fuel.type + peak.rpm + aspiration + length, train)
predicted <- predict(modelWrap, test, type="class")di modela
#


#
# Funkcija za pridobivanje verjetnostnih napovedi
#

myPredictFuncProb <- function(model, testdata)
{
	predict(model, teined)
{
	obsMat <- model.matrix(~observed-1)
	sum((obsMat - predicted) ^ 2) / nrow(predicted)	
}

set.seed(0)
wrapper(insurance ~ ., train, myTrainFunc, myPredictFunc
