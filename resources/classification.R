#
#
# KLASIFIKACIJA
#
#

# Prenesite datoteko "census.txt" v lokalno mapo. To mapo nastavite kot 
# delovno mapo okolja R. To lahko naredite s pomocjo ukaza "setwd" oziroma iz 
# menuja s klikom na File -> Change dir...
# 
# Na primer:
# setwd("c:\\vaje\\data\\")


# Nalozimo ucno mnozico
census <- read.table("census.txt", header = T, sep = ",", stringsAsFactors = T)


# Povzetek o atributih v podatkovni mnozici
summary(census)


##############################################################################
#
# Primer
#
# Napovedovanje letnega zasluzka
# (nominalna spremenljivka "income", ki ima dve vrednosti: "Large" in "Small")
#
##############################################################################

#
# Zelimo zgraditi model, ki bo za podano osebo napovedal, v katero skupino (glede na letni zaslu�ek)
# se bo uvrstil.
#
# Ciljni atribut "income" je diskreten (skupina) - zato govorimo o klasifikaciji.   
#
# Zelimo preveriti, ali model zgrajen na podlagi zgodovinskih podatkov, lahko 
# uporabimo za na ucno in testno mnozico v rezmerju 70:30.

# Ukaz set.seed nastavi generator nakljucnih stevil.
# Uporabimo ga takrat, ko zelimo ponovljivo sekvenco generiranih stevil.
set.seed(0)

sel <- sample(1:nrow(census), size=as.integer(nrow(census) * 0.7), replace=F)

train <- census[sel,]
test <- census[-sel,]


# Poglejmo osnovne karakteristike ucne in tev, porazdelitev ciljne spremenljivke).

nrow(train)
table(train$income)

nrow(test)
table(test$income)


#
#
# VECINSKI KLASIFIKATOR
#
# vedno klasificira v razred z najvec ucnimi primeri
#

# poglejmo pogostost posameznih razredov
table(train$income)

# v nasem primeru je vecinse testne primere klasificirali v vecinski razred)

sum(test$income == "Small") / length(test$income)



#
#
# ODLOCITVENA DREVESA
#
#

# Za gradnjo odlocitvenih dreves potrebujecitveno drevo
dt <- rpart(income ~ ., data = train)

#
# Prvi argument funkcije rpart je formula, ki doloca kaksen model zelimo zgraditi.
# Drugi argument predstavlja ucnlevo od znaka "~") in atribute, ki jih model lahko 
# uporabi pri napovedovanju vrednosti ciljnega atributa (desno od znaka "~").
#
# Formula "income ~ . " oznacuje, da zelimo zgraditialih atributov v ucni mnozici.
#
# Ce bi, na primer, zeleli pri gradnji modela za "income" uporabiti samo 
# atribut "workclass", bi to oznacili s formulo "income ~ workclass". Ce bi pa zeleli 
#ek", bi to oznacili 
# s formulo "income ~ workclass + education + hours_per_week".
#


# Za izris drevesa potrebujemo knjiznico "rpart.plot", ki jo je potrebno pred prvo ratall.packages("rpart.plot")
#

library(rpart.plot)

rpart.plot(dt)


# Neznani primer klasificiramo tako, da zacnemo pri korenu drevesa in potujemo po
# ustrezniljujemo v levem sinu, sicer se premaknemo v desnega. 
# Listi drevesa dolocajo razred, v katerega klasificiramo neznani primer.

# Prave vrednosti testnih primerov
observed <- test$ipredict", ki potrebuje model, testne primere in obliko, 
# v kateri naj poda svoje napovedi. Nastavitev "class" pomeni, da nas zanimajo
# samo razdicted)

# Zgradimo tabelo napacnih klasifikacij
tab <- table(observed, predicted)
tab

# Elementi na diagonali predstavljajo pravilno klasificirane tes kot verjetnost, da bo nakljucno izbran primer pravilno klasificiran
#

sum(diag(tab)) / sum(tab)

# Lahko napisemo funkcijo za izracun klasifikacijskcije za klasifikacijsko tocnost...
CA(observed, predicted)


# Klasifikacijsko tocnost lahko izracunamo tudi tako...
q <- observed == predicted
meaciranih pozitivnih primerov
#
# Specificnost - odstotek pravilno klasificiranih negativnih primerov
#
# Preciznost - odstotek pravilno klasificiranih primerov, ki so bil <- function(obs, pred, pos.class)
{
	tab <- table(obs, pred)

	tab[pos.class, pos.class] / sum(tab[pos.class,])
}

# Funkcija za izracun specificnosti mnames(tab) != pos.class)

	tab[neg.class, neg.class] / sum(tab[neg.class,])
}

# Funkcija za izracun preciznosti modela
Precision <- function(orved, predicted)
Sensitivity(observed, predicted, "Large")
Specificity(observed, predicted, "Large")
Precision(observed, predicted, "Larolocenemu razredu.
#

# Napovedane verjetnosti pripadnosti razredom (odgovor dobimo v obliki matrike)
predictedMat <- predict(dt, test, type =a z 0)

library(nnet)
observedMat <- class.ind(test$income)
head(observedMat)

#
# Brierjeva mera - oceni razlicnost med ciljno (idealno) distribucijo 
#       re <- function(obsMat, predMat)
{
	sum((obsMat - predMat) ^ 2) / nrow(predMat)
}

# Izracunajmo Brierjevo mero za napovedi nasega drevesa
brierdictedMat) ^ 2, 1, sum) 
mean(bs)


#
# TRIVIALNI MODEL
#
# vedno napove apriorno distribucijo razredov
#


p0 <- table(train$come)/nrow(train)
p0

p0Mat <- matrix(rep(p0, times=nrow(test)), nrow = nrow(test), byrow=T)
colnames(p0Mat) <- names(p0)
0Mat)

brier.score(observedMat, p0Mat)





#
# Informacijska vsebina odgovora (Information score)
#

# poleg aposteriornetnosti razredov (poda jih model)
# ocena uposteva tudi apriorne verjetnosti razredov


inf.score <- function(trainClass, testClass, predMat)
{
	result <- 0
s[[testClass[i]]]
		p.posterior <- predMat[i, testClass[i]]

		if (p.posterior >= p.prior)
			result <- result - log2(p.prior) + log2(p.po(train$income, test$income, predictedMat)


#
# Informacijska vsebina odgovora modela, ki vedno napove apriorno verjetnost razredov
#

it(dt, test, type = "prob")
head(predMat)


# napovedane verjetnosti za pozitiven razred uredimo narascajoce in odstranimo ponovitve
p <- unique(sort(preength(p)] + diff(p)/2
threshVec
 
# dodamo se -Inf in Inf, ter pragove uredimo padajoce
threshVec <- c(-Inf, threshVec, Inf)
thre ifelse(predMat[,"Large"] > th, "Large", "Small")
	predicted <- factor(predicted, levels=c("Large", "Small"))

	sensVec <- append(sensVesensVec
specVec

plot(x=1-specVec, y=sensVec, type="l", xlab="1-Specificity", ylab="Sensitivity", main="ROC curve")
abline(a=0, b=1um(dY * dX)/2
AUC

# AUC predstavlja verjetnost, da bo klasifikator nakljucno izbranemu pozitivnemu primeru 
# dodelil visjo oceno 
