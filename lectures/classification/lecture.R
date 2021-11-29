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
# Zelimo zgraditi model, ki bo za podano osebo napovedal, v katero skupino (glede na letni zaslužek)
# se bo uvrstil.
#
# Ciljni atribut "income" je diskreten (skupina) - zato govorimo o klasifikaciji.   
#
# Zelimo preveriti, ali model zgrajen na podlagi zgodovinskih podatkov, lahko 
# uporabimo za napovedovanje novih, do sedaj nepoznanih oseb. 
#

# Koliko je ucnih primerov?
nrow(census)

# Podatke bomo nakljucno razdelili na ucno in testno mnozico v rezmerju 70:30.

# Ukaz set.seed nastavi generator nakljucnih stevil.
# Uporabimo ga takrat, ko zelimo ponovljivo sekvenco generiranih stevil.
set.seed(0)

sel <- sample(1:nrow(census), size=as.integer(nrow(census) * 0.7), replace=F)

train <- census[sel,]
test <- census[-sel,]


# Poglejmo osnovne karakteristike ucne in testne mnozice 
# (stevilo primerov, porazdelitev ciljne spremenljivke).

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

# v nasem primeru je vecinski razred "Small"

# Izracunajmo tocnost vecinskega klasifikatorja
# (delez pravilnih napovedi, ce bi vse testne primere klasificirali v vecinski razred)

sum(test$income == "Small") / length(test$income)



#
#
# ODLOCITVENA DREVESA
#
#

# Za gradnjo odlocitvenih dreves potrebujemo funkcijo iz knjiznice "rpart".
# Nalozimo jo
library(rpart)

# Zgradimo odlocitveno drevo
dt <- rpart(income ~ ., data = train)

#
# Prvi argument funkcije rpart je formula, ki doloca kaksen model zelimo zgraditi.
# Drugi argument predstavlja ucne primere, na podlagi katerih se bo zgradil model.
#
# Formula doloca ciljni atribut (levo od znaka "~") in atribute, ki jih model lahko 
# uporabi pri napovedovanju vrednosti ciljnega atributa (desno od znaka "~").
#
# Formula "income ~ . " oznacuje, da zelimo zgraditi model za napovedovanje 
# ciljnega atributa "income" s pomocjo vseh ostalih atributov v ucni mnozici.
#
# Ce bi, na primer, zeleli pri gradnji modela za "income" uporabiti samo 
# atribut "workclass", bi to oznacili s formulo "income ~ workclass". Ce bi pa zeleli 
# uporabiti atribute "workclass", "education" in "hours_per_week", bi to oznacili 
# s formulo "income ~ workclass + education + hours_per_week".
#


# Za izris drevesa potrebujemo knjiznico "rpart.plot", ki jo je potrebno pred prvo rabo instalirati. 
#
# Knjiznice instaliramo z ukazom install.packages():
#
#     install.packages("rpart.plot")
#

library(rpart.plot)

rpart.plot(dt)


# Neznani primer klasificiramo tako, da zacnemo pri korenu drevesa in potujemo po
# ustreznih vejah do lista. V vsakem notranjem vozliscu testiramo pogoj in, ce je
# le-ta izpolnjen, nadaljujemo v levem sinu, sicer se premaknemo v desnega. 
# Listi drevesa dolocajo razred, v katerega klasificiramo neznani primer.

# Prave vrednosti testnih primerov
observed <- test$income
head(observed)

# Napovedane vrednosti modela
# Uporabimo funkcijo "predict", ki potrebuje model, testne primere in obliko, 
# v kateri naj poda svoje napovedi. Nastavitev "class" pomeni, da nas zanimajo
# samo razredi, v katere je model klasificiral testne primere.
 
predicted <- predict(dt, test, type = "class")
head(predicted)

# Zgradimo tabelo napacnih klasifikacij
tab <- table(observed, predicted)
tab

# Elementi na diagonali predstavljajo pravilno klasificirane testne primere...

#
# Klasifikacijska tocnost (delez pravilno klasificiranih primerov)
# 
# interpretiramo kot verjetnost, da bo nakljucno izbran primer pravilno klasificiran
#

sum(diag(tab)) / sum(tab)

# Lahko napisemo funkcijo za izracun klasifikacijske tocnosti
CA <- function(obs, pred)
{
	tab <- table(obs, pred)

	sum(diag(tab)) / sum(tab)
}

# Klic funkcije za klasifikacijsko tocnost...
CA(observed, predicted)


# Klasifikacijsko tocnost lahko izracunamo tudi tako...
q <- observed == predicted
mean(q)


#
# Za dvorazredne probleme lahko uporabimo dodatne mere:
#
# Senzitivnost - odstotek pravilno klasificiranih pozitivnih primerov
#
# Specificnost - odstotek pravilno klasificiranih negativnih primerov
#
# Preciznost - odstotek pravilno klasificiranih primerov, ki so bili klasificirani kot pozitivni
#

# Funkcija za izracun senzitivnosti modela
Sensitivity <- function(obs, pred, pos.class)
{
	tab <- table(obs, pred)

	tab[pos.class, pos.class] / sum(tab[pos.class,])
}

# Funkcija za izracun specificnosti modela
Specificity <- function(obs, pred, pos.class)
{
	tab <- table(obs, pred)
	neg.class <- which(row.names(tab) != pos.class)

	tab[neg.class, neg.class] / sum(tab[neg.class,])
}

# Funkcija za izracun preciznosti modela
Precision <- function(obs, pred, pos.class)
{
	tab <- table(obs, pred)

	tab[pos.class, pos.class] / sum(tab[, pos.class])
}

table(observed, predicted)
Sensitivity(observed, predicted, "Large")
Specificity(observed, predicted, "Large")
Precision(observed, predicted, "Large")




#
# Druga oblika napovedi modela (nastavitev "prob") vraca verjetnosti, 
# da posamezni testni primer pripada dolocenemu razredu.
#

# Napovedane verjetnosti pripadnosti razredom (odgovor dobimo v obliki matrike)
predictedMat <- predict(dt, test, type = "prob")
head(predictedMat)

# Indikator pripadnosti razredom 
# (dejanski razred primera je oznacen z 1, ostali pa z 0)

library(nnet)
observedMat <- class.ind(test$income)
head(observedMat)

#
# Brierjeva mera - oceni razlicnost med ciljno (idealno) distribucijo 
#                  in dejansko napovedano distribucijo
#

# Funkcija za izracun Brierjeve mere
brier.score <- function(obsMat, predMat)
{
	sum((obsMat - predMat) ^ 2) / nrow(predMat)
}

# Izracunajmo Brierjevo mero za napovedi nasega drevesa
brier.score(observedMat, predictedMat)


# Brierjevo mero lahko izracunamo tudi tako...
bs <- apply((observedMat - predictedMat) ^ 2, 1, sum) 
mean(bs)


#
# TRIVIALNI MODEL
#
# vedno napove apriorno distribucijo razredov
#


p0 <- table(train$income)/nrow(train)
p0

p0Mat <- matrix(rep(p0, times=nrow(test)), nrow = nrow(test), byrow=T)
colnames(p0Mat) <- names(p0)
head(p0Mat)

brier.score(observedMat, p0Mat)





#
# Informacijska vsebina odgovora (Information score)
#

# poleg aposteriornih verjetnosti razredov (poda jih model)
# ocena uposteva tudi apriorne verjetnosti razredov


inf.score <- function(trainClass, testClass, predMat)
{
	result <- 0

	priors <- table(trainClass)/length(trainClass)

	for (i in 1:nrow(predMat))
	{
		p.prior <- priors[[testClass[i]]]
		p.posterior <- predMat[i, testClass[i]]

		if (p.posterior >= p.prior)
			result <- result - log2(p.prior) + log2(p.posterior)
		else
			result <- result + log2(1-p.prior) - log2(1-p.posterior)				
	}

	result/nrow(predMat)
}

inf.score(train$income, test$income, predictedMat)


#
# Informacijska vsebina odgovora modela, ki vedno napove apriorno verjetnost razredov
#

inf.score(train$income, test$income, p0Mat)





#
# Krivulja ROC
#

# model poda verjetnostne napovedi
predMat <- predict(dt, test, type = "prob")
head(predMat)


# napovedane verjetnosti za pozitiven razred uredimo narascajoce in odstranimo ponovitve
p <- unique(sort(predMat[,"Large"]))
p
 

# pragove dolocimo kot vmesne vrednosti med elementi vektorja p 
threshVec <- p[-length(p)] + diff(p)/2
threshVec
 
# dodamo se -Inf in Inf, ter pragove uredimo padajoce
threshVec <- c(-Inf, threshVec, Inf)
threshVec <- sort(threshVec, decreasing=T)
threshVec

sensVec <- vector()
specVec <- vector()
for (th in threshVec)
{
	predicted <- ifelse(predMat[,"Large"] > th, "Large", "Small")
	predicted <- factor(predicted, levels=c("Large", "Small"))

	sensVec <- append(sensVec, Sensitivity(observed, predicted, "Large"))
	specVec <- append(specVec, Specificity(observed, predicted, "Large"))
}

sensVec
specVec

plot(x=1-specVec, y=sensVec, type="l", xlab="1-Specificity", ylab="Sensitivity", main="ROC curve")
abline(a=0, b=1, col="grey")

# Ploscina pod krivuljo ROC
dX <- c(diff(1-specVec), 0)
dY <- c(diff(sensVec), 0)
AUC <- sum(sensVec * dX) + sum(dY * dX)/2
AUC

# AUC predstavlja verjetnost, da bo klasifikator nakljucno izbranemu pozitivnemu primeru 
# dodelil visjo oceno (pripadnost pozitivnemu razredu) kot nakljucno izbranemu negativnemu primeru 

