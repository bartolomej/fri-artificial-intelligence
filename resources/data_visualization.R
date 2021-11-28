###############################################################################
#
# VIZUALIZACIJA PODATKOV
#
###############################################################################

# Prenesite datoteki "movies.txt" in "players.txt" v lokalno mapo. To mapo nastavite kot delovno
# mapo okolja R. To lahko naredite s pomocjo ukaza "setwd" oziroma iz menuja 
# s klikom na File -> Change dir...
# 
# na primer:
# setwd("c:\\vaje\\data\\")


# Branje podatkov iz tekstovne datoteke
# (header=T oznacuje, da datoteka vsebuje vrstico z imeni atributov (stolpcev v podatkovnem okvirju)
#  sep="," doloca, da je znak "," uporabljen kot locilo med vrednostmi v datoteki)
md <- read.table(file="movies.txt", sep=",", header=TRUE)

# Opis delovanja funkcije, seznam argumentov klica in primer uporabe dobimo tako, da za znakom "?" vnesemo ime funkcije
?read.table

# nekaj uporabnih funkcij
head(md)
summary(md)
str(md)
names(md)

# Faktoriziramo atribut "mpaa" (kategorije vsebine)
# Opomba: pred verzijo 4.0.0 je read.table() priva <- as.factor(md$mpaa)

# Faktoriziramo tudi binarne atribute, ki opredeljujejo zvrst filma

# Atribut "Action" je 18., atribut "Short" pa 24. stolpec

# Za faktorizacijo teh stolpcev lahko uporabimo zanko "for"
for (i in 18:24)
	md[,i] <- as.factor(md[,i])

# Sedaj so nominalni atributi predstavljeni kot faktorji
summary(md)



#
# Funkcije za konverzijo podatkovnih tipov:
#
# as.numeric
# as.integer
# as.character
# as.logical
# as.factor
# as.ordered
#
# vrednosti, ki jih ni mogdstavljene kot NA
#


# Primeri naslavljanja podatkov
md[30,]
md[30,3]
md[30,"length"]
md[,3]
md$length

# Uporabne funkcije pri vizualizaciji podatkov
plot(md$votes)
hist(md$rating)
boxplot(md$rating)
barplot(t#########################################################################
#
# PRIMER 1: Koliksen je delez komedij med vsemi filmi?
#
#################################znih vrednosti v vektorju
table(md$Comedy)

# delez komedij lahko graficno prikazemo
barplot(table(md$Comedy))
pie(table(md$Comedy))


# Grafi so bolj berljivi, ce ustrezno poimenujemo oznakOstale zvrsti", "Komedije")
tab

sum(tab)

barplot(tab, ylab="Stevilo filmov", main="Razmerje med komedijami in ostalimi zvrsti filmov")
barplot(tab / sum(tab) * 100, ylab="Delez filmov v % = "Delez komedij med ostalimi zvrstmi filmov")


#########################################################################
#
# PRIMER 2: Kako so porazdeljene ocene komedij?
#
#################################################

# Izrisimo histogram ocen za komedije
hist(md[md$Comedy == "1", "rating"], xlab="Ocena filma", ylab="Frekvenca", main="Histog

summary(md$rating[md$Comedy == 1])


########################################################################################
# 
# PRIMER 3: Ali je ocenjene kot akcijske komedije?
#
########################################################################################

#ceno ostalih filmov
mean(md[!drama,"rating"])

# Drame imajo, v povprecju, nekoliko visjo oceno od ostalih filmov

# Izrisimo boxp=c("Ostale zvrsti", "Drame"), ylab="Ocena filma", main="Primerjava ocen filmov med dramami in ostalimi zvrstmi")



# Izberimo roma akcijske komedije
selActCom <- md$Comedy == "1" & md$Action == "1"
ratingAC <- md$rating[selActCom]
mean(ratingAC)

boxplot(rating################
#
# PRIMER 4: Kaksen je delez komedij (po letih) od 1960 naprej?
#
###############################################################################able(md$Comedy[sel], md$year[sel])

# prva vrstica ustreza ne-komedijam
# druga vrstica ustraza komedijam

tab <- table(md$Comedy[sb[2,]/tabAll

ratio <- tab[2,]/tabAll
barplot(ratio, xlab="Leto", ylab="Relativna frekvenca", main="Relativna frekvenca komedij po leleza komedij med filmi po letih")

###############################################################################
# 
# PRIMER 5: Ali je vec#######################

# povprecna ocena
mean(md$rating)

# koliko je filmov z nadpovprecno oceno?
tab <- table(md$rating > mean(md$rating))
tab

names(tab) <- Razmerje med podpovprecno in nadpovprecno ocenjenimi filmi")
pie(tab, main="Razmerje med podpovprecno in nadpovprecno ocenjeni filmi")


# Boxplot diagram podaja vpogled v porazdelitev vrednosti atributa
boxplot(md$rating, ylab="Ocena filma", main=ot ocen filmov")

# Vodoravna crta znotraj pravokotnika predstavlja mediano

# Izrisimo se povprecno vrednost atributa
ablimean(md$rating))

# Iz diagrama je razvidno, da mediana ni na sredini pravokotnika. 
# To pomeni, da je porazdelitev vrednosti atrz najvecjim proracunom tudi najbolje ocenjeni?
#
###############################################################################

# vecina filmov nima podaa(md$budget))

# izberimo samo tiste vrstice, ki vsebujejo podatek o proracunu filma
sel <- is.na(md$budget)
mdsub <- md[!sel
nrow(mdsub)
summary(mdsub$budget)


plot(mdsub$budget, mdsub$rating, xlab="Proracun v $", ylab="Ocena", main="Ocena fiisnosti od proracuna")


# poglejmo korelacijski koeficient med proracunom in oceno filma
cor(mdsub$budget, mdsub$rating)st(mdsub$budget, mdsub$rating)

# nasi podatki ne sugerirajo na obstoj korelacije med proracunom in oceno filma 

 
 
#jmo izkoristek proracuna na dobljeno oceno  
ratio <- mdsub$budget/mdsub$rating

# Ali so filmi "ekonomicni"?
hist(ratio)

# Velika vecina filmov relativno "pocenza dobljeno oceno)?
mdsub[which.max(ratio),]

 
# Diskretizirajmo proracun filmov na:
# nizek (do 1M), srednji (med 1M in visok (vec kot 100M)

disbudget <- cut(mdsub$budget, c(0, 1000000, 50000000, 500000000), labels=c("nizek", "srednji", ))
barplot(table(disbudget)/length(disbudget), xlab="Proracun filma", ylab="Relativna frekvenca", main="Delez filmov glede na visima", ylab="Ocena filma", main="Boxplot ocen filmov glede na visino proracuna")

# Bolj dragi filmi izkazujejo manjso variat ocen 


###############################################################################
#
# PRIMER 7: 
# Koliko stane, kumulativno, produkcija filmov po stevajte samo filme, ki imajo podatek o proracunu!)
#
######################################################################

# Izberimo filme, ki imajo podatek o proracunu
sel <- !is.na(md$budget) & md$year >= 1990 & md$year <= 2000

# Kumulatn proracun filmov po letnicah lahko izracunamo s pomocjo funkcije "aggregate"

sum.budget <- aggregate(as.double(budget) ~  data = md[sel,], sum) 
sum.budget
plot(sum.budget, type="l", xlab="Leto", ylab="Vrednost v $", main="Kumulativna vrednost proracunov filmov po letih")

avg.pe="l", xlab="Leto", ylab="Vrednost v $", main="Povprecna vrednost proracunov filmov po letih")


################################### igralcev v posameznih sezonah v obdobju 1970-2000?
#
##############################################################################

# Branje podatkov iz tekstovne darimo prazen vektor
h <- vector()

# Uporabimo zanko za sprehod cez leta v obdobju 1970-2000
for (y in 1970:2000)
{
	# Izberemo igraktivnih igralcev dodamo na konec vektorja
	h <- c(h, mean(players$height[sel]))
}

# Izrisemo zbrane podatke o povprecni visini igralcev (u visine igralcev v ligi NBA po letih")


