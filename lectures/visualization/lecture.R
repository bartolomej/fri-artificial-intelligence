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
str(md) # display object structure
names(md)

# Faktoriziramo atribut "mpaa" (kategorije vsebine)
# Opomba: pred verzijo 4.0.0 je read.table() privzeto faktoriziral stringe

table(md$mpaa)
# prazen string zamenjamo z oznako "UNK"
md$mpaa[md$mpaa == ""] <- "UNK"

# The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors).
md$mpaa <- as.factor(md$mpaa)

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
# vrednosti, ki jih ni mogoce konvertirati bodo predstavljene kot NA
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
barplot(table(md$Drama))
pie(table(md$mpaa))



###############################################################################
#
# PRIMER 1: Koliksen je delez komedij med vsemi filmi?
#
###############################################################################

# z ukazom table() dobimo frekvenco posameznih vrednosti v vektorju
table(md$Comedy)

# delez komedij lahko graficno prikazemo
barplot(table(md$Comedy))
pie(table(md$Comedy))


# Grafi so bolj berljivi, ce ustrezno poimenujemo oznake, osi, naslov grafa...

tab <- table(md$Comedy)
names(tab) <- c("Ostale zvrsti", "Komedije")
tab

sum(tab)

barplot(tab, ylab="Stevilo filmov", main="Razmerje med komedijami in ostalimi zvrsti filmov")
barplot(tab / sum(tab) * 100, ylab="Delez filmov v %", main="Delez komedij med ostalimi zvrstmi filmov")

pie(tab, main = "Delez komedij med ostalimi zvrstmi filmov")


###############################################################################
#
# PRIMER 2: Kako so porazdeljene ocene komedij?
#
###############################################################################

# Izrisimo histogram ocen za komedije
hist(md[md$Comedy == "1", "rating"], xlab="Ocena filma", ylab="Frekvenca", main="Histogram ocen komedij")

# Boxplot ocen komedij
boxplot(md[md$Comedy == "1", "rating"], ylab="Ocena filma", main="Ocene komedij")

summary(md$rating[md$Comedy == 1])


########################################################################################
# 
# PRIMER 3: Ali so drame v povprecju bolje ocenjene od ostalih filmov?
#           Ali so romanticne komedije v povprecju bolje ocenjene kot akcijske komedije?
#
########################################################################################

# Izberimo drame
drama <- md$Drama == "1"

# Izracunajmo povprecno oceno dram
mean(md[drama,"rating"])

# Izracunajmo povprecno oceno ostalih filmov
mean(md[!drama,"rating"])

# Drame imajo, v povprecju, nekoliko visjo oceno od ostalih filmov

# Izrisimo boxplot diagram ocen za razlicne vrednosti atributa Drama 
boxplot(rating ~ Drama, data=md)
boxplot(rating ~ Drama, data=md, names=c("Ostale zvrsti", "Drame"), ylab="Ocena filma", main="Primerjava ocen filmov med dramami in ostalimi zvrstmi")



# Izberimo romanticne komedije
selRomCom <- md$Comedy == "1" & md$Romance == "1"
ratingRC <- md$rating[selRomCom]
mean(ratingRC)

# Izberimo akcijske komedije
selActCom <- md$Comedy == "1" & md$Action == "1"
ratingAC <- md$rating[selActCom]
mean(ratingAC)

boxplot(ratingRC, ratingAC, names=c("Romanticne kom.", "Akcijske kom."))



###############################################################################
#
# PRIMER 4: Kaksen je delez komedij (po letih) od 1960 naprej?
#
###############################################################################

sel <- md$year >= 1960

# ukaz table() lahko uporabimo za stetje frekvenc parov vrednosti
table(md$Comedy[sel], md$year[sel])

# prva vrstica ustreza ne-komedijam
# druga vrstica ustraza komedijam

tab <- table(md$Comedy[sel], md$year[sel])
tab
tab[2,]

# prestejmo vse filme po izbranih letih
table(md$year[sel])

tabAll <- table(md$year[sel])
tab[2,]/tabAll

ratio <- tab[2,]/tabAll
barplot(ratio, xlab="Leto", ylab="Relativna frekvenca", main="Relativna frekvenca komedij po letih")

plot(x=names(ratio), y=as.vector(ratio), type="l", xlab="Leto", ylab="Relativna frekvenca komedij", main="Gibanje deleza komedij med filmi po letih")

###############################################################################
# 
# PRIMER 5: Ali je vec podpovprecnih ali nadpovprecnih filmov (glede na oceno)?
#
###############################################################################

# povprecna ocena
mean(md$rating)

# koliko je filmov z nadpovprecno oceno?
tab <- table(md$rating > mean(md$rating))
tab

names(tab) <- c("podpovprecno ocenjeni", "nadpovprecno ocenjeni")
barplot(tab, ylab="Stevilo filmov", main="Razmerje med podpovprecno in nadpovprecno ocenjenimi filmi")
pie(tab, main="Razmerje med podpovprecno in nadpovprecno ocenjenimi filmi")


# Boxplot diagram podaja vpogled v porazdelitev vrednosti atributa
boxplot(md$rating, ylab="Ocena filma", main="Boxplot ocen filmov")

# Vodoravna crta znotraj pravokotnika predstavlja mediano

# Izrisimo se povprecno vrednost atributa
abline(h=mean(md$rating))

# Iz diagrama je razvidno, da mediana ni na sredini pravokotnika. 
# To pomeni, da je porazdelitev vrednosti atributa nagnjena. 


###############################################################################
#
# PRIMER 6: Ali so filmi z najvecjim proracunom tudi najbolje ocenjeni?
#
###############################################################################

# vecina filmov nima podatka o proracunu (vrednost NA)
summary(md$budget)

is.na(md$budget)
table(is.na(md$budget))
which(is.na(md$budget))

# izberimo samo tiste vrstice, ki vsebujejo podatek o proracunu filma
sel <- is.na(md$budget)
mdsub <- md[!sel,]

nrow(mdsub)
summary(mdsub$budget)


plot(mdsub$budget, mdsub$rating, xlab="Proracun v $", ylab="Ocena", main="Ocena filma v odvisnosti od proracuna")


# poglejmo korelacijski koeficient med proracunom in oceno filma
cor(mdsub$budget, mdsub$rating)
cor.test(mdsub$budget, mdsub$rating)

# nasi podatki ne sugerirajo na obstoj korelacije med proracunom in oceno filma 



# Izracunajmo izkoristek proracuna na dobljeno oceno  
ratio <- mdsub$budget/mdsub$rating

# Ali so filmi "ekonomicni"?
hist(ratio)

# Velika vecina filmov relativno "poceni" pride do svojih ocen.

# Kateri film ima najslabsi izkoristek (najvec porabljenega denarja za dobljeno oceno)?
mdsub[which.max(ratio),]


# Diskretizirajmo proracun filmov na:
# nizek (do 1M), srednji (med 1M in 100M) in visok (vec kot 100M)

disbudget <- cut(mdsub$budget, c(0, 1000000, 50000000, 500000000), labels=c("nizek", "srednji", "visok"))
barplot(table(disbudget)/length(disbudget), xlab="Proracun filma", ylab="Relativna frekvenca", main="Delez filmov glede na visino proracuna")

# Izrisimo boxplot dobljenih ocen glede na proracun filma
boxplot(mdsub$rating ~ disbudget, xlab="Proracun filma", ylab="Ocena filma", main="Boxplot ocen filmov glede na visino proracuna")

# Bolj dragi filmi izkazujejo manjso variabilnost ocen 


###############################################################################
#
# PRIMER 7: 
# Koliko stane, kumulativno, produkcija filmov po letih od 1990 do 2000?
# Koliko stane, v povprecju, produkcija filma po letih od 1990 do 2000?
# (upostevajte samo filme, ki imajo podatek o proracunu!)
#
###############################################################################

# Izberimo filme, ki imajo podatek o proracunu
sel <- !is.na(md$budget) & md$year >= 1990 & md$year <= 2000

# Kumulativen proracun filmov po letnicah lahko izracunamo s pomocjo funkcije "aggregate"

sum.budget <- aggregate(as.double(budget) ~ year, data = md[sel,], sum) 
sum.budget
plot(sum.budget, type="l", xlab="Leto", ylab="Vrednost v $", main="Kumulativna vrednost proracunov filmov po letih")

avg.budget <- aggregate(as.double(budget) ~ year, data = md[sel,], mean)
avg.budget
plot(avg.budget, type="l", xlab="Leto", ylab="Vrednost v $", main="Povprecna vrednost proracunov filmov po letih")


##############################################################################
#
# PRIMER 8: (podatkovna mnozica "players.txt")
# Koliksna je povprecna visina igralcev v posameznih sezonah v obdobju 1970-2000?
#
##############################################################################

# Branje podatkov iz tekstovne datoteke
players <- read.table("players.txt", sep=",", header = T)
summary(players)

# Ustvarimo prazen vektor
h <- vector()

# Uporabimo zanko za sprehod cez leta v obdobju 1970-2000
for (y in 1970:2000)
{
  # Izberemo igralce, ki so bili aktivni v opazovanem letu
  sel <- players$firstseason <= y & players$lastseason >= y
  
  # Povprecno visino aktivnih igralcev dodamo na konec vektorja
  h <- c(h, mean(players$height[sel]))
}

# Izrisemo zbrane podatke o povprecni visini igralcev (uporabimo type="l" za izris crt)
plot(1970:2000, h, type="l", xlab="Leto", ylab="Visina v cm", main="Gibanje povprecne visine igralcev v ligi NBA po letih")

