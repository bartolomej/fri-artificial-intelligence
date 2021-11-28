###############################################################################
#
# UVOD V R
#
###############################################################################

# kalkulator
(50 + 1.45)/12.5


# operatorji prirejanja
x = 945
y <- sin(0.47)^2 * sqrt(5)
y^2 -> z


# trenutno vrednost objekta (spremenljivke) dobimo tako, da vnesemo njegovo ime
x
y
z


# izpis in odstranjevanje objektov iz pomnilnika
ls()
rm(y)
rm(x,z)

# za brisanje vseh objektov iz pomnilnika
rm(list=ls())


#
# Vektorji (osnovni podatkovni objekti v R)
#

# gradnja vektorja z nastevanjem vrednosti elementov
v <- c(14,7,23.5,76.2)
v

# gradnja aritmeticnih nizov
v <- 1:10
v

v <- seq(from=5, to=10, by=2)
v

# gradnja vektorja s ponavljanjem elementov
w <- rep(v, times = 2)
w


# skalarji so vektorji z enim elementom
w <- 45.0
w

# vektor lahko zgradimo s pomocjo drugih vektorjev
z <- c(v, 2.5, w)
z



#
# Uporabne funkcije nad vektorji
#

v <- c(8, 4, 2, 3, 6, 9, 1)

length(v)
max(v)
min(v)
which.min(v)
sum(v)
mean(v)
sd(v)
rev(v)
sort(v)
sortne konstante)
b <- c(TRUE, FALSE, F, T)
b
mode(b)

x <- 5 > 3
x
mode(x)

# vektor stringov (elementi so znakovni nizi)
s <- c("character", "logical", "numeric", "complex")
mode(s)


# elementi vektorja morajo biti istega tipa (v nasprotnem primeru R samodejno konvertira razlicne tipe)
x <- c(F, T, 34.56, 'aaa')
x


#
# Operacije z vektorji
#

v1 <- c(10,20,30,40)
v2 <- 1:4

# aritmeticne operacije se izvajajo nad istoleznimi elementi
v1 + v2
v1 * v2


# funkcije se izvajajo po elementih vektorja
v1^2
sqrt(v1)
exp(v1)
log2(v1)


# ce vektorja nista enako dolga, se med izvajanjem aritmeticnih operacij 
# elementi krajsega vektorja ciklicno ponavljajo
v1 * 10
v1 + 1
v1 + c(100, 200)




#
# Naslavljanje elementov vektorja
#

x <- c(-10,20,-30,40,-50,60,-70,80)
x

# elemente lahko naslovimo z nastevanjem indeksov (polozajev), ki nas zc(1,4,5)]
x[1:3]

# negativne vrednosti indeksov pomenijo, da zelimo nasloviti vse elemente razen navedenih
x[-1]
x[-c(4,6)]
x[-(1:3)]


# elemente je mozno nasloviti tudi z logicnim vektoTRUE

# logicni vektor (rezultat primerjave po elementih vektorja)
x > 0

# naslavljanje z logicnim vektorjem (vrne elemente, ki ustrezajo logicni konstor ==
# za preverjanje neenakosti uporabljamo operator !=


# funkcija which() vrne indekse, ki ustrezajo vrednosti TRUE
which(x > 0)




# ko naslavljamo elemente z njihovim imenom
point['x']
point[c('x','z')]

# ce ne podamo indeksov, naslovimo vse elemente vektorja
point[] <- 0
point

# popolnoma drug rezu"a", "b", "c", "d")

# spreminjanje vrednosti elementov
x[2] <- "BBBBB"
x

x[c(1,3)] <- c("AAAAA", "CCCCC")
x

# dodajanje novega elementa
x[e vrednosti elementov
is.na(x)


# odstranjevanje elementov vektorja
x <- x[-c(1,3)]
x

x <- c(x[2],x[3])
x




#
# Faktorji
#

gender nder)
gender

# argument "levels" definira mozne vrednosti elementov
smeri <- factor(c('levo','levo','desno'), levels = c('levo','desno','gor','dol'))
smeri

# izpis seznama dovoljenih vrednosti elementov

smeri[1] <- "posevno"
smeri

smeri[1] <- "gor"
smeri

# frekvencna tabela vrednosti 
table(gender)
table(smeri)



# naslavljanje komponent seznama (z uporabo imen)
student$id
student$name
student$marks

# naslavljanje komponent seznama (z uporabo indeksov)
student[[1]]
stud)
student



#
# Podatkovni okvirji (Data frames)
#
 
# gradnja podatkovnega okvirja
height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
f"))
student <- c(T, T, F, F, T, T, F, F, F, T)

df <- data.frame(gender, height, weight, student)
df

# nekaj uporabnih funkcij
sum3]

df$height

df[df$height < 180,]
df[df$gender == "m",]


# dodajanje novega stolpca v podatkovni okvic(20, 21, 30, 25, 27, 19, 24, 27, 28, 24))
df

df$name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")

