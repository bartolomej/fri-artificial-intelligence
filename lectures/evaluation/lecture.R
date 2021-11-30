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
q <- observed == predicted
sum(q)/length(q)



#
#
# K-KRATNO PRECNO PREVERJANJE (K-FOLD CROSS-VALIDATION)
#
#

# stevilo vseh primerov v podatkovni mnozici
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



# lahko proceduro izvedemo tudi tako, da za vsak testni primer hranimo indikator pravilne klasifikacije

set.seed(0)
fold.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
fold.id <- fold.id[s]
q <- vector()
for (i in 1:k)
{	
  print(paste("Iteracija", i))
  flush.console()
  
  sel <- fold.id == i
  
  dt <- rpart(insurance ~ ., data[!sel,])
  predicted <- predict(dt, data[sel,], type= "class")
  observed <- data$insurance[sel]
  q[sel] <- predicted == observed
}

mean(q)
sd(q)/sqrt(length(q))




#
# Precno preverjanje po principu izpusti enega (leave one out)
#

predicted <- factor(levels=levels(data$insurance))

for (i in 1:nrow(data))
{	
  print(paste("Izlocen primer na indeksu", i))
  flush.console()
  
  dt <- rpart(insurance ~ ., data[-i,])
  predicted[i] <- predict(dt, data[i,], type="class")
}

q <- data$insurance == predicted
mean(q)
sd(q)/sqrt(length(q))
