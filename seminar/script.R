stavbe <- read.table("trainset.txt", header=T, sep=",", stringsAsFactors=T)
stavbe <- read.table("trainset.txt", header=T, sep=",")

# TODO: convert date strings to approprite date value (timestamp, date object,..)
summary(stavbe)

# vizualizacija posameznih atributov
pie(table(stavbe$regija), xlab="Regija")
hist(stavbe$stavba)
pie(table(stavbe$namembnost), xlab="Namembnost")
hist(stavbe$povrsina, xlab="Povrsina (m^2)", main="Histogram povrsine stavb")
hist(stavbe$poraba, xlab="Poraba (kWh)", main="Histogram porabe stavb")
hist(stavbe$leto_izgradnje, xlab="Leto izgradnje", main="Histogram leta izgradnje stavb")
hist(stavbe$temp_zraka, xlab="Temperatura zraka (°C)", main="Histogram temperature zraka")
hist(stavbe$temp_rosisca, xlab="Temperatura rosisca (°C)", main="Histogram temperature rosisca")
hist(stavbe$oblacnost, xlab="Oblacnost", main="Histogram stopnje pokritosti neba z oblaki")
hist(stavbe$oblacnost, xlab="Padavine (mm)", main="Histogram kolicine padavin")
hist(stavbe$oblacnost, xlab="Pritisk (mbar)", main="Histogram zracnega pritiska")
hist(stavbe$smer_vetra, xlab="Smer vetra (°)", main="Histogram smeri vetra")
hist(stavbe$smer_vetra, xlab="Hitrost vetra (m/s)", main="Histogram hitrosti vetra")

install.packages("tidyverse")

# vizualizacija korelacije med povrsino hise in porabo elektrike
x <- stavbe$povrsina
y <- stavbe$poraba
plot(x, y, col="lightblue")
abline(lm(y ~ x), col = "red", lwd = 3)

# vizualizacija korelacije med leto izgradnje in porabo elektrike
x <- stavbe$leto_izgradnje
y <- stavbe$poraba
plot(x, y, col="lightblue")
abline(lm(y ~ x), col = "red", lwd = 3)


# korelacijska matrika
# https://rkabacoff.github.io/datavis/Models.html
install.packages("mosaicData")
install.packages("ggcorrplot")

data(stavbe, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(stavbe, is.numeric)

# calulate the correlations
r <- cor(df, use="complete.obs")
round(r,2)

library(ggplot2)
library(ggcorrplot)
ggcorrplot(r,
           hc.order=T, # uredi po korelaciji
           type="lower") # prikazi samo v spodnjem trikotniku


# generiramo ucno in testno mnozico

sel <- sample(1:nrow(stavbe), as.integer(nrow(stavbe) * 0.7), replace=F)
train <- stavbe[sel,]
test <- stavbe[-sel,]

library(rpart)
dt <- rpart(namembnost ~ ., data=train) # zgradimo odlocitveno drevo (decision tree)
rpart.plot(dt)

