# Combining classification models

## Uvod podatkov

``` r
vehicle <- read.table("vehicle.txt", sep=",", header = T, stringsAsFactors = T)
summary(vehicle)
```

    ##       Comp             Circ           D.Circ           Rad.Ra     
    ##  Min.   : 73.00   Min.   :33.00   Min.   : 40.00   Min.   :104.0  
    ##  1st Qu.: 87.00   1st Qu.:40.00   1st Qu.: 70.00   1st Qu.:141.0  
    ##  Median : 93.00   Median :44.00   Median : 80.00   Median :167.0  
    ##  Mean   : 93.68   Mean   :44.86   Mean   : 82.09   Mean   :168.9  
    ##  3rd Qu.:100.00   3rd Qu.:49.00   3rd Qu.: 98.00   3rd Qu.:195.0  
    ##  Max.   :119.00   Max.   :59.00   Max.   :112.00   Max.   :333.0  
    ##    Pr.Axis.Ra        Max.L.Ra         Scat.Ra          Elong      
    ##  Min.   : 47.00   Min.   : 2.000   Min.   :112.0   Min.   :26.00  
    ##  1st Qu.: 57.00   1st Qu.: 7.000   1st Qu.:146.2   1st Qu.:33.00  
    ##  Median : 61.00   Median : 8.000   Median :157.0   Median :43.00  
    ##  Mean   : 61.69   Mean   : 8.567   Mean   :168.8   Mean   :40.93  
    ##  3rd Qu.: 65.00   3rd Qu.:10.000   3rd Qu.:198.0   3rd Qu.:46.00  
    ##  Max.   :138.00   Max.   :55.000   Max.   :265.0   Max.   :61.00  
    ##   Pr.Axis.Rect     Max.L.Rect   Sc.Var.Maxis    Sc.Var.maxis        Ra.Gyr     
    ##  Min.   :17.00   Min.   :118   Min.   :130.0   Min.   : 184.0   Min.   :109.0  
    ##  1st Qu.:19.00   1st Qu.:137   1st Qu.:167.0   1st Qu.: 318.2   1st Qu.:149.0  
    ##  Median :20.00   Median :146   Median :178.5   Median : 364.0   Median :173.0  
    ##  Mean   :20.58   Mean   :148   Mean   :188.6   Mean   : 439.9   Mean   :174.7  
    ##  3rd Qu.:23.00   3rd Qu.:159   3rd Qu.:217.0   3rd Qu.: 587.0   3rd Qu.:198.0  
    ##  Max.   :29.00   Max.   :188   Max.   :320.0   Max.   :1018.0   Max.   :268.0  
    ##    Skew.Maxis       Skew.maxis       Kurt.maxis     Kurt.Maxis   
    ##  Min.   : 59.00   Min.   : 0.000   Min.   : 0.0   Min.   :176.0  
    ##  1st Qu.: 67.00   1st Qu.: 2.000   1st Qu.: 5.0   1st Qu.:184.0  
    ##  Median : 71.50   Median : 6.000   Median :11.0   Median :188.0  
    ##  Mean   : 72.46   Mean   : 6.377   Mean   :12.6   Mean   :188.9  
    ##  3rd Qu.: 75.00   3rd Qu.: 9.000   3rd Qu.:19.0   3rd Qu.:193.0  
    ##  Max.   :135.00   Max.   :22.000   Max.   :41.0   Max.   :206.0  
    ##     Holl.Ra       Class    
    ##  Min.   :181.0   bus :218  
    ##  1st Qu.:190.2   opel:212  
    ##  Median :197.0   saab:217  
    ##  Mean   :195.6   van :199  
    ##  3rd Qu.:201.0             
    ##  Max.   :211.0

``` r
set.seed(0)

sel <- sample(1:nrow(vehicle), size=as.integer(nrow(vehicle)*0.7), replace=F)
train <- vehicle[sel,]
test <- vehicle[-sel,]


table(train$Class)
```

    ## 
    ##  bus opel saab  van 
    ##  154  143  155  140

``` r
table(test$Class)
```

    ## 
    ##  bus opel saab  van 
    ##   64   69   62   59

``` r
library(CORElearn)

# definirajmo pomocno funkcijo za Classification Accuracy
CA <- function(observed, predicted)
{
    mean(observed == predicted)
}
```

Ustvarimo tri razlicne modele, vsak ima malo drugacno klasifikacijsko
tocnost.

``` r
modelDT <- CoreModel(Class ~ ., train, model="tree")
modelNB <- CoreModel(Class ~ ., train, model="bayes")
modelKNN <- CoreModel(Class ~ ., train, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$Class, predDT)
caDT
```

    ## [1] 0.6456693

``` r
predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$Class, predNB)
caNB
```

    ## [1] 0.5472441

``` r
predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$Class, predKNN)
caKNN
```

    ## [1] 0.6614173

``` r
pred <- data.frame(predDT, predNB, predKNN)
head(pred)
```

    ##   predDT predNB predKNN
    ## 1    van    van     van
    ## 2   opel   saab    opel
    ## 3    van    van     van
    ## 4    bus    bus     bus
    ## 5    van    van     van
    ## 6    bus    bus     bus

## Glasovanje

Napoved lahko poskusimo izboljsati, z vpeljavo *glasovanja*.

``` r
voting <- function(predictions)
{
    res <- vector()

    for (i in 1 : nrow(predictions))    
      {
        vec <- unlist(predictions[i,]) # pretvorimo vrstico v vektor
        res[i] <- names(which.max(table(vec))) # poiscemo napoved z najvec glasovi
    }

    res
}

predicted <- factor(voting(pred), levels=levels(train$Class))
CA(test$Class, predicted)
```

    ## [1] 0.6811024

## Utezeno glasovanje

Glasovanje s pomocjo verjetnostni napovedi - t.i. *utezeno glasovanje*.

``` r
predDT.prob <- predict(modelDT, test, type="prob")
head(predDT.prob)
```

    ##             bus      opel       saab       van
    ## [1,] 0.00000000 0.2040816 0.30612245 0.4897959
    ## [2,] 0.00000000 0.5058824 0.49411765 0.0000000
    ## [3,] 0.00000000 0.2040816 0.30612245 0.4897959
    ## [4,] 0.81250000 0.0000000 0.00000000 0.1875000
    ## [5,] 0.01515152 0.0000000 0.01515152 0.9696970
    ## [6,] 0.97727273 0.0000000 0.02272727 0.0000000

``` r
predDT.prob <- predict(modelDT, test, type="prob")
predNB.prob <- predict(modelNB, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")

# sestejemo napovedane verjetnosti s strani razlicnih modelov
predProb <- predDT.prob + predNB.prob + predKNN.prob

head(predProb)
```

    ##               bus         opel        saab          van
    ## [1,] 2.526663e-05 0.2043691315 0.306671020 2.488935e+00
    ## [2,] 2.048092e-03 1.4601621480 1.536170354 1.619406e-03
    ## [3,] 6.378844e-06 0.4045508914 0.306761624 2.288681e+00
    ## [4,] 2.796881e+00 0.0057467909 0.009017475 1.883544e-01
    ## [5,] 1.032925e-01 0.0002014773 0.015311548 2.881194e+00
    ## [6,] 2.663603e+00 0.0530582625 0.283253202 8.552264e-05

``` r
head(max.col(predProb))
```

    ## [1] 4 3 4 1 4 1

``` r
# izberemo razred z najvecjo verjetnostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(vehicle$Class))
head(predicted)
```

    ## [1] van  saab van  bus  van  bus 
    ## Levels: bus opel saab van

``` r
CA(test$Class, predicted)
```

    ## [1] 0.6889764

Pri utezenem glasovanju lahko upostevamo tudi tocnost modelov - obtezimo
glasove z indikatorjem tocnosti modela. Tako imajo kvalitetnejsi modeli
vecji glas.

``` r
library(ipred)

mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict <- function(object, newdata) {pred <- predict(object, newdata, type="class"); destroyModels(object); pred}

res <- errorest(Class ~ ., train, model=mymodel, predict=mypredict, target.model="tree")
caDT.cv <- 1 - res$error # klasifikacijska tocnost (1 - napaka)
caDT.cv
```

    ## [1] 0.6689189

``` r
res <- errorest(Class ~ ., train, model=mymodel, predict=mypredict, target.model="bayes")
caNB.cv <- 1 - res$error
caNB.cv
```

    ## [1] 0.5709459

``` r
mymodelKNN <- function(formula, data, valK){CoreModel(formula, data, model="knn", kInNN=valK)}
res <- errorest(Class ~ ., train, model=mymodelKNN, predict=mypredict, valK=5)
caKNN.cv <- 1 - res$error
caKNN.cv
```

    ## [1] 0.7347973

``` r
# sedaj pri sestevanju napovedane verjetnosti utezimo s pricakovano tocnostjo modela
predProb <- caDT.cv * predDT.prob + caNB.cv * predNB.prob + caKNN.cv * predKNN.prob

predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(vehicle$Class))

CA(test$Class, predicted)
```

    ## [1] 0.6968504

## Bagging

Bagging (*Bootstrap Aggregating*) - zdruzujemo napovedi razlicnih
dreves/modelov. Vzorcimo iz ucne mnozice (z vracanjem). Izberemo
priblizno 2/3 primerov. Na podlagi izbranih primerov zgradimo
odlocitveno drevo, dobimo en model, ter ta postopek dobimo velikokrat.

Na koncu za vsak testni primer izvedemo navadno glasovanje na
pridobljeni mnozici modelov.

> Pri tem postopku, je priporocljivo da zgradimo overfittana drevesa
> (parameter `minNodeWeightTree=1`), saj se izkaze dalahko pridemo do
> dobre napovedi, ce zdruzimo napovedi iz mnozice takih dreves.

``` r
n <- nrow(train)
m <- 30 # koliko razlicnih modelov zgradimo

models <- list()
for (i in 1:m)
{
    # nakljucno izberemo n primerov z vracanjem (elementi se lahko ponavljajo)
    sel <- sample(1:n, n, replace = T)
    bootstrap <- train[sel,]
    # minNodeWeightTree = najmanjse stevilo primerov na dolocen list drevesa
    models[[i]] <- CoreModel(Class ~ ., bootstrap, model="tree", minNodeWeightTree=1)
}

pred <- NULL
for (i in 1:m)
    pred <- cbind(pred, as.character(predict(models[[i]], test, type="class")))

head(pred)
```

    ##      [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]   [,10] 
    ## [1,] "van"  "opel" "opel" "van"  "van"  "van"  "saab" "van"  "saab" "van" 
    ## [2,] "saab" "saab" "saab" "opel" "opel" "saab" "opel" "opel" "opel" "opel"
    ## [3,] "van"  "saab" "saab" "opel" "saab" "van"  "saab" "opel" "saab" "opel"
    ## [4,] "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus" 
    ## [5,] "van"  "van"  "saab" "van"  "van"  "van"  "van"  "van"  "van"  "van" 
    ## [6,] "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus" 
    ##      [,11]  [,12]  [,13]  [,14]  [,15]  [,16]  [,17]  [,18]  [,19]  [,20] 
    ## [1,] "van"  "opel" "van"  "opel" "opel" "van"  "van"  "van"  "saab" "saab"
    ## [2,] "saab" "opel" "opel" "opel" "opel" "opel" "opel" "opel" "opel" "opel"
    ## [3,] "van"  "opel" "van"  "opel" "opel" "saab" "opel" "saab" "saab" "opel"
    ## [4,] "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus" 
    ## [5,] "van"  "van"  "van"  "van"  "van"  "van"  "van"  "van"  "van"  "van" 
    ## [6,] "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus" 
    ##      [,21]  [,22]  [,23]  [,24]  [,25]  [,26]  [,27]  [,28]  [,29]  [,30] 
    ## [1,] "opel" "saab" "van"  "opel" "van"  "van"  "van"  "saab" "saab" "van" 
    ## [2,] "opel" "opel" "opel" "opel" "opel" "opel" "opel" "saab" "opel" "opel"
    ## [3,] "opel" "opel" "opel" "van"  "van"  "van"  "opel" "saab" "opel" "saab"
    ## [4,] "bus"  "bus"  "bus"  "van"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus" 
    ## [5,] "van"  "van"  "van"  "opel" "van"  "van"  "van"  "van"  "van"  "van" 
    ## [6,] "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"  "bus"

``` r
predClass <- voting(pred)
predicted <- factor(predClass, levels=levels(train$Class))
CA(test$Class, predicted)
```

    ## [1] 0.7125984

Imamo tudi ze implementiramo knjiznico za izvedbo bagginga.

``` r
library(ipred)

bag <- bagging(Class ~ ., train, nbagg=30)
predicted <- predict(bag, test, type="class")
CA(test$Class, predicted)
```

    ## [1] 0.7244094

Poskusimo se z metodo nakljucnega gozda (podobna baggingu).

> Random forests or random decision forests are an ensemble learning
> (use multiple learning algorithms) method for classification,
> regression and other tasks that operates by constructing a multitude
> of decision trees at training time. For classification tasks, the
> output of the random forest is the class selected by most trees.

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
rf <- randomForest(Class ~ ., train)
predicted <- predict(rf, test, type = "class")
CA(test$Class, predicted)
```

    ## [1] 0.7322835

## Boosting

Opis boostinga iz wikipedije
\[wikipedije\](<https://en.wikipedia.org/wiki/Boosting_(machine_learning)>:
\> Boosting is based on the question posed by Kearns and Valiant “Can a
set of weak learners create a single strong learner?” A weak learner is
defined to be a classifier that is only slightly correlated with the
true classification (it can label examples better than random guessing).
In contrast, a strong learner is a classifier that is arbitrarily
well-correlated with the true classification.

Osnova postopka je utezitev posameznega ucnega primera (utez oznacuje
tezavnost primera). Na zacetku imajo vsi ucni primeri enako utez. Vsak
naslednji model se uci na tezjih primerih.

``` r
models <- list()

n <- nrow(train)

# na zacetku imajo vsi primeri enako utez
w <- rep(1/n, n)

# stevilo vseh modelov
m <- 100

i <- 1
while (i <= m)
{
    # nakljucno izberemo primere glede na utezi (elementi se lahko ponavljajo)
  # verjetnost da izberem primer (prob) je sorazmerna z njegovo tezavnostjo/utezjo
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
```

    ## [1] "Zakljucil korak 1 izmed 100"
    ## [1] "Zakljucil korak 2 izmed 100"
    ## [1] "Zakljucil korak 3 izmed 100"
    ## [1] "Zakljucil korak 4 izmed 100"
    ## [1] "Zakljucil korak 5 izmed 100"
    ## [1] "Zakljucil korak 6 izmed 100"
    ## [1] "Zakljucil korak 7 izmed 100"
    ## [1] "Zakljucil korak 8 izmed 100"
    ## [1] "Zakljucil korak 9 izmed 100"
    ## [1] "Zakljucil korak 10 izmed 100"
    ## [1] "Zakljucil korak 11 izmed 100"
    ## [1] "Zakljucil korak 12 izmed 100"
    ## [1] "Zakljucil korak 13 izmed 100"
    ## [1] "Zakljucil korak 14 izmed 100"
    ## [1] "Zakljucil korak 15 izmed 100"
    ## [1] "Zakljucil korak 16 izmed 100"
    ## [1] "Zakljucil korak 17 izmed 100"
    ## [1] "Zakljucil korak 18 izmed 100"
    ## [1] "Zakljucil korak 19 izmed 100"
    ## [1] "Zakljucil korak 20 izmed 100"
    ## [1] "Zakljucil korak 21 izmed 100"
    ## [1] "Zakljucil korak 22 izmed 100"
    ## [1] "Zakljucil korak 23 izmed 100"
    ## [1] "Zakljucil korak 24 izmed 100"
    ## [1] "Zakljucil korak 25 izmed 100"
    ## [1] "Zakljucil korak 26 izmed 100"
    ## [1] "Zakljucil korak 27 izmed 100"
    ## [1] "Zakljucil korak 28 izmed 100"
    ## [1] "Zakljucil korak 29 izmed 100"
    ## [1] "Zakljucil korak 30 izmed 100"
    ## [1] "Zakljucil korak 31 izmed 100"
    ## [1] "Zakljucil korak 32 izmed 100"
    ## [1] "Zakljucil korak 33 izmed 100"
    ## [1] "Zakljucil korak 34 izmed 100"
    ## [1] "Zakljucil korak 35 izmed 100"
    ## [1] "Zakljucil korak 36 izmed 100"
    ## [1] "Zakljucil korak 37 izmed 100"
    ## [1] "Zakljucil korak 38 izmed 100"
    ## [1] "Zakljucil korak 39 izmed 100"
    ## [1] "Zakljucil korak 40 izmed 100"
    ## [1] "Zakljucil korak 41 izmed 100"
    ## [1] "Zakljucil korak 42 izmed 100"
    ## [1] "Zakljucil korak 43 izmed 100"
    ## [1] "Zakljucil korak 44 izmed 100"
    ## [1] "Zakljucil korak 45 izmed 100"
    ## [1] "Zakljucil korak 46 izmed 100"
    ## [1] "Zakljucil korak 47 izmed 100"
    ## [1] "Zakljucil korak 48 izmed 100"
    ## [1] "Zakljucil korak 49 izmed 100"
    ## [1] "Zakljucil korak 50 izmed 100"
    ## [1] "Zakljucil korak 51 izmed 100"
    ## [1] "Zakljucil korak 52 izmed 100"
    ## [1] "Zakljucil korak 53 izmed 100"
    ## [1] "Zakljucil korak 54 izmed 100"
    ## [1] "Zakljucil korak 55 izmed 100"
    ## [1] "Zakljucil korak 56 izmed 100"
    ## [1] "Zakljucil korak 57 izmed 100"
    ## [1] "Zakljucil korak 58 izmed 100"
    ## [1] "Zakljucil korak 59 izmed 100"
    ## [1] "Zakljucil korak 60 izmed 100"
    ## [1] "Zakljucil korak 61 izmed 100"
    ## [1] "Zakljucil korak 62 izmed 100"
    ## [1] "Zakljucil korak 63 izmed 100"
    ## [1] "Zakljucil korak 64 izmed 100"
    ## [1] "Zakljucil korak 65 izmed 100"
    ## [1] "Zakljucil korak 66 izmed 100"
    ## [1] "Zakljucil korak 67 izmed 100"
    ## [1] "Zakljucil korak 68 izmed 100"
    ## [1] "Zakljucil korak 69 izmed 100"
    ## [1] "Zakljucil korak 70 izmed 100"
    ## [1] "Zakljucil korak 71 izmed 100"
    ## [1] "Zakljucil korak 72 izmed 100"
    ## [1] "Zakljucil korak 73 izmed 100"
    ## [1] "Zakljucil korak 74 izmed 100"
    ## [1] "Zakljucil korak 75 izmed 100"
    ## [1] "Zakljucil korak 76 izmed 100"
    ## [1] "Zakljucil korak 77 izmed 100"
    ## [1] "Zakljucil korak 78 izmed 100"
    ## [1] "Zakljucil korak 79 izmed 100"
    ## [1] "Zakljucil korak 80 izmed 100"
    ## [1] "Zakljucil korak 81 izmed 100"
    ## [1] "Zakljucil korak 82 izmed 100"
    ## [1] "Zakljucil korak 83 izmed 100"
    ## [1] "Zakljucil korak 84 izmed 100"
    ## [1] "Zakljucil korak 85 izmed 100"
    ## [1] "Zakljucil korak 86 izmed 100"
    ## [1] "Zakljucil korak 87 izmed 100"
    ## [1] "Zakljucil korak 88 izmed 100"
    ## [1] "Zakljucil korak 89 izmed 100"
    ## [1] "Zakljucil korak 90 izmed 100"
    ## [1] "Zakljucil korak 91 izmed 100"
    ## [1] "Zakljucil korak 92 izmed 100"
    ## [1] "Zakljucil korak 93 izmed 100"
    ## [1] "Zakljucil korak 94 izmed 100"
    ## [1] "Zakljucil korak 95 izmed 100"
    ## [1] "Zakljucil korak 96 izmed 100"
    ## [1] "Zakljucil korak 97 izmed 100"
    ## [1] "Zakljucil korak 98 izmed 100"
    ## [1] "Zakljucil korak 99 izmed 100"
    ## [1] "Zakljucil korak 100 izmed 100"

``` r
# izpisimo utez pri glasovanju posameznih modelov 
for (i in 1:length(models))
    print(models[[i]]$quality)
```

    ## [1] 0.9092069
    ## [1] 1.078934
    ## [1] 1.231464
    ## [1] 1.18485
    ## [1] 0.9818778
    ## [1] 0.7379555
    ## [1] 1.241719
    ## [1] 1.098123
    ## [1] 1.430512
    ## [1] 1.145128
    ## [1] 1.117649
    ## [1] 1.548377
    ## [1] 1.248477
    ## [1] 0.7779973
    ## [1] 1.276014
    ## [1] 1.432688
    ## [1] 1.499575
    ## [1] 1.473354
    ## [1] 1.398913
    ## [1] 0.9398228
    ## [1] 1.138513
    ## [1] 1.315265
    ## [1] 1.358568
    ## [1] 1.30863
    ## [1] 1.409521
    ## [1] 1.001064
    ## [1] 1.098457
    ## [1] 1.172286
    ## [1] 1.015986
    ## [1] 0.9286847
    ## [1] 0.8451337
    ## [1] 0.6690368
    ## [1] 0.6177703
    ## [1] 0.8756915
    ## [1] 0.5495762
    ## [1] 1.1932
    ## [1] 0.7282621
    ## [1] 0.5328712
    ## [1] 0.3761925
    ## [1] 0.8127947
    ## [1] 0.9779973
    ## [1] 1.284225
    ## [1] 0.7436057
    ## [1] 1.141637
    ## [1] 0.5261008
    ## [1] 1.063873
    ## [1] 1.371201
    ## [1] 1.150356
    ## [1] 1.18462
    ## [1] 1.634671
    ## [1] 1.211002
    ## [1] 1.311422
    ## [1] 1.098153
    ## [1] 1.226414
    ## [1] 1.549706
    ## [1] 1.347247
    ## [1] 1.204866
    ## [1] 1.185278
    ## [1] 1.420416
    ## [1] 1.518292
    ## [1] 1.052045
    ## [1] 0.4580168
    ## [1] 0.9610647
    ## [1] 0.9997388
    ## [1] 0.9756125
    ## [1] 1.459395
    ## [1] 1.388335
    ## [1] 1.100972
    ## [1] 1.462643
    ## [1] 0.5013088
    ## [1] 0.7236823
    ## [1] 1.301605
    ## [1] 1.408671
    ## [1] 1.443285
    ## [1] 1.150816
    ## [1] 1.033344
    ## [1] 0.6803027
    ## [1] 1.071829
    ## [1] 1.015571
    ## [1] 1.20864
    ## [1] 1.304564
    ## [1] 1.329479
    ## [1] 0.6727041
    ## [1] 1.461209
    ## [1] 1.142074
    ## [1] 0.6391781
    ## [1] 1.445131
    ## [1] 1.362784
    ## [1] 1.264002
    ## [1] 1.252273
    ## [1] 1.402473
    ## [1] 0.7439225
    ## [1] 1.153971
    ## [1] 0.6311492
    ## [1] 0.9241025
    ## [1] 0.6494996
    ## [1] 1.006162
    ## [1] 1.15383
    ## [1] 1.143351
    ## [1] 0.8479608

``` r
# pred glasovanjem pripravimo matriko (st. vrstic = st. testnih primerov, st. stolpcev = st. razredov)
predMat <- matrix(0, nrow=nrow(test), ncol=nlevels(train$Class))
colnames(predMat) <- levels(train$Class)
head(predMat)
```

    ##      bus opel saab van
    ## [1,]   0    0    0   0
    ## [2,]   0    0    0   0
    ## [3,]   0    0    0   0
    ## [4,]   0    0    0   0
    ## [5,]   0    0    0   0
    ## [6,]   0    0    0   0

``` r
# vsak model klasificira testne primere in glasuje za izbrane razrede v skladu s svojo kvaliteto
for (i in 1:length(models))
{
    pred <- as.character(predict(models[[i]]$model, test, type="class"))
    for (j in 1:length(pred))
        predMat[j, pred[j]] <- predMat[j, pred[j]] + models[[i]]$quality
}

head(predMat)
```

    ##            bus      opel      saab      van
    ## [1,]  0.000000 30.299408 24.598687 54.87982
    ## [2,]  0.000000 69.855425 39.922495  0.00000
    ## [3,]  0.000000 57.713588 28.596844 23.46749
    ## [4,] 67.911256  8.626346 11.915361 21.32496
    ## [5,]  2.150993 11.622641 21.470578 74.53371
    ## [6,] 89.780954 11.176219  8.820747  0.00000

``` r
predClass <- colnames(predMat)[max.col(predMat)]
predicted <- factor(predClass, levels(vehicle$Class))

CA(test$Class, predicted)
```

    ## [1] 0.7362205

Boosting je implementiran tudi v knjiznici `adabag`.

``` r
library(adabag)
```

    ## Loading required package: rpart

    ## Loading required package: caret

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     margin

    ## Loading required package: lattice

    ## Loading required package: foreach

    ## Loading required package: doParallel

    ## Loading required package: iterators

    ## Loading required package: parallel

    ## 
    ## Attaching package: 'adabag'

    ## The following object is masked from 'package:ipred':
    ## 
    ##     bagging

``` r
bm <- boosting(Class ~ ., train, mfinal=100)
predictions <- predict(bm, test)
names(predictions)
```

    ## [1] "formula"   "votes"     "prob"      "class"     "confusion" "error"

``` r
predicted <- predictions$class
CA(test$Class, predicted)
```

    ## [1] 0.7322835
