# metode za obdelavo atributov

ToDate <- function(value) as.Date(value);
ToMonth <- function(value) month(as.Date(value));
ToSeason <- function(value)
{
  numeric.date <- 100*month(value)+day(value)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}
IsWeekend <- function(value)
{
  weekday <- strftime(value,'%A')
  str_detect(weekday, "Sunday|Monday")
}
IsWinter <- function(value)
{
  ToSeason(value) == "Winter"
}

Factorize <- function (data)
{
  data[,1] <- as.Date(data[,1])
  for (i in 2:ncol(data)) {
    if (!is.numeric(data[,i])) {
      data[,i] <- as.factor(data[,i]) 
    }
  }
  data
}


# METODE ZA OCENJEVANJE KLASIFIKACIJSKIH MODELOV

Sensitivity <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  
  tab[pos.class, pos.class] / sum(tab[pos.class,])
}

Specificity <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  neg.class <- which(row.names(tab) != pos.class)
  
  tab[neg.class, neg.class] / sum(tab[neg.class,])
}

Precision <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  
  tab[pos.class, pos.class] / sum(tab[, pos.class])
}

CA <- function (observed, predicted)
{
  tab <- table(observed, predicted)
  sum(diag(tab)) / sum(tab) # zadnji izraz v funkciji se samodejno vrne kot razultat
}

BrierScore <- function(obsMat, predMat)
{
  sum((obsMat - predMat) ^ 2) / nrow(predMat)
}

InfScore <- function(trainClass, testClass, predMat)
{
  result <- 0
  
  priors <- table(trainClass) / length(trainClass)
  
  for (i in 1:nrow(predMat))
  {
    p.prior <- priors[[testClass[i]]]
    p.posterior <- predMat[i, testClass[i]]
    
    if (p.posterior >= p.prior)
      # vem vec kot prej, pristejemo kolicino informacije
      result <- result - log2(p.prior) + log2(p.posterior)
    else
      # vem manj, odstejemo
      result <- result + log2(1-p.prior) - log2(1-p.posterior)                
  }
  
  result / nrow(predMat)
}

EvaluateClassModel <- function (model, train, test)
{
  predictedMat <- predict(model, test, type="prob")
  observedMat <- class.ind(test$namembnost)
  
  observed <- test$namembnost
  predicted <- predict(model, test, type="class")
  
  brier <- BrierScore(observedMat, predictedMat)
  print(paste("Brier score:", brier))
  
  ca <- CA(observed, predicted)
  print(paste("Classification accuracy:", ca))
  
  infGain <- InfScore(train$namembnost, test$namembnost, predictedMat)
  print(paste("Information score:", infGain))
}

EvaluateClass <- function (train, test, predictedMat, observedMat)
{
  observed <- test$namembnost
  predicted <- predict(model, test, type="class")
  
  brier <- BrierScore(observedMat, predictedMat)
  print(paste("Brier score:", brier))
  
  ca <- CA(observed, predicted)
  print(paste("Classification accuracy:", ca))
  
  infGain <- InfScore(train$namembnost, test$namembnost, predictedMat)
  print(paste("Information score:", infGain))
}



# METODE ZA OCENJEVANJE REGRESIJSKIH MODELOV

# srednja absolutna napaka
MAE <- function(obs, pred)
{
  mean(abs(obs - pred))
}

# srednja kvadratna napaka
MSE <- function(obs, pred)
{
  mean((obs - pred)^2)
}

# Relativne mere ocenjujejo model v primerjavi s trivialno predikcijo

# relativna srednja absolutna napaka
RMAE <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
RMSE <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

EvaluateRegBaseModel <- function(model, train, test)
{
  predicted <- predict(model, test)
  observed <- test$poraba
  EvaluateRegModel(observed, predicted, train)
}

EvaluateRegExtModel <- function(model, train, test)
{
  predicted <- expm1(predict(model, test))
  observed <- expm1(test$poraba)
  EvaluateRegModel(observed, predicted, train)
}

EvaluateRegModel <- function(observed, predicted, train)
{
  mae <- MAE(observed, predicted)
  print(paste("Srednja absolutna napaka:", mae))
  
  mse <- MSE(observed, predicted)
  print(paste("Srednja kvadratna napaka:", mse))
  
  meanValue <- mean(train$poraba)
  trivialPred <- rep(meanValue, nrow(test))
  
  rmae <- RMAE(observed, predicted, meanValue)
  print(paste("Relativna srednja absolutna napaka:", rmae))
  
  rmse <- RMSE(observed, predicted, meanValue)
  print(paste("Relativna srednja kvadratna napaka:", rmse))
  
  plot(observed)
  points(predicted, col="red")
}

EvaluateTrivialRegModel <- function(observed, predicted)
{
  mae <- MAE(observed, predicted)
  print(paste("Srednja absolutna napaka:", mae))
  
  mse <- MSE(observed, predicted)
  print(paste("Srednja kvadratna napaka:", mse))
  
  rmae <- RMAE(observed, predicted, meanValue)
  print(paste("Relativna srednja absolutna napaka:", rmae))
  
  rmse <- RMSE(observed, predicted, meanValue)
  print(paste("Relativna srednja kvadratna napaka:", rmse))
  
  plot(observed)
  points(predicted, col="red")
}

# IZBOLJSAVA REGRESIJSKEGA MODELA Z METODO OVOJNICE

wrapper <- function(formula, dataset, trainfunc, predictfunc, evalfunc, cvfolds = 10)
{
  df <- model.frame(formula, dataset)
  n <- nrow(df)
  
  cur.formula <- paste(names(df)[1]," ~ ", sep = "")
  candidates <- names(df)[-1]
  
  global.best <- Inf
  global.formula <- cur.formula
  
  while(length(candidates))
  {
    selected.att <- 1
    local.best <- Inf
    
    bucket.id <- rep(1:cvfolds, length.out=n)
    s <- sample(1:n, n, FALSE)
    bucket.id <- bucket.id[s]
    
    for (i in 1:length(candidates))
    {
      local.formula <- paste(cur.formula, candidates[i], sep = "")
      cat("formula to evaluate:", local.formula, "...\n")
      flush.console()
      
      cv.results <- vector()
      for (j in 1:cvfolds)
      {	
        sel <- bucket.id == j
        
        model <- trainfunc(as.formula(local.formula), df[!sel,])
        predicted <- predictfunc(model, df[sel,])
        observed <- df[sel,1]
        trained <- df[!sel,1]
        
        cv.results[j] <- evalfunc(predicted, observed, trained) 
      }
      
      local.result <- mean(cv.results)
      
      if (local.result < local.best)
      {
        local.best <- local.result
        selected.att <- i
      }
    }
    
    cat("selected attribute: ", candidates[selected.att], "\n")
    
    flush.console()
    
    if (local.best < global.best)
    {
      global.formula <- paste(cur.formula, candidates[selected.att], sep = "")
      global.best <- local.best
    }
    
    cur.formula <- paste(cur.formula, candidates[selected.att], " + ", sep = "")
    candidates <- candidates[-selected.att]
  }
  
  cat("best model: estimated error = ", global.best,", selected feature subset = ", global.formula, "\n")
}

runWrapper <- function (formula, traindata)
{
  myTrainFunc <- function(formula, traindata)
  {
    rpart(formula, traindata)   
  }
  
  # Funkcija za pridobivanje napovedi modela (razredi)
  myPredictFunc <- function(model, testdata)
  {
    predict(model, testdata, type="class")
  }
  
  # Atribute lahko izberemo glede na klasifikacijsko tocnost modela
  myEvalFunc <- function(predicted, observed, trained)
  {
    # vracamo napako modela, saj wrapper minimizira vrednost ocene
    1.0 - mean(observed == predicted)   
  }
  
  set.seed(0)
  wrapper(formula, traindata, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)
}

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

runVoting <- function (modelsDataFrame, observed)
{
  predicted <- factor(voting(modelsDataFrame), levels=levels(train$namembnost))
  ca <- CA(observed, predicted)
  print(paste("Classification accuracy:", ca))
}

runWeightedVoting <- function (predProb, observed) 
{
  predClass <- colnames(predProb)[max.col(predProb)]
  predicted <- factor(predClass, levels(observed))
  ca <- CA(observed, predicted)
  print(paste("Classification accuracy:", ca))
}


runClassification <- function (formula, train, test)
{
  calcCA <- function(label, model) 
  {
    predicted <- predict(model, test, type="class")
    observed <- droplevels(test$namembnost)
    ca <- CA(observed, predicted)
    print(paste(label, "classification accuracy:", ca))
  }
  
  # trivialni klasifikator
  trivialCa <- sum(test$namembnost == "izobrazevalna") / length(test$namembnost)
  print(paste("Trivial classification accuracy:", trivialCa))
  
  # odlocitveno drevo
  dt <- rpart(formula, data=train)
  calcCA("odlocitveno drevo", dt)
  
  # odlocitveno drevo z rezanjem (manualno nastavljenim cp parametrom)
  #dt <- rpart(formula, data=train, cp=0)
  #cpTab <- printcp(dt)
  #row <- which.min(cpTab[,"xerror"])
  #th <- mean(c(cpTab[row, "CP"], cpTab[row-1, "CP"]))
  #dr <- prune(dt, cp=th)
  #calcCA(dt)
  
  # naivni bayes
  nb <- CoreModel(formula, data=train, model="bayes")
  calcCA("naivni bayes", nb)
  
  # k-najblizjih sosedov
  knn <- CoreModel(formula, data=train, model="knn", kInNN=5)
  calcCA("k-najblizjih sosedov", knn)
  
  # nakljucni gozd
  dr <- randomForest(formula, data=droplevels(train))
  calcCA("nakljucni gozd", dr)
}

drawCharts <- function (data)
{
  pie(table(data$regija), xlab="Regija")
  pie(table(data$stavba), xlab="Oznaka stavbe")
  pie(table(data$namembnost), xlab="Namembnost")
  hist(data$povrsina, xlab="Povrsina (m^2)", main="Histogram povrsine stavb")
  hist(data$poraba, xlab="Poraba (kWh)", main="Histogram porabe stavb")
  hist(data$leto_izgradnje, xlab="Leto izgradnje", main="Histogram leta izgradnje stavb")
  hist(data$temp_zraka, xlab="Temperatura zraka (°C)", main="Histogram temperature zraka")
  hist(data$temp_rosisca, xlab="Temperatura rosisca (°C)", main="Histogram temperature rosisca")
  hist(data$oblacnost, xlab="Oblacnost", main="Histogram stopnje pokritosti neba z oblaki")
  hist(data$oblacnost, xlab="Padavine (mm)", main="Histogram kolicine padavin")
  hist(data$oblacnost, xlab="Pritisk (mbar)", main="Histogram zracnega pritiska")
  hist(data$smer_vetra, xlab="Smer vetra (°)", main="Histogram smeri vetra")
  hist(data$hitrost_vetra, xlab="Hitrost vetra (m/s)", main="Histogram hitrosti vetra")
}

