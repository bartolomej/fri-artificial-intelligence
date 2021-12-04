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

EvaluateRegModel <- function(model, train, test)
{
  predicted <- predict(model, test)
  observed <- test$poraba
  
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