d <- read.table("trainset.txt", header=T, sep=",")

stavbe <- d[1:100,]
head(stavbe)
summary(stavbe)

for (i in list(2,4))
  stavbe[,i] <- as.factor(stavbe[,i])

# generiramo ucno in testno mnozico

divideDataSet <- function (data, trainRatio = 0.7)
{
  sel <- sample(1:nrow(data), as.integer(nrow(data) * trainRatio), replace=F)
  train <- stavbe[sel,]
  test <- stavbe[-sel,]
  list(train=train, test=test)
}

dataset <- divideDataSet(stavbe, 0.1)

dataset$train

scatter.smooth(x=dataset$train$povrsina, y=dataset$train$poraba, col="lightblue", xlab="Povrsina", ylab="Poraba")

library(rpart)
dt <- rpart(namembnost ~ ., data=train) # zgradimo odlocitveno drevo (decision tree)
rpart.plot(dt)


# priprava in evaluacija atributov

library(lubridate)

toMonth <- function(date) month(as.Date(date));
toSeason <- function(input.date)
{
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

evalClassFeatures <- function (formula, data)
{
  shortSighted <- list("InfGain", "GainRatio", "Gini", "MDL")
  nonShortSighted <- list("Relief", "ReliefFequalK", "ReliefFexpRank")
  estimators <- c(shortSighted, nonShortSighted)
  for (estimator in estimators) {
    pie(sort(attrEval(formula, data, estimator), decreasing=T), main=estimator)
  }
}

table(getSeason(stavbe$datum))

sort(attrEval(namembnost ~ ., dataset$test, "Relief"), decreasing=T)

stavbe$letni_cas <- toSeason(stavbe$datum)
stavbe$mesec <- toSeason(stavbe$datum)

a <- attrEval(poraba ~ povrsina, dataset$train, "InfGain")

r1 <- lm(poraba ~ regija, dataset$train)
r2 <- lm(poraba ~ mesec, dataset$train)

hist(dataset$test$poraba)

predicted <- predict(r1, dataset$test)
observed <- dataset$test$poraba
plot(observed)
points(predicted, col="red")

summary(dataset$train)

dataset$train$datum



plot(stavbe)
