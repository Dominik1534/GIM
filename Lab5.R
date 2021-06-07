
install.packages("C50")
install.packages("mlr")
install.packages("rFerns")
install.packages("randomForestSRC")
install.packages("kernlab")
install.packages("cmaes")
install.packages("ranger")
library(C50)
library(mlr)
library(MASS)
library(randomForestSRC)
library(kernlab)
library(cmaes)
library(ranger)
##Zadanie 1
Melanoma$status<-as.factor(Melanoma$status)

built_tree<-C5.0(status~.,data=Melanoma)

summary(built_tree)

plot(built_tree)

##Zadanie 2

#ramka danych
smartfony <- read.csv('smartfony.csv')

summary(smartfony)

#!!!!!!!!!!!!!!!!!! MINIMAX ML !!!!!!!!!!!!!!!!!
data = smartfony[4:9]

## Define the task
regr.task = makeRegrTask(id = "Surviral_prediction", data = data, target = "ocena_klienta")

# tuning parameters
set.seed(1234)

# Define a search space for each learner'S parameter

ps_ksvm = makeParamSet(
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)

ps_rf = makeParamSet(
  makeIntegerParam("num.trees", lower = 1L, upper = 200L)
)

# Choose a resampling strategy
rdesc = makeResampleDesc("CV", iters = 3L)

# Choose a performance measure
#meas = rmse
#meas = timepredict
#meas = list(ber, acc)
#meas = list(rmse, timepredict)
meas = list(rmse, timetrain)


# Choose a tuning method
ctrl = makeTuneControlCMAES(budget = 100L)

# Make tuning wrappers
tuned.ksvm = makeTuneWrapper(learner = "regr.ksvm", resampling = rdesc, par.set = ps_ksvm, control = ctrl, show.info = FALSE)
tuned.rf = makeTuneWrapper(learner = "regr.ranger", resampling = rdesc, par.set = ps_rf, control = ctrl, show.info = FALSE)

## Benchmark experiment

# https://mlr.mlr-org.com/articles/tutorial/benchmark_experiments.html

# Three learners to be compared
lrns = list(makeLearner("regr.lm"), tuned.ksvm, tuned.rf)
#lrns = list(tuned.ksvm, tuned.rf)

# Conduct the benchmark experiment
#bmr = benchmark(learners = lrns, tasks = regr.task, resamplings = rdesc, measures = rmse, 
#                show.info = FALSE)
bmr = benchmark(learners = lrns, tasks = regr.task, resamplings = rdesc, measures = meas, show.info = TRUE)



## Performance

getBMRAggrPerformances(bmr)

#plotBMRBoxplots(bmr)

plotBMRBoxplots(bmr,style="box") + theme_bw(base_size = 16)
## 
plotBMRBoxplots(bmr, rmse, style = "box") + theme_bw(base_size = 16)
plotBMRBoxplots(bmr, timetrain, style = "box") + theme_bw(base_size = 16)
plotBMRBoxplots(bmr, rmse, style = "violin") + theme_bw(base_size = 16)
plotBMRBoxplots(bmr, timetrain, style = "violin") + theme_bw(base_size = 16)
plotBMRRanksAsBarChart(bmr, pos = "stack")




