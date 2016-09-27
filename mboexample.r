library(OpenML)
library(mlr)

setOMLConfig(apikey = "")

OpenML.datasetID = 316

getMultiplexer.classif = function(){
  
  #Decision tree
  lrn1 = makeLearner("classif.rpart")
  ps1 = makeParamSet(
    makeNumericParam("cp", lower=-4, upper=-1, trafo=function(x) 10^x),
    makeIntegerParam("minsplit", lower=1, upper=7, trafo=function(x) 2^x),
    makeIntegerParam("minbucket", lower=0, upper=6, trafo=function(x) 2^x)
  )
  
  # random forest 
  lrn2 = makeLearner("classif.randomForest") 
  # random forest hyper-parameter search space
  ps2 = makeParamSet(
    makeIntegerParam("mtry", lower=1, upper=30),
    makeIntegerParam("ntree", lower=1, upper=9, trafo=function(x) 2^x)
  )
  
  # SVM 
  lrn3 = makeLearner("classif.svm")
  # SVM hyper-parameter search space
  ps3 = makeParamSet(
    makeNumericParam("cost", lower=-12, upper=12, trafo = function(x) 2^x),
    makeDiscreteParam("kernel", values = c("radial"), default = "radial"),
    makeNumericParam("gamma", lower=-12, upper=12, trafo = function(x) 2^x)
  )
  
  # Logistic regresion
  lrn4 = makeLearner("classif.multinom")
  
  
  #Gradient Boosting
  lrn5 = makeLearner("classif.gbm")
  ps5 = makeParamSet(
    makeIntegerParam("n.trees", lower=500, upper=10000),
    makeIntegerParam("interaction.depth", lower=1, upper=5),
    makeNumericParam("shrinkage", lower=-4, upper=-1, trafo = function(x) 10^x)
  )
  
  #Naive Bayes
  lrn6 = makeLearner("classif.naiveBayes")
  
  #KNN
  lrn7 = makeLearner("classif.kknn")
  ps7 = makeParamSet(
    makeIntegerParam("k", lower=1, upper=50)
  )
  
  #LDA
  lrn9 = makeLearner("classif.lda")
  
  #QDA
  lrn10 = makeLearner("classif.qda")
  
  bls = list(lrn1,lrn2,lrn3,lrn5,lrn6,lrn7,lrn4,lrn9) 
  lrn = makeModelMultiplexer(bls)
  
  ps = makeModelMultiplexerParamSet(lrn,
                                    classif.rpart = ps1,
                                    classif.randomForest = ps2,
                                    classif.svm = ps3,
                                    classif.gbm = ps5,
                                    classif.kknn = ps7
  )
  
  return(list(lrn,ps));
}


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

getregrLearnerForMBOTuning = function(){
  
  learner = makeLearner("regr.randomForest", predict.type = "se")
  learner = makeImputeWrapper(learner, classes = list(numeric = imputeMedian(), factor = imputeMode()))
  
  return (learner)
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

getMBOControl = function(budget,par.set) {

  mbo.control = makeMBOControl()
  mbo.control = setMBOControlInfill(mbo.control, crit = "eimtl") #"ei")
  mbo.control = setMBOControlInfill(mbo.control, opt = "focussearch",
                                    opt.restarts = 2L, opt.focussearch.maxit = 2L, opt.focussearch.points = 1000L)
  mbo.control = setMBOControlInfill(crit.eimtl.openmldid = OpenML.datasetID, crit.eimtl.parset = par.set)
  mbo.control = setMBOControlTermination(control=mbo.control, iters=budget)
  
  return(mbo.control)
}



#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

getdataset <- function(){
  oml.dataset = getOMLDataSet(did = OpenML.datasetID)
  return(oml.dataset)
}


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------


tuningTask = function(dataset, learner, par.set, budget, perf.measures) {
  
  
  # make a check is imputation is needed
  if (any(is.na(dataset))) {
    catf(" - Data imputation required ...")
    temp = impute(data = dataset, classes = list(numeric = imputeMean(), factor = imputeMode()))
    dataset = temp$data
  }  
  
  obj = makeClassifTask(data = dataset)
  rdesc = makeResampleDesc("CV", iters = 5L)
  
  #MBO
  mbo.control = getMBOControl(budget = budget, par.set = par.set)
  mbo.learner = getregrLearnerForMBOTuning()
  
  ctrl.mbo = mlr:::makeTuneControlMBO(mbo.control = mbo.control, learner = mbo.learner)
  
  inner = makeResampleDesc("Holdout", split=4/5)
  outer = makeResampleInstance("CV", iters=5, task=obj)
  
  tuned.learner = makeTuneWrapper(learner=learner, resampling=inner, par.set=par.set,
                                  control=ctrl.mbo, show.info=FALSE, measures=perf.measures)
  set.seed(1)
  res = resample(learner=tuned.learner, task=obj, resampling=outer,
                 extract=getTuneResult, models=TRUE, show.info = FALSE,
                 measures=perf.measures)
  return(res)
  
}

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

main = function() {
  
  library("parallelMap")
  
  parallelStart(mode="multicore", cpus=5, level="mlr.resample", show.info=FALSE)
  
	dataset <- getdataset()
  
  perf.measures = list(acc, timetrain, timepredict, timeboth)
  
  budget = 1000
  
  multiplexer.params = getMultiplexer.classif();
  
  learner = multiplexer.params[[1]]
  par.set = multiplexer.params[[2]]
  
  set.seed(1)
  mbo.result = tuningTask(dataset = dataset, learner = learner, par.set = par.set, budget = budget, 
                          perf.measures = perf.measures)
  
  print(mbo.result) 
  
  
  parallelStop() 
}


#------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


main()

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
