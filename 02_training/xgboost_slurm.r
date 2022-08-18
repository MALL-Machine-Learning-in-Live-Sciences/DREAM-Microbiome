xgboost.bmr.slurm = function(data, name, path = '', filename = '', cv.inner, cv.outer,
                            booster, eta, xg.lambda, max_depth, eval_metric){
  require(mlr)
  print('Making task')
  task =  makeClassifTask(id = paste(name, 'nfeat', ncol(data)-1, sep = '_'), data = data, target = 'target')
  
  print('Removing Constant Features')
  task = removeConstantFeatures(task)
  
  print('Normalizing Features')
  task = normalizeFeatures(task)
  
  print('Select Hyperparameters')
  
  # Search strategy
  ctrl<-makeTuneControlGrid()
  
  if (cv.inner == 'CV') {
    inner = makeResampleDesc(method = 'CV', predict = 'both', iters = 10, stratify = TRUE)
  } else if (cv.inner == 'RepCV'){
    inner = makeResampleDesc(method = 'RepCV', predict = 'both', reps = 5, folds = 10, stratify = TRUE)
  } else{
    inner = makeResampleDesc(method = 'Holdout', predict = 'both', stratify = TRUE)
  }
  # Hyperparameter Tuning
  psxGB<-makeParamSet(
    makeDiscreteLearnerParam("booster", values = booster),
    makeDiscreteLearnerParam("eta", values = eta),
    makeDiscreteLearnerParam("lambda", values = xg.lambda),
    makeDiscreteLearnerParam("max_depth", values = max_depth),
    makeDiscreteParam("eval_metric", eval_metric)
  )
  l = makeLearner("classif.xgboost", predict.type = "prob")
  lrn_xgboost<-makeTuneWrapper(l,  resampling = inner, par.set = psxGB, measures = acc, control=ctrl,  show.info = T)
  
  print('Select outer resampling')
  
  # Outer Cross-Validation
  if (cv.outer == 'CV') {
    outer = makeResampleDesc(method = 'CV', predict = 'both', iters = 10, stratify = TRUE)
  } else if (cv.outer == 'RepCV'){
    outer = makeResampleDesc(method = 'RepCV', predict = 'both', reps = 50, folds = 10, stratify = TRUE)
  } else if (cv.outer == 'Holdout'){
    outer = makeResampleDesc(method = 'Holdout', predict = 'both', stratify = TRUE)
  } else{
    outer = makeResampleDesc(method = 'LOO', predict = 'both')
  }
  print('Training the model')
  
  if (!('parallelMap' %in% installed.packages()[,"Package"])){
    message('Installing packages...')
    install.packages('parallelMap')
    library(parallelMap)
    parallelStartMulticore(48L, level = 'mlr.tuneParams')
  } else {
    library(parallelMap)
    parallelStartMulticore(48L, level = 'mlr.tuneParams')
  }
  # Benchmarking
  bmr = benchmark(lrn_xgboost, task , outer , measures =  list(auc, acc, mmce) , show.info = T , models = T)
  
  saveRDS(bmr, file = paste(out.path, name, '_', ncol(data)-1, '_', out.filename.xgboost, sep = ''))
  
  parallelStop()
  
  
}
