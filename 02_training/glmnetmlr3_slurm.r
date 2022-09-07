glmnet.bmr.slurm = function(data, set.seed, name, path = '', filename = '', cv.inner, cv.outer,
  gl.s, gl.nlambda,gl.lambda.min.ratio, gl.alpha,gl.devmax, workers){
  t1 = Sys.time()
  require(mlr3)
  require(mlr3verse)
  require(mlr3pipelines)
  require(future)

  print('Making task')
  data$target= as.factor(data$target)
  task = TaskClassif$new(id = paste(name, 'nfeat', ncol(data)-1, sep = '_'), backend = data ,
                         target = "target", positive ="preterm")
  task$col_roles$stratum = "target"
  
  print('Removing Constant Features')
  rcf = po("removeconstants")
  rcf = rcf$train(list(task = task))
  task=rcf$output
  
  print('Normalizing Features')
  nf = po("scale")
  nf = nf$train(input = list(task))
  task = nf$output

  print('Select Hyperparameters')
  
  # Search strategy
  tuner = tnr("grid_search", resolution = 10)
  terminator = trm("evals", n_evals = 50)
  
  # Set inner resample
  if (cv.inner == 'CV') {
    inner = rsmp("cv", folds = 10)
  } else if (cv.inner == 'RepCV'){
    inner = rsmp("repeated_cv", repeats= 5, folds = 10)
  } else{
    inner = rsmp("holdout", ratio = 0.6)
  }

  # Declare learner and measure
  learner = lrn("classif.glmnet",
                predict_type = "prob") %>>% 
    po("threshold")
  l = GraphLearner$new(learner)
  measure = msr("classif.prauc")
  

  # Hyperparameter Tuning
  psGL = ps(
    classif.glmnet.alpha = p_dbl(lower = gl.alpha[1], upper = gl.alpha[2]),
    classif.glmnet.s = p_dbl(lower = gl.s[1], upper = gl.s[2]),
    classif.glmnet.nlambda = p_int(lower = gl.nlambda, upper= gl.nlambda),
    classif.glmnet.lambda.min.ratio = p_dbl(lower = gl.lambda.min.ratio, upper = gl.lambda.min.ratio),
    threshold.thresholds = p_dbl(lower = 0.1, upper = 0.9)
  )

at = AutoTuner$new(learner = learner, resampling = inner, measure = measure,
                     terminator = terminator, tuner = tuner, search_space = psGL,
                     store_tuning_instance = FALSE, store_benchmark_result = FALSE, store_models = FALSE)
  
  print('Select outer resampling')
  
  # Set outer resample
  if (cv.outer == 'CV') {
    outer = rsmp("cv", folds = 10)
  } else if (cv.outer == 'RepCV'){
    outer = rsmp("repeated_cv", repeats= 5, folds = 10)
  } else if (cv.outer == 'Holdout'){
    outer = rsmp("holdout", ratio = 0.6)
  } else{
    outer = rsmp("loo")
  }
  set_threads(learner,n = workers)
  print('Training the model')
  bench_design = benchmark_grid(task, at, outer)
  future::plan(list(future::tweak("multisession", workers = workers),
                    future::tweak("multisession", workers = 1)))
  
  bmr = benchmark(bench_design, store_models = TRUE)
  measures = list(msr("classif.acc", id = "Accuracy"),msr("classif.auc", id = "AUCROC"),
    msr("classif.prauc", id = "PRAUC"),msr("classif.sensitivity", id = "Sensitivity"),
    msr("classif.specificity", id = "Specificity")
    )
  
  df_iter = as.data.frame(bmr$score(measures))
  cols = c("task_id", "learner_id", "iteration","Accuracy","AUCROC", "PRAUC","Sensitivity","Specificity"  )
  df_iter = df_iter[cols]
  name2save = substr(name,1,nchar(name)-4)

  saveRDS(bmr, file = paste(out.path, name2save, '_', out.filename.glmnet, sep = ''))
  saveRDS(df_iter, file = paste(out.path,"df_iter_", name2save, '_', out.filename.glmnet, sep = ''))

  t2 = Sys.time()
  runtime = t1-t2
  print(runtime)
  print(name)
  
}
