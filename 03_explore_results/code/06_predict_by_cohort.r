setwd('~/git/DREAM-Microbiome/02_training/results/by_participant/')

require(mlr3)
require(mlr3pipelines)
require(mlr3misc)
require(mlr3benchmark)
require(mlr3tuning)
require(mlr3extralearners)
require(mlr3learners)
require(mlr3measures)

measures = list(msr("classif.acc", id = "Accuracy"),msr("classif.auc", id = "AUCROC"),
                msr("classif.prauc", id = "PRAUC"),msr("classif.sensitivity", id = "Sensitivity"),
                msr("classif.specificity", id = "Specificity"))

preterm = 'all_32'  #all_28 all_32

files = list.files(pattern = preterm)
files = files[-grep('df_iter', files)]
files = files[grep('noScaled', files)]
files = files[grep('xgboost', files)]


i = 1
(name.train = gsub('_xgboost', '', files[i]))
bmr = readRDS(files[i])
bmr$aggregate()
rr = bmr$aggregate()[learner_id == "classif.xgboost.tuned", resample_result][[1]]

# aggregate
pred = rr$prediction()
list(pred$confusion, 
     pred$score(measures = measures))

costs = matrix(c(0, 1, 2, 0), 2)
(thold = costs[2,1] / (costs[2,1] + costs[1,2]))
threshold = c(preterm = thold,                                                  
              term = 1 - thold)
pred2 = pred$set_threshold(threshold = threshold)
list(pred2$confusion, 
     pred2$score(measures = measures))



# by folds
preds = rr$predictions()
for (i in seq_along(preds)) {
  p = preds[[i]]
  p$set_threshold(threshold = threshold)
  print(list(p$confusion))
}



# Prediction by cohort
# ===
data = readRDS(paste0('~/git/DREAM-Microbiome/02_training/toRun/by_participant/', name.train))
# signature = colnames(readRDS(paste0('~/git/DREAM-Microbiome/02_training/toRun/basal_jlb_v3/', name.train)))
data$target = as.factor(data$target)
# data[,grep('NIH.', colnames(data))] = apply(data[,grep('NIH.', colnames(data))], 2, function(x) as.numeric(x))
# cohorts = readRDS('~/git/DREAM-Microbiome/02_training/data/task_preterm_by_cohort.rds')
str(data)
cohortA = data[grep('A', rownames(data)),]
cohortB = data[grep('B', rownames(data)),]
cohortC = data[grep('C', rownames(data)),]
cohortD = data[grep('D', rownames(data)),]
cohortE = data[grep('E', rownames(data)),]
cohortF = data[grep('F', rownames(data)),]
cohortG = data[grep('G', rownames(data)),]
cohortH = data[grep('H', rownames(data)),]
cohortI = data[grep('I', rownames(data)),]
cohortJ = data[grep('J', rownames(data)),]
# require(dplyr)
# cohortI$id = sapply(strsplit(rownames(cohortI), '-'), '[[', 1)
# I.target = cohortI %>%
#   select(id, target) %>% 
#   group_by(id) %>% 
#   slice(1)
# cohortI = cohortI %>%
#   group_by(id) %>%
#   slice(1)
# 
# cohortI = subset(cohortI, select = -c(id))

# cohortI = cohortI %>% 
#   group_by(id) %>%
#   summarise(across(-target, median)) %>% 
#   mutate(target = I.target$target) %>% 
#   select(-id)
# rownames(cohortI) = I.target$id



makeTask = function(name, data){
  task = TaskClassif$new(id = name,
                         backend = data,
                         target = "target",
                         positive ="preterm")
  return(task)
}

cohortA = makeTask('cohortA', cohortA)
cohortB = makeTask('cohortB', cohortB)
cohortC = makeTask('cohortC', cohortC)
cohortD = makeTask('cohortD', cohortD)
cohortE = makeTask('cohortE', cohortE)
cohortF = makeTask('cohortF', cohortF)
cohortG = makeTask('cohortG', cohortG)
cohortH = makeTask('cohortH', cohortH)
cohortI = makeTask('cohortI', cohortI)
cohortJ = makeTask('cohortJ', cohortJ)

cohorts = list(cohortA = cohortA,
               cohortB = cohortB,
               cohortC = cohortC, 
               cohortD = cohortD, 
               cohortE = cohortE, 
               cohortF = cohortF, 
               cohortG = cohortG, 
               cohortH = cohortH, 
               cohortI = cohortI,
               cohortJ = cohortJ)

# get model
# iter = 1
# c = 1
res = list()
allres = list()
for (iter in 1:50) {
  data = as.data.table(bmr)
  outer_learners = map(data$learner, "learner")
  model = outer_learners[[iter]]
  model
  
  for (c in seq_along(cohorts)) {
    extPred = model$predict(task = cohorts[[c]])
    extPred$set_threshold(threshold) 
    res[[c]] = data.frame(
      cohort = names(cohorts)[c],
      iter = iter,
      acc = extPred$score(measures = measures)[1],
      auc = extPred$score(measures = measures)[2],
      prauc = extPred$score(measures = measures)[3],
      sens = extPred$score(measures = measures)[4],
      spec = extPred$score(measures = measures)[5])
    r = data.table::rbindlist(res)
  }
  allres[[iter]] = r
}
allres = data.table::rbindlist(allres)


# select and save best model!
best = outer_learners[[6]]
saveRDS(best, file = paste0('~/git/DREAM-Microbiome/04_docker/task1/try4/model/', files))


