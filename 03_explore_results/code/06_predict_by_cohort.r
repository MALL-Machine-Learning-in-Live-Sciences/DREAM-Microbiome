setwd('~/git/DREAM-Microbiome/02_training/results/nineth-experiment/')

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
files = files[grep('reduced', files)]
files = files[grep('rf', files)]


i = 1
(name.train = gsub('_rf', '', files[i]))
bmr = readRDS(files[i])
bmr$aggregate()
rr = bmr$aggregate()[learner_id == "classif.randomForest.tuned", resample_result][[1]]

# aggregate
pred = rr$prediction()
list(pred$confusion, 
     pred$score(measures = measures))

costs = matrix(c(0, 2, 3, 0), 2)
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
  print(list(p$score(measures = measures)))
}



# Prediction by cohort
# ===
data = readRDS(paste0('~/git/DREAM-Microbiome/02_training/toRun/basal/', preterm, '.rds'))
signature = colnames(readRDS(paste0('~/git/DREAM-Microbiome/02_training/toRun/basal_jlb_v3/', name.train)))
data$target = as.factor(data$target)
data[,grep('NIH.', colnames(data))] = apply(data[,grep('NIH.', colnames(data))], 2, function(x) as.numeric(x))
# cohorts = readRDS('~/git/DREAM-Microbiome/02_training/data/task_preterm_by_cohort.rds')
str(data)
cohortA = data[grep('A', rownames(data)), match(signature, colnames(data))]
cohortC = data[grep('C', rownames(data)), match(signature, colnames(data))]
cohortD = data[grep('D', rownames(data)), match(signature, colnames(data))]
cohortE = data[grep('E', rownames(data)), match(signature, colnames(data))]
cohortF = data[grep('F', rownames(data)), match(signature, colnames(data))]
cohortG = data[grep('G', rownames(data)), match(signature, colnames(data))]
cohortH = data[grep('H', rownames(data)), match(signature, colnames(data))]
cohortI = data[grep('I', rownames(data)), match(signature, colnames(data))]

require(dplyr)
cohortI$id = sapply(strsplit(rownames(cohortI), '-'), '[[', 1)
I.target = cohortI %>%
  select(id, target) %>% 
  group_by(id) %>% 
  slice(1)
cohortI = cohortI %>% 
  group_by(id) %>%
  summarise(across(-target, median)) %>% 
  mutate(target = I.target$target) %>% 
  select(-id)
rownames(cohortI) = I.target$id



makeTask = function(name, data){
  task = TaskClassif$new(id = name,
                         backend = data,
                         target = "target",
                         positive ="preterm")
  return(task)
}

cohortA = makeTask('cohortA', cohortA)
cohortC = makeTask('cohortC', cohortC)
cohortD = makeTask('cohortD', cohortD)
cohortE = makeTask('cohortE', cohortE)
cohortF = makeTask('cohortF', cohortF)
cohortG = makeTask('cohortG', cohortG)
cohortH = makeTask('cohortH', cohortH)
cohortI = makeTask('cohortI', cohortI)

# get model
for (iter in 1:50) {
  data = as.data.table(bmr)
  outer_learners = map(data$learner, "learner")
  model = outer_learners[[iter]]
  model
  
  extPred = model$predict(task = cohortH)
  extPred$set_threshold(threshold) 
  print(list(
    extPred$score(measures = measures)))
}




