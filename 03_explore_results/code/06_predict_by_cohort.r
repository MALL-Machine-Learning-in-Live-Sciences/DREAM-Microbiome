setwd('~/git/DREAM-Microbiome/02_training/results/nineth-experiment/')

measures = list(msr("classif.acc", id = "Accuracy"),msr("classif.auc", id = "AUCROC"),
                msr("classif.prauc", id = "PRAUC"),msr("classif.sensitivity", id = "Sensitivity"),
                msr("classif.specificity", id = "Specificity"))

require(mlr3)
files = list.files(pattern = 'all_32')
files = files[-grep('df_iter', files)]
files = files[-grep('reduced', files)]
files = files[grep('rf', files)]


i = 4
files[i]
bmr = readRDS(files[i])
bmr$aggregate()
rr = bmr$aggregate()[learner_id == "classif.randomForest.threshold.tuned", resample_result][[1]]

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
  print(list(p$confusion))
}



# Prediction by cohort
# ===
iter = 3
data = as.data.table(bmr)
outer_learners = map(data$learner, "learner")
model = outer_learners[[iter]]
model

cohorts = readRDS('~/git/DREAM-Microbiome/02_training/data/task_preterm_by_cohort.rds')
test = cohorts$I
extPred = model$predict(task = test)
measures = list(msr("classif.acc", id = "Accuracy"),msr("classif.auc", id = "AUCROC"),
                msr("classif.prauc", id = "PRAUC"),msr("classif.sensitivity", id = "Sensitivity"),
                msr("classif.specificity", id = "Specificity"))
extPred$set_threshold(threshold) 
list(extPred$confusion, 
     extPred$score(measures = measures))



