setwd(dirname(rstudioapi::getSourceEditorContext()$path))

require(mlr3)
require(mlr3pipelines)

lm = list.files(path = "../../02_training/bestModels/")
model.list = list()
for (i in seq_along(lm)) {
  model.list[[i]] = readRDS(file = paste0("../../02_training/bestModels/",lm[i]))
}
names(model.list) = lm

ld = c("all_32_2500_rf.rds", "all_32.rds")
data.list = list()
for (i in seq_along(lm)) {
  data.list[[i]] = readRDS(file = paste0("../../02_training/toRun/features_comparaison/", ld[i]))
}
names(data.list) = ld

# Adding project ID
for (i in seq_along(data.list)) {
  project <- rownames(data.list[[i]])
  project = substr(project, 1,1 )
  data.list[[i]] <- cbind(data.list[[i]], project)
}

# Split for dataset and project
project.list.1 = list()
project.list.2 = list()
projects = unique(substr(rownames(data.list$all_32_2500_rf.rds), 1, 1))
for (i in seq_along(projects)) {
  project.list.1[[i]] = data.list$all_32_2500_rf.rds[which(data.list$all_32_2500_rf.rds$project == projects[i]),]
  project.list.1[[i]]$project <- NULL
  project.list.2[[i]]= data.list$all_32.rds[which(data.list$all_32.rds$project == projects[i]),]
  project.list.2[[i]]$project <- NULL
}
names(project.list.1) = projects
names(project.list.2) = projects

task.list.1 = list()
task.list.2 = list()
ld =substr(ld,1,nchar(ld)-4)
for (i in seq_along(project.list.1)) {
  #Making tasks for list 1
  project.list.1[[i]]$target = as.factor(project.list.1[[i]]$target)
  project.list.1[[i]][sapply(project.list.1[[i]], is.numeric)] <- lapply(project.list.1[[i]][sapply(project.list.1[[i]], is.numeric)], as.numeric)
  task = TaskClassif$new(id = paste(ld[1], "_", names(project.list.1)[i], ncol(project.list.1[[i]])-1, sep = '_'), backend = project.list.1[[i]] ,
                         target = "target", positive ="preterm")
  task$col_roles$stratum = "target"
  
  #print('Removing Constant Features')
  #rcf = po("removeconstants")
  #rcf = rcf$train(list(task = task))
  #task=rcf$output
  
  print('Normalizing Features')
  nf = po("scale")
  nf = nf$train(input = list(task))
  task = nf$output
  task.list.1[[i]] = task

  #Making tasks for list 2
  project.list.2[[i]]$target = as.factor(project.list.2[[i]]$target)
  project.list.2[[i]][sapply(project.list.2[[i]], is.numeric)] <- lapply(project.list.2[[i]][sapply(project.list.2[[i]], is.numeric)], as.numeric)
  task2 = TaskClassif$new(id = paste(ld[2], "_", names(project.list.2)[i], ncol(project.list.2[[i]])-1, sep = '_'), backend = project.list.2[[i]] ,
                         target = "target", positive ="preterm")
  task2$col_roles$stratum = "target"
  
  #print('Removing Constant Features')
  #rcf = po("removeconstants")
  #rcf = rcf$train(list(task = task2))
  #task2 = rcf$output
  
  print('Normalizing Features')
  nf = po("scale")
  nf = nf$train(input = list(task2))
  task2 = nf$output
  task.list.2[[i]] = task2
  
}
names(task.list.1) = projects
names(task.list.2) = projects


# saveRDS(project.list.2, file = '../../02_training/data/preterm_by_cohort.rds')

costs = matrix(c(0, 1, 2, 0), 2)
th = costs[2,1] / (costs[2,1] + costs[1,2])
th

thold = th
threshold = c(preterm = thold,                                                  
              term = 1 - thold)

measures = list(msr("classif.acc", id = "Accuracy"),msr("classif.auc", id = "AUCROC"),
                msr("classif.prauc", id = "PRAUC"),msr("classif.sensitivity", id = "Sensitivity"),
                msr("classif.specificity", id = "Specificity"))

pre.list.1 = list()
pre.list.2 = list()

for (i in seq_along(task.list.1)) {
  #Making predictions for tasks 1
  prediction = model.list$model_all_32_2500_rf.rds$predict(task = task.list.1[[i]])
  prediction$set_threshold(threshold = threshold)
  pre.list.1[[i]] = list(prediction$confusion, 
                         prediction$score(measures = measures))
  
  #Making predictions for tasks 2
  prediction2 = model.list$model_all_32.rds$predict(task = task.list.2[[i]])
  prediction2$set_threshold(threshold = threshold)
  pre.list.2[[i]] = list(prediction2$confusion, 
                         prediction2$score(measures = measures))
  
}
names(pre.list.1) = projects   # vi features
names(pre.list.2) = projects   # all features



# Check results!
i = 8
list(pre.list.1[[i]], pre.list.2[[i]])

head(prediction$prob)

