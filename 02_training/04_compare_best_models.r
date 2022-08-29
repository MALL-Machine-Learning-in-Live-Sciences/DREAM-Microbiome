setwd(dirname(rstudioapi::getSourceEditorContext()$path))

lm = list.files(path = "bestModels/")
model.list = list()
for (i in seq_along(lm)) {
  model.list[[i]] = readRDS(file = paste0("bestModels/",lm[i]))
}
names(model.list) = lm

ld = c("all_32_2500_rf.rds", "all_32.rds")
data.list = list()
for (i in seq_along(lm)) {
  data.list[[i]] = readRDS(file = paste0("toRun/features_comparaison/", ld[i]))
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


thold = 0.70
threshold = c(preterm = 1-thold,
              term = thold)
pre.list.1 = list()
pre.list.2 = list()
require(mlr3pipelines)
for (i in seq_along(task.list.1)) {
  require(caret)
  require(epiR)
  #Making predictions for tasks 1
  prediction = model.list$model_all_32_2500_rf.rds$predict(task = task.list.1[[i]])
  prediction$set_threshold(threshold = threshold)
  cm = confusionMatrix(reference= factor(prediction$data$truth),
                       data = factor(prediction$data$response),
                       positive = "preterm")
  d = c(cm$table[1],cm$table[3],cm$table[2], cm$table[4])
  tests <- epi.tests(d, method = "exact", digits = 4, 
                     conf.level = 0.95)
  pre.list.1[[i]] = tests
  
  #Making predictions for tasks 2
  prediction2 = model.list$model_all_32.rds$predict(task = task.list.2[[i]])
  prediction2$set_threshold(threshold = threshold)
  cm2 = confusionMatrix(reference= factor(prediction2$data$truth),
                       data = factor(prediction2$data$response),
                       positive = "preterm")
  d2 = c(cm2$table[1],cm2$table[3],cm2$table[2], cm2$table[4])
  tests2 <- epi.tests(d2, method = "exact", digits = 4, 
                     conf.level = 0.95)
  pre.list.2[[i]] = tests2
  
}
names(pre.list.1) = projects
names(pre.list.2) = projects

#ForestPlots
projects
preterm = c(table(project.list.1$A$target)[[1]],
            table(project.list.1$C$target)[[1]],
            table(project.list.1$D$target)[[1]],
            table(project.list.1$E$target)[[1]],
            table(project.list.1$F$target)[[1]],
            table(project.list.1$G$target)[[1]],
            table(project.list.1$H$target)[[1]],
            table(project.list.1$I$target)[[1]])

term = c(table(project.list.1$A$target)[[2]],
                     table(project.list.1$C$target)[[2]],
                     table(project.list.1$D$target)[[2]],
                     table(project.list.1$E$target)[[2]],
                     table(project.list.1$F$target)[[2]],
                     table(project.list.1$G$target)[[2]],
                     table(project.list.1$H$target)[[2]],
                     table(project.list.1$I$target)[[2]])

est = c(pre.list.1$A$detail$est[3],
        pre.list.1$C$detail$est[3],
        pre.list.1$D$detail$est[3],
        pre.list.1$E$detail$est[3],
        pre.list.1$F$detail$est[3],
        pre.list.1$G$detail$est[3],
        pre.list.1$H$detail$est[3],
        pre.list.1$I$detail$est[3])

low = c(pre.list.1$A$detail$lower[3],
        pre.list.1$C$detail$lower[3],
        pre.list.1$D$detail$lower[3],
        pre.list.1$E$detail$lower[3],
        pre.list.1$F$detail$lower[3],
        pre.list.1$G$detail$lower[3],
        pre.list.1$H$detail$lower[3],
        pre.list.1$I$detail$lower[3])

up = c(pre.list.1$A$detail$upper[3],
        pre.list.1$C$detail$upper[3],
        pre.list.1$D$detail$upper[3],
        pre.list.1$E$detail$upper[3],
        pre.list.1$F$detail$upper[3],
        pre.list.1$G$detail$upper[3],
        pre.list.1$H$detail$upper[3],
        pre.list.1$I$detail$upper[3])


k = as.data.frame(cbind(projects, preterm, term, est, low, up))
k$est = as.numeric(k$est)
k$low = as.numeric(k$low)
k$up = as.numeric(k$up)
k$se <- (log(k$up) - log(k$est))/2
# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
k$` ` <- paste(rep(" ", 8), collapse = " ")

p = forestploter::forest(data = k[,c(1:3, 8:8)], est = k$est,lower = k$low,
                     upper = k$up,sizes = k$se, ci_column = 4,ref_line = 0.5,
                     xlim = c(0, 1),ticks_at= c(0.2, 0.4, 0.6, 0.8,1))
plot(p)
