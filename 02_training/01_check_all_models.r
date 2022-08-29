#Check models
library(mlr3)
library(mlr3misc)
library(mlr3tuning)
library(mlr3viz)
library(viridis)
library(data.table)
library(purrr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load arguments
experiment = "results/sixth-experiment/"  #"results/first-experiment/", "results/second-experiment/",results/third-experiment/", "fourth-experiment"
type_load = "bencmark"                       # bencmark,iters  // If we want load df with iters or whole benchmark
measure = "Sensitivity"                   # "Accuracy", "AUCROC", "PRAUC","Sensitivity", "Specificity" 
alg = "rf"                           # "glmnet", "rf", "xgboost"
early = 32                                # early preterm = 28, preterm = 32
all = T                                   # F, T  
phylotypes = F                            # F, T  
deep = '1e0'                              # "1e_1", "1e0", "5e_1"
level = 'species'                          # "species", "genus", "family"

if (type_load == "iters") {
  # Function for rename without rds
  trim = function(x){
    substr(x,1,nchar(x)-4)
  }
  
  dfs = list()
  if (all == T) {
    l = list.files(path = experiment, pattern = paste0("df_iter_all_",early,"_2500" ))
    for (i in seq_along(l)) {
      dfs[[i]] = readRDS(file = paste0(experiment,l[i]))
    }
    nl =  lapply(X = l, FUN = trim)
    names(dfs) = unlist(nl)
    
  }else if (phylotypes == T){
    l = list.files(path = experiment, pattern = paste0("df_iter_phylotypes_",deep,"_",early))
    for (i in seq_along(l)) {
      dfs[[i]] = readRDS(file = paste0(experiment,l[i]))
    }
    nl =  lapply(X = l, FUN = trim)
    names(dfs) = unlist(nl)
    
  }else if(all == F & phylotypes == F) {
    l = list.files(path = experiment, pattern = paste0("df_iter_taxonomy_relabd_",level,"_", early))
    for (i in seq_along(l)) {
      dfs[[i]] = readRDS(file = paste0(experiment,l[i]))
    }
    nl =  lapply(X = l, FUN = trim)
    names(dfs) = unlist(nl)
    
  }
  print(paste("Dataframe(s) loaded:"))
  print(unlist(nl))
  
  # Unlist dfs and merge in a single df for plotting
  dfs2 <- lapply(dfs,function(x) rbind(colnames=colnames(x),x))
  df <- Reduce(function(x,y) merge(x,y,all=T),dfs2)
  df = df[!df$task_id == "task_id", ]
  num.cols = c("iteration","Accuracy", "AUCROC", "PRAUC","Sensitivity", "Specificity" )
  df[num.cols] = lapply(df[num.cols], as.numeric)
  
  
  ### Plot without the average per repetition 
  library(ggplot2)
  library(ggpubr)
  p1 = ggplot(df, aes_string(x="learner_id", y=measure, fill="learner_id")) +
    geom_jitter(aes_string(color="learner_id"))+
    geom_violin(color = "darkred", show.legend = TRUE) +
    theme(axis.title.y= element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks = element_blank())+
    stat_compare_means(label.x.npc = 0.5) + ggtitle(label = paste(measure,"plot without the average per repetition" )) +
    theme(plot.title = element_text(hjust = 0.5))
  p1 = change_palette(p = p1, palette = viridis(length(table(df$learner_id))))
  print(p1)
  
  
  ### Plot with the average per repetition
  fact = length(table(df$learner_id))
  reps = rep(c(rep('1r', 10*fact),rep('2r', 10*fact),rep('3r', 10*fact),rep('4r', 10*fact),rep('5r', 10*fact),
               rep('6r', 10*fact),rep('7r', 10*fact),rep('8r', 10*fact),rep('9r', 10*fact),rep('10r', 10*fact),
               rep('11r', 10*fact),rep('12r', 10*fact),rep('13r', 10*fact),rep('14r', 10*fact),rep('15r', 10*fact),
               rep('16r', 10*fact),rep('17r', 10*fact),rep('18r', 10*fact),rep('19r', 10*fact),rep('20r', 10*fact),
               rep('21r', 10*fact),rep('22r', 10*fact),rep('23r', 10*fact),rep('24r', 10*fact),rep('25r', 10*fact),
               rep('26r', 10*fact),rep('27r', 10*fact),rep('28r', 10*fact),rep('29r', 10*fact),rep('30r', 10*fact),
               rep('31r', 10*fact),rep('32r', 10*fact),rep('33r', 10*fact),rep('34r', 10*fact),rep('35r', 10*fact),
               rep('36r', 10*fact),rep('37r', 10*fact),rep('38r', 10*fact),rep('39r', 10*fact),rep('40r', 10*fact),
               rep('41r', 10*fact),rep('42r', 10*fact),rep('43r', 10*fact),rep('44r', 10*fact),rep('45r', 10*fact),
               rep('46r', 10*fact),rep('47r', 10*fact),rep('48r', 10*fact),rep('49r', 10*fact),rep('50r', 10*fact)))
  
  df.orded =df[order(df$iteration),]
  df.reps = cbind.data.frame(df.orded, reps)
  library(plyr)
  df.all = ddply(df.reps, .(task_id, learner_id, reps), summarize,
                  Accuracy = mean(Accuracy),AUCROC = mean(AUCROC),
                  PRAUC = mean(PRAUC), Sensitivity = mean(Sensitivity), Specificity = mean(Specificity))
  
  p2 = ggplot(df.all, aes_string(x="learner_id", y=measure, fill="learner_id")) +
    geom_jitter(aes_string(color="learner_id"))+
    geom_violin(color = "darkred", show.legend = TRUE) +
    theme(axis.title.y= element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks = element_blank())+
    stat_compare_means(label.x.npc = 0.5) + ggtitle(label = paste(measure,"plot with the average per repetition" )) +
    theme(plot.title = element_text(hjust = 0.5))
  p2 = change_palette(p = p2, palette = viridis(length(table(df.all$learner_id))))
  print(p2)
  #View(df.orded[which(df.orded$learner_id == "classif.xgboost.tuned"),])
  #View(df.orded[which(df.orded$learner_id == "classif.randomForest.tuned"),])
  #View(df.orded[which(df.orded$learner_id == "classif.glmnet.tuned"),])
  
}else if(type_load == "bencmark"){
  
  if (all == T) {
    bch = readRDS(paste0(experiment,"all_", early, "_", alg, ".rds"))
  }else if (phylotypes == T){
    bch = readRDS(paste0(experiment,"phylotypes_", deep, "_", early, "_", alg, ".rds"))
  }else if(all == F & phylotypes == F) {
    bch = readRDS(paste0(experiment,"taxonomy_relabd_", level, "_", early, "_", alg, ".rds"))
  }
  print(paste("Benchmark from task", bch$tasks$task_id,"with algorithm",alg, "selected"))
  
########################################
# We clarify the measures of interest 
measures = list(msr("classif.acc", id = "Accuracy"),msr("classif.auc", id = "AUCROC"),
                msr("classif.prauc", id = "PRAUC"),msr("classif.sensitivity", id = "Sensitivity"),
                msr("classif.specificity", id = "Specificity")
)

# Declare algorithm id for model extraction
if (alg == "glmnet") {
  alg_id = "classif.glmnet.tuned"
}else if(alg == "rf"){
  alg_id = "classif.randomForest.tuned"
}else if(alg == "xgboost"){
  alg_id = "classif.xgboost.tuned"
}

## Extract Feature Importance 
#bch = readRDS("results/third-experiment/taxonomy_relabd_species_32_xgboost.rds")
data = as.data.table(bch)
outer_learners = map(data$learner, "learner")
if (alg_id == "classif.xgboost.tuned") {
  #imp = as.data.frame(best_tuned_model$importance()) # Esta funciona para xgboost
  l = list()
  for (i in seq_along(outer_learners)) {
    imp <- tryCatch(
      {
        as.data.frame(outer_learners[[i]]$importance())
      },error=function(cond) {
        message(paste("This models hasnt feature importance"))
      }
    )
    if (is.null(imp) == FALSE){
      l[[i]] = data.frame(imp)
    } else{
    }
  }
  
  # Filter models without Feature importance
  ls = Filter(Negate(is.null), l)
  
  # Map from list to al huge dataframe
  library(tidyverse)
  df_all = ls %>% map_df(rownames_to_column, 'features')
  
  # Sum all importance 
  df_imp = df_all %>% 
    group_by(features) %>% 
    summarise(imp.total = sum(outer_learners..i...importance..))
  
}else if (alg_id == "classif.randomForest.tuned"){
  l = list()
  for (i in seq_along(outer_learners)) {
    imp <- tryCatch(
      {
        as.data.frame(outer_learners[[i]]$model$importance) # Esta funciona para rf
      },error=function(cond) {
        message(paste("This models hasnt feature importance"))
      }
    )
    if (is.null(imp) == FALSE){
      l[[i]] = data.frame(imp)
    } else{
    }
  }
  # Filter models without Feature importance
  ls = Filter(Negate(is.null), l)
  
  # Map from list to al huge dataframe
  library(tidyverse)
  df_all = ls %>% map_df(rownames_to_column, 'features')
  
  # Sum all importance 
  df_imp = df_all %>% 
    group_by(features) %>% 
    summarise(imp.total = sum(MeanDecreaseGini))
  
}else if (alg_id == "classif.glmnet.tuned"){
  l = list()
  for (i in seq_along(outer_learners)) {
    model = outer_learners[[i]]$model
    res = sapply(seq_len(nrow(model$beta)), function(i) {
      ind = which(model$beta[i,] != 0)[1]
      model$lambda[ind]
    })
    names(res) = model$beta@Dimnames[[1]]
    sort(res, decreasing = TRUE)
    res = as.data.frame(res)
    res[is.na(res)] <- 0
    # Save df with betas
    l[[i]] = res
  }
  # Filter models without Feature importance
  ls = Filter(Negate(is.null), l)
  
  # Map from list to al huge dataframe
  library(tidyverse)
  df_all = ls %>% map_df(rownames_to_column, 'features')
  
  # Sum all importance 
  df_imp = df_all %>% 
    group_by(features) %>% 
    summarise(imp.total = sum(res)) 
}


plotFI = ggplot(df_imp, aes(x = reorder(features, imp.total), y = imp.total))+
  geom_segment( aes(xend=features, yend=0,), color = viridis(3)[2]) +
  geom_point( size=2, color=viridis(1)) +
  coord_flip() +
  theme_light(base_size = 16)+
  theme( axis.text=element_text(size=8),axis.title.y = element_blank(),axis.title.x = element_blank(), axis.ticks.x = element_blank(),legend.title=element_text(size=10), 
         legend.text=element_text(size=10))+ ggtitle(label = paste(alg," feature importance over all iterations")) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
print(plotFI)

print("Average result of the 500 iterations:")
z = bch$aggregate(measures)
print(z)

}
bch = readRDS("results/sixth-experiment/all_32_2500_rf_rf.rds")
data = as.data.table(bch)
outer_learners = map(data$learner, "learner")
iter = 4
model = outer_learners[[iter]]
saveRDS(object = model, file = "bestModels/model_all_32_2500_rf.rds")


#all_32 = readRDS("toRun/more_10_cweek/all_32.rds")
#thold = 0.00025
#all_32_FCBF = FCBF.FS(data = all_32, thold = thold)
#saveRDS(object = all_32_FCBF,file = "toRun/features_comparaison/all_32_FCBF.rds")
#a = df_imp[which(df_imp$imp.total > 5000),]
#a = c(a$features, "target")
#all_32_5000_rf = all_32[a]
#saveRDS(object = all_32_5000_rf,file = "toRun/features_comparaison/all_32_5000_rf.rds")
#b = df_imp[which(df_imp$imp.total > 2500),]
#b = c(b$features, "target")
#all_32_2500_rf = all_32[b]
#saveRDS(object = all_32_2500_rf,file = "toRun/features_comparaison/all_32_2500_rf.rds")




