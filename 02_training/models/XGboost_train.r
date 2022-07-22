setwd('/mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/')
source('02_training/config_file.r')
source('02_training/xgboost_slurm.r')

args = commandArgs(trailingOnly = T)
data = readRDS(args[1])
names(data) = make.names(names(data))
name = substr(args[2], 1, nchar(args[2]))
set.seed = set.seed

xgboost.bmr.slurm(data = data,
                   name = name,
                   path = out.path,
                   filename = out.filename.xgboost,
                   cv.inner = cv.inner, 
                   cv.outer = cv.outer,
                   booster = booster,
                   eta = eta,
                   xg.lambda = xg.lambda,
                   max_depth = max_depth,
                   eval_metric = eval_metric
                   )