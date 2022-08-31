setwd('/mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/')
source('02_training/config_file.r')
source('02_training/xgboostmlr3_slurm.r')

args = commandArgs(trailingOnly = T)
data = readRDS(args[1])
names(data) = make.names(names(data))
name = substr(args[2], 1, nchar(args[2]))
set.seed(seed)

xgboost.bmr.slurm(data = data,
                   name = name,
                   path = out.path,
                   filename = out.filename.xgboost,
                   cv.inner = cv.inner, 
                   cv.outer = cv.outer,
                   xg.booster = xg.booster,
                   xg.alpha = xg.alpha,
                   xg.eta = xg.eta,
                   xg.lambda = xg.lambda,
                   xg.gamma = xg.gamma,
                   xg.max_depth = xg.max_depth,
                   workers = workers
                   )
