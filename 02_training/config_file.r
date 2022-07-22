# Config file
# ====

setwd('/mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/')

# Arguments
ExperimentName = 'first-experiment'
input.dir.path = '02_training/toRun/first_experiment/'
out.dir.path = '02_training/results/'

path.algs = '02_training/models/'
pattern.algs = '_train.r'

part = 'shared'
qos = 'shared_short'
time = '02:00:00'
nodes = 1
ntasks = 20

out.path = paste(out.dir.path, ExperimentName, '/', sep = '')
out.filename.glmnet='glmnet.rds'
out.filename.rf='rf.rds'
out.filename.xgboost = "xgboost.rds"

if (dir.exists(out.path) == FALSE) {
  dir.create(out.path)
  message(paste('Creating ', ExperimentName, ' directory!'))
}


# Glmnet
lambda = c(0.0001,0.001,0.01,0.1,1)
alpha = c(0,0.15,0.25,0.35,0.5,0.65,0.75,0.85,1)

# Random Forest
mtry = c(2:8)
ntree = 1000
nodesize = c(1:3)

# XGboost
booster = c("gbtree", "gblinear", "dart")
eta = c(0,1)
xg.lambda = c(0,1)
max_depth = c(1,20)
eval_metric = "logloss"

cv.inner = 'Holdout'
cv.outer = 'RepCV'

# Estos parámetros no los estás pasando a los modelos 
predict = c('both') # train or both
iters = 10
reps = 5
folds = 3
strat= TRUE
