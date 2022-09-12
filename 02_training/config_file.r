# Config file
# ====

setwd('/mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/')

# Arguments
ExperimentName = 'ten-experiment'
input.dir.path = '02_training/toRun/basal_jlb_v4/'
out.dir.path = '02_training/results/'

path.algs = '02_training/models/'
pattern.algs = '_mlr3.r'

part = 'thinnodes'
qos = 'default'
time = '02:00:00'
mem = '120G'
nodes = 1
ntasks = 24

out.path = paste(out.dir.path, ExperimentName, '/', sep = '')
out.filename.glmnet='glmnet.rds'
out.filename.rf='rf.rds'
out.filename.xgboost = "xgboost.rds"

if (dir.exists(out.path) == FALSE) {
  dir.create(out.path)
  message(paste('Creating ', ExperimentName, ' directory!'))
}

#ML params
seed = 1342
workers = 24

# Glmnet
gl.alpha = c(0,1)
gl.nlambda = 500
gl.lambda.min.ratio = 10^(-2)
gl.s = c(0,1)

# Random Forest
rf.mtry = c(2,8)
rf.ntree = 400
rf.nodesize = c(1,3)

# XGboost
xg.booster = c("gbtree", "gblinear", "dart")
xg.alpha = c(0, 1)
xg.eta = c(0, 1)
xg.lambda = c(0.2, 0.8)
xg.gamma = c(0.2, 0.8)
xg.max_depth = c(3, 18)


cv.inner = 'Holdout'
cv.outer = 'RepCV'

# Estos parámetros no los estás pasando a los modelos 
predict = c('both') # train or both
iters = 10
reps = 5
folds = 3
strat= TRUE
