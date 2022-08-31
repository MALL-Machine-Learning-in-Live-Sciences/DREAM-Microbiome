# Arguments
input.dir.path = '/mnt/netapp2/Store_uni/home/ulc/co/jlb/redes-tf/input/'
path.algs = '~/git/tcga_coad_surv/Sunetal/MachineLearning/'
pattern.algs = '_train.r'

ExperimentName = 'response-multiclass'

part = 'shared'
qos = 'shared_short'
time = '02:00:00'
nodes = 1
ntasks = 20

out.path = paste('/mnt/netapp2/Store_uni/home/ulc/co/jlb/redes-tf/output/', ExperimentName, '/', sep = '')
out.filename.svm='svm.rds'
out.filename.glmnet='glmnet.rds'
out.filename.rf='rf.rds'

if (dir.exists(out.path) == FALSE) {
  dir.create(out.path)
  message(paste('Creating ', ExperimentName, ' directory!'))
}


# SVM
C = 2^c(-12:12)
sigma = 2^c(-12:12)
# Glmnet
lambda = c(0.0001,0.001,0.01,0.1,1)
alpha = c(0,0.15,0.25,0.35,0.5,0.65,0.75,0.85,1)
# Random Forest
mtry = c(2:8)
ntree = 1000
nodesize = c(1:3)

cv.inner = 'Holdout'
cv.outer = 'RepCV'

predict = c('both') # train or both
iters = 10
reps = 5
folds = 3
strat= TRUE
