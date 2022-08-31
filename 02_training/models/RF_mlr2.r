source('~/git/tcga_coad_surv/Sunetal/config-file.r')
source('~/git/tcga_coad_surv/Sunetal/rf_slurm.r')

args = commandArgs(trailingOnly = T)
data = readRDS(args[1])
names(data) = make.names(names(data))
name = substr(args[2], 1, nchar(args[2]))
set.seed = set.seed

rf.bmr.slurm(data = data,
             name = name,
             path = out.path,
             filename = out.filename.rf,
             cv.inner = cv.inner, 
             cv.outer = cv.outer,
             mtry = mtry,
             ntree = ntree,
             nodesize = nodesize)
