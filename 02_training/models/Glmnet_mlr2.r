source('~/git/tcga_coad_surv/Sunetal/config-file.r')
source('~/git/tcga_coad_surv/Sunetal/glmnet_slurm.r')
args = commandArgs(trailingOnly = T)
data = readRDS(args[1])
names(data) = make.names(names(data))
name = substr(args[2], 1, nchar(args[2]))
set.seed = set.seed

glmnet.bmr.slurm(data = data,
                 name = name,
                 path = out.path,
                 filename = out.filename.glmnet,
                 cv.inner = cv.inner, 
                 cv.outer = cv.outer,
                 lambda = lambda,
                 alpha = alpha)
