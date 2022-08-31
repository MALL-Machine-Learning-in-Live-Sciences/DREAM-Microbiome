source('~/git/tcga_coad_surv/Sunetal/config-file.r')
source('~/git/tcga_coad_surv/Sunetal/svm_slurm.r')

args = commandArgs(trailingOnly = T)
data = readRDS(args[1])
names(data) = make.names(names(data))
name = substr(args[2], 1, nchar(args[2]))
set.seed = set.seed

svm.bmr.slurm(data = data,
              name = name,
              path = out.path,
              filename = out.filename.svm,
              cv.inner = cv.inner, 
              cv.outer = cv.outer,
              C = C,
              sigma = sigma)
