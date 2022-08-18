setwd('/mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/')
source('02_training/config_file.r')

# Execute in parallel from Cesga
input.paths = dir(path = input.dir.path)
input.algs = dir(path = path.algs, pattern = pattern.algs)


for (i in 1:length(input.paths)) {
  for (j in 1:length(input.algs)) {
    
    exec.dir = '02_training/Exec/'
    outslurm.dir = '02_training/outs/'

    if (dir.exists(exec.dir) == FALSE) {
      dir.create(exec.dir)
      message('Creating directory --Exec-- ...')
    }
    
    sink(paste(exec.dir, substr(input.algs[j], 1, nchar(input.algs[j]) - 7),
               substr(input.paths[i], 1, nchar(input.paths[i])), '.sh', sep = ''))
    
    cat("#!/bin/bash \n")
    
    cat(paste("#SBATCH", "-p", part, '\n'))
    cat(paste("#SBATCH", "-t", time, '\n'))
    cat(paste("#SBATCH", paste0("--mem=",mem),'\n'))
    cat(paste("#SBATCH", "-N", nodes, '\n'))
    cat(paste("#SBATCH", "-n", ntasks, '\n'))    
    
    cat("module load cesga/2018 gcc/6.4.0 R/4.0.2", '\n')
    
    cat(paste("name=", input.paths[i], '\n', sep = ''))
    cat(paste("data=", input.dir.path, input.paths[i], '\n', sep = ''))
    cat(paste('Rscript /mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/02_training/models/', input.algs[j],  " $data", " $name", sep=''))
    
    sink(file = NULL)
    
    system(paste('sbatch ', '/mnt/netapp2/Store_uni/home/ulc/co/jlb/git/DREAM-Microbiome/02_training/Exec/', substr(input.algs[j], 1, nchar(input.algs[j])-7) ,substr(input.paths[i], 1, nchar(input.paths[i])), '.sh', sep = ''))
    
  } 
}
