require(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../02_training/toRun/basal_jlb_v2/')

signatures = readRDS('~/git/DREAM-Microbiome/02_training/data/signatures.rds')

files = list.files(pattern = 'all')
# data = list()
# i = 1
for (i in seq_along(files)) {
  d = readRDS(files[i])
  
  # select signatures
  d1 = d[, c(signatures[[1]], 'target')]
  d2 = d[, c(signatures[[2]], 'target')]
  d3 = d[, c(signatures[[3]], 'target')]
  d4 = d[, c(signatures[[4]], 'target')]
  
  saveRDS(d1, file = paste0('../basal_jlb_v3/', 'd1_', files[i]))
  saveRDS(d2, file = paste0('../basal_jlb_v3/', 'd2_', files[i]))
  saveRDS(d3, file = paste0('../basal_jlb_v3/', 'd3_', files[i]))
  saveRDS(d4, file = paste0('../basal_jlb_v3/', 'd4_', files[i]))
}


all28 = readRDS('../basal_jlb_v2/all_28.rds')
all32 = readRDS('../basal_jlb_v2/all_32.rds')

table(all28$target)
table(all32$target)

all28 = all28[which(all28$collect_week < 28),]
all32 = all32[which(all32$collect_week < 32),]

table(all28$target)
table(all32$target)

saveRDS(all28, file = '../basal_jlb_v2/all_28_reduced.rds')
saveRDS(all32, file = '../basal_jlb_v2/all_32_reduced.rds')