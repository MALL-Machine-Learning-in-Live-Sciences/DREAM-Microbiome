require(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../02_training/toRun/basal/')

files = list.files(pattern = 'all')
# data = list()
# i = 1
for (i in seq_along(files)) {
  d = readRDS(files[i])
  
  # remove patients of cohort E and I
  d = d[-grep('E', rownames(d)),]
  d = d[-grep('I', rownames(d)),]
  
  # data[[i]] = d
  saveRDS(d, file = paste0('../basal_jlb_v2/', files[i]))
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