require(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../02_training/toRun/basal/')

files = list.files()
# data = list()
# i = 1
for (i in seq_along(files)) {
  d = readRDS(files[i])
  
  # remove patients of cohort E
  d = d[-grep('E', rownames(d)),]
  
  # calculate median in cohort I
  I = d[grep('I', rownames(d)),]
  I$id = sapply(strsplit(rownames(I), '-'), '[[', 1)
  I.target = I %>%
    select(id, target) %>% 
    group_by(id) %>% 
    slice(1)
  I = I %>% 
    group_by(id) %>%
    summarise(across(-target, median)) %>% 
    mutate(target = I.target$target) %>% 
    select(-id)
  rownames(I) = I.target$id
  d = d[-grep('I', rownames(d)),]
  d = rbind.data.frame(d, I)
  
  # data[[i]] = d
  saveRDS(d, file = paste0('../basal_jlb_v2/', files[i]))
}
