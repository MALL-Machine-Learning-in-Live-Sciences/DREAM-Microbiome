data = readRDS('d:/Users/jlinares/Downloads/all_32_reduced.rds')


patient_id = unlist(lapply(strsplit(rownames(data), '-'), '[[', 1))
specimen_id = rownames(data)
data = cbind.data.frame(
  patient_id = patient_id,
  specimen_id = specimen_id,
  data
)

mean.p = function(n, c){
  res = sum(n * c) / sum(c)
  return(res)
}

zscore = function(x){
  res = (x - mean(x))/sd(x)
  return(res)
}

require(dplyr)
data_r = 
  data %>% 
  as_tibble() %>% 
  group_by(patient_id) %>% 
  summarise(across(-c(specimen_id, collect_week, target), function(x) mean.p(x, collect_week)))

data_r = as.data.frame(data_r)
rownames(data_r) = data_r$patient_id
data_r = subset(data_r, select = -c(patient_id))

y = data %>% 
  as_tibble() %>% 
  group_by(patient_id) %>%
  select(target) %>% 
  slice(1)



# To run
setwd('~/git/DREAM-Microbiome/02_training/toRun/by_participant/')
# Zscore by patient
data_z = as.data.frame(t(apply(data_r, 1, function(x) zscore(x))))
data_z$target = y$target
saveRDS(data_z, file = 'all_32_byparticipant_zscore.rds')

# log2 by colunm and scaled
data_log = as.data.frame(apply(data_r, 2, function(x) log2(x + 1)))
data_log = scale(data_log)
data_log = cbind.data.frame(data_log, target = y$target)
saveRDS(data_log, file = 'all_32_byparticipant_log.rds')


# no scaling
data_ns = cbind.data.frame(data_r, target = y$target)
saveRDS(data_ns, file = 'all_32_byparticipant_noScaled.rds')



