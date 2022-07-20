setwd(dirname(rstudioapi::getSourceEditorContext()$path))
meta = read.csv('../../extdata/metadata/metadata.csv', header = T)
# tax = read.csv('../../extdata/taxonomy/taxonomy_relabd.genus.csv', header = T, row.names = 1)
alpha = read.csv('../../extdata/alpha_diversity/alpha_diversity.csv', header = T)
tax = alpha
tax$term = meta$was_term

require(dplyr)
require(tidyverse)

tax.f = tax %>% 
  as_tibble() %>% 
  mutate(participant_id = meta$participant_id) %>% 
  group_by(participant_id, term) %>%
  summarise_all('mean')

tax.f = as.data.frame(tax.f)
rownames(tax.f) = tax.f[,1]; tax.f = tax.f[,-1] 

# Split by project
projID = substr(rownames(tax.f), 1, 1)
projects = unique(projID)
subsets = list()
for (p in seq_along(projects)) {
  sub = which(projID == projects[p])
  sub = tax.f[sub,]
  zeros = colSums(sub == 0) / nrow(sub) * 100
  subsets[[p]] = sub[, which(zeros < 90)]
}
names(subsets) = projects


features = lapply(subsets, function(x) names(x))
which(table(unlist(features)) > 5)
