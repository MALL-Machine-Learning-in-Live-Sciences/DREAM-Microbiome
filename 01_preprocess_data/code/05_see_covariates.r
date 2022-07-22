setwd(dirname(rstudioapi::getSourceEditorContext()$path))

alpha = read.csv('../../extdata/alpha_diversity/alpha_diversity.csv', header = T)
rownames(alpha) = alpha$specimen; alpha = alpha[,-1]

valencias = read.csv('../../extdata/community_state_types/cst_valencia.csv')
rownames(valencias) = valencias$specimen; valencias = valencias[,-1]

meta = read.csv('../../extdata/metadata/metadata.csv', header = T)


alpha$id = meta$participant_id
alpha$term = meta$was_term
# alpha = alpha[which(meta$collect_wk > 30),]

valencias$id = meta$participant_id
valencias$term = meta$was_term


require(dplyr)
require(tidyverse)

valencias.f = valencias %>% 
  as_tibble() %>% 
  group_by(id, term, CST, subCST) %>%
  summarise_all('median')


require(ggpubr)
ggboxplot(valencias, 
          x = 'CST', 
          y = 'score') + 
  stat_compare_means()

table(valencias$CST, valencias$term)


class(alpha$bwpd)
head(alpha)
names(alpha)


corrplot::corrplot(cor(alpha))
