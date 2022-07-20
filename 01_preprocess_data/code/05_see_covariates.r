setwd(dirname(rstudioapi::getSourceEditorContext()$path))

alpha = read.csv('../../extdata/alpha_diversity/alpha_diversity.csv', header = T)
rownames(alpha) = alpha$specimen; alpha = alpha[,-1]
meta = read.csv('../../extdata/metadata/metadata.csv', header = T)


alpha$id = meta$participant_id
alpha$term = meta$was_term
alpha = alpha[which(meta$collect_wk > 30),]


require(ggpubr)
ggboxplot(subsets$E, 
          x = 'term', 
          y = 'shannon') + 
  stat_compare_means(method = 't.test')

class(alpha$bwpd)
head(alpha)
names(alpha)
