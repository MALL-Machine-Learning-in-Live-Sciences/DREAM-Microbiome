# Select best model
# ===
require(tidyverse)
setwd('~/git/DREAM-Microbiome/02_training/results/eigth-experiment/')
files = list.files(pattern = 'all_32_reduced_rf')
files

bmr = readRDS(files[1])
df = readRDS(files[2])
data = as.data.table(bmr)
outer_learners = map(data$learner, "learner")


# Variable importance
# ===
models = outer_learners
imp = list()
# i = 1
for (i in seq_along(models)) {
  m = models[[i]]
  imp[[i]] = m$model[[1]]$model$importance
}
imp = as.data.frame(imp)
imp = rowSums(imp)

toPlot = data.frame(
  features = names(imp),
  imp = imp
)

toPlot = toPlot[order(toPlot$imp, decreasing = T),]

require(viridis)
plotFI = ggplot(toPlot, aes(x = reorder(features, imp), y = imp))+
  geom_segment( aes(xend=features, yend=0,), color = viridis(3)[2]) +
  geom_point( size=2, color=viridis(1)) +
  coord_flip() +
  theme_light(base_size = 16)+
  theme( axis.text=element_text(size=8),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(), 
         axis.ticks.x = element_blank(),
         legend.title=element_text(size=10), 
         legend.text=element_text(size=10)) +
  ggtitle(label = "Feature importance over all iterations") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
print(plotFI)


# Create new signatures
cohorts = readRDS('~/git/DREAM-Microbiome/02_training/data/task_preterm_by_cohort.rds')
cohorts = cohorts[-4]

f = c('collect_week',
      'score',
      'phylo_entropy',
      'NIH.Racial.Category_Black.or.African.American',
      'NIH.Racial.Category_White',
      'NIH.Racial.Category_American.Indian.or.Alaska.Native',
      'NIH.Racial.Category_Asian',
      'NIH.Racial.Category_Native.Hawaiian.or.Other.Pacific.Islander')

corres = list()
for (i in 1:7) {
  d = cohorts[[i]]$data()
  d = subset(d, select = -c(target))
  require(corrplot)
  cor = cor(d)
  corres[[i]] = cor = cor[match(toPlot$features, rownames(cor)),
                          match(f, colnames(cor))]
  
}


cormean = do.call(cbind, corres)
cormean = array(cormean, dim=c(dim(corres[[1]]), length(corres)))
cormean = apply(cormean, c(1, 2), mean, na.rm = TRUE)

colnames(cormean) = f
rownames(cormean) = toPlot$features

corrplot(cormean, method = 'square', tl.cex = .2, cl.cex = .2, tl.col = 'black')

a = rownames(cormean)[which(abs(cormean[,'collect_week']) < 0.2)]
b = rownames(cormean)[which(abs(cormean[,'score']) < 0.2)]
c = rownames(cormean)[which(abs(cormean[,'phylo_entropy']) < 0.2)]

importance = toPlot$features[which(toPlot$imp > 200)]
features = unique(c(intersect(importance, intersect(a, intersect(b, c))), f))
features1 = features[-grep('score', features)]
features2 = features[-grep('phylo_entropy', features)]

signatures = list(
  topRF = importance,
  feat1 = features,
  feat2 = features1, 
  feat3 = features2
)

signatures
saveRDS(signatures, file = '../../data/signatures.rds')

 

