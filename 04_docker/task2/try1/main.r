# Prediction
# ===
# install.packages('mlr3')
require(mlr3)
require(data.table)
require(dplyr)
require(scales)

# Paths (to change!)
#inputDir = '~/projects/DREAM-Microbiome/extdata/'
modelfile = '/usr/local/bin/model/model_28_v2.rds'
outPath = '/output/predictions.csv'

# Load data
#setwd(inputDir)

# Covariates
meta = read.csv('/input/metadata/metadata.csv', header = T, row.names = 2)
names(meta)[5] <- "collect_week"
phylo_entropy = read.csv('/input/alpha_diversity/alpha_diversity.csv', header = T, row.names = 1)
score = read.csv('/input/community_state_types/cst_valencia.csv', header = T, row.names = 1)

# Phylotypes
phylotypes1e1 = read.csv('/input/phylotypes/phylotype_relabd.1e_1.csv', header = T, row.names = 1)
colnames(phylotypes1e1) = paste(colnames(phylotypes1e1),"1e1",sep="_")
phylotypes5e1 = read.csv('/input/phylotypes/phylotype_relabd.5e_1.csv', header = T, row.names = 1)
colnames(phylotypes5e1) = paste(colnames(phylotypes5e1),"5e1",sep="_")
phylotypes1e0 = read.csv('/input/phylotypes/phylotype_relabd.1e0.csv', header = T, row.names = 1)
colnames(phylotypes1e0) = paste(colnames(phylotypes1e0),"1e0",sep="_")


# Taxonomy
family = read.csv('/input/taxonomy/taxonomy_relabd.family.csv', header = T, row.names = 1)
colnames(family) = paste(colnames(family),"f",sep="_")
genus = read.csv('/input/taxonomy/taxonomy_relabd.genus.csv', header = T, row.names = 1)
colnames(genus) = paste(colnames(genus),"g",sep="_")
species = read.csv('/input/taxonomy/taxonomy_relabd.species.csv', header = T, row.names = 1)
colnames(species) = paste(colnames(species),"s",sep="_")

identical(rownames(species), rownames(phylotypes1e0))
# Create data
data = cbind(
  meta,
  phylo_entropy,
  score,
  phylotypes1e1,
  phylotypes5e1,
  phylotypes1e0,
  family,
  genus,
  species
)
model = readRDS(modelfile)
features = model$state$train_task$feature_names
data = data[features]

# Predictions
# ====
pred = model$predict_newdata(data)
#costs = matrix(c(0, 2, 3, 0), 2)
#(thold = costs[2,1] / (costs[2,1] + costs[1,2]))
#threshold = c(preterm = thold,                                                  
#              term = 1 - thold)
#pred$set_threshold(threshold = threshold)

predictions = data.frame(
  specimen = rownames(meta),
  participant_id = meta$participant_id,
  collect_week = meta$collect_week,
  was_preterm = ifelse(pred$data$response == 'preterm', 1, 0),
  probability = pred$data$prob[,1]
)
head(predictions)

score = function(prob){
  x = abs(prob - 0.5)
  return(x)
}

predictions = predictions %>% 
  as_tibble() %>% 
  mutate(scoreProbs = unlist(sapply(probability, score)),
         scoreWeek = rescale(ntile(collect_week, 10),
                             to = c(0.5 , 1)),
         score = scoreProbs * scoreWeek)

# Remodeling predictions!
res = as.data.table(predictions)
res = res[order(-score), .SD[1,], by=participant_id]

res = subset(res, select = c(participant_id, was_preterm, probability))

names(res)[1] = 'participant'
names(res)[2] = 'was_early_preterm'
write.csv(res, file = outPath, quote = F, row.names = F)


