# Prediction
# ===

# install.packages('mlr3')
require(mlr3)
require(data.table)
require(dplyr)
require(scales)

# Paths (to change!)
inputDir = '~/git/DREAM-Microbiome/extdata/'
modelfile = '~/git/DREAM-Microbiome/04_docker/model/model_all_32.rds'
outPath = '~/git/DREAM-Microbiome/04_docker/output/predictions.csv'

# Load data
setwd(inputDir)

# Covariates
meta = read.csv('metadata/metadata.csv', header = T)
phylo_entropy = read.csv('alpha_diversity/alpha_diversity.csv', header = T, row.names = 1)
score = read.csv('community_state_types/cst_valencia.csv', header = T, row.names = 1)

# Phylotypes
phylotypes1 = read.csv('phylotypes/phylotype_relabd.1e_1.csv', header = T, row.names = 1)
phylotypes2 = read.csv('phylotypes/phylotype_relabd.5e_1.csv', header = T, row.names = 1)
phylotypes3 = read.csv('phylotypes/phylotype_relabd.1e0.csv', header = T, row.names = 1)

# Taxonomy
specie = read.csv('taxonomy/taxonomy_relabd.species.csv', header = T, row.names = 1)
genus = read.csv('taxonomy/taxonomy_relabd.genus.csv', header = T, row.names = 1)
family = read.csv('taxonomy/taxonomy_relabd.family.csv', header = T, row.names = 1)


# Create data
data = data.frame(
   'pt__00005.1' = phylotypes3$pt__00005,
   'pt__00042' = phylotypes1$pt__00042,
   'pt__00009.1' = phylotypes2$pt__00009,
   'pt__00006.1' = phylotypes2$pt__00006,
   'pt__00005.2' = phylotypes2$pt__00005,
   'pt__00002' = phylotypes1$pt__00002,
   'pt__00003.1' = phylotypes3$pt__00003,
   'pt__00002.2' = phylotypes2$pt__00002,
   'pt__00001.1' = phylotypes3$pt__00001,
   "pt__00007.1" = phylotypes3$pt__00007,
   "pt__00001.2" = phylotypes2$pt__00001,
   "pt__00019" = phylotypes1$pt__00019,
   "pt__00021" = phylotypes1$pt__00021,
   "pt__00032" = phylotypes3$pt__00032,
   "pt__00001" = phylotypes1$pt__00001,
   
   'score' = score$score,
   'phylo_entropy' = phylo_entropy$phylo_entropy,
   'collect_week' = meta$collect_wk,
   
   # family
   'Prevotellaceae' = family$Prevotellaceae,
   'Lactobacillaceae' = family$Lactobacillaceae,
   'Bifidobacteriaceae' = family$Bifidobacteriaceae,
   'Ruminococcaceae' = family$Ruminococcaceae,
   'Veillonellaceae' = family$Veillonellaceae,
   'Lachnospiraceae' = family$Lachnospiraceae,
   'Bacteroidaceae' = family$Bacteroidaceae,
   
   # genus
   'Prevotella' = genus$Prevotella,
   'Lactobacillus' = genus$Lactobacillus,
   'Bacteroides' = genus$Bacteroides,
   'Porphyromonas' = genus$Porphyromonas,
   
   # specie
   'Prevotella.bivia' = specie$Prevotella.bivia,
   'Fenollaria.massiliensis.timonensis' = specie$Fenollaria.massiliensis.timonensis,
   'Lactobacillus.iners' = specie$Lactobacillus.iners,
   row.names = meta$specimen
   )

# Load model
model = readRDS(modelfile)

# Predictions
# ====
pred = model$predict_newdata(data)
costs = matrix(c(0, 2, 3, 0), 2)
(thold = costs[2,1] / (costs[2,1] + costs[1,2]))
threshold = c(preterm = thold,
              term = 1 - thold)
pred$set_threshold(threshold = threshold)

predictions = data.frame(
  specimen = meta$specimen,
  participant_id = meta$participant_id,
  collect_week = meta$collect_wk,
  was_preterm = ifelse(pred$data$response == 'preterm', 1, 0),
  probability = pred$data$prob[,1]
)

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

write.csv(res, file = outPath, quote = F, row.names = F)

