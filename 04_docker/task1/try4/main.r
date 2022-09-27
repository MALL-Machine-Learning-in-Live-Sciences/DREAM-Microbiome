# Prediction
# ===
require(mlr3)
require(data.table)
require(dplyr)
require(scales)

# Paths (to change!)
#inputDir = '~/git/DREAM-Microbiome/extdata/'
modelfile = '/usr/local/bin/model/all_32_byparticipant_noScaled_xgboost.rds'
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

# Load model
# ====
model = readRDS(modelfile)
features = model$state$train_task$feature_names
data = data[features]


# Load validation data
# ======
input = data
input = cbind.data.frame(
  patient_id = meta$participant_id,
  specimen_id = rownames(meta),
  collect_week = meta$collect_week,
  data
)


# Check!!!
# ===
stopifnot('patient_id' %in% colnames(input))
stopifnot('specimen_id' %in% colnames(input))
stopifnot('collect_week' %in% colnames(input))

stopifnot('bwpd' %in% colnames(input))
stopifnot('inv_simpson' %in% colnames(input))
stopifnot('Lactobacillaceae_f' %in% colnames(input))
stopifnot('Lactobacillus_g' %in% colnames(input))
stopifnot('Lactobacillus.crispatus_s' %in% colnames(input))
stopifnot('Lactobacillus.iners_s' %in% colnames(input))
stopifnot('phylo_entropy' %in% colnames(input))
stopifnot('pt__00001_1e0' %in% colnames(input))
stopifnot('pt__00001_1e1' %in% colnames(input))
stopifnot('pt__00002_5e1' %in% colnames(input))
stopifnot('pt__00003_5e1' %in% colnames(input))
stopifnot('pt__00021_1e1' %in% colnames(input))
stopifnot('quadratic' %in% colnames(input))
stopifnot('rooted_pd' %in% colnames(input))
stopifnot('shannon' %in% colnames(input))
stopifnot('unrooted_pd' %in% colnames(input))

# Functions
mean.p = function(n, c){
  res = sum(n * c) / sum(c)
  return(res)
}


# 
input = 
  input %>% 
  as_tibble() %>% 
  group_by(patient_id) %>% 
  summarise(across(-c(specimen_id, collect_week), function(x) mean.p(x, collect_week)))

input = as.data.frame(input)
rownames(input) = input$patient_id
input = subset(input, select = -c(patient_id))

# Predictions
# ====
pred = model$predict_newdata(input)
costs = matrix(c(0, 1, 2, 0), 2)
(thold = costs[2,1] / (costs[2,1] + costs[1,2]))
threshold = c(preterm = thold,                                                  
              term = 1 - thold)
pred$set_threshold(threshold = threshold)

predictions = data.frame(
  participant_id = rownames(input),
  was_preterm = ifelse(pred$data$response == 'preterm', 1, 0),
  probability = pred$data$prob[,1]
)

names(predictions)[1] = 'participant'
write.csv(predictions, file = outPath, quote = F, row.names = F)

