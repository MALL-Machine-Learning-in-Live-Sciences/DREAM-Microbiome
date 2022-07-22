# Create datasets to run
# ===
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
outDir = '../../02_training/toRun/'

# Arguments
phylotypes = T
deep = '1e_1'      # 1e_1 1e0 5e_1
counts = 'relabd' # relabd nreads
level = 'family' # species genus family
nreps = 0.2       # percentage of patients with feature corr > corr
corr = 0.5        # min abs correlation value 
early = 32        # 28 32

# Load data
meta = read.csv('../../extdata/metadata/metadata.csv', header = T)
valencias = read.csv('../../extdata/community_state_types/cst_valencia.csv', row.names = 1)
alpha = read.csv('../../extdata/alpha_diversity/alpha_diversity.csv', row.names = 1)

if (phylotypes == T) {
  tax = read.csv(paste0('../../extdata/phylotypes/phylotype_', 
                        counts, '.', deep, '.csv'), 
                 header = T, row.names = 1)
  long.feats = readRDS(
    paste0('../../01_preprocess_data/data/feature_selection/phylotypes_',
           deep, '.rds'))$all
  de.feats = readRDS(
    paste0('../../01_preprocess_data/data/feature_selection/phylotypes_',
           deep, '_', early, '.rds'))
  outPath = paste0('phylotypes_', deep, '_', early, '.rds')
} else{
  tax = read.csv(paste0('../../extdata/taxonomy/taxonomy_',
                        counts, '.', level, '.csv'), 
                 header = T, row.names = 1)
  long.feats = readRDS(
    paste0('../../01_preprocess_data/data/feature_selection/taxonomy_',
           counts, '_', level, '.rds'))$all
  de.feats = readRDS(
    paste0('../../01_preprocess_data/data/feature_selection/taxonomy_',
           counts, '_', level, '_', early, '.rds'))
  outPath = paste0('taxonomy_', counts, '_', level, '.rds')
}


# Unique
# ===
features = unique(long.feats, de.feats)

tax = tax[,match(features, colnames(tax))]

res = data.frame(
  # valencias
  score = valencias$score,
  
  # alpha diversity
  phylo_entropy = alpha$phylo_entropy,
  
  # metadata
  collect_week = meta$collect_wk,
  delivery_week = meta$delivery_wk,
  NIH.Racial.Category = meta$NIH.Racial.Category,
  
  # metagenomic
  tax,
  row.names = meta$specimen
)
outPath = paste0(outDir, outPath)
rm(list = setdiff(ls(), c("res", "early", "outPath")))

if (early == 28) {
  res$target = ifelse(res$delivery_week < 32, 'preterm', 'term')
  res = res[,-grep('delivery_week', colnames(res))]
} else if(early == 32){
  res$target = ifelse(res$delivery_week < 37, 'preterm', 'term')
  res = res[,-grep('delivery_week', colnames(res))]
}

saveRDS(res, file = outPath)



