setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Arguments
phylotypes = F
deep = '5e_1'     # 1e_1 1e0 5e_1
counts = 'nreads' # nreads relabd  # If use relabd we have to round and transform to Integer
level = 'family'  # species genus family
upper = 32        # T1 = 32 weeks, T2 = 28 weeks
use.median = F    # T = agglomerate visits per patient (mean of all visits)
centered = TRUE   # Center PCA
scaled = TRUE     # Scale PCA

# Load Data
print("Loading data")
meta = read.csv('../../extdata/metadata/metadata.csv', header = T, row.names = 2)
if (phylotypes == T) {
  tax = read.csv(paste0('../../extdata/phylotypes/phylotype_', 
                        counts, '.', deep, '.csv'), 
                 header = T, row.names = 1)
} else{
  tax = read.csv(paste0('../../extdata/taxonomy/taxonomy_',
                        counts, '.', level, '.csv'), 
                 header = T, row.names = 1)
}

print("Preparing data")

# I select the limit of weeks
meta.cw = meta[which(meta$collect_wk <= upper),]

# Take median of specimen or continue with counts independently
if (use.median == T){
  library(dplyr)
  print("Calculating the mean of specimens")
  participant_id = meta.cw$participant_id
  data = cbind(participant_id,tax[rownames(meta.cw),])
  data = aggregate(data[,-1], list(participant_id = data[,1]), FUN = mean)
  tax = data  %>% 
    mutate_if(is.numeric, round)
  rownames(tax) <- tax[,1]
  tax <- tax[,-1]
  meta.cw <- meta.cw %>% distinct(participant_id, .keep_all = TRUE)
  rownames(meta.cw)= meta.cw$participant_id
}else{
  print("The mean of specimens has not been selected, we proceed with all specimens. ")
}

## prefiltering step to remove OTUs for which the sum of counts are below a set
## threshold (0.01%) compared to the total sum of all counts
##  Separate the OPTUs from the specimen ID in order to filter OTUs
dim(tax_spe)
cols <- sapply(tax_spe, is.numeric)
var <- sapply(tax_spe, is.character)
var = tax_spe[var]
cols = tax_spe[cols]
cols <- which(colSums(cols)*100/(sum(colSums(cols))) > 0.01)
tax_spe <- tax_spe[, cols]


