setwd(dirname(rstudioapi::getSourceEditorContext()$path))
outDir = '../../01_preprocess_data/data/feature_selection/'

# Arguments
phylotypes = F
deep = '1e0'      # 1e_1 1e0 5e_1
counts = 'relabd' # relabd nreads
level = 'species' # species genus family
nreps = 20        # min number of patients with tax corr > 0.5
corr = 0.5        # min abs correlation value 
pvalue = 0.05

# Load data
meta = read.csv('../../extdata/metadata/metadata.csv', header = T)
if (phylotypes == T) {
  tax = read.csv(paste0('../../extdata/phylotypes/phylotype_', 
                        counts, '.', deep, '.csv'), 
                 header = T, row.names = 1)
  outPath = paste0('phylotypes_', deep, '.rds')
} else{
  tax = read.csv(paste0('../../extdata/taxonomy/taxonomy_',
                        counts, '.', level, '.csv'), 
                 header = T, row.names = 1)
  outPath = paste0('taxonomy_', counts, '_', level, '.rds')
}


# NO TERM
# =================
no.term = meta[which(meta$was_term == 'False' & meta$project != 'I'),]            # project I no has longitudinal data
# no.term = no.term[which(no.term$collect_wk >= 20 & no.term$collect_wk <= 32),]    # Select samples collected beetween 25 -30 weeks
participants = names(which(table(no.term$participant_id) > 2))                    # Select visits of preterm participants with more than 2 visits 
# ex.participants = names(which(table(no.term$participant_id)>= 2))                 # Select visits of preterm participants with equal or less than 2 visits 


visits = list()
for (i in seq_along(participants)) {
  visits[[i]] = meta[which(meta$participant_id == participants[i]), ]$specimen
}
names(visits) = participants
saveRDS(visits, file = '../../01_preprocess_data/data/visits_of_noterms.rds')


# calculate correlation between species and collect_wk
corrs.noterm = list()
for (j in seq_along(visits)) {
  
  collect = meta[which(meta$participant_id == names(visits)[j]), ]$collect_wk
  tax.d = tax[match(visits[[j]], rownames(tax)),]
  res = apply(tax.d, 2, function(x) cor(x, collect, method = 'spearman'))
  res = na.omit(res)
  res = res[order(abs(res), decreasing = T)]
  
  res = res[which(abs(res) > corr)]               # select only those species with abs(corr) > 0.5
  # res = res[which(res < pvalue)]
  
  corrs.noterm[[j]] = res
  print(paste0('Calculated corr for: ', names(visits)[j]))
}
names(corrs.noterm) = names(visits)

# hist(unlist(corrs), 100)
# lapply(corrs, function(x) length(x))


species = substr(names((unlist(corrs.noterm))), 8, 300)
species.no_term = table(species)[which(table(species) > nreps)]
# names(species.no_term)
# plot(collect.l$A00008, tax.d.l$A00008$Fenollaria.massiliensis)




# TERM
# =================
term = meta[which(meta$was_term == 'True' & meta$project != 'I'),] # project I no has longitudinal data
# term = term[which(term$collect_wk >= 20 & term$collect_wk <= 32),] 
participants = names(which(table(term$participant_id) > 2)) # Select visits of preterm participants with more than 2 visits 
# ex.participants = names(which(table(term$participant_id)>= 2))# Select visits of preterm participants with equal or less than 2 visits 

visits = list()
for (i in seq_along(participants)) {
  visits[[i]] = meta[which(meta$participant_id == participants[i]), ]$specimen
}
names(visits) = participants
saveRDS(visits, file = '../../01_preprocess_data/data/visits_of_terms.rds')

# calculate correlation between species and collect_wk
corrs.term = list()
for (j in seq_along(visits)) {
  
  collect = meta[which(meta$participant_id == names(visits)[j]), ]$collect_wk
  tax.d = tax[match(visits[[j]], rownames(tax)),]
  res = apply(tax.d, 2, function(x) cor(x, collect, method = 'spearman'))
  res = na.omit(res)
  res = res[order(abs(res), decreasing = T)]
  
  res = res[which(abs(res) > corr)]               # select only those species with abs(corr) > 0.5
  # res = res[which(res < pvalue)]
  
  corrs.term[[j]] = res
  print(paste0('Calculated corr for: ', names(visits)[j]))
}
names(corrs.term) = names(visits)

# hist(unlist(corrs), 100)
# lapply(corrs, function(x) length(x))

species = substr(names((unlist(corrs.term))), 8, 300)
species.term = table(species)[which(table(species) > nreps)]
# names(species.term)
# plot(collect.l$A00011, tax.d.l$A00011$)

#Spps
shared = intersect(x= names(species.no_term), y = names(species.term))
only.no_term = setdiff(x = names(species.no_term), y = names(species.term))
only.term = setdiff(x = names(species.term), y = names(species.no_term))
all.spp = union(x = names(species.no_term), y = names(species.term))


result = list(
  shared = shared,
  no_term = only.no_term,
  term = only.term,
  all = all.spp
)

saveRDS(result, file = paste0(outDir, outPath))