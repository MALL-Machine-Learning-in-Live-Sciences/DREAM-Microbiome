# See species
# =========================================
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load data
# Arguments
phylotypes = F
deep = '1e_1'      # 1e_1 1e0 5e_1
counts = 'relabd' # relabd nreads
level = 'genus' # species genus family

# Load data
meta = read.csv('../../extdata/metadata/metadata.csv', header = T)
if (phylotypes == T) {
  tax = read.csv(paste0('../../extdata/phylotypes/phylotype_', 
                        counts, '.', deep, '.csv'), 
                 header = T, row.names = 1)
  features = readRDS(paste0('../../01_preprocess_data/data/feature_selection/phylotypes_',
                            deep, '.rds'))
  filename = paste0('phylotypes_', deep, '.pdf')
} else{
  tax = read.csv(paste0('../../extdata/taxonomy/taxonomy_',
                        counts, '.', level, '.csv'), 
                 header = T, row.names = 1)
  features = readRDS(paste0('../../01_preprocess_data/data/feature_selection/taxonomy_',
                            counts, '_', level, '.rds'))
  filename = paste0('taxonomy_', counts, '_', level, '.pdf')
}

outPath = '~/git/DREAM-Microbiome/01_preprocess_data/plots/'
v.nt = readRDS('../../01_preprocess_data/data/visits_of_noterms.rds')
v.t = readRDS('../../01_preprocess_data/data/visits_of_terms.rds')

names = c(rep('shared', length(features$shared)),
          rep('no_term', length(features$no_term)),
          rep('term', length(features$term)))

features = c(features$shared, features$no_term, features$term)
names(features) = names


# tax = alpha
# features = names(alpha)
# names(features) = rep('Alpha Diversity', length(features))


# Plot features
# =====
res = list()
for (feat in seq_along(features)) {

  bicho = features[feat]
  
  toPlot_noterm = list()
  for (i in names(v.nt)) {
    participant = i
    
    specimens = meta[which(meta$participant_id == participant),]$specimen
    abundance = tax[specimens,bicho]
    weeks = meta[which(meta$participant_id == participant),]$collect_wk
    
    toPlot_noterm[[i]] = data.frame(
      id = participant,
      visit = specimens,
      abundance = abundance,
      weeks = as.numeric(weeks),
      bicho = bicho,
      features = names(bicho),
      term = 'No-term'
    )
    # print(plot(weeks,
    #            abundance, 
    #            xlim = c(5,40),
    #            ylim = c(0, 0.55),
    #            title(main = paste(bicho, participant, 'no term'))))
  }
  toPlot_noterm = data.table::rbindlist(toPlot_noterm)
  
  
  toPlot_term = list()
  for (i in names(v.t)) {
    participant = i
    
    specimens = meta[which(meta$participant_id == participant),]$specimen
    abundance = tax[specimens,bicho]
    weeks = meta[which(meta$participant_id == participant),]$collect_wk
    
    toPlot_term[[i]] = data.frame(
      id = participant,
      visit = specimens,
      abundance = abundance,
      weeks = as.numeric(weeks),
      bicho = bicho,
      features = names(bicho),
      term = 'Term'
    )
    # print(plot(weeks,
    #            abundance,
    #            xlim = c(5,40),
    #            ylim = c(0, 0.77),
    #            title(main = paste(bicho, participant, 'term'))))
  }
  toPlot_term = data.table::rbindlist(toPlot_term)
  
  toPlot = rbind.data.frame(toPlot_term, toPlot_noterm)
  head(toPlot)
  
  res[[feat]] = toPlot
  
}
res = data.table::rbindlist(res)


# Plotting
require(ggplot2)
ggplot(data = res,
       aes(x = weeks, 
           y = abundance, 
           group = interaction(as.factor(id), as.factor(term)),
           color = as.factor(term))) +
  facet_wrap(bicho ~ features, scales = 'free_y') +
  geom_line() +
  xlim(10,32) +
  theme(legend.title = element_blank())


# ggsave(filename = filename,
#        path = outPath,
#        device = 'pdf',
#        width = 20, 
#        height = 20, 
#        dpi = 'retina')





