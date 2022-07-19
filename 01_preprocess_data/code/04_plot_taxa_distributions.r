# See species
# =========================================
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
meta = read.csv('../../extdata/metadata/metadata.csv', 
                header = T)
tax = read.csv('../../extdata/taxonomy/taxonomy_relabd.species.csv',
               header = T, row.names = 1)
v.nt = readRDS('../../01_preprocess_data/data/visits_of_noterms.rds')
v.t = readRDS('../../01_preprocess_data/data/visits_of_terms.rds')


features = readRDS('../../01_preprocess_data/data/feature_selection/taxonomy_relabd_species.rds')
features = features$term

# Atopobium vaginae
# =====
res = list()
for (feat in features) {

  bicho = feat
  
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
  facet_wrap(~bicho) +
  geom_line() +
  xlim(10,32) +
  theme(legend.title = element_blank())






