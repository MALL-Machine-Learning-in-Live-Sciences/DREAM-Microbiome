setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Arguments
phylotypes = T
deep = '1e0'      # 1e_1 1e0 5e_1
counts = 'relabd' # relabd nreads
level = 'genus'   # species genus family
nreps = 15        # min number of patients with tax corr > 0.5

# Load data
meta = read.csv('../../extdata/metadata/metadata.csv', header = T)
if (phylotypes == T) {
  tax = read.csv(paste0('../../extdata/phylotypes/phylotype_', 
                        counts, '.', deep, '.csv'), 
                 header = T, row.names = 1)
} else{
  tax = read.csv(paste0('../../extdata/taxonomy/taxonomy_',
                        counts, '.', level, '.csv'), 
                 header = T, row.names = 1)
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
# sum(lengths(visits))


# calculate correlation between species and collect_wk
collect.l = list()
tax.d.l =list()
corrs = list()
for (j in seq_along(visits)) {
  
  collect = meta[which(meta$participant_id == names(visits)[j]), ]$collect_wk
  tax.d = tax[match(visits[[j]], rownames(tax)),]
  res = apply(tax.d, 2, function(x) cor(x, collect))
  res = na.omit(res)
  res = res[order(abs(res), decreasing = T)]
  
  res = res[which(abs(res) > 0.5)]               # select only those species with abs(corr) > 0.5
  
  corrs[[j]] = res
  collect.l[[j]] = collect
  tax.d.l[[j]] = tax.d
  print(paste0('Calculated corr for: ', names(visits)[j]))
}
names(corrs) = names(visits)
names(collect.l) = names(visits)
names(tax.d.l) = names(visits)

# hist(unlist(corrs), 100)
# lapply(corrs, function(x) length(x))


species = substr(names((unlist(corrs))), 8, 300)
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
# head(visits)
# sum(lengths(visits))
# calculate correlation between species and collect_wk
collect.l = list()
tax.d.l =list()
corrs = list()
for (j in seq_along(visits)) {
  
  collect = meta[which(meta$participant_id == names(visits)[j]), ]$collect_wk
  tax.d = tax[match(visits[[j]], rownames(tax)),]
  res = apply(tax.d, 2, function(x) cor(x, collect))
  res = na.omit(res)
  res = res[order(abs(res), decreasing = T)]
  
  res = res[which(abs(res) > 0.5)]               # select only those species with abs(corr) > 0.5
  
  corrs[[j]] = res
  collect.l[[j]] = collect
  tax.d.l[[j]] = tax.d
  print(paste0('Calculated corr for: ', names(visits)[j]))
}
names(corrs) = names(visits)
names(collect.l) = names(visits)
names(tax.d.l) = names(visits)

# hist(unlist(corrs), 100)
# lapply(corrs, function(x) length(x))

species = substr(names((unlist(corrs))), 8, 300)
species.term = table(species)[which(table(species) > nreps)]
# names(species.term)
# plot(collect.l$A00011, tax.d.l$A00011$Ureaplasma)

#Spps
shared = intersect(x= names(species.no_term), y = names(species.term))
only.no_term = setdiff(x = names(species.no_term), y = names(species.term))
only.term = setdiff(x = names(species.term), y = names(species.no_term))
all.spp = union(x = names(species.no_term), y = names(species.term))
