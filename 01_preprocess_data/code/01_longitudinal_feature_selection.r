# Metadata
meta = read.csv('extdata/metadata/metadata.csv', header = T)
no.term = meta[which(meta$was_term == 'False' & meta$project != 'I'),]            # project I no has longitudinal data

# Select visits of preterm participants with more than 2 visits 
participants = names(which(table(no.term$participant_id) > 2))
visits = list()
for (i in seq_along(participants)) {
  visits[[i]] = meta[which(meta$participant_id == participants[i]), ]$specimen
}
names(visits) = participants
head(visits)

# species taxonomic data with relative abundance
tax = read.csv('extdata/taxonomy/taxonomy_relabd.species.csv', header = T, row.names = 1)
tax[1:5, 1:5]


# calculate correlation between species and collect_wk
j = 1
corrs = list()
for (j in seq_along(visits)) {
  
  collect = meta[which(meta$participant_id == names(visits)[j]), ]$collect_wk
  tax.d = tax[match(visits[[j]], rownames(tax)),]
  res = apply(tax.d, 2, function(x) cor(x, collect))
  res = na.omit(res)
  res = res[order(abs(res), decreasing = T)]
  
  res = res[which(abs(res) > 0.5)]               # select only those species with abs(corr) > 0.5
  
  corrs[[j]] = res
  print(paste0('Calculated corr for: ', names(visits)[j]))
}
names(corrs) = names(visits)


hist(unlist(corrs), 100)
lapply(corrs, function(x) length(x))


species = substr(names((unlist(corrs))), 8, 300)
table(species)[which(table(species) > 10)]




plot(collect, tax.d$Ureaplasma)
