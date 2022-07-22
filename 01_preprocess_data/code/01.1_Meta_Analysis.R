setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Arguments
phylotypes = T
deep = '5e_1'     # 1e_1 1e0 5e_1
counts = 'nreads' # nreads relabd  # If use relabd we have to round and transform to Integer
level = 'family'   # species genus family
upper = 32        # T1 = 32 weeks, T2 = 28 weeks
use.median = T    # T = agglomerate visits per patient (mean of all visits)

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
# Selecciono el limite de semanas
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

# Guardo los nombres de los projects para luego
names.lists = names(table(meta.cw$project))

# les sumo 0.1 a todos para el logData de mas adelante
tax = tax + 1
# Hago dos listas:
# 1 para guardar la expresion de dichas visitas por project
# 2 para guardar metadata por project

listMatrix = list()
listPhen = list()
for (i in seq_along(names.lists)) {
  spe.ids = rownames(meta.cw[which(meta.cw$project == names.lists[i]),])
  listMatrix[[i]] = t(as.matrix(tax[spe.ids,]))
  listPhen[[i]] = meta.cw[which(meta.cw$project == names.lists[i]),]
}
# Rename lists
names(listMatrix) =  paste("Study", names.lists,  sep="")
names(listPhen) =  paste("Study", names.lists,  sep="")


# Creamos el objeto Meta 
library(DExMA)
oma = createObjectMA(listEX = listMatrix,
                     listPheno = listPhen,
                     namePheno = c(rep("was_term", length(listMatrix))),
                     expGroups = list(StudyA="False",StudyB="False",StudyC="False",
                                      StudyD="False",StudyE="False",StudyF="False",
                                      StudyG="False",StudyH="False",StudyI="False",
                                      StudyJ="False"),
                     refGroups = list(StudyA="True",StudyB="True",StudyC="True",
                                      StudyD="True",StudyE="True",StudyF="True",
                                      StudyG="True",StudyH="True",StudyI="True",
                                      StudyJ="True")
                        )

# Como a lo largo de los 10 estudios todas las especies están nombradas iguales nada de quality control
new.oma = dataLog(oma)
head(new.oma[[1]][[1]])
heterogeneityTest(new.oma)
resultsMA <- metaAnalysisDE(new.oma)
spps = resultsMA[which(resultsMA$Pval < 0.05), ]
spps = rownames(spps)


if (phylotypes == T) {
  if(use.median == T){
    saveRDS(object = spps,file = paste0("../data/MetaA/phylotypes/median_phylotypes_",deep,"_",upper,".rds"))
  }else{
    saveRDS(object = spps,file = paste0("../data/MetaA/phylotypes/phylotypes_",deep,"_",upper,".rds"))
  }}else if (phylotypes== F){
    if(use.median == T){
      saveRDS(object = spps,file = paste0("../data/MetaA/",level,"/","median","_",level,"_",upper,".rds"))
    }else{
      saveRDS(object = spps,file =  paste0("../data/MetaA/",level,"/","visits_",level,"_",upper,".rds"))
    }
  }

