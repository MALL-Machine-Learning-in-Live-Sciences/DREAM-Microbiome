# Upset plot comparaison for FCBF features per cohort
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path = "../../02_training/toRun/exact_visits/"
lf = list.files(path = path, pattern = "all")
l = list()
for (i in seq_along(lf)) {
  l[[i]] = readRDS(file = paste0(path,lf[i]))
}
#Function for rename without rds
trim = function(x){
  substr(x,1,nchar(x)-4)
}
n =  lapply(X = lf, FUN = trim)
names(l) = unlist(n)

# Apply FCBF on 
all28 = l$all_28
all32 = l$all_32

ad = read.csv(file = "../../extdata/alpha_diversity/alpha_diversity.csv", 
             header = T, row.names = 1)

val = read.csv(file = "../../extdata/community_state_types/cst_valencia.csv", 
              header = T, row.names = 1)

meta = read.csv(file = "../../extdata/metadata/metadata.csv", 
               header = T, row.names = 2)
# Retain only 2 cols, cause we have already metadata in other dfs
maintain = c("project", "collect_wk")
meta = meta[,maintain]

p_1 = read.csv(file = "../../extdata/phylotypes/phylotype_relabd.1e_1.csv",
               header = T, row.names = 1)
p_0 = read.csv(file = "../../extdata/phylotypes/phylotype_relabd.1e0.csv",
               header = T, row.names = 1)
p_5 = read.csv(file = "../../extdata/phylotypes/phylotype_relabd.5e_1.csv",
               header = T, row.names = 1)
fam = read.csv(file = "../../extdata/taxonomy/taxonomy_relabd.family.csv",
               header = T, row.names = 1)
gen = read.csv(file = "../../extdata/taxonomy/taxonomy_relabd.genus.csv",
               header = T, row.names = 1)
spe= read.csv(file = "../../extdata/taxonomy/taxonomy_relabd.species.csv",
              header = T, row.names = 1)

#identical(rownames(fam), rownames(spe))
big_box = data.frame(cbind(meta,ad,val,p_1,p_0,p_5,fam,gen,spe), check.names = TRUE)

#identical(rownames(d.28), rownames(add.28 ))

# Select columns from old dfs and select patiens 
cols = c("NIH.Racial.Category_American.Indian.or.Alaska.Native","NIH.Racial.Category_Asian",
         "NIH.Racial.Category_Black.or.African.American", "NIH.Racial.Category_Native.Hawaiian.or.Other.Pacific.Islander",
         "NIH.Racial.Category_White", "target")
add.28 = all28[cols]
add.32 = all32[cols]
# Retain patients
d.28 = big_box[rownames(all28),]
d.32 = big_box[rownames(all32),]
# Add columns 
d.28 = cbind(d.28, add.28)
d.32 = cbind(d.32, add.32)



# FCBF FS
l.28 = list()
l.32 = list()
thold = 0.0025
FCBF.FS = function(data, thold){
  require(FCBF)
  #Split target from other variables
  targets = as.factor(data$target)
  data$target <- NULL
  
  # Discretize RA from OTUS in High and LOW. The transpose is done since the function wants the OTUS in the rows.
  discretization = discretize_exprs(t(data))
  #su_plot(discretization, targets)
  # Remove posibles
  library(tidyr)
  discretization =  discretization %>% drop_na()
  
  #Execute Algm
  fcbf = FCBF::fcbf(feature_table = discretization,
                    target_vector = targets,
                    minimum_su =thold,
                    verbose = TRUE)
  
  #Select new features
  filtered.feat = rownames(fcbf)
  filtered.data = as.data.frame(cbind(data[filtered.feat], target = targets))
  
  # Return dataframe only w the features select from FCBF
  return(filtered.feat)
}
for (i in seq_along(1:length(table(d.28$project)))){
  # Early Preterm
  df.28 = d.28[which(d.28$project == names(table(d.28$project))[i]),]
  df.28$project <- NULL
  df.28 = FCBF.FS(data = df.28, thold = thold)
  l.28[[i]] = df.28
  
  # Preterm
  df.32 = d.32[which(d.32$project == names(table(d.32$project))[i]),]
  df.32$project <- NULL
  df.32 = FCBF.FS(data = df.32, thold = thold)
  l.32[[i]] = df.32
}
names(l.28) = names(table(d.28$project))
names(l.32) = names(table(d.28$project))

# Upset plot
#Panel1
library(UpSetR)
library(ComplexHeatmap)
library(viridis)
library(ggplot2)
library(ggpubr)

m1 = make_comb_mat(fromList(l.28))
m2 = make_comb_mat(fromList(l.32))

UpSet(m1,comb_order = order(comb_size(m1),
                            decreasing = TRUE),
                            comb_col = viridis( length(comb_degree(m1)))
      )

UpSet(m2,comb_order = order(comb_size(m2),decreasing = TRUE),
      comb_col = viridis( length(comb_degree(m2)))
      )

