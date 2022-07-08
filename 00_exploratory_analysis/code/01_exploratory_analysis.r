# Declaring functions and utils variables, *source in future*
my_palette = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF",
               "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF") #viridis(length(unique(table(dd$batch.t))))
Scatter.Density = function(object,batch = NULL, trt = NULL,xlim = NULL, ylim = NULL,color.set = NULL,batch.legend.title = 'Batch',trt.legend.title = 'Treatment',density.lwd = 0.2,title = NULL, title.cex = 1.5, legend.cex = 0.7, legend.title.cex = 0.75){
  library(ggplot2)
  library(gridExtra) 
  library(ggpubr) 
  library(grid) 
  data = as.data.frame(object[['variates']][['X']])
  expl.var = object[['prop_expl_var']]
  
  if(is.null(batch)){batch <- batch}else{batch <- as.factor(batch)}
  if(is.null(trt)){trt <- trt}else{trt <- as.factor(trt)}
  
  # color set
  if(is.null(color.set)){
    color.set = color.mixo(seq_len(10))
  }else{
    color.set = color.set
  }
  
  # main plot
  pMain <- ggplot(data = data, aes(x = data[ ,1], y = data[ ,2], colour = batch,
                                   shape = trt)) +
    geom_point() + xlab(paste0('PC1: ',  round(as.numeric(expl.var$X[[1]])*100,digits = 2),
                               '% expl.var')) +
    ylab(paste0('PC2: ',  round(as.numeric(expl.var$X[[2]])*100,digits = 2), '% expl.var')) +
    scale_shape_manual(values = c(19, 15, 1, 6, 4, 19, 2, 17)) +
    scale_color_manual(values = color.set) + theme_bw() +
    labs(colour = batch.legend.title, shape = trt.legend.title) +
    scale_x_continuous(limits = xlim) + scale_y_continuous(limits = ylim) +
    theme(legend.position = 'right', legend.box = 'horizontal',
          legend.direction = 'vertical',
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(0.1, 'cm'),
          legend.title = element_text(size = rel(legend.title.cex)),
          legend.spacing.x = unit(0.1, 'cm'),
          legend.spacing.y = unit(0.1, 'cm'),
          legend.text = element_text(size = rel(legend.cex)))
  
  xlim.update <- layer_scales(pMain)$x$get_limits()
  ylim.update <- layer_scales(pMain)$y$get_limits()
  
  # top density plot
  pTop <- ggplot(data = data, aes(x = data[ ,1], fill = batch,
                                  linetype = trt)) +
    geom_density(size = density.lwd, alpha = 0.5) + ylab('Density') +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(0.8)),
          plot.title = element_text(hjust = 0.5, size = rel(title.cex)),
          axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(), legend.position = 'none') +
    scale_fill_manual(values = color.set) +
    scale_x_continuous(limits = xlim.update) + labs(title = title)
  
  # right density plot
  pRight <- ggplot(data = data, aes(x = data[ ,2],
                                    fill = batch, linetype = trt)) +
    geom_density(size = density.lwd, alpha = 0.5) +  coord_flip() +
    ylab('Density') +
    theme(axis.title.x = element_text(size = rel(0.8)),
          axis.title.y = element_blank(), axis.line = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(),
          panel.background = element_blank(), legend.position = 'none') +
    scale_fill_manual(values = color.set) +
    scale_x_continuous(limits = ylim.update)
  
  if(is.null(batch) && is.null(trt)){legend <-
    grid.rect(gp = gpar(col="white"))}else{
      legend <- get_legend(pMain)
    }
  
  grid.arrange(pTop, legend, pMain + theme(legend.position = 'none'), pRight,
               ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
  
}
show.pca.v1 = function(dataset, batch = "batch", cond = "target", taxons, make_log = FALSE ){
  library(ggfortify)
  library(ggplot2)
  library(viridis)
  # log transform if requires 
  if (make_log == TRUE) {
    pca.data <- log2(dataset[taxons]+1)
  } else {
    pca.data <- dataset[taxons]
  }
  batch.t <- dataset[, batch] 
  target.t  <- dataset[,cond]
  dd = cbind(pca.data, batch.t, target.t)
  pca.re <- stats::prcomp(pca.data,
                          center = TRUE,
                          scale. = FALSE)
  ggplot2::autoplot(pca.re, data = dd, colour = "batch.t", shape = "target.t", size = 3) + scale_colour_manual(values = my_palette)
}
show.pca.v2 = function(dataset, batch = "batch", cond = "target", taxons){
  require(PCAtools)
  library(viridis)
  cols <- sapply(dataset, is.numeric)
  var <- sapply(dataset, is.character)
  mat = dataset[taxons]
  meta = dataset[var]
  batch.t <- dataset[, batch]
  p = PCAtools::pca(mat = t(mat), metadata = meta, center = TRUE, scale = FALSE,rank = 3)
  biplot(p, drawConnectors = FALSE,labSize = 0,showLoadings = FALSE,
         colby = batch,colkey = my_palette, shape = cond, legendPosition = "right")
}
show.pca.Density = function(dataset, batch = "batch", cond = "target", taxons, make_log = FALSE){
  library(gridExtra)
  library(mixOmics)
  library(ggplot2)
  library(PLSDAbatch)
  # log transform if requires 
  if (make_log == TRUE) {
    pca.data <- log2(dataset[taxons]+1)
  } else {
    pca.data <- dataset[taxons]
  }
  # PCA and Plot
  pca.res<- mixOmics::pca(pca.data, ncomp = 3, center = TRUE, scale = FALSE)
  batch.t <- dataset[, batch] # Batch data
  target.t  <- dataset[,cond] # Target data
  Scatter.Density(object = pca.res, batch = batch.t, trt = target.t,
                  batch.legend.title = "Batch", trt.legend.title = "Target",legend.cex = 0.9,
                  legend.title.cex = 0.9 ,title = "PCA with Batch/Target density",
                  color.set = my_palette)
}
show.pca.3D = function(dataset, batch = "batch", cond = "target", taxons, make_log = FALSE){
  library(plotly)
  # log transform if requires 
  if (make_log == TRUE) {
    pca.data <- log2(dataset[taxons]+1)
  } else {
    pca.data <- dataset[taxons]
  }
  batch.t <- dataset[, batch] 
  target.t  <- dataset[,cond]
  dd = cbind(pca.data, batch.t, target.t)
  pca.re <- stats::prcomp(pca.data,
                          center = TRUE,
                          scale. = FALSE, rank. = 3)
  #Plot
  components <- pca.re[["x"]]
  components <- data.frame(components)
  components$PC2 <- -components$PC2
  components$PC3 <- -components$PC3
  components = cbind(components, dataset[, batch],dataset[, cond])
  tot_explained_variance_ratio <- summary(pca.re)[["importance"]]['Proportion of Variance',]
  tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)
  tit = paste('Total Explained Variance =',tot_explained_variance_ratio)
  
  fig <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d", mode = "markers",
                 symbol = ~dataset[, cond],color = ~dataset[, batch], size = 3,alpha = 1,
                 colors = my_palette
                )
  fig <- plotly_build(fig)
  fig$x$data[[1]]$marker$symbol <- 'x' 
  fig$x$data[[3]]$marker$symbol <- 'x'
  fig$x$data[[5]]$marker$symbol <- 'x'
  fig$x$data[[7]]$marker$symbol <- 'x'
  fig$x$data[[9]]$marker$symbol <- 'x'
  fig$x$data[[11]]$marker$symbol <- 'x'
  fig$x$data[[13]]$marker$symbol <- 'x'
  fig$x$data[[15]]$marker$symbol <- 'x'
  fig$x$data[[17]]$marker$symbol <- 'x'
  fig$x$data[[19]]$marker$symbol <- 'x'
  
  fig <- fig %>%
    layout(
      title = tit,
      scene = list(bgcolor = "#e5ecf6")
    )
  fig
}
make.heatmap = function(dataset, taxons, batch = "batch", cond = "target", title = "Heatmap Batch/Target" ){
  library(pheatmap)
  library(viridis)
  ###  scale on OTUs
  mat.data <- scale(dataset[taxons], center = T, scale = T) 
  ###  scale on samples
  mat.data <- scale(t(mat.data), center = T, scale = T)
  # Rename Batch and Cond labels for plotting
  names(dataset)[names(dataset) == batch] <- "Batch"
  names(dataset)[names(dataset) == cond ]<- "Target"
  # Plot annotations
  anno_col <- data.frame(Batch = dataset["Batch"], Target = dataset["Target"])
  
  anno_metabo_colors <- list(Batch = c("A" = my_palette[1], 'B' = my_palette[2], "C" = my_palette[3],
                                       "D" = my_palette[4], "E" = my_palette[5], "F" = my_palette[6],
                                       "G"= my_palette[7],"H"=my_palette[8],"I"=my_palette[9],
                                       "J"= my_palette[10]),
                             Target = c("False" = viridis(2)[1], "True" = viridis(2)[2]))
  
  
  pheatmap(mat.data, 
           scale = 'none', 
           cluster_rows = F, 
           cluster_cols = F, 
           fontsize_row = 5, fontsize_col = 8,
           fontsize = 8,
           clustering_distance_rows = 'euclidean',
           clustering_method = 'ward.D',
           treeheight_row = 30,
           annotation_col = anno_col,
           annotation_colors = anno_metabo_colors,
           border_color = 'NA',
           show_colnames = FALSE,
           show_rownames = TRUE,
           main = title)
}
make.cluster = function(dataset, taxons, batch = "batch", cond = "target", k = 2){
  require(factoextra)
  dataframe = dataset
  cols <- sapply(dataset, is.numeric)
  var <- sapply(dataset, is.character)
  mat = dataset[taxons]
  meta = dataset[var]
  batch.t <- dataset[, batch]
  target.t <- dataset[, cond]
  set.seed(1312)
  opt = fviz_nbclust(mat, FUNcluster = kmeans) 
  print(opt)
  km.res <- kmeans(mat, k, iter.max = 100, nstart = 25 )
  a = fviz_cluster(km.res,mat, geom = "point")
  dataframe$cluster <- km.res$cluster
  dataframe$cluster <- sub("^", "C", dataframe$cluster)
  lista = list(dataframe, km.res, a)
  names = c("dataframe", "km.res", "plot")
  names(lista) = names
  return(lista)
}

# Exploratory analysis
library(dplyr)
setwd("projects/DREAM-Microbiome/")

# Metadata
meta = read.csv('extdata/metadata/metadata.csv', header = T)
term = meta %>% dplyr::select(project, specimen, was_term)

# 1.Lets check if there is a batch effect and try to correct it.
## 1.1. Species level
#####
## 1.1. Species level
# Taxonomy
tax_spe = read.csv('extdata/taxonomy/taxonomy_nreads.species.csv', header = T)
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

## As we have the OTUs separated we apply a CLR transformation
library(mixOmics)
#tax_spe <- tax_spe + 1
#tax_spe <- logratio.transfo(tax_spe, logratio = 'CLR')
class(tax_spe) <- 'matrix'
## Recover the previously removed specimen IDs
tax_spe = cbind(var, tax_spe)
dim(tax_spe)

## Merge with Metadata through specimen ID
bigbox_term_spe = merge(term, tax_spe)
rownames(bigbox_term_spe) <- bigbox_term_spe[,1]
bigbox_term_spe <- bigbox_term_spe[,-1]

## Order species by var 
var = apply(bigbox_term_spe[,3:length(colnames(bigbox_term_spe))], 2, function(x) var(na.omit(x)))
var.ordenado <- sort(var,decreasing = T)

### 1.1.1. PCAs
show.pca.v1(dataset = bigbox_term_spe,batch = "project",
            cond ="was_term",taxons = names(var.ordenado[1:200])
)
show.pca.v2(dataset = bigbox_term_spe, batch = "project",
            cond = "was_term", taxons = names(var.ordenado[1:200])
)
show.pca.Density(dataset = bigbox_term_spe, batch = "project",
                 cond = "was_term", taxons = names(var.ordenado[1:200])
)
show.pca.3D(dataset = bigbox_term_spe, batch = "project",
            cond = "was_term", taxons = names(var.ordenado[1:200]))

### 1.1.2. Box plots, Density plots and Linear Models
library(PLSDAbatch)
dd <- data.frame(value = bigbox_term_spe[,names(var.ordenado[1])], batch = bigbox_term_spe$project)
box_plot(df = dd,color.set = my_palette, title = names(var.ordenado[3]),ylab = "", batch.legend.title = 'Project (batch)')

ggplot(dd, aes(x = value, fill = batch)) + 
  geom_density(alpha = 1) + scale_fill_manual(values = my_palette) + 
  labs(title = names(var.ordenado[1]), x = '', fill = 'Project (batch)') + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), 
                     panel.grid = element_blank())
# Probar con esto si al ppio hacemos una CLR transformation para que los datos
# sigan una distribución normal
spe.lm <- lm(dd[,1] ~ bigbox_term_spe$was_term + bigbox_term_spe$project)
summary(spe.lm)

### 1.1.3. Heatmap
library(pheatmap)
bigbox_term_spe
dd = bigbox_term_spe[
  with(bigbox_term_spe, order(project, was_term)),
]
make.heatmap(dataset = dd, taxons = names(var.ordenado[1:20]), batch = "project",cond = "was_term")


### 1.1.4. Clustering 
paquetes <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
lapply(paquetes, require, character.only = TRUE)

m.distancia <- get_dist(bigbox_term_spe[names(var.ordenado[1:20])], method = "euclidean") 
fviz_dist(m.distancia,show_labels = TRUE, gradient = list(low = "blue", mid = "white", high = "red")) #MEM limit with all OTUs
## Ppal indexes
fviz_nbclust(bigbox_term_spe[names(var.ordenado[1:200])], kmeans, method = "wss")# 4 6 or 7
fviz_nbclust(bigbox_term_spe[names(var.ordenado[1:200])], kmeans, method = "silhouette") # 2
## All indexes TOO MUCH COMPUTATION TIME
resnumclust2<-NbClust(bigbox_term_spe[names(var.ordenado[1:20])],
                      distance = "euclidean", min.nc=2, max.nc=10,
                      method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust2)
clusters = make.cluster(dataset = bigbox_term_spe, k = 7,batch = "project", cond = "was_term", taxons = names(var.ordenado[1:200]) )
clusters$plot
### 1.1.5. Accounting Batch Effect
### Linear model
### Separate columns and make matrix factor for models 
cols = c("project", "was_term")
m = bigbox_term_spe[cols]
dd = as.matrix(m)
spe.batch = dd[,-2]
spe.target = dd [,-1]
spe.batch = as.factor(spe.batch)
spe.target = as.factor(spe.target)

lm.spe <- apply(bigbox_term_spe[names(var.ordenado[1:200])], 2, FUN = function(x){
  res.lm <- lm(x ~ bigbox_term_spe$was_term + bigbox_term_spe$project)
  summary.res <- summary(res.lm)
  p <- summary.res$coefficients[2,4]
})
lm.spe_adjp <- p.adjust(lm.spe, method = 'fdr')

### SVA 
library(sva)
# sponge data
spe.mod <- model.matrix( ~ spe.target) # full model
spe.mod0 <- model.matrix( ~ 1, data = spe.target) # null model
spe.sva.n <- num.sv(dat = t(bigbox_term_spe[names(var.ordenado[1:200])]), mod = spe.mod)
spe.sva <- sva(dat = t(bigbox_term_spe[names(var.ordenado[1:200])]), mod = spe.mod, 
                  mod0 = spe.mod0, n.sv = spe.sva.n)

spe.mod.bat <- cbind(spe.mod, spe.sva$sv)
spe.mod0.bat <- cbind(spe.mod0, spe.sva$sv)

spe.sva.trt_p <- f.pvalue(t(bigbox_term_spe[names(var.ordenado[1:200])]), spe.mod.bat, spe.mod0.bat)
spe.sva.trt_adjp <- p.adjust(spe.sva.trt_p, method='fdr')

### 1.1.6. Correcting for batch effects
### SVA
spe.combat.sva <- t(ComBat(t(bigbox_term_spe[names(var.ordenado[1:200])]), batch = spe.batch, 
                           mod = spe.mod, par.prior = F, prior.plots = F))
### LIMMA
spe.combat.limma <- t(removeBatchEffect(t(bigbox_term_spe[names(var.ordenado[1:200])]), batch = spe.batch, 
                                        design = spe.mod))
cols <- sapply(bigbox_term_spe, is.character)
vars  = bigbox_term_spe[cols]
k  = bigbox_term_spe[cols] 
k = cbind(spe.combat.limma, vars)
show.pca.3D(dataset = k, batch = "project", cond = "was_term",taxons = names(var.ordenado[1:200]))


#####
# rm(list = ls()[!ls() %in% c( "Scatter.Density","my_palette")])# CLEAN ENVIROMENT

## 1.2. Genus level
#####
## 1.2. Genus level
# Taxonomy
tax_gen = read.csv('extdata/taxonomy/taxonomy_nreads.genus.csv', header = T)
## prefiltering step to remove OTUs for which the sum of counts are below a set
## threshold (0.01%) compared to the total sum of all counts
##  Separate the OPTUs from the specimen ID in order to filter OTUs
dim(tax_gen)
cols <- sapply(tax_gen, is.numeric)
var <- sapply(tax_gen, is.character)
var = tax_gen[var]
cols = tax_gen[cols]
cols <- which(colSums(cols)*100/(sum(colSums(cols))) > 0.01)
tax_gen <- tax_gen[, cols]
dim(tax_gen)

## As we have the OTUs separated we apply a CLR transformation
library(mixOmics)
tax_gen <- tax_gen + 1
tax_gen <- logratio.transfo(tax_gen, logratio = 'CLR')
class(tax_gen) <- 'matrix'
## Recover the previously removed specimen IDs
tax_gen = cbind(var, tax_gen)
dim(tax_gen)

# Merge with metadata through specimen ID
bigbox_term_gen = merge(term, tax_gen)
rownames(bigbox_term_gen) <- bigbox_term_gen[,1]
bigbox_term_gen <- bigbox_term_gen[,-1]
## Order species by var 
var = apply(bigbox_term_gen[,3:length(colnames(bigbox_term_gen))], 2, function(x) var(na.omit(x)))
var.ordenado <- sort(var,decreasing = T)

## 1.2.1. PCAs
show.pca.v1(dataset = bigbox_term_gen,batch = "project",
            cond ="was_term",taxons = names(var.ordenado[1:109])
)
show.pca.v2(dataset = bigbox_term_gen, batch = "project",
            cond = "was_term", taxons = names(var.ordenado[1:109])
)
show.pca.Density(dataset = bigbox_term_gen, batch = "project",
                 cond = "was_term", taxons = names(var.ordenado[1:109])
)
show.pca.3D(dataset = bigbox_term_gen, batch = "project",
            cond = "was_term", taxons = names(var.ordenado[1:109]))

### 1.2.2. Box plots, Density plots and Linear Models
library(PLSDAbatch)
dd <- data.frame(value = bigbox_term_gen[,names(var.ordenado[1])], batch = bigbox_term_gen$project)
box_plot(df = dd,color.set = my_palette, title = names(var.ordenado[1]),ylab = "", batch.legend.title = 'Project (batch)')

ggplot(dd, aes(x = value, fill = batch)) + 
  geom_density(alpha = 1) + scale_fill_manual(values = my_palette) + 
  labs(title = names(var.ordenado[1]), x = '', fill = 'Project (batch)') + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), 
                     panel.grid = element_blank())
# Probar con esto si al ppio hacemos una CLR transformation para que los datos
# sigan una distribución normal
gen.lm <- lm(dd[,1] ~ bigbox_term_gen$was_term + bigbox_term_gen$project)
summary(gen.lm)

### 1.2.3. Heatmap
library(pheatmap)
bigbox_term_gen
dd = bigbox_term_gen[
  with(bigbox_term_gen, order(project, was_term)),
]
make.heatmap(dataset = dd, taxons = names(var.ordenado[1:20]), batch = "project",cond = "was_term")

### 1.2.4. Clustering 
paquetes <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
lapply(paquetes, require, character.only = TRUE)

m.distancia <- get_dist(bigbox_term_gen[names(var.ordenado[1:109])], method = "euclidean") 
fviz_dist(m.distancia,show_labels = TRUE, gradient = list(low = "blue", mid = "white", high = "red")) #MEM limit with all OTUs
## Ppal indexes
fviz_nbclust(bigbox_term_gen[names(var.ordenado[1:109])], kmeans, method = "wss")# 4 or 5 even 6 or 7
fviz_nbclust(bigbox_term_gen[names(var.ordenado[1:109])], kmeans, method = "silhouette") # 2
## All indexes TOO MUCH COMPUTATIONAL TIME 
resnumclust2<-NbClust(bigbox_term_gen[names(var.ordenado[1:109])],
                      distance = "euclidean", min.nc=2, max.nc=10,
                      method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust2)
# K-means
clusters = make.cluster(dataset = bigbox_term_gen, k = 2,batch = "project", cond = "was_term", taxons = names(var.ordenado[1:109]) )
clusters$plot

### 1.2.5. Accounting Batch Effect
### Linear model
### Separate columns and make matrix factor for models 
cols = c("project", "was_term")
m = bigbox_term_gen[cols]
dd = as.matrix(m)
gen.batch = dd[,-2]
gen.target = dd [,-1]
gen.batch = as.factor(gen.batch)
gen.target = as.factor(gen.batch)

lm.gen <- apply(bigbox_term_gen[names(var.ordenado[1:109])], 2, FUN = function(x){
  res.lm <- lm(x ~ gen.target + gen.batch)
  summary.res <- summary(res.lm)
  p <- summary.res$coefficients[2,4]
})
lm.spe_adjp <- p.adjust(lm.spe, method = 'fdr')

### SVA 
library(sva)
# sponge data
gen.mod <- model.matrix( ~ gen.target) # full model
gen.mod0 <- model.matrix( ~ 1, data = gen.target) # null model
gen.sva.n <- num.sv(dat = t(bigbox_term_gen[names(var.ordenado[1:109])]), mod = gen.mod)
gen.sva <- sva(dat = t(bigbox_term_gen[names(var.ordenado[1:109])]), mod = gen.mod, 
               mod0 = gen.mod0, n.sv = gen.sva.n)

gen.mod.bat <- cbind(gen.mod, gen.sva$sv)
gen.mod0.bat <- cbind(gen.mod0, gen.sva$sv)

gen.sva.trt_p <- f.pvalue(t(bigbox_term_gen[names(var.ordenado[1:109])]), gen.mod.bat, gen.mod0.bat)
gen.sva.trt_adjp <- p.adjust(gen.sva.trt_p, method='fdr')

### 1.2.6. Correcting for batch effects
### SVA
gen.combat.sva <- t(ComBat(t(bigbox_term_gen[names(var.ordenado[1:109])]), batch = gen.batch, 
                          mod = gen.mod, par.prior = F, prior.plots = F))
### LIMMA
gen.combat.limma <- t(removeBatchEffect(t(bigbox_term_gen[names(var.ordenado[1:109])]), batch = gen.batch, 
                                    design = gen.mod))

#####
# rm(list = ls()[!ls() %in% c( "Scatter.Density","my_palette")])# CLEAN ENVIROMENT

## 1.3. Fam level
#####
## 1.3. Fam level
# Taxonomy
tax_fam = read.csv('extdata/taxonomy/taxonomy_nreads.family.csv', header = T)
## prefiltering step to remove OTUs for which the sum of counts are below a set
## threshold (0.01%) compared to the total sum of all counts
##  Separate the OPTUs from the specimen ID in order to filter OTUs
dim(tax_fam)
cols <- sapply(tax_fam, is.numeric)
var <- sapply(tax_fam, is.character)
var = tax_fam[var]
cols = tax_fam[cols]
cols <- which(colSums(cols)*100/(sum(colSums(cols))) > 0.01)
tax_fam <- tax_fam[, cols]
dim(tax_fam)

## As we have the OTUs separated we apply a CLR transformation
library(mixOmics)
tax_fam <- tax_fam + 1
tax_fam <- logratio.transfo(tax_fam, logratio = 'CLR')
class(tax_fam) <- 'matrix'
## Recover the previously removed specimen IDs
tax_fam = cbind(var, tax_fam)
dim(tax_fam)

## Merge with metadata through specimen ID
bigbox_term_fam = merge(term, tax_fam)
rownames(bigbox_term_fam) <- bigbox_term_fam[,1]
bigbox_term_fam <- bigbox_term_fam[,-1]
## Order species by var 
var = apply(bigbox_term_fam[,3:length(colnames(bigbox_term_fam))], 2, function(x) var(na.omit(x)))
var.ordenado <- sort(var,decreasing = T)

### 1.3.1 PCAs
show.pca.v1(dataset = bigbox_term_fam,batch = "project",
            cond ="was_term",taxons = names(var.ordenado[1:68])
)
show.pca.v2(dataset = bigbox_term_fam, batch = "project",
            cond = "was_term", taxons = names(var.ordenado[1:68])
)
show.pca.Density(dataset = bigbox_term_fam, batch = "project",
                 cond = "was_term", taxons = names(var.ordenado[1:68])
)
show.pca.3D(dataset = bigbox_term_fam, batch = "project",
            cond = "was_term", taxons = names(var.ordenado[1:68]))

### 1.3.2. Box plots, Density plots and Linear Models
library(PLSDAbatch)
dd <- data.frame(value = bigbox_term_fam[,names(var.ordenado[1])], batch = bigbox_term_fam$project)
box_plot(df = dd,color.set = my_palette, title = names(var.ordenado[1]),ylab = "", batch.legend.title = 'Project (batch)')

ggplot(dd, aes(x = value, fill = batch)) + 
  geom_density(alpha = 1) + scale_fill_manual(values = my_palette) + 
  labs(title = names(var.ordenado[1]), x = '', fill = 'Project (batch)') + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), 
                     panel.grid = element_blank())
# Probar con esto si al ppio hacemos una CLR transformation para que los datos
# sigan una distribución normal
fam.lm <- lm(dd[,1] ~ bigbox_term_fam$was_term + bigbox_term_fam$project)
summary(fam.lm)

### 1.3.3. Heatmap
library(pheatmap)
bigbox_term_fam
dd = bigbox_term_fam[
  with(bigbox_term_fam, order(project, was_term)),
]
make.heatmap(dataset = dd, taxons = names(var.ordenado[1:20]), batch = "project",cond = "was_term")

### 1.3.4. Clustering 
paquetes <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
lapply(paquetes, require, character.only = TRUE)

m.distancia <- get_dist(bigbox_term_fam[names(var.ordenado[1:20])], method = "euclidean") 
fviz_dist(m.distancia,show_labels = TRUE, gradient = list(low = "blue", mid = "white", high = "red")) #MEM limit with all OTUs
## Ppal indexes
fviz_nbclust(bigbox_term_fam[names(var.ordenado[1:68])], kmeans, method = "wss")# 7
fviz_nbclust(bigbox_term_fam[names(var.ordenado[1:68])], kmeans, method = "silhouette") # 9
## All indexes TOO MUCH COMPUTATION TIME
resnumclust2<-NbClust(bigbox_term_fam[names(var.ordenado[1:68])],
                      distance = "euclidean", min.nc=2, max.nc=10,
                      method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust2)
clusters = make.cluster(dataset = bigbox_term_fam, k = 9,batch = "project", cond = "was_term", taxons = names(var.ordenado[1:68]) )
clusters$plot

### 1.3.5. Accounting Batch Effect
### Linear model
### Separate columns and make matrix factor for models 
cols = c("project", "was_term")
m = bigbox_term_fam[cols]
dd = as.matrix(m)
fam.batch = dd[,-2]
fam.target = dd [,-1]
fam.batch = as.factor(fam.batch)
fam.target = as.factor(fam.target)

lm.fam <- apply(bigbox_term_fam[names(var.ordenado[1:68])], 2, FUN = function(x){
  res.lm <- lm(x ~ bigbox_term_fam$was_term + bigbox_term_fam$project)
  summary.res <- summary(res.lm)
  p <- summary.res$coefficients[2,4]
})
lm.fam_adjp <- p.adjust(lm.fam, method = 'fdr')

### SVA 
library(sva)
# sponge data
fam.mod <- model.matrix( ~ fam.target) # full model
fam.mod0 <- model.matrix( ~ 1, data = fam.target) # null model
fam.sva.n <- num.sv(dat = t(bigbox_term_fam[names(var.ordenado[1:68])]), mod = fam.mod)
fam.sva <- sva(dat = t(bigbox_term_fam[names(var.ordenado[1:68])]), mod = fam.mod, 
               mod0 = fam.mod0, n.sv = fam.sva.n)

fam.mod.bat <- cbind(fam.mod, fam.sva$sv)
fam.mod0.bat <- cbind(fam.mod0, fam.sva$sv)

fam.sva.trt_p <- f.pvalue(t(bigbox_term_fam[names(var.ordenado[1:68])]), fam.mod.bat, fam.mod0.bat)
fam.sva.trt_adjp <- p.adjust(fam.sva.trt_p, method='fdr')

### 1.3.6. Correcting for batch effects
### SVA
fam.combat.sva <- t(ComBat(t(bigbox_term_fam[names(var.ordenado[1:68])]), batch = fam.batch, 
                           mod = fam.mod, par.prior = F, prior.plots = F))
### LIMMA
library(limma)
fam.combat.limma <- t(removeBatchEffect(t(bigbox_term_fam[names(var.ordenado[1:68])]), batch = fam.batch, 
                                        design = fam.mod))
  
#####
# rm(list = ls()[!ls() %in% c( "Scatter.Density","my_palette")])# CLEAN ENVIROMENT

#####
# 2. Exploratory Analysis
#####
# 2. We checked which organisms are correlated with collect week (at three levels:
# species, genus and family).In order to check which organisms are more significant
# throughout the pregnancy and if there are any that remain or suddenly cease to be present.
cw = meta %>% select(project, specimen, collect_wk)
# Merge both data by specimen ID
bigbox_cw_spe = merge(cw, tax_spe)

# We take the data from each project separately for a small exploratory analysis. 
v = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
nms = c("study_A","study_B","study_C","study_D","study_E",
        "study_F","study_G","study_H","study_I","study_J")
studies = list()
for (i in seq_along(v)) {
  studies[[i]] = filter(bigbox_cw_spe, project == v[i]) 
}
names(studies) = nms


