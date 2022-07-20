#CLUST FEATURE SELECTION
require(dynamicTreeCut)

coAbundances = function(datos){
  
  #Data Scale
  # sdata = scale(x = as.matrix(datos))
  sdata = apply(datos, 2, function(x) x-mean(x)/sd(x))
  
  # Dissimilarity matrix
  d <- dist(t(sdata), method = 'euclidean')
  
  # Hierarchical clustering using Complete Linkage
  hc1 <- hclust(d, method = 'ward.D2')
  
  # Plot the obtained dendrogram
  plot(hc1, cex = 0.6, hang = -1)
  
  #Dinamic tree
  dtree = cutreeDynamic(dendro = hc1,
                        cutHeight = 20,
                        minClusterSize = 5,
                        method = "tree")
  return(dtree)
  
}

get_clusters = function(data, coab){
  
  clusters = sort(unique(coab))
  clusters = clusters[-1]
  
  res = list()
  for (i in seq_along(clusters)) {
    
    ci = data[, which(coab == clusters[i])]
    ci = rowSums(ci)
    res[[i]] = data.frame(ci)
    names(res[[i]]) = paste0('Cluster_', i)
    
  }
  res = as.data.frame(res)
  return(res)
  
}

# Load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
tax = read.csv('../../extdata/taxonomy/taxonomy_relabd.genus.csv',
               header = T, row.names = 1)


coabA = coAbundances(subsets$A)
coabC = coAbundances(subsets$C)
clusters = get_clusters(tax, coab)

intersect(names(subsets$C[which(coabC == 2)]),
          names(subsets$A[which(coabA == 2)]))
