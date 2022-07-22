setwd(dirname(rstudioapi::getSourceEditorContext()$path))

lvl = "species"
phylotypes = T
deep = "5e_1" # 1e_1 1e0 5e_1
if (phylotypes == T) {
  l = list.files(path =paste0( "../data/MetaA/phylotypes/"), pattern = deep)
  lista = list()
  for (i in seq_along(l)) {
    lista[[i]] = readRDS(file =paste0( "../data/MetaA/phylotypes/",l[i]) )
  }
  names(lista) = l
}else{
  l = list.files(path =paste0( "../data/MetaA/",lvl), pattern = lvl)
  lista = list()
  for (i in seq_along(l)) {
    lista[[i]] = readRDS(file =paste0( "../data/MetaA/",lvl,"/",l[i]) )
  }
  names(lista) = l
}


int.28 = intersect(lista[[1]], lista[[3]])
int.32 = intersect(lista[[2]], lista[[4]])

if (phylotypes == T) {
  saveRDS(object = int.28, file = paste0("../data/MetaA/phylotypes_",deep,"_28.rds"))
  saveRDS(object = int.32, file = paste0("../data/MetaA/phylotypes_",deep,"_32.rds"))
  
}else{
  saveRDS(object = int.28, file = paste0("../data/MetaA/taxonomy_",lvl,"_28.rds"))
  saveRDS(object = int.32, file = paste0("../data/MetaA/taxonomy_",lvl,"_32.rds"))
}
