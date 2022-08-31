# Extract <= 28 and <= 32 visits respectively
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
l.28 = list()
l.32 = list()
path = "toRun/basal/"
lf.28 = list.files(path = path,pattern = "28")
lf.32 = list.files(path = path,pattern = "32")

for (i in seq_along(lf.28)) {
  l.28[[i]] = readRDS(file = paste0(path,lf.28[i]))
  l.32[[i]] = readRDS(file = paste0(path,lf.32[i]))
}

# Function for rename without rds
trim = function(x){
  substr(x,1,nchar(x)-4)
}
n.28 =  lapply(X = lf.28, FUN = trim)
n.32 =  lapply(X = lf.32, FUN = trim)
names(l.28) = unlist(n.28)
names(l.32) = unlist(n.32)

# Select only visits <= 28 or <= 32
for (i in seq_along(l.28)) {
  l.28[[i]] = l.28[[i]][which(l.28[[i]]$collect_week <= 28),]
  l.32[[i]] = l.32[[i]][which(l.32[[i]]$collect_week <= 32),]
}

# Save dfs 
out.path = "toRun/exact_visits/"
if (dir.exists(out.path) == FALSE) {
  dir.create(out.path)
  message(paste('Creating ', out.path, ' directory!'))
}

for (i in seq_along(l.28)) {
  saveRDS(object = l.28[[i]], file = paste0(out.path, names(l.28)[i], ".rds"))
  saveRDS(object = l.32[[i]], file = paste0(out.path, names(l.32)[i], ".rds"))
}
