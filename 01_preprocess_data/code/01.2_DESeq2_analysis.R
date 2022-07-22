setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Arguments
phylotypes = F      # If use T we have to round and transform to Integer
deep = '1e0'     # 1e_1 1e0 5e_1
counts = 'nreads' # nreads relabd  # If use relabd we have to round and transform to Integer
level = 'species'   # species genus family
upper = 28        # T1 = 32 weeks, T2 = 28 weeks
use.median = F    # T = agglomerate visits per patient (mean of all visits)
study = "E"       # From A to J
alpha = 0.05      # Value padj from DESeq2
log2FC = 1.5        # Value log2FoldChange from DESeq2

# Load Data
print("Loading data")
meta = read.csv('../../extdata/metadata/metadata.csv', header = T, row.names = 2)
if (phylotypes == T) {
  tax = read.csv(paste0('../../extdata/phylotypes/phylotype_', 
                        counts, '.', deep, '.csv'), 
                 header = T, row.names = 1)
  tax = tax  %>% 
    mutate_if(is.numeric, round)
  
} else{
  tax = read.csv(paste0('../../extdata/taxonomy/taxonomy_',
                        counts, '.', level, '.csv'), 
                 header = T, row.names = 1)
}

print("Preparing data")
meta.cw = meta[which(meta$collect_wk <= upper),]
meta.cw = meta.cw[which(meta.cw$project == study),]
#table(meta.cw$project, meta.cw$was_term)
tax.cw = tax[rownames(meta.cw),]
identical(sort(rownames(tax.cw)), sort(rownames(meta.cw)))

# Keep columns we are interested in
project = meta.cw$project
participant_id = meta.cw$participant_id
was_term = meta.cw$was_term

# Take median of specimen or continue with counts independently
if (use.median == T){
  library(dplyr)
  print("Calculating the mean of specimens")
  data = cbind(participant_id,tax.cw)
  data = aggregate(data[,-1], list(participant_id = data[,1]), FUN = mean)
  data = data  %>% 
    mutate_if(is.numeric, round)
  tmp1 <- meta.cw %>% distinct(participant_id, .keep_all = TRUE)
  maintain = c("project","participant_id", "was_term")
  tmp1 = tmp1[maintain]
  data = merge(tmp1,data)
}else{
  print("The mean of specimens has not been selected, we proceed with all specimens. ")
  data = cbind(project, participant_id,was_term,tax.cw)
}

print("Performing differential expression analysis")
library(DESeq2)
countData <- t(as.matrix(data[sapply(data, is.numeric)])+1) # Aquí le añadí 1 porque me ponia que no aguantaba ceros el DESEQ
condition <- as.factor(as.matrix(data["was_term"]))
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = DataFrame(condition),
                              design = ~ condition)
dds <- DESeq(dds,test="Wald", fitType="parametric")
res = results(dds, cooksCutoff = FALSE)
otus.res = res[which(res$padj < alpha & abs(res$log2FoldChange) > log2FC), ]
spps = otus.res@rownames
spps



if (phylotypes == T) {
  if(use.median == T){
    saveRDS(object = spps,file = paste0("../data/Deseq/phylotypes/phylotypes_",deep,"_",upper,"_","study",study,".rds"))
  }else{
    saveRDS(object = spps,file = paste0("../data/Deseq/phylotypes/median_phylotypes_",deep,"_",upper,"_","study",study,".rds"))
  }}else if (phylotypes== F){
    if(use.median == T){
      saveRDS(object = spps,file = paste0("../data/Deseq/",level,"/","median","_",level,"_",upper,"_study",study,".rds"))
    }else{
      saveRDS(object = spps,file =  paste0("../data/Deseq/",level,"/","visits_",level,"_",upper,"_study",study,".rds"))
    }
  }