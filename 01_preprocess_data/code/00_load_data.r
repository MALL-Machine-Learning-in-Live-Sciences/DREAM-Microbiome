# Load data
setwd('~/git/DREAM-Microbiome/')

# Alpha diversity
alpha = read.csv('extdata/alpha_diversity/alpha_diversity.csv', header = T, row.names = 1)

# Metadata
meta = read.csv('extdata/metadata/metadata.csv', header = T)

# Phylotypes
# nreads ==> counts of phylotypes at different distances (0.1, 0.5, 1)
# relabd ==> relative abundance of phylotypes
# pt     ==> mapping?
list.files('extdata/phylotypes/')
phylotype.map = read.csv('extdata/phylotypes/pt.1e-1.csv', header = T)
phylotype.ra = read.csv('extdata/phylotypes/phylotype_relabd.1e_1.csv', header = T, row.names = 1)
phylotype.counts = read.csv('extdata/phylotypes/phylotype_nreads.1e_1.csv', header = T, row.names = 1)

# Mapping sv and specimens
sv = read.csv('extdata/sv_counts/sp_sv_long.csv', header = T)
head(sv)

# Valencia states
valencia = read.csv('extdata/community_state_types/cst_valencia.csv', header = T)
head(valencia)

# Pairwise distance
pairwise = read.csv('extdata/pairwise_distance/krd_distance_long.csv', header = T)
head(pairwise)

# Taxonomy
tax = read.csv('extdata/taxonomy/taxonomy_nreads.species.csv', header = T)
tax[1:5, 1:5]
