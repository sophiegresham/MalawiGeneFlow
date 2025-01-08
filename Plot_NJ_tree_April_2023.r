#rerun 07.04.2023
#Astatoreochromis straeleni was missing from the original distance matrix (SRR9675381)
#add this in and make sure all non-malawi samples are accounted for


library(ape)
library(ggtree)
library(Biostrings)
library(ggplot2)
library(readr)
library(binr)
library(treeio)

#read in distance matrix

setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo")
dist <- read.csv("_data/results/wholegenomedist_april2023.csv", sep = ",", row.names = 1)
#head(dist)
n <- length(dist$X0)

#read in metadata
metamain <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/variantcalling/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp.tsv", sep = "\t")
metamain[metamain$sequence_id == "SRR9675381",] #check whether missing sample is in metadata
uni <- as.vector(unique(metamain$sequence_id))
colnames(metamain)[1] <- "Sample_name"

#read in samples list
samples <- read.table("_data/results/distancematrix/Malawicallsetfeb2021.pass.snps.biallelic.anc_samp.noninversion.chr1.allsamples.allsites.distance.dist.april2023.dist.id", head = F)
#head(samples)
colnames(samples)[1] <- "Sample_name"
#head(samples)
length(samples$Sample_name) #should be same length as n

#merge samples list and metadata
meta2 <- merge(samples, metamain, by = "Sample_name", sort = T, all = TRUE)
length(meta2$Sample_name)
#head(metamain)
#head(meta2)
meta3 <- meta2[c(samples$Sample_name),]
length(meta3$Sample_name)
length(unique(meta3$Sample_name)) #should be the same as the previous line
#meta3$spgen <- paste(meta3$genus, meta3$species, meta3$Sample_name, meta3$location, meta3$sublocation, sep = "_")
meta3$spgen <- paste(meta3$genus, meta3$species, meta3$Sample_name, sep = "_")
meta3$spgen_neat <- paste(meta3$genus, meta3$species, meta3$Sample_name, sep = " ")
meta3$spgen_noid <- paste(meta3$genus, meta3$species, sep = " ")


#check new names are the same as in ID file
head(meta3$Sample_name)
head(samples$Sample_name)
length(meta3$Sample_name)
length(samples$Sample_name)

#change column and row names to sample names in distance matrix
colnames(dist) <- meta3$spgen
rownames(dist) <- meta3$spgen
#colnames(dist)
head(dist)

#apply(dist, 1, function(x) sum(x == "NA"))
dist[22,] #ruaha blue - more samples available for this species
dist[419,] #otopharynx brooksi - more samples available for this species
dist[552,] #stigmatochromis pholidophorus - more samples available for this species
dist[9,] #astatoreochromis straeleni

#remove some samples with index in python [21,418,551,8] but in r they are not 0 indexed so remove [22,419,552,9]
#edit - april 2023 - not removing index 8 anymore (astatoreochromis straeleni) - its the only sample from this species and hasn't got NAs so not sure why it was removed in the first place
dist2 <- dist[-c(22,419,552)]
dist3 <- dist2[-c(22,419,552),]

#rerun nj tree with samples removed
dist4 <- data.matrix(dist3)
class(dist4)
tre <- nj(dist4)
class(tre)
#plot(tre, cex = 0.6)
outgroup_index <- length(dist3[1,])
dist3[outgroup_index,] #should be ancestral
tre2 <- root(tre, out = outgroup_index)
tre2 <- ladderize(tre2)
write.tree(tre2, "_data/figures/njtree/newick_tree_rooted_to_ancsamp_wholegenome_april2023_samplesrm.nwk")

#update with more samples removed from metadata update
dist2 <- dist[-c(22,419,552,2,329,389,390,401,412,419,544,552,11,20)]
dist3 <- dist2[-c(22,419,552,2,329,389,390,401,412,419,544,552,11,20),]

#dist[2,] #abactochromis labrosus
#dist[329,] #lethrinops cf macrochir
#dist[389,] #mylochromis melanotaenia
#dist[390,] #mylochromis sp.
#dist[401,] #nimbochromis fuscotaeniatus
#dist[412,] #orthochromis red-cheek
#dist[419,] #otopharynx brooksi
#dist[544,] #serranochromis checkerboard
#dist[552,] #stigmatichromis pholidophorus

#april update
#remove also the two ruaha blue sampels that clsutered in the LVRS clade
#dist[11,] #which(rownames(dist) == "Astatotilapia_Ruaha-blue_cichlid7050787")
#dist[20,] #which(rownames(dist) == "Astatotilapia_Ruaha-blue_cichlid7050788")

#rerun nj tree with samples removed
dist4 <- data.matrix(dist3)
class(dist4)
tre <- nj(dist4)
class(tre)
#plot(tre, cex = 0.6)
outgroup_index <- length(dist3[1,])
dist3[outgroup_index,] #should be ancestral
tre2 <- root(tre, out = outgroup_index)
tre2 <- ladderize(tre2)
write.tree(tre2, "_data/figures/njtree/newick_tree_rooted_to_ancsamp_wholegenome_april2023_samplesrm.nwk")

#rerun but with neater names

colnames(dist) <- meta3$spgen_neat
rownames(dist) <- meta3$spgen_neat

#update with more samples removed from metadata update
dist2 <- dist[-c(22,419,552,2,329,389,390,401,412,419,544,552,11,20)]
dist3 <- dist2[-c(22,419,552,2,329,389,390,401,412,419,544,552,11,20),]

#rerun nj tree with samples removed
dist4 <- data.matrix(dist3)
class(dist4)
tre <- nj(dist4)
class(tre)
#plot(tre, cex = 0.6)
outgroup_index <- length(dist3[1,])
dist3[outgroup_index,] #should be ancestral
tre2 <- root(tre, out = outgroup_index)
tre2 <- ladderize(tre2)
write.tree(tre2, "_data/figures/njtree/newick_tree_rooted_to_ancsamp_wholegenome_april2023_samplesrm_neatnames.nwk")

#remove ruaha blue LVRS cluster samples?
#remove sample names from the final tree? or have just one sample per species?

##################

#calculate genetic distance of each non-malawi species to the malawi radiation
#use this distance to make an order of non-malawi species for plots

#if species is Malawi then replace the species name with Malawi

#head(meta3)

meta4 <- meta3

rows_to_modify <- meta4[which(meta4$clade == "mbuna" | 
            meta4$clade == "deepbenthic" |
            meta4$clade == "shallowbenthic" |
            meta4$clade == "acalliptera" |
            meta4$clade == "utaka" |
            meta4$clade == "diplotaxodon" |
            meta4$clade == "rhamphochromis"),]

#rows_to_modify <- rows_to_modify[!is.na(meta4$spgen[rows_to_modify])]
meta4$spgen[which(meta4$clade == "mbuna" | 
            meta4$clade == "deepbenthic" |
            meta4$clade == "shallowbenthic" |
            meta4$clade == "acalliptera" |
            meta4$clade == "utaka" |
            meta4$clade == "diplotaxodon" |
            meta4$clade == "rhamphochromis")] <- gsub("$", "_malawi", meta4$spgen[which(meta4$clade == "mbuna" |
                                                                  meta4$clade == "deepbenthic" |
                                                                  meta4$clade == "shallowbenthic" |
                                                                  meta4$clade == "acalliptera" |
                                                                  meta4$clade == "utaka" |
                                                                  meta4$clade == "diplotaxodon" |
                                                                  meta4$clade == "rhamphochromis")])

dist_ver2 <- dist
colnames(dist_ver2) <- meta4$spgen
rownames(dist_ver2) <- meta4$spgen


dist_malawi <- dist_ver2[,grepl("malawi", names(dist_ver2))]
dist_malawi2 <- dist_malawi[-grep("malawi", rownames(dist_malawi)),]

#head(dist_malawi2)

nmalawi <- length(dist_malawi2[1,])
dist_malawi2$mean_dist_to_malawi <- rowSums(dist_malawi2, na.rm = TRUE)/nmalawi
dist_malawi3 <- dist_malawi2[,nmalawi + 1, drop = FALSE]
dist_malawi4 <- dist_malawi3[order(-dist_malawi3$mean_dist_to_malawi),, drop = FALSE]

dist_malawi4[1:50,, drop = FALSE]
dist_malawi4[50:101,, drop = FALSE]
dist_malawi4[102:150,, drop = FALSE]


##################

#create NJ but just for non-malawi species - collapse nodes into just one species

#keep only non-malawi species (exclude victoria)

#change column and row names to sample names in distance matrix
dist_noid <- dist
colnames(dist_noid) <- meta3$spgen_noid
rownames(dist_noid) <- meta3$spgen_noid
#colnames(dist)
head(dist_noid)


