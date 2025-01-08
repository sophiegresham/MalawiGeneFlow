#estimate split times of selected nodes - hannes method

#net divergence = median between group dxy - mean of median within group heterozygosity
#time split (generations) = net divergence/(2 * per site mutation rate* accesible genome length)

#calulate dxy and heterozygosity

#per site muation rate = 3*3.5e-9 - per bp per generation (Malinsky et al. 2018) 
#accessible genome length = 491640513


#read in distance matrix
dist <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/results/wholegenomedist_june2024.csv", sep = ",", row.names = 1)

#read in metadata
metamain <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2024-02-23_callset202103_malawi2perspecies_newalignments_subversion_tree_rm_kilossana_add_bloyeti.tsv", sep = "\t")
#metamain <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/variantcalling/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp.tsv", sep = "\t")
metamain[metamain$sequence_id == "SRR9675381",] #check whether missing sample is in metadata
uni <- as.vector(unique(metamain$sequence_id))
colnames(metamain)[1] <- "Sample_name"

#read in samples list
samples <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/results/distancematrix/Malawicallsetfeb2021.pass.snps.biallelic.anc_samp.noninversion.chr1.allsamples.allsites.distance.dist.april2023.dist.id", head = F)
#head(samples)
colnames(samples)[1] <- "Sample_name"
#head(samples)
length(samples$Sample_name) #should be same length as n

#merge samples list and metadata
meta2 <- merge(samples, metamain, by = "Sample_name", sort = T, all = TRUE)
length(meta2$Sample_name)
#head(metamain)
#head(meta2)
meta3 <- meta2[c(samples$Sample_name),] #not sure why this isn't working anymore
length(meta3$Sample_name)
length(unique(meta3$Sample_name)) #should be the same as the previous line
#meta3$spgen <- paste(meta3$genus, meta3$species, meta3$Sample_name, meta3$location, meta3$sublocation, sep = "_")
meta3$spgen <- paste(meta3$genus, meta3$species, meta3$Sample_name, sep = "_")
meta3$spgen_neat <- paste(meta3$genus, meta3$species, meta3$Sample_name, sep = " ")
meta3$spgen_noid <- paste(meta3$genus, meta3$species, sep = " ")

head(meta2)
#meta2[meta2$Sample_name %in% c('CA','AZ','PH'),]


#change columnn and row names to be sample names
colnames(dist) <- meta3$Sample_name
rownames(dist) <- meta3$Sample_name

head(dist)

#get index of samples to remove
samples_to_remove <- c("cichlid7020252", "cichlid7050782", "cichlid6994220")
#grep(samples_to_remove[1], colnames(dist)) #419
#grep(samples_to_remove[2], colnames(dist)) #22
#grep(samples_to_remove[3], colnames(dist)) #552

#remove samples from dataframe
dist2 <- dist[-c(22,419,552)]
dist3 <- dist2[-c(22,419,552),]

#head(dist3)

#check that sample names in dist match up with het dataframe
#setdiff(colnames(dist3), het_allchr$INDV) #should give no output

#subset distance matrix get pairwise differences for 2 selected groups
#select rows with samples from group 1
#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$clade_SG %in% "LVRS",]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$region %in% "Malawi",]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
dxy_median

#calculate heterozygosity for all individuals
#run for each chromosome in terminal:
#vcftools --gzvcf /scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/inputfiles/vcf/Malawicallsetfeb2021.sf_stringent1.pass.snps.biallelic.anc_samp.noninversion.chr12.vcf.gz --het --out output_chr12.het

setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/treedating/data")


#which samples are missing?

het12 <- read.csv("output_chr12.het.het", sep = "\t")
head(het12)
het22 <- read.csv("output_chr22.het.het", sep = "\t")

length(het12$INDV)
length(het22$INDV)

merged <- merge(het1, het12, by = "INDV", all = TRUE)
rows_with_na <- merged[apply(
  merged, 
  1, 
  function(x) any(is.na(x))
  ), ]
rows_with_na

merged <- merge(het1, het22, by = "INDV", all = TRUE)
rows_with_na <- merged[apply(
  merged, 
  1, 
  function(x) any(is.na(x))
  ), ]
rows_with_na


samples_to_remove <- c("cichlid7020252", "cichlid7050782", "cichlid6994220")

#read in het data for each chromosome and combine into one file

het1 <- read.csv("output_chr1.het.het", sep = "\t")

het1 <- het1[!het1$INDV %in% samples_to_remove,]
het1$O.HET. <- het1$N_SITES - het1$O.HOM.
#head(het1)

het_allchr <- het1[,c("INDV", "O.HET.")]
#head(het_allchr)
length(het_allchr$INDV)

chromosomes <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23)

#for (i in 2:2){
for (i in 2:length(chromosomes)){
    chr_no <- chromosomes[i]
    heti <- read.csv(paste("output_chr",chr_no,".het.het", sep = ""), sep = "\t")
    heti <- heti[!heti$INDV %in% samples_to_remove,]
    heti$O.HET. <- heti$N_SITES - heti$O.HOM.
    #print(head(heti$O.HET.))
    print(length(het_allchr$INDV))
    
    het_allchr$O.HET. <- het_allchr$O.HET. + heti$O.HET.
    
}


#divide by the accessible genome size
het_allchr$O.HET. <- het_allchr$O.HET./491640513

head(het_allchr)
mean(het_allchr$O.HET.)

#output_chr12.het.het - 2 samples missing
#output_chr22.het.het - 1 sample missing

#find which samples are missing - where are these in the metadata?


#calculate the median heterozygosity for 2 selected groups
#group 1
het_allchr_group1 <- het_allchr[het_allchr$INDV %in% meta3_group1$Sample_name,]
length(het_allchr_group1$INDV) #should be 50
group1_het_median <- median(het_allchr_group1$O.HET.)

#group 2
het_allchr_group2 <- het_allchr[het_allchr$INDV %in% meta3_group2$Sample_name,]
length(het_allchr_group2$INDV) #should be 500
group2_het_median <- median(het_allchr_group2$O.HET.)

#calculate the mean of the two medians of both groups
het_mean <- mean(c(group1_het_median, group2_het_median))

het_mean

net_divergence <- dxy_median - het_mean
net_divergence

mutation_rate1 <- 3.5e-9*0.95
mutation_rate2 <- 3.5e-9*1.05

split_time_gen <- net_divergence/(2 * mutation_rate1)
split_time_gen

split_time <- split_time_gen*3 #3 years average gen time
split_time


3.5e-9*0.95
3.5e-9*1.05

#subset distance matrix get pairwise differences for 2 selected groups
#select rows with samples from group 1

pseudo1 <- c("Orthochromis_indermauri")

psuedo2 <- c("Orthochromis_sp-red-cheek", "Pseudocrenilabrus_philander", "Pseudocrenilabrus_multicolor")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% pseudo1,]
meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$full_name  %in% psuedo2 ,]
meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
dxy_median

#calculate the median heterozygosity for 2 selected groups
#group 1
het_allchr_group1 <- het_allchr[het_allchr$INDV %in% meta3_group1$Sample_name,]
length(het_allchr_group1$INDV) #should be 50
group1_het_median <- median(het_allchr_group1$O.HET.)

#group 2
het_allchr_group2 <- het_allchr[het_allchr$INDV %in% meta3_group2$Sample_name,]
length(het_allchr_group2$INDV) #should be 500
group2_het_median <- median(het_allchr_group2$O.HET.)

#calculate the mean of the two medians of both groups
het_mean <- mean(c(group1_het_median, group2_het_median))

het_mean

net_divergence <- dxy_median - het_mean
net_divergence


split_time_gen <- net_divergence/(2 * 3.5e-9)
split_time_gen

split_time <- split_time_gen*3 #3 years average gen time
split_time

#Malawi:
#split time (gen) = 1534705.19627417
#split time = 4604115.58882252

#subset distance matrix get pairwise differences for 2 selected groups
#select rows with samples from group 1

csa1 <- c("Serranochromis_angusticeps", "Serranochromis_macrocephalus", "Serranochromis_sp-checkerboard", "Pharyngochromis_acuticeps1", "Pharyngochromis_acuticeps2", "Sargochromis_carlottae", "Serranochromis_robustus")

csa2 <- c("Thoracochromis_brauschi")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% csa1,]
meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$full_name  %in% csa2 ,]
meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
dxy_median

#calculate the median heterozygosity for 2 selected groups
#group 1
het_allchr_group1 <- het_allchr[het_allchr$INDV %in% meta3_group1$Sample_name,]
length(het_allchr_group1$INDV) #should be 50
group1_het_median <- median(het_allchr_group1$O.HET.)

#group 2
het_allchr_group2 <- het_allchr[het_allchr$INDV %in% meta3_group2$Sample_name,]
length(het_allchr_group2$INDV) #should be 500
group2_het_median <- median(het_allchr_group2$O.HET.)

#calculate the mean of the two medians of both groups
het_mean <- mean(c(group1_het_median, group2_het_median))

het_mean

net_divergence <- dxy_median - het_mean
net_divergence


split_time_gen <- net_divergence/(2 * 3.5e-9)
split_time_gen

split_time <- split_time_gen*3 #3 years average gen time
split_time

#Malawi:
#split time (gen) = 1,534,705.19627417
#split time = 4,604,115.58882252


#0.00396831739535672
#566902.48505096
#1700707.45515288

#subset distance matrix get pairwise differences for 2 selected groups
#select rows with samples from group 1

ortho1 <- c("Orthochromis_uvinzae")

ortho2 <- c("Orthochromis_mazimeroensis", "Orthochromis_malagaraziensis")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% ortho1,]
meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$full_name  %in% ortho2,]
meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
dxy_median

#calculate the median heterozygosity for 2 selected groups
#group 1
het_allchr_group1 <- het_allchr[het_allchr$INDV %in% meta3_group1$Sample_name,]
length(het_allchr_group1$INDV) #should be 50
group1_het_median <- median(het_allchr_group1$O.HET.)

#group 2
het_allchr_group2 <- het_allchr[het_allchr$INDV %in% meta3_group2$Sample_name,]
length(het_allchr_group2$INDV) #should be 500
group2_het_median <- median(het_allchr_group2$O.HET.)

#calculate the mean of the two medians of both groups
het_mean <- mean(c(group1_het_median, group2_het_median))

het_mean

net_divergence <- dxy_median - het_mean
net_divergence


split_time_gen <- net_divergence/(2 * 3.5e-9)
split_time_gen

split_time <- split_time_gen*3 #3 years average gen time
split_time

#Malawi:
#split time (gen) = 1,534,705.19627417
#split time = 4,604,115.58882252

#subset distance matrix get pairwise differences for 2 selected groups
#select rows with samples from group 1

benthics <- c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka")

pelagics <- c("diplotaxodon", "rhamphochromis")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$clade_SG %in% benthics,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$clade_SG %in% pelagics,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
dxy_median

#calculate the median heterozygosity for 2 selected groups
#group 1
het_allchr_group1 <- het_allchr[het_allchr$INDV %in% meta3_group1$Sample_name,]
length(het_allchr_group1$INDV) #should be 50
group1_het_median <- median(het_allchr_group1$O.HET.)

#group 2
het_allchr_group2 <- het_allchr[het_allchr$INDV %in% meta3_group2$Sample_name,]
length(het_allchr_group2$INDV) #should be 500
group2_het_median <- median(het_allchr_group2$O.HET.)

#calculate the mean of the two medians of both groups
het_mean <- mean(c(group1_het_median, group2_het_median))

het_mean

net_divergence <- dxy_median - het_mean
net_divergence

split_time_gen <- net_divergence/(2 * 3.5e-9)
split_time_gen

split_time <- split_time_gen*3 #3 years average gen time
split_time

#Malawi:
#split time (gen) = 1,534,705.19627417
#split time = 4,604,115.58882252

#######################

# Divergence between clades - for discussion
# divergence of CSA, Psuedocrenilabrus and Orthochromis from Malawi
# also divergence of ruaha blue from Malawi and Victoria lineages to each other for comparison

#CSA and Malawi

#subset distance matrix get pairwise differences for 2 selected groups
#select rows with samples from group 1

csa <- c("Serranochromis_angusticeps", "Serranochromis_macrocephalus", "Serranochromis_sp-checkerboard", "Pharyngochromis_acuticeps1", "Pharyngochromis_acuticeps2", "Sargochromis_carlottae", "Serranochromis_robustus", "Thoracochromis_brauschi")

malawi <- c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka", "diplotaxodon", "rhamphochromis")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% csa,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$clade_SG %in% malawi,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
CSA_divergence <- dxy_median

#orthochromis and Malawi

ortho <- c("Orthochromis_uvinzae", "Orthochromis_mazimeroensis", "Orthochromis_malagaraziensis")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% ortho,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$clade_SG %in% malawi,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
orthochromis_divergence <- dxy_median

#pseudocrenilabrus and Malawi

pseudo <- c("Orthochromis_indermauri", "Orthochromis_sp-red-cheek", "Pseudocrenilabrus_philander", "Pseudocrenilabrus_multicolor")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% pseudo,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$clade_SG %in% malawi,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
pseudocrenilabrus_divergence <- dxy_median

#ruaha blue and Malawi

ruahablue <- c("Astatotilapia_sp-Ruaha-blue")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% ruahablue,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$clade_SG %in% malawi,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
ruaha_blue_divergence <- dxy_median

#Malawi to Malawi

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$clade_SG %in% malawi,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$clade_SG %in% malawi,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)
malawi_divergence <- dxy_median

#victoria riverines to LVRS

#victoria_donor <- c("Astatotilapia_gracilior", "Astatotilapia_pharyngalis")
victoria_donor <- c("Haplochromis_stappersii")

LVRS <- c("Gaurochromis_sp-stone", "Haplochromis_vonlinnei",
          "Pyxichromis_orthostoma", "Haplochromis_chilotes1", "Haplochromis_nubilus", 
         "Haplochromis_howesi", "Neochromis_gigas", "Neochromis_sp-yellow-anal-scraper", 
         "Neochromis_rufocaudalis", "Neochromis_sp-long-black", "Neochromis_omnicaeruleus", 
         "Neochromis_sp-unicuspid-scraper", "Neochromis_greenwoodi", "Haplochromis_bicolor", 
         "Haplochromis_xenognathus", "Haplochromis_chilotes2", "Haplochromis_ishmaeli", 
         "Haplochromis_laparogramma", "Yssichromis_sp-plumbus", "Enterochromis_cinctus", 
         "Pundamilia_pundamilia", "Pundamilia_nyererei", "Lipochromis_cryptodon", 
         "Astatotilapia_latifasciata", "Astatotilapia_sp-nubila-rocks", "Haplochromis_lividus", 
          "Haplochromis_paucidens", "Haplochromis_vittatus")

#and select columns with samples from group 2
meta3_group1 <- meta3[meta3$full_name %in% victoria_donor,]
#meta3_group1$Sample_name
meta3_group2 <- meta3[meta3$full_name %in% LVRS,]
#meta3_group2$Sample_name
dist4 <- dist3[as.vector(meta3_group1$Sample_name), as.vector(meta3_group2$Sample_name), drop = FALSE]
#dist4

#check that columns and row names match what set as subseted
setdiff(rownames(dist4), meta3_group1$Sample_name)  #should give no output
setdiff(colnames(dist4), meta3_group2$Sample_name)  #should give no output

#check for NAs
which(is.na(dist4)) #should give no output

#calculate median distance
dxy_median <- median(unlist(dist4), na.rm = TRUE)


victoria_divergence <- dxy_median
victoria_divergence

CSA_divergence/victoria_divergence
pseudocrenilabrus_divergence/victoria_divergence
orthochromis_divergence/victoria_divergence
ruaha_blue_divergence/victoria_divergence


unique(meta3$full_name)






#extra:
#use updated NJ tree script and metadata? does this change anything?
#remove all extra samples that were removed before
