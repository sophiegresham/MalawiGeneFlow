#November 2024
#changed some non-malawi species names - see log file



library(dplyr)
library(ggplot2)

setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/dtools/dtrios_cluster/output/sophie_malawi_nonmalawi_may2023")
BBAA <- read.table("sophie_malawi_nonmalawi_may2023_BBAA.txt", sep = "\t", header = TRUE)

head(BBAA)

#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-24-08_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
malawisamples_regions_dtriosver <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2023-11-05_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
length(malawisamples_regions_dtriosver$sequence_id)
tail(malawisamples_regions_dtriosver)
#malawisamples_regions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2024-04-08_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
malawisamples_regions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2024-05-11_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
length(malawisamples_regions$sequence_id)


unique_species <- malawisamples_regions[!duplicated(malawisamples_regions$full_name),]
groupings <- data.frame(unique_species$full_name, unique_species$clade_SG)
colnames(groupings)[1] <- "P3"
#unique(groupings)

malawisamples_regions[c(611,612),]

#some species names have changed in the metadata but not in the BBAA file
#we need to find and replace these

#get old and new species list
malawisamples_old <- malawisamples_regions_dtriosver[-c(611,612),]
malawisamples <- malawisamples_regions[-c(612),] 
malawisamples2 <- malawisamples[!malawisamples$sequence_id == "cichlid7020224",]

malawisamples_old2 <- malawisamples_old[,c("sequence_id", "full_name")]
malawisamples3 <- malawisamples2[,c("sequence_id", "full_name")]

colnames(malawisamples_old2) <- c("sequence_id", "full_name_old")

#check the lists are the same
#tail(malawisamples3)
#tail(malawisamples_old2)
#length(malawisamples3$sequence_id)
#length(malawisamples_old2$sequence_id)

#merge into one dataframe
merged_samples <- merge(malawisamples3, malawisamples_old2, by = "sequence_id", all.x = TRUE, all.y = TRUE)
#length(merged_samples$sequence_id)

#find differences between old and new names
merged_samples$diff <- ifelse(as.character(merged_samples$full_name) == as.character(merged_samples$full_name_old), 'same',
                             ifelse(as.character(merged_samples$full_name) != as.character(merged_samples$full_name_old), 'diff', 'none'))

#select only the different species
different <- merged_samples[merged_samples$diff == "diff",]
different2 <- unique(different$full_name_old)
head(different)

BBAA_change_names <- BBAA

#replace P1, P2 and P3 names with the new names
replace_names <- function(old_name) {
  new_name <- different$full_name[different$full_name_old == old_name]
  if (length(new_name) == 0) {
    return(old_name)  # If no new name found, return old name
  } else {
    return(new_name[1])  # Otherwise, return the new name
  }
}

different$full_name_old <- as.character(different$full_name_old)
different$full_name <- as.character(different$full_name)
BBAA_change_names$P1 <- as.character(BBAA_change_names$P1)
BBAA_change_names$P2 <- as.character(BBAA_change_names$P2)
BBAA_change_names$P3 <- as.character(BBAA_change_names$P3)

BBAA_change_names$P1 <- sapply(BBAA_change_names$P1, replace_names)
BBAA_change_names$P2 <- sapply(BBAA_change_names$P2, replace_names)
BBAA_change_names$P3 <- sapply(BBAA_change_names$P3, replace_names)



#check changes have been made

#should have something in dataframes here
#head(BBAA[BBAA$P1 == "Astatotilapia_sp-Ruaha-yellow1",])
#head(BBAA[BBAA$P2 == "Astatotilapia_sp-Ruaha-yellow1",])
#head(BBAA[BBAA$P3 == "Astatotilapia_sp-Ruaha-yellow1",])
#but not here
#BBAA_change_names[BBAA_change_names$P1 == "Astatotilapia_sp-Ruaha-yellow1",]
#BBAA_change_names[BBAA_change_names$P2 == "Astatotilapia_sp-Ruaha-yellow1",]
#BBAA_change_names[BBAA_change_names$P3 == "Astatotilapia_sp-Ruaha-yellow1",]

#head(BBAA[BBAA$P1 == "Astatotilapia_latifasciata",])
#head(BBAA[BBAA$P2 == "Astatotilapia_latifasciata",])
#head(BBAA[BBAA$P3 == "Astatotilapia_latifasciata",])
#head(BBAA_change_names[BBAA_change_names$P1 == "Astatotilapia_latifasciata",])
#head(BBAA_change_names[BBAA_change_names$P1 == "Haplochromis_latifasciatus",])
#head(BBAA_change_names[BBAA_change_names$P2 == "Astatotilapia_latifasciata",])
#head(BBAA_change_names[BBAA_change_names$P2 == "Haplochromis_latifasciatus",])
#head(BBAA_change_names[BBAA_change_names$P3 == "Astatotilapia_latifasciata",])
#head(BBAA_change_names[BBAA_change_names$P3 == "Haplochromis_latifasciatus",])

head(BBAA[BBAA$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",])
head(BBAA_change_names[BBAA_change_names$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",])
head(BBAA_change_names[BBAA_change_names$P1 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",])


#how many unique Malawi and non-malawi species

malawisamples <- malawisamples_regions[-c(612),] #ancestral sample

species_count <- malawisamples %>% group_by(region) %>%
  summarize(num_species = n_distinct(full_name))
species_count

unique(malawisamples$clade_SG)

#malawi 239
#non-malawi 76

#17.12.2024
#test P1, P2 and P3 = Non-Malawi

#are there any signs of gene flow between the non-Malawi groups
#this could inpact interpretation of results

#BBAA_test <- BBAA[order(-BBAA$f4.ratio),]
#BBAA_test[100:150,]

pvalue <- 5e-2
malawisamples <- malawisamples_regions[-c(612),]
malawisamples2 <- malawisamples[!malawisamples$sequence_id == "cichlid7020224",]
malawisamples2$genus_species <- paste(malawisamples2$genus, malawisamples2$species, sep = "_")

#non_malawi_groups <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_Victoria-cluster", "LVRS", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Orthochromis", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
non_malawi_groups <- c("Serr_Pharyng_Sarg_Thora", "LVRS", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Orthochromis", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano <- malawisamples2[malawisamples2$clade_SG %in% non_malawi_groups,]
outgroups_serrano <- as.vector(unique(malawisamples_serrano$genus_species))

BBAA_keep <- BBAA_change_names[BBAA_change_names$P3 %in% outgroups_serrano,]
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_serrano,]
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P1 %in% outgroups_serrano,]

l <- length(BBAA_keep3$P2)

colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(BBAA_keep4)[11] <- "P3_group"
colnames(groupings)[1] <- "P2"
BBAA_keep5 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(BBAA_keep5)[12] <- "P2_group"
colnames(groupings)[1] <- "P1"
BBAA_keep6 <- merge(BBAA_keep5, groupings, by = "P1", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(BBAA_keep6)[13] <- "P1_group"


significant <- BBAA_keep6[BBAA_keep6$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
BBAA_keep7 <- BBAA_keep6 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

BBAA_test <- BBAA_keep7[order(-BBAA_keep7$f4.ratio),]
#BBAA_test[400:450,]

BBAA_test <- significant[significant$P3_group %in% "Serr_Pharyng_Sarg_Thora" ,]
BBAA_test[order(-BBAA_test$f4.ratio),]


#variables:
pvalue <- 5e-2

#malawisamples <- malawisamples_regions[-c(611,612),]
malawisamples <- malawisamples_regions[-c(612),]
malawisamples2 <- malawisamples[!malawisamples$sequence_id == "cichlid7020224",]
malawisamples2$genus_species <- paste(malawisamples2$genus, malawisamples2$species, sep = "_")


#get list of P3 species
non_malawi_groups <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_Victoria-cluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Orthochromis", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano <- malawisamples2[malawisamples2$clade_SG %in% non_malawi_groups,]
outgroups_serrano <- as.vector(unique(malawisamples_serrano$genus_species))

#get list of P1 species (all LVRS species)
malawisamples_victoria <- malawisamples2[malawisamples2$clade_SG == "LVRS",]
#length(malawisamples_victoria$sequence_id)
#tail(malawisamples_victoria)
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))
#length(outgroups_victoria)
#sort(outgroups_victoria)

#keep only non-malawi groups in P3
BBAA_keep <- BBAA_change_names[BBAA_change_names$P3 %in% outgroups_serrano,]
#length(BBAA_keep$P1) this is the same as before

#get list of Malawi species
malawisamples_onlymalawi <- malawisamples2[malawisamples2$region == "Malawi",]
#length(malawisamples_onlymalawi$sequence_id)
#BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% outgroups_victoria,]
#BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% unique(malawisamples_onlymalawi$genus_species),]

#keep only victoria group and malawi species in P1 and P2
#BBAA_keep1 <- BBAA_keep[BBAA_keep$P1 %in% outgroups_victoria,]
#length(BBAA_keep1$P1)


BBAA_keep1 <- BBAA_keep[BBAA_keep$P1 %in% outgroups_victoria & BBAA_keep$P2 %in% unique(malawisamples_onlymalawi$genus_species),]
length(BBAA_keep1$P1)
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria & BBAA_keep$P1 %in% unique(malawisamples_onlymalawi$genus_species),]
length(BBAA_keep2$P1)
BBAA_keep3 <- rbind(BBAA_keep1, BBAA_keep2)
length(BBAA_keep3$P1)
l <- length(BBAA_keep3$P2)

#remove ruaha blue LVRS cluster species - from P3
#BBAA_keep3_noruahalvrs <- BBAA_keep3
BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P3 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]

#remove a.kilossana
BBAA_keep3_noruahalvrs_nokilossana <- BBAA_keep3_noruahalvrs[!BBAA_keep3_noruahalvrs$P3 == "Astatotilapia_sp-kilossana",]


#write BBAA with only Malawi P1 and P2 and Outgroup P3
#write.table(BBAA_keep3, "out_sim_wholegenome_dtrios_parallel_allchroms_allsamples_newmetahannesdec2021_rmSRR12700905_rmSRR12700906_combined_split_combined_MalawiP1_LVRSP2_SerranoGroupP3_BBAA.txt", quote = FALSE, row.names = FALSE, sep = "\t")

#reorder for easier reading
BBAA_keep3_noruahalvrs_nokilossana2 <- as.data.frame(lapply(BBAA_keep3_noruahalvrs_nokilossana, unlist))
BBAA_keep4 <- BBAA_keep3_noruahalvrs_nokilossana2[order(BBAA_keep3_noruahalvrs_nokilossana2$P2),]
BBAA_keep5 <- BBAA_keep4[order(BBAA_keep4$P3),]

colnames(groupings)[1] <- "P3"
BBAA_keep6 <- merge(BBAA_keep5, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep7 <- merge(BBAA_keep6, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)

#change P2 victoria/P1 malawi trios to have negative stat values (d/f4)
logical_vec <- BBAA_keep7$P2 %in% outgroups_victoria
BBAA_keep7$Dstatistic[logical_vec] <- -BBAA_keep7$Dstatistic[logical_vec]
BBAA_keep7$Z.score[logical_vec] <- -BBAA_keep7$Z.score[logical_vec]
BBAA_keep7$f4.ratio[logical_vec] <- -BBAA_keep7$f4.ratio[logical_vec]


#which tests are significant?
significant <- BBAA_keep7[BBAA_keep7$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
not_significant <- BBAA_keep7[BBAA_keep7$p.value >= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of all significant tests:")
length(significant$P1)
print("total number of all trios:")
length(BBAA_keep7$P1)
percentsig <- length(significant$P1)/length(BBAA_keep6$P1) *100
print("percentage of all significant tests:")
percentsig
print("####################")

#label significant comparisons
BBAA_keep8 <- BBAA_keep7 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))


sig_malawi_P2 <- BBAA_keep8[BBAA_keep8$significant == "TRUE" & BBAA_keep8$P2 %in% unique(malawisamples_onlymalawi$genus_species),]
malawi_P2 <- BBAA_keep8[BBAA_keep8$P2 %in% unique(malawisamples_onlymalawi$genus_species),]
print("number of P2=Malawi significant tests:")
length(sig_malawi_P2$P1)
print("total number of all P2=Malawi trios:")
length(malawi_P2$P1)
print("percentage of P2=Malawi significant tests:")
length(sig_malawi_P2$P1)/length(malawi_P2$P1) * 100
print("percentage of P2=Malawi significant tests out of all tests:")
length(sig_malawi_P2$P1)/length(BBAA_keep7$P1) * 100
print("####################")

sig_victoria_P2 <- BBAA_keep8[BBAA_keep8$significant == "TRUE" & BBAA_keep8$P2 %in% outgroups_victoria,]
victoria_P2 <- BBAA_keep8[BBAA_keep8$P2 %in% outgroups_victoria,]
print("number of P2=Victoria significant tests:")
length(sig_victoria_P2$P1)
print("total number of all P2=Victoria trios:")
length(victoria_P2$P1)
print("percentage of P2=Victoria significant tests:")
length(sig_victoria_P2$P1)/length(victoria_P2$P1) * 100
print("percentage of P2=Victoria significant tests out of all tests:")
length(sig_victoria_P2$P1)/length(BBAA_keep7$P1) * 100


#significant[order(significant$p.value, decreasing = FALSE),]

#before removing A.kilossana:
#[1] "number of all significant tests:"
#296767
#[1] "total number of all trios:"
#331254
#[1] "percentage of all significant tests:"
#89.5889559069475
#[1] "####################"
#[1] "number of P2=Malawi significant tests:"
#171551
#[1] "total number of all P2=Malawi trios:"
#192287
#[1] "percentage of P2=Malawi significant tests:"
#89.2161196544748
#[1] "percentage of P2=Malawi significant tests out of all tests:"
#51.7883557632512
#[1] "####################"
#[1] "number of P2=Victoria significant tests:"
#125216
#[1] "total number of all P2=Victoria trios:"
#138967
#[1] "percentage of P2=Victoria significant tests:"
#90.1048450351522
#[1] "percentage of P2=Victoria significant tests out of all tests:"
#37.8006001436964


#check no a.kilossana or ruaha blue victoria clade
#these should all have nothing in them
BBAA_keep3_noruahalvrs[BBAA_keep3_noruahalvrs$P1 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]
BBAA_keep3_noruahalvrs[BBAA_keep3_noruahalvrs$P2 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]
BBAA_keep3_noruahalvrs[BBAA_keep3_noruahalvrs$P3 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]

BBAA_keep3_noruahalvrs_nokilossana[BBAA_keep3_noruahalvrs_nokilossana$P1 == "Astatotilapia_sp-kilossana",]
BBAA_keep3_noruahalvrs_nokilossana[BBAA_keep3_noruahalvrs_nokilossana$P2 == "Astatotilapia_sp-kilossana",]
BBAA_keep3_noruahalvrs_nokilossana[BBAA_keep3_noruahalvrs_nokilossana$P3 == "Astatotilapia_sp-kilossana",]


#check file
#which groups are in P1, P2 and P3
head(BBAA_keep7)
unique(malawi_P2$unique_species.clade_SG.y)
unique(malawi_P2$unique_species.clade_SG.x)
length(unique(malawi_P2$unique_species.clade_SG.x))
unique(victoria_P2$unique_species.clade_SG.y)
length(unique(victoria_P2$unique_species.clade_SG.x))

###statistics for manuscript text
#################################

#P3 = ruaha blue

#unique(significant_comb$P3)
ruahablue <- significant[significant$P3 == "Astatotilapia_sp-Ruaha-blue",]
print('ruaha blue')
max(ruahablue$f4.ratio)
mean(ruahablue$f4.ratio)
min((ruahablue$f4.ratio))
#head(ruahablue %>% arrange(desc(f4.ratio)))
#head(ruahablue %>% arrange(f4.ratio))
max(ruahablue$Dstatistic)
#hist(ruahablue$f4.ratio, breaks = 100, xlim = c(0.01,0.10))
#plot(ruahablue$Dstatistic)
#plot(ruahablue$Z.score)

#P3 = orthochromis indermauri
print('ortho')
orthoindo <- significant[significant$P3 == "Orthochromis_indermauri",]
max(orthoindo$f4.ratio)

#P3 = interochromis loocki
intero <- significant[significant$P3 == "Interochromis_loocki",]
max(intero$f4.ratio)

#which malawi species are significant?
#intero2 <- intero[intero$P1 == "Astatotilapia_flaviijosephi",]
#table(intero2$unique_species.clade_SG.y)
#intero2 <- intero[intero$P1 == "Astatotilapia_bloyeti",]
#table(intero2$unique_species.clade_SG.y)

#intero <- BBAA_keep8[BBAA_keep8$P3 == "Interochromis_loocki",]
#intero2 <- intero[intero$P1 == "Astatotilapia_flaviijosephi",]
#table(intero2$unique_species.clade_SG.y)

#P3 - orthochromis
orthochromis <- significant[significant$unique_species.clade_SG.x == "Orthochromis",]
max(orthochromis$f4.ratio)
max(orthochromis$Dstatistic)

#P3 - Psuedocrenilabrus
csa <- significant[significant$unique_species.clade_SG.x == "Serr_Pharyng_Sarg_Thora",]
max(csa$f4.ratio)
max(csa$Dstatistic)

#P3 - groups orthochromis, csa, or pseudocrenilabrus
newgroups <- significant[significant$unique_species.clade_SG.x == "Orthochromis" | significant$unique_species.clade_SG.x == "Pseudo_Cteno_Ortho2" | significant$unique_species.clade_SG.x == "Serr_Pharyng_Sarg_Thora",]
max(newgroups$f4.ratio)
min(newgroups$f4.ratio[which(newgroups$f4.ratio>0)])

#P3 - gigliolii
gigliolii <- significant[significant$unique_species.clade_SG.x == "gigliolii",]
print('gigliolii')
max(gigliolii$f4.ratio)
min(gigliolii$f4.ratio)
mean(gigliolii$f4.ratio)
max(gigliolii$Dstatistic)

#P3 = pseudocrenilabrus multicolor - investigate negative trios
pseudo <- significant[significant$P3 == "Pseudocrenilabrus_multicolor",]
#pseudo2 <- pseudo[pseudo$Dstatistic<0,] 
#unique(pseudo2$unique_species.clade_SG.y)
#length(unique(pseudo2$P2))
#table(pseudo2$P2)
#unique(pseudo2$P2)


pseudo2 <- significant[significant$P3 == "Orthochromis_indermauri",]
max(pseudo2$Dstatistic)
max(pseudo2$f4.ratio)
pseudo2 <- significant[significant$P3 == "Pseudocrenilabrus_philander",]
max(pseudo2$Dstatistic)
max(pseudo2$f4.ratio)
pseudo2 <- significant[significant$P3 == "Orthochromis_sp-red-cheek",]
max(pseudo2$Dstatistic)

pseudo_multi <- significant[significant$P3 == "Pseudocrenilabrus_multicolor",]
#pseudo_multi
#plot(pseudo_multi$Dstatistic)
#unique(pseudo_multi$unique_species.clade_SG.y)
sub <- pseudo_multi[pseudo_multi$Dstatistic <= 0,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#head(sub)
#unique(sub$unique_species.clade_SG.y)
#unique(sub$P1)

unique(significant$P3)
tangan <- significant[significant$P3 == "Tropheus_sp-black",]
tangan <- significant[significant$P3 == "Tropheus_annectens",]
tangan <- significant[significant$P3 == "Shuja_horei",]
tangan <- significant[significant$P3 == "Pseudosimochromis_curvifrons",]
tangan <- significant[significant$P3 == "Pseudosimochromis_babaulti",]
tangan <- significant[significant$P3 == "Petrochromis_orthognathus",]
tangan <- significant[significant$P3 == "Petrochromis_fasciolatus",]
tangan <- significant[significant$P3 == "Lobochilotes_labiatus",]
tangan <- significant[significant$P3 == "Gnathochromis_pfefferi",]
head(tangan)
unique(tangan$P2)
length(unique(tangan$P2))

#add in clade names for P1, P2 and P3

colnames(groupings)[1] <- "P3"
BBAA_change_names2 <- merge(BBAA_change_names, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_change_names3 <- merge(BBAA_change_names2, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P1"
BBAA_change_names4 <- merge(BBAA_change_names3, groupings, by = "P1", all.x = TRUE, all.y = FALSE, sort = FALSE)
head(BBAA_change_names4)
#colnames(BBAA_change_names4)[11:13] <- c("P3_clade", "P2_clade", "P1_clade")

#head(BBAA_change_names)
#BBAA_change_names[BBAA_change_names$P1 == "Alticorpus_geoffreyi" & BBAA_change_names$P2 == "Alticorpus_mentale" & BBAA_change_names$P3 == "Abactochromis_labrosus",]$Dstatistic/BBAA_change_names[BBAA_change_names$P1 == "Alticorpus_geoffreyi" & BBAA_change_names$P2 == "Aulonocara_ethelwynnae" & BBAA_change_names$P3 == "Abactochromis_labrosus",]$Dstatistic
#BBAA_change_names[BBAA_change_names$P1 == "Alticorpus_geoffreyi" & BBAA_change_names$P2 == "Alticorpus_mentale" & BBAA_change_names$P3 == "Orthochromis_uvinzae",]$Dstatistic/BBAA_change_names[BBAA_change_names$P1 == "Alticorpus_geoffreyi" & BBAA_change_names$P2 == "Aulonocara_ethelwynnae" & BBAA_change_names$P3 == "Orthochromis_uvinzae",]$Dstatistic
#BBAA_change_names[BBAA_change_names$P1 %in% unique(malawisamples_onlymalawi$genus_species) & BBAA_change_names$P2 %in% unique(malawisamples_onlymalawi$genus_species) & BBAA_change_names$P3 %in% unique(malawisamples_onlymalawi$genus_species),]


#filter

#orthochromis
#filter <- BBAA_change_names4[BBAA_change_names4$P2_clade == "Orthochromis" & BBAA_change_names4$P1_clade == "Orthochromis" & BBAA_change_names4$P3_clade == c("shallowbenthic", "deepbenthic", "utaka", "mbuna", "acalliptera", "diplotaxodon", "rhamphochromis", "LVRS"),]
#filter <- BBAA_change_names4[BBAA_change_names4$P2_clade == "Orthochromis" & BBAA_change_names4$P1_clade == "Orthochromis" & !BBAA_change_names4$P3_clade == "Orthochromis",]


#csa
#filter <- BBAA_change_names4[BBAA_change_names4$P2_clade == "Serr_Pharyng_Sarg_Thora" & BBAA_change_names4$P1_clade == "Serr_Pharyng_Sarg_Thora" & BBAA_change_names4$P3_clade == c("shallowbenthic", "deepbenthic", "utaka", "mbuna", "acalliptera", "diplotaxodon", "rhamphochromis", "LVRS"),]
#filter <- BBAA_change_names4[BBAA_change_names4$P2_clade == "Serr_Pharyng_Sarg_Thora" & BBAA_change_names4$P1_clade == "Serr_Pharyng_Sarg_Thora" & !BBAA_change_names4$P3_clade == "Serr_Pharyng_Sarg_Thora",]


#pseudocrenilabrus
filter <- BBAA_change_names4[BBAA_change_names4$P2_clade == "Pseudo_Cteno_Ortho2" & BBAA_change_names4$P1_clade == "Pseudo_Cteno_Ortho2" & BBAA_change_names4$P3_clade == c("shallowbenthic", "deepbenthic", "utaka", "mbuna", "acalliptera", "diplotaxodon", "rhamphochromis", "LVRS"),]
#filter <- BBAA_change_names4[BBAA_change_names4$P2_clade == "Pseudo_Cteno_Ortho2" & BBAA_change_names4$P1_clade == "Pseudo_Cteno_Ortho2" & !BBAA_change_names4$P3_clade == "Pseudo_Cteno_Ortho2",]



l <- length(filter$P2)

#label significant trios
#significant <- filter[filter$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#filter2 <- filter %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))
#head(significant)
#unique(significant$P3_clade)

#what percentage of trios are sig?
#l <- (length(filter2[filter2$significant == "TRUE",]$P1)/length(filter2$P1))*100
#l

#filter2[order(filter2$f4.ratio, decreasing = TRUE),]







par(mar = c(5, 15, 4, 2) + 0.1)

significant$P3_clade <- factor(significant$P3_clade, levels = unique(significant$P3_clade))

boxplot(f4.ratio ~ P3_clade, data = significant, horizontal = TRUE, las = 2, ylab = "")
boxplot(Dstatistic ~ P3_clade, data = significant, horizontal = TRUE, las = 2, ylab = "")


#install.packages("svglite")
library(svglite)

head(significant)

######### FINAL PLOT #########

#figure 2

#add in tree
#change dstat and f4-ratio to percentage to match manuscript text?


#library(gridExtra)
#library(gtable)
library(egg)
library(svglite)
library(cowplot)

nonmalawi_group_order <- c("LVRS", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")

# Order the data frames by clade and P3
significant_group_order <- significant %>% 
    arrange(factor(unique_species.clade_SG.x, levels = nonmalawi_group_order), P3)

not_significant_group_order <- not_significant %>% 
    arrange(factor(unique_species.clade_SG.x, levels = nonmalawi_group_order), P3)

# Reorder P3 in both data frames using the same order
species_order <- unique(significant_group_order$P3)
significant_group_order$P3 <- factor(significant_group_order$P3, levels = species_order)
not_significant_group_order$P3 <- factor(not_significant_group_order$P3, levels = species_order)


#get percentage of sig trios per P3
total_count <- table(BBAA_keep8$P3)
sig_count <- table(significant$P3)
counts <- data.frame(total_count, sig_count)
counts2 <- counts[-c(3)]
colnames(counts2) <- c("P3", "total_count", "sig_count")
counts2$percentage <- (counts2$sig_count/counts2$total_count)*100

all_samples_group_info <- data.frame(malawisamples2$genus_species, malawisamples2$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")
all_samples_group_info2 <- all_samples_group_info[!duplicated(all_samples_group_info$P3),]

counts3 <- merge(counts2, all_samples_group_info2, by = "P3", all.x = TRUE, all.y = FALSE, no.dups = TRUE)
counts4 <- counts3[counts3$clade != "mbuna" & counts3$clade != "deepbenthic" & counts3$clade != "shallowbenthic"
                   & counts3$clade != "utaka" & counts3$clade != "acalliptera" & counts3$clade != "diplotaxodon"
                   & counts3$clade != "rhamphochromis" & counts3$clade != "LVRS" & counts3$clade != "ruaha_blue_LVRScluster",]
nonmalawi_group_order <- c("ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")
counts4_order <- counts4 %>% 
    arrange(factor(clade, levels = nonmalawi_group_order), P3)

text_labels <- data.frame(row = seq_along(round(counts4_order$percentage, 1)),
                          percentage = paste0(round(counts4_order$percentage, 1), "%"))

#expression(paste(bolditalic("Serranochromis"), bold(" sp. 'checkerboard'"))),
#expression(paste(italic("Astatotilapia")," sp. 'Ruaha blue'"))

species_order_neat <- c(
    expression(paste(bolditalic("Astatotilapia"), bold(" sp. 'Ruaha blue'"))), 
    expression(paste(bolditalic("Astatotilapia gigliolii"))),
    expression(paste(bolditalic("Astatotilapia burtoni"))), 
    expression(paste(bolditalic("Astatoreochromis straeleni"))), 
    expression(paste(bolditalic("Haplochromis vanheusdeni"))), 
    expression(paste(bolditalic("Interochromis loocki"))), 
    expression(paste(bolditalic("Jabarichromis pfefferi"))),
    expression(paste(bolditalic("Limnotilapia dardennii"))), 
    expression(paste(bolditalic("Lobochilotes labiata"))), 
    expression(paste(bolditalic("Petrochromis fasciolatus"))), 
    expression(paste(bolditalic("Petrochromis orthognathus"))),
    expression(paste(bolditalic("Pseudosimochromis babaulti"))), 
    expression(paste(bolditalic("Pseudosimochromis curvifrons"))),
    expression(paste(bolditalic("Shuja horei"))), 
    expression(paste(bolditalic("Tropheus annectens"))), 
    expression(paste(bolditalic("Tropheus"), bold(" sp. 'black'"))),
    expression(paste(bolditalic("Pharyngochromis acuticeps"), bold(" 2"))), 
    expression(paste(bolditalic("Pharyngochromis acuticeps"), bold(" 1"))),
    expression(paste(bolditalic("Sargochromis carlottae"))),
    expression(paste(bolditalic("Serranochromis angusticeps"))),
    expression(paste(bolditalic("Serranochromis macrocephalus"))), 
    expression(paste(bolditalic("Serranochromis robustus"))),
    expression(paste(bolditalic("Serranochromis"), bold(" sp. 'checkerboard'"))),
    expression(paste(bolditalic("Thoracochromis brauschi"))), 
    expression(paste(bolditalic("Orthochromis indermauri"))),
    expression(paste(bolditalic("Orthochromis"), bold(" sp. 'red cheek'"))),
    expression(paste(bolditalic("Pseudocrenilabrus multicolor"))),
    expression(paste(bolditalic("Pseudocrenilabrus philander"))),
    expression(paste(bolditalic("Ctenochromis scatebra"))),
    expression(paste(bolditalic("Orthochromis malagaraziensis"))),
    expression(paste(bolditalic("Orthochromis mazimeroensis"))),
    expression(paste(bolditalic("Orthochromis uvinzae")))
)



custom_colours <- c(
  "ruaha_blue" = "steelblue4",
  "gigliolii" = "firebrick2",
  "other_riverine_haplochromines_burtoni" = "palegreen3",
  "other_riverine_haplochromines_vanheusdeni" = "darkorange",
  "Tanganyika" = "lightskyblue2",
  "Serr_Pharyng_Sarg_Thora" = "darkorchid3",
  "Pseudo_Cteno_Ortho2" = "palevioletred2",
  "Ctenochromis_pectoralis" = "tan4",
  "Orthochromis" = "chartreuse4")

#plot

sizepoints <- 0.08

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_plusnonsig_nov2024_highres.png",width=23,height=14,units="in",res=1300)
#svglite("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_plusnonsig_apr2024.svg", width=23, height=14, system_fonts = list(sans = "Arial Unicode MS")) #alternative way to save svg
#par(mar = c(10,10,10,10))
d <- ggplot() +
    geom_jitter(data = significant_group_order, aes(x = P3, y = Dstatistic), alpha = 0, show.legend = FALSE, size = sizepoints)+ #plot transparent to get order correct
    #geom_vline(xintercept = 0, color = "black") + 
    geom_jitter(data = not_significant_group_order, aes(x = P3, y = Dstatistic), color = "lightgrey", alpha = 0.3, show.legend = FALSE, size = sizepoints)+
    geom_jitter(data = significant_group_order, aes(x = P3, y = Dstatistic, colour = unique_species.clade_SG.x), alpha = 1, show.legend = FALSE, size = sizepoints)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 20, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold')) +
    #scale_color_brewer(palette = "Paired") + 
    ylab("D-statistic") + xlab("P3 Non-Malawi Species") +
    scale_x_discrete(labels = species_order_neat) +
    scale_color_manual(values = custom_colours) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(2,2,1,1, "cm"))

d2 <- ggplot() +
    geom_jitter(data = significant_group_order, aes(x = P3, y = f4.ratio), alpha = 0, show.legend = FALSE, size = sizepoints)+ #plot transparent to get order correct
    #geom_vline(xintercept = 0, color = "black", size = 5) + 
    geom_jitter(data = not_significant_group_order, aes(x = P3, y = f4.ratio), color = "lightgrey", alpha = 0.3, show.legend = FALSE, size = sizepoints)+
    geom_jitter(data = significant_group_order, aes(x = P3, y = f4.ratio, colour = unique_species.clade_SG.x), alpha = 1, show.legend = FALSE, size = sizepoints)+
    coord_flip() +
    theme(axis.text.y = element_blank(), axis.text.x = element_text(size = 20, face = 'bold'),
         axis.title.y = element_blank(), axis.title.x = element_text(size = 25, face = 'bold')) +
    #scale_color_brewer(palette = "Paired") + 
    ylab(expression(paste(bold("f"[4]*"-ratio")))) + xlab("") +
    scale_color_manual(values = custom_colours) +
    scale_y_continuous(limits = c(min(significant_group_order$f4.ratio) - 0.001, max(significant_group_order$f4.ratio) + 0.005), breaks = c(-0.05, 0, 0.05, 0.1), labels = c("-0.05", "0.00", "0.05", "0.10"))+
    #scale_y_discrete(limits=c("-0.05", "0", "0.05", "0.10"))+
    theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(2,5,1,1, "cm"))

d3 <- ggplot() +
    annotate("text", x = max(significant_group_order$f4.ratio) + 0.01, y = unique(significant_group_order$P3),
             label = text_labels$percentage, size = 6, color = "black", hjust = 0) + 
    coord_cartesian(clip = "off") +
    theme_void()

#arrange plots so that they are the same width (excluding the yaxis)
combinedplot <- ggarrange(d + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 15)),
          d2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
          d3 + theme(plot.margin = margin(r = 50, l = 1)),
          nrow = 1, widths = c(5,5,0.15))
combinedplot

dev.off()


#save as svg as well - doesnt work
#ggsave("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_plusnonsig_apr2024.svg", plot = combinedplot, width = 23, height = 14, units = "in")


#https://www.r-bloggers.com/2019/05/the-small-multiples-plot-how-to-combine-ggplot2-plots-with-one-shared-axis/

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023





#investigate blocks:
# - are the blocks driven in a change of P1 or a change in P2?
# - which P1 species have the higher D-stats/Z-scores?

library(gridExtra)
library(ggplot2)


custom_colours_malawi <- c(
  "mbuna" = "steelblue4",
  "deepbenthic" = "darkorange",
  "utaka" = "mediumorchid3",
  "acalliptera" = "firebrick2",
  "shallowbenthic" = "lightskyblue2",
  "diplotaxodon" = "darkolivegreen3",
  "rhamphochromis" = "darkgreen"
)

significant_group_order_noneg <- significant_group_order[significant_group_order$f4.ratio > 0,] 


#supplementary figure {result2_gigliolii_f4-ratio_variance}
gigliolii <- significant_group_order_noneg[significant_group_order_noneg$P3 == "Astatotilapia_gigliolii",]

p1 <- ggplot(gigliolii, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(gigliolii, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(gigliolii, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_gigliolii_nov2024.png",width=20,height=10,units="in",res=1500)
ggarrange(p1 + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 0)),
          p2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
          p3 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
         nrow = 1, widths = c(5,5,5))
dev.off()



#supplementary figure {result2_Ruaha_blue_f4-ratio_variance}
ruaha <- significant_group_order[significant_group_order$unique_species.clade_SG.x == "ruaha_blue",]

p1 <- ggplot(ruaha, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(ruaha, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(ruaha, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_ruaha_blue_nov2024.png",width=20,height=10,units="in",res=1500)
ggarrange(p1 + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 0)),
          p2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
          p3 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
         nrow = 1, widths = c(5,5,5))
dev.off()


#generate legend to be pasted into plots
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_ruaha_blue_legend_nov2024.png",width=15,height=10,units="in",res=1500)
p1 <- ggplot(ruaha, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(ruaha, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(ruaha, aes(y = P1, x = f4.ratio, color = unique_species.clade_SG.y)) + 
geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = TRUE, size = 0.2) + 
theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), 
      axis.title.x = element_text(size = 20, face = 'bold'), 
      axis.text.x = element_text(size = 15, face = 'bold'), 
      legend.position = "bottom") + 
guides(color = guide_legend(override.aes = list(size = 5), title = "P2 Malawi Ecomorphological Group"))+
scale_color_manual(values = custom_colours_malawi)
ggarrange(p1 + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 0)),
          p2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
          p3 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15, b = 50)),
         nrow = 1, widths = c(1,1,10))
dev.off()



non_malawi_groups_noortho <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_Victoria-cluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples2[malawisamples2$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples2[malawisamples2$clade_SG %in% "Orthochromis",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples2[malawisamples2$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))



#keep only non-malawi group in P1
BBAA_keep <- BBAA_change_names[BBAA_change_names$P1 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]
BBAA_keep3_noruahalvrs_nokilossana <- BBAA_keep3_noruahalvrs[!BBAA_keep3_noruahalvrs$P1 == "Astatotilapia_sp-kilossana",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs_nokilossana, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep5 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)

#head(BBAA_keep5)

l <- length(BBAA_keep5$P1)

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
not_significant <- BBAA_keep5[BBAA_keep5$p.value >= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of all significant tests:")
length(significant$P1)
print("total number of all trios:")
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
print("percentage of all significant tests:")
percentsig

#BBAA_keep5 %>% arrange(desc(f4.ratio))
#head(BBAA_keep5 %>% arrange(f4.ratio))

BBAA_keep5_sub <- BBAA_keep5
BBAA_keep5_sub_species_P1 <- unique(BBAA_keep5_sub$P1)
BBAA_keep5_sub_species_P2 <- unique(BBAA_keep5_sub$P2)
BBAA_keep5_sub$P1 <- factor(BBAA_keep5_sub$P1, levels = BBAA_keep5_sub_species_P1)
BBAA_keep5_sub$P2 <- factor(BBAA_keep5_sub$P2, levels = BBAA_keep5_sub_species_P2)


#hist(BBAA_keep5_sub$f4.ratio)
#par(mar=c(10,15,1,1))
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "P3")
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P2, horizontal=TRUE, las=2, ylab = "")

## supplementary figure {result2_p1nonmalawi_p2victoria_p3orthochromis}

par(mar=c(10,15,10,10))
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4-ratio_P1nonmalawi_P2victoria_P3orthochromis_nov2024.png",width=10,height=8,units="in",res=1000)
ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Non-Malawi species") +
  ggtitle("P1 = Non-Malawi, P2 = Victoria, P3 = Orthochromis")+
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

par(mar=c(10,15,10,10))
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_P1nonmalawi_P2victoria_P3orthochromis_nov2024.png",width=10,height=8,units="in",res=1000)
ggplot(data = BBAA_keep5_sub, aes(x=Dstatistic, y=P1)) +
  geom_boxplot() +
  xlab("D statistic") +
  ylab("P1 Non-Malawi species") +
  ggtitle("P1 = Non-Malawi, P2 = Victoria, P3 = Orthochromis")+
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()

non_malawi_groups_noortho <- c("Orthochromis", "ruaha_blue_Victoria-cluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples2[malawisamples2$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples2[malawisamples2$clade_SG %in% "Serr_Pharyng_Sarg_Thora",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples2[malawisamples2$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))



#keep only non-malawi group in P1
BBAA_keep <- BBAA_change_names[BBAA_change_names$P1 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]
BBAA_keep3_noruahalvrs_nokilossana <- BBAA_keep3_noruahalvrs[!BBAA_keep3_noruahalvrs$P1 == "Astatotilapia_sp-kilossana",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs_nokilossana, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep5 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)

#head(BBAA_keep5)

l <- length(BBAA_keep5$P1)

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
not_significant <- BBAA_keep5[BBAA_keep5$p.value >= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of all significant tests:")
length(significant$P1)
print("total number of all trios:")
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
print("percentage of all significant tests:")
percentsig

#BBAA_keep5 %>% arrange(desc(f4.ratio))
#head(BBAA_keep5 %>% arrange(f4.ratio))

BBAA_keep5_sub <- BBAA_keep5
BBAA_keep5_sub_species_P1 <- unique(BBAA_keep5_sub$P1)
BBAA_keep5_sub_species_P2 <- unique(BBAA_keep5_sub$P2)
BBAA_keep5_sub$P1 <- factor(BBAA_keep5_sub$P1, levels = BBAA_keep5_sub_species_P1)
BBAA_keep5_sub$P2 <- factor(BBAA_keep5_sub$P2, levels = BBAA_keep5_sub_species_P2)


#hist(BBAA_keep5_sub$f4.ratio)
#par(mar=c(10,15,1,1))
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "P3")
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P2, horizontal=TRUE, las=2, ylab = "")

## supplementary figure {result2_p1nonmalawi_p2victoria_p3orthochromis}

par(mar=c(10,15,10,10))
#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4-ratio_P1nonmalawi_P2victoria_P3csa_nov2024.png",width=10,height=4,units="in",res=500)
ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Non-Malawi species") +
  ggtitle("P1 = Non-Malawi, P2 = Victoria, P3 = CSA")+
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
#dev.off()

ggplot(data = BBAA_keep5_sub, aes(x=Dstatistic, y=P1)) +
  geom_boxplot() +
  xlab("D statistic") +
  ylab("P1 Non-Malawi species") +
  ggtitle("P1 = Non-Malawi, P2 = Victoria, P3 = CSA")+
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples2[malawisamples2$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA_change_names[BBAA_change_names$P1 %in% outgroups_victoria,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% "Astatotilapia_sp-Ruaha-blue",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep5 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)

#head(BBAA_keep5)

l <- length(BBAA_keep5$P1)

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
not_significant <- BBAA_keep5[BBAA_keep5$p.value >= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of all significant tests:")
length(significant$P1)
print("total number of all trios:")
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
print("percentage of all significant tests:")
percentsig


BBAA_keep5_sub <- BBAA_keep5
BBAA_keep5_sub_species <- unique(BBAA_keep5_sub$P1)
BBAA_keep5_sub$P1 <- factor(BBAA_keep5_sub$P1, levels = BBAA_keep5_sub_species)

#### supplementary figure {result1_P1victoria_P2ruahablueLVRS_P3ruahablue} ####

#hist(BBAA_keep5_sub$f4.ratio)
par(mar=c(10,15,10,10))
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4-ratio_P1victoria_P2ruahablueLVRS_P3ruahablue_nov2024.png",width=10,height=8,units="in",res=500)
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "")
ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Victoria species") +
  ggtitle("P1 = Victoria, P2 = A. sp. 'Ruaha blue Victoria clade', P3 = A. sp. 'Ruaha blue'")+
  theme_bw()
dev.off()

## supplementary figure {result3_f4_ratio_variance_malawi_groups_aflaviijosephi}

#just a. flavijosephi

significant_group_order$p3group_colour <- with(significant_group_order, 
                         ifelse(unique_species.clade_SG.x == "ruaha_blue", 'steelblue4',
                         ifelse(unique_species.clade_SG.x == "gigliolii", 'firebrick2', 
                         ifelse(unique_species.clade_SG.x == "other_riverine_haplochromines_burtoni", 'palegreen3', 
                         ifelse(unique_species.clade_SG.x == "other_riverine_haplochromines_vanheusdeni", 'darkorange', 
                         ifelse(unique_species.clade_SG.x == "Tanganyika", 'lightskyblue2', 
                         ifelse(unique_species.clade_SG.x == "Serr_Pharyng_Sarg_Thora", 'darkorchid3', 
                         ifelse(unique_species.clade_SG.x == "Pseudo_Cteno_Ortho2", 'palevioletred2',      
                         ifelse(unique_species.clade_SG.x == "Ctenochromis_pectoralis", 'tan4',
                         ifelse(unique_species.clade_SG.x == "Orthochromis", 'chartreuse4',''))))))))))

#remove LVRS ruaha blue
significant_group_order_noruahaLVRScluster <- significant_group_order[!significant_group_order$P3 %in% c("Astatotilapia_sp-Ruaha-blue-Victoria-cluster"),]
#remove negative trios 
significant_group_order_noruahaLVRScluster_noneg <- significant_group_order_noruahaLVRScluster[significant_group_order_noruahaLVRScluster$f4.ratio > 0,] 

P3sigspecies <- unique(significant_group_order_noruahaLVRScluster_noneg$P3)

p1_species <- unique(significant_group_order_noruahaLVRScluster_noneg$P1)

#for (l in 1:length(p1_species)){
for (l in 1:1){
    significant_group_order_noruahaLVRScluster_noneg_onep1 <- significant_group_order_noruahaLVRScluster_noneg[significant_group_order_noruahaLVRScluster_noneg$P1 %in% p1_species[32],]
    #print(head(significant_group_order_noruahaLVRScluster_noneg_onep1))

    P3sigspecies <- unique(significant_group_order_noruahaLVRScluster_noneg_onep1$P3)  

    #dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_dstat_malawigrouped_perP3_", p1_species[32], "_apr2024.png", sep = ""), width=17, height=19, units="in", res=500)
    dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3_", p1_species[32], "_apr2024.png", sep = ""), width=17, height=19, units="in", res=500)
    par(oma = c(4, 4, 4, 1)) #bottom, left, top, right
    par(mfrow=c(ceiling(length(P3sigspecies)/4),4)) 
    #par(mfrow=c(ceiling(length(P3sigspecies)/5),5)) #width=20, height=16
    #par(mfrow=c(ceiling(length(P3sigspecies)/7),7)) #width=27, height=12
    for (i in 1:length(P3sigspecies)){
    #for (i in 1:1){
        sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == P3sigspecies[i],]
        sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
        #boxplot(sub$Dstatistic~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = paste("P3 = ",P3sigspecies[i], sep = ""), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = paste("P3 = ",P3sigspecies[i], sep = ""), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c("Diplotaxodon", "Rhamphochromis", "A. calliptera", "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )
    }
    mtext(paste("P1 Victoria = ", p1_species[32], sep = ""), side = 3, line = 1, outer = TRUE, font = 2)
    #mtext("D statistic", side = 2, line = 1, outer = TRUE, font = 2)
    mtext("f4-ratio", side = 2, line = 1, outer = TRUE, font = 2)
    mtext("P2 Malawi ecomorphological group", side = 1, line = 2, outer = TRUE, font = 2)
    dev.off()
}



#figure 2b 

#select only a few P3 species to show in figure - 6 species:
#orthochromis - malagarziensis, tanganyika - interochromis loocki, csa - Serranochromis macrocephalus, pseudocrenilabrus - orthochromis red cheek

significant_group_order_noruahaLVRScluster_noneg_onep1 <- significant_group_order_noruahaLVRScluster_noneg[significant_group_order_noruahaLVRScluster_noneg$P1 %in% p1_species[32],]

dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3_", p1_species[32], "selectP3species_apr2024.png", sep = ""), width=17, height=5, units="in", res=500)

par(oma = c(4, 14, 5, 1)) #bottom, left, top, right
par(mar = c(1, 1, 2, 1))
par(mfrow=c(1,6)) 

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Astatotilapia_sp-Ruaha-blue",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, horizontal = TRUE, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("A."), bold(" sp. 'Ruaha blue'"))), las = 1, yaxt = "n", col = sub$p3group_colour, border = "gray23")
axis(side = 2, labels = FALSE)
text(x = par("usr")[1] - 0.05 * (par("usr")[2] - par("usr")[1]), # Adjust the position for y-axis labels
     labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), 
                expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
     xpd = NA,
     y = 1:7,  # Place the labels along the y-axis
     srt = 0,  # Keep the labels horizontal
     adj = 1,
     cex = 1.5
)

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Astatotilapia_gigliolii",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, horizontal = TRUE, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("A. gigliolii"))), las = 1, yaxt = "n", col = sub$p3group_colour, border = "gray23")
axis(side = 2, labels = FALSE)


sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Interochromis_loocki",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, horizontal = TRUE, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("I. loocki"))), las = 1, yaxt = "n", col = sub$p3group_colour, border = "gray23")
axis(side = 2, labels = FALSE)
mtext(expression(paste(bold("Tanganyika group"))),side=3,line=2, cex = 0.8)


sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Serranochromis_macrocephalus",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, horizontal = TRUE, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("S. macrocephalus"))), las = 1, yaxt = "n", col = sub$p3group_colour, border = "gray23")
axis(side = 2, labels = FALSE)
mtext(expression(paste(bold("CSA group"))),side=3,line=2, cex = 0.8)


sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Orthochromis_sp-red-cheek",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, horizontal = TRUE, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("O."), bold(" sp. 'red cheek'"))), las = 1, yaxt = "n", col = sub$p3group_colour, border = "gray23")
axis(side = 2, labels = FALSE)
mtext(expression(paste(bolditalic("Pseudocrenilabrus"), bold(" group"))),side=3,line=2, cex = 0.8)


sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Orthochromis_malagaraziensis",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, horizontal = TRUE, xlab = "", ylab = "", 
        main = expression(paste(bold("P3 = "),bolditalic("O. malagaraziensis"))), las = 1, yaxt = "n",
        col = sub$p3group_colour, border = "gray23", names.arg = "")
axis(side = 2, labels = FALSE)
mtext(expression(paste(bolditalic("Orthochromis"), bold(" group"))),side=3,line=2, cex = 0.8)



mtext(expression(paste(bold("P1 Victoria = "), bolditalic("Astatotilapia flaviijosephi"))), side = 3, line = 3, outer = TRUE, font = 2)
mtext(expression(paste(bold("f"[4]*"-ratio"))), side = 1, line = 2, outer = TRUE, font = 2)
mtext("P2 Malawi ecomorphological group", side = 2, line = 12, outer = TRUE, font = 2)

dev.off()


#alternative way of plotting figure above - barplots are vertical not horizontal


#select only a few P3 species to show in figure - 6 species:
#orthochromis - malagarziensis, tanganyika - interochromis loocki, csa - Serranochromis macrocephalus, pseudocrenilabrus - orthochromis red cheek

significant_group_order_noruahaLVRScluster_noneg_onep1 <- significant_group_order_noruahaLVRScluster_noneg[significant_group_order_noruahaLVRScluster_noneg$P1 %in% p1_species[32],]

#dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3_", p1_species[32], "selectP3species_apr2024.png", sep = ""), width=14, height=8, units="in", res=500)

par(oma = c(4, 4, 4, 1)) #bottom, left, top, right
par(mfrow=c(2,3)) 

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Astatotilapia_sp-Ruaha-blue",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("Astatotilapia sp. 'Ruaha blue'"))), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Astatotilapia_gigliolii",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii"))), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Interochromis_loocki",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("Interochromis loocki"))), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Serranochromis_macrocephalus",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("Serranochromis macrocephalus"))), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Orthochromis_sp-red-cheek",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("Orthochromis"), bold("sp. 'red cheek'"))), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )

sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Orthochromis_malagaraziensis",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = expression(paste(bold("P3 = "),bolditalic("Orthochromis malagaraziensis"))), las = 1, xaxt = "n", col = sub$p3group_colour, border = "gray23")
        axis(side = 1, labels = FALSE)
        text(x = 1:7,
         labels = c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka"),
         xpd = NA,
         y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]) ,
         srt = 35,
         adj = 1,
         cex = 1.2
        )

mtext(expression(paste(bold("P1 Victoria = "), bolditalic("Astatotilapia flaviijosephi"))), side = 3, line = 1, outer = TRUE, font = 2)
mtext(expression(paste(bold("f"[4]*"-ratio"))), side = 2, line = 1, outer = TRUE, font = 2)
mtext("P2 Malawi ecomorphological group", side = 1, line = 2, outer = TRUE, font = 2)

#dev.off()



######################################
## supplementary figure {result3_f4_ratio_variance_malawi_groups_aflaviijosephi}

#just a. flavijosephi

library(gridExtra)
library(agricolae)
library(dplyr)
library(ggpubr)

significant_group_order$p3group_colour <- with(significant_group_order, 
                         ifelse(unique_species.clade_SG.x == "ruaha_blue", 'steelblue4',
                         ifelse(unique_species.clade_SG.x == "gigliolii", 'firebrick2', 
                         ifelse(unique_species.clade_SG.x == "other_riverine_haplochromines_burtoni", 'palegreen3', 
                         ifelse(unique_species.clade_SG.x == "other_riverine_haplochromines_vanheusdeni", 'darkorange', 
                         ifelse(unique_species.clade_SG.x == "Tanganyika", 'lightskyblue2', 
                         ifelse(unique_species.clade_SG.x == "Serr_Pharyng_Sarg_Thora", 'darkorchid3', 
                         ifelse(unique_species.clade_SG.x == "Pseudo_Cteno_Ortho2", 'palevioletred2',      
                         ifelse(unique_species.clade_SG.x == "Ctenochromis_pectoralis", 'tan4',
                         ifelse(unique_species.clade_SG.x == "Orthochromis", 'chartreuse4',''))))))))))

#expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii")))

group_order_neat <- c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "mbuna", "shallow benthic", "deep benthic", "utaka")

#remove LVRS ruaha blue
significant_group_order_noruahaLVRScluster <- significant_group_order[!significant_group_order$P3 %in% c("Astatotilapia_sp-Ruaha-blue-Victoria-cluster"),]
#remove negative trios 
significant_group_order_noruahaLVRScluster_noneg <- significant_group_order_noruahaLVRScluster[significant_group_order_noruahaLVRScluster$f4.ratio > 0,] 

P3sigspecies <- unique(significant_group_order_noruahaLVRScluster_noneg$P3)
P3sigspecies_neat <- c(expression(paste(bold("P3 = "), bolditalic("Astatotilapia "), bold("sp. "), bolditalic("'Ruaha blue'"))), 
                       expression(paste(bold("P3 = "), bolditalic("Astatotilapia gigliolii"))), 
                       expression(paste(bold("P3 = "), bolditalic("Interochromis loocki"))),
                       expression(paste(bold("P3 = "), bolditalic("Limnotilapia dardennii"))),
                       expression(paste(bold("P3 = "), bolditalic("Pharyngochromis acuticeps "), bold("1"))),
                       expression(paste(bold("P3 = "), bolditalic("Pharyngochromis acuticeps "), bold("2"))),
                       expression(paste(bold("P3 = "), bolditalic("Sargochromis carlottae"))),
                       expression(paste(bold("P3 = "), bolditalic("Serranochromis angusticeps"))), 
                       expression(paste(bold("P3 = "), bolditalic("Serranochromis macrocephalus"))),
                       expression(paste(bold("P3 = "), bolditalic("Serranochromis robustus"))),
                       expression(paste(bold("P3 = "), bolditalic("Serranochromis "), bold("sp. "), bolditalic("'checkerboard'"))),
                       expression(paste(bold("P3 = "), bolditalic("Thoracochromis brauschi"))), 
                       expression(paste(bold("P3 = "), bolditalic("Orthochromis indermauri"))), 
                       expression(paste(bold("P3 = "), bolditalic("Orthochromis "), bold("sp. "), bolditalic("'red cheek'"))), 
                       expression(paste(bold("P3 = "), bolditalic("Pseudocrenilabrus multicolor"))), 
                       expression(paste(bold("P3 = "), bolditalic("Pseudocrenilabrus philander"))), 
                       expression(paste(bold("P3 = "), bolditalic("Ctenochromis scatebra"))), 
                       expression(paste(bold("P3 = "), bolditalic("Orthochromis malagaraziensis"))), 
                       expression(paste(bold("P3 = "), bolditalic("Orthochromis mazimeroensis"))),
                       expression(paste(bold("P3 = "), bolditalic("Orthochromis uvinzae"))))

p1_species <- unique(significant_group_order_noruahaLVRScluster_noneg$P1)

#for (l in 1:length(p1_species)){
for (l in 1:1){
    significant_group_order_noruahaLVRScluster_noneg_onep1 <- significant_group_order_noruahaLVRScluster_noneg[significant_group_order_noruahaLVRScluster_noneg$P1 %in% p1_species[7],]
    #print(head(significant_group_order_noruahaLVRScluster_noneg_onep1))

    P3sigspecies <- unique(significant_group_order_noruahaLVRScluster_noneg_onep1$P3)  

    #dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_dstat_malawigrouped_perP3_", p1_species[32], "_apr2024.png", sep = ""), width=17, height=19, units="in", res=500)
    dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3_", p1_species[7], "_nov2024.png", sep = ""), width=17, height=19, units="in", res=500)
    par(oma = c(4, 4, 4, 1)) #bottom, left, top, right
    plot_list <- list() 
    
    #par(mfrow=c(ceiling(length(P3sigspecies)/4),4)) #4 in a row
    #par(mfrow=c(ceiling(length(P3sigspecies)/5),5)) #width=20, height=16
    #par(mfrow=c(ceiling(length(P3sigspecies)/7),7)) #width=27, height=12
    for (i in 1:length(P3sigspecies)){
    #for (i in 8:8){

        sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == P3sigspecies[i],]
        sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
        value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
        hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
        sig.letters2 <- data.frame(
                      unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
                      label = NA,
                      stringsAsFactors = FALSE
        )
        
        sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
        label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
        #label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + label_data$max_value*0.001)
        max_all <- max(label_data$max_value, na.rm = TRUE)
        min_all <- min(label_data$max_value, na.rm = TRUE)
        label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
        sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
        sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
        plot_list[[i]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = 10, face = "bold")) +
          scale_fill_identity() +
          labs(title = P3sigspecies_neat[i])
    }
    
    title_text <- textGrob(expression(bold("P1 Victoria = ") * bolditalic("Astatotilapia flaviijosephi")), gp = gpar(fontsize = 14))
    left_text <- textGrob(expression(bold("f"[4]*"-ratio")), rot = 90, gp = gpar(fontsize = 12))
    bottom_text <- textGrob(expression(bold("P2 Malawi ecomorphological group")), gp = gpar(fontsize = 12))
    grid.arrange(grobs = plot_list, ncol = 4, nrow = 5,
             top = title_text, left = left_text, bottom = bottom_text)
    dev.off()
}

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023


p1_species[7]

library(scales)
library(grid)

#alternative way of plotting figure above - barplots are vertical not horizontal

#select only a few P3 species to show in figure - 6 species:
#orthochromis - malagarziensis, tanganyika - interochromis loocki, csa - Serranochromis macrocephalus, pseudocrenilabrus - orthochromis red cheek

significant_group_order_noruahaLVRScluster_noneg_onep1 <- significant_group_order_noruahaLVRScluster_noneg[significant_group_order_noruahaLVRScluster_noneg$P1 %in% p1_species[32],]

group_order_neat <- c(expression(paste(italic("Diplotaxodon"))), expression(paste(italic("Rhamphochromis"))), expression(paste(italic("A. calliptera"))), "Mbuna", "Shallow benthic", "Deep benthic", "Utaka")

size_title <- 10

plot_list <- list() 

#ruaha blue
sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Astatotilapia_sp-Ruaha-blue",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
sig.letters2 <- data.frame(
        unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
        label = NA,
        stringsAsFactors = FALSE
)
      
sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
max_all <- max(label_data$max_value, na.rm = TRUE)
min_all <- min(label_data$max_value, na.rm = TRUE)
label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
plot_list[[1]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          coord_flip() +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(size = 15),
            plot.title = element_text(size = size_title, face = "bold", hjust = 0.5)) +
          scale_fill_identity() +
          labs(title = expression(atop(paste(bold(" Test ")), 
                              paste(bold("P3 = "),bolditalic("A."), bold(" sp. 'Ruaha blue'")))))

#gigliolii
sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Astatotilapia_gigliolii",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
sig.letters2 <- data.frame(
        unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
        label = NA,
        stringsAsFactors = FALSE
)
      
sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
max_all <- max(label_data$max_value, na.rm = TRUE)
min_all <- min(label_data$max_value, na.rm = TRUE)
label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
plot_list[[2]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          coord_flip() +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
          theme_bw() +
          theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = size_title, face = "bold", hjust = 0.5)) +
          scale_fill_identity() +
          labs(title = expression(atop(paste(bold(" Test ")), 
                              paste(bold("P3 = "), bolditalic("A. gigliolii")))))

#tanganyika
sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Interochromis_loocki",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
sig.letters2 <- data.frame(
        unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
        label = NA,
        stringsAsFactors = FALSE
)
      
sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
max_all <- max(label_data$max_value, na.rm = TRUE)
min_all <- min(label_data$max_value, na.rm = TRUE)
label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
plot_list[[3]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          coord_flip() +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
          theme_bw() +
          theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = size_title, face = "bold", hjust = 0.5)) +
          scale_fill_identity() +
          labs(title = expression(atop(paste(bold("Tanganyika group")), 
                              paste(bold("P3 = "), bolditalic("I. loocki")))))

#csa
sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Serranochromis_macrocephalus",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
sig.letters2 <- data.frame(
        unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
        label = NA,
        stringsAsFactors = FALSE
)
      
sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
max_all <- max(label_data$max_value, na.rm = TRUE)
min_all <- min(label_data$max_value, na.rm = TRUE)
label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
plot_list[[4]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          coord_flip() +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
          theme_bw() +
          theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = size_title, face = "bold", hjust = 0.5)) +
          scale_fill_identity() +
          labs(title = expression(atop(paste(bold("CSA group")), 
                              paste(bold("P3 = "), bolditalic("S. macrocephalus")))))

#pseudocrenilabrus
sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Orthochromis_sp-red-cheek",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
sig.letters2 <- data.frame(
        unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
        label = NA,
        stringsAsFactors = FALSE
)
      
sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
max_all <- max(label_data$max_value, na.rm = TRUE)
min_all <- min(label_data$max_value, na.rm = TRUE)
label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
plot_list[[5]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          coord_flip() +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
          theme_bw() +
          theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = size_title, face = "bold", hjust = 0.5)) +
          scale_fill_identity() +
          labs(title = expression(atop(paste(bolditalic("Pseudocrenilabrus"), " ", bold("group")), 
                              paste(bold("P3 = "), bolditalic("O. "), bold("sp. 'red cheek'")))))

#orthochromis
sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 == "Orthochromis_malagaraziensis",]
sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
value_max <- sub %>% group_by(unique_species.clade_SG.y) %>% summarize(max_value = max(f4.ratio, na.rm = TRUE))
hsd <- HSD.test(aov(f4.ratio ~ unique_species.clade_SG.y, data = sub), trt = "unique_species.clade_SG.y", group = TRUE)
sig.letters2 <- data.frame(
        unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y),
        label = NA,
        stringsAsFactors = FALSE
)
      
sig.letters2$label[match(rownames(hsd$groups), sig.letters2$unique_species.clade_SG.y)] <- hsd$groups$groups
label_data <- merge(sig.letters2, value_max, by = "unique_species.clade_SG.y", all.x = TRUE)
max_all <- max(label_data$max_value, na.rm = TRUE)
min_all <- min(label_data$max_value, na.rm = TRUE)
label_data$max_value <- ifelse(is.na(label_data$max_value), NA, label_data$max_value + (max_all-min_all)*0.08)
sub_filled <- expand.grid(unique_species.clade_SG.y = levels(sub$unique_species.clade_SG.y))
sub_filled <- merge(sub_filled, sub, all.x = TRUE)
        
plot_list[[6]] <- ggplot(data = sub_filled, aes(x = unique_species.clade_SG.y, y = f4.ratio)) +
          geom_boxplot(aes(fill = p3group_colour), na.rm = FALSE) +
          coord_flip() +
          geom_text(data = label_data, aes(x = unique_species.clade_SG.y, y = max_value, label = label), vjust = 0, na.rm = TRUE) +
          stat_boxplot(geom = 'errorbar', width = 0.1) +
          xlab("") + ylab("") +
          scale_x_discrete(labels = group_order_neat) +
          scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
          theme_bw() +
          theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(size = size_title, face = "bold", hjust = 0.5)) +
          scale_fill_identity() +
          labs(title = expression(atop(paste(bolditalic("Orthochromis"), " ", bold("group")), 
                              paste(bold("P3 = "), bolditalic("O. malagaraziensis")))))

dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3_", p1_species[32], "selectP3species_nov2024.png", sep = ""), width=17, height=5, units="in", res=500)

#title_text <- textGrob(expression(bold("P1 Victoria = ") * bolditalic("Astatotilapia flaviijosephi")), gp = gpar(fontsize = 14))
#bottom_text <- textGrob(expression(bold("f"[4]*"-ratio")), gp = gpar(fontsize = 12))
#left_text <- textGrob(expression(bold("P2 Malawi ecomorphological group")), rot = 90, gp = gpar(fontsize = 12))

combinedplot <- ggarrange(plot_list[[1]] + theme(plot.margin = margin(r = 0, t = 15)), 
                          plot_list[[2]] + theme(plot.margin = margin(l = 1, t = 15)), 
                          plot_list[[3]] + theme(plot.margin = margin(l = 1, r = 0, t = 15)), 
                          plot_list[[4]] + theme(plot.margin = margin(l = 1, r = 0, t = 15)),
                          plot_list[[5]] + theme(plot.margin = margin(l = 1, r = 0, t = 15)), 
                          plot_list[[6]] + theme(plot.margin = margin(l = 1, r = 2, t = 15)),
          nrow = 1, widths = c(29.7,18,18,18,18,18))
combinedplot

dev.off()

#add in y and x axis labels, and overall title - do this manually in powerpoint
#make widths even

hsd



table <- matrix(, nrow = 20, ncol = 42)
#for (p in 1:length(p1_species)){ 
for (p in 32:32){ 

    significant_group_order_noruahaLVRScluster_noneg_onep1 <- significant_group_order_noruahaLVRScluster_noneg[significant_group_order_noruahaLVRScluster_noneg$P1 %in% p1_species[p],]
    p3_species <- unique(significant_group_order_noruahaLVRScluster_noneg_onep1$P3)

    for (l in 1:length(p3_species)){    
        #for (l in 1:1){
        #print(p3_species[l])
        sub <- significant_group_order_noruahaLVRScluster_noneg_onep1[significant_group_order_noruahaLVRScluster_noneg_onep1$P3 %in% p3_species[l],]
        percent <- ((max(sub$f4.ratio) - min(sub$f4.ratio))/mean(sub$f4.ratio))*100
        table[l,p] <- as.numeric(percent)
    }
}
table
mean(table, na.rm = TRUE)

#16.4169470888949

#updated:

#variables:
pvalue <- 5e-2

#malawisamples <- malawisamples_regions[-c(611,612),]
malawisamples <- malawisamples_regions[-c(612),]
malawisamples2 <- malawisamples[!malawisamples$sequence_id == "cichlid7020224",]
malawisamples2$genus_species <- paste(malawisamples2$genus, malawisamples2$species, sep = "_")

#get list of P3 species
non_malawi_groups <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_Victoria-cluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Orthochromis", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano <- malawisamples2[malawisamples2$clade_SG %in% non_malawi_groups,]
outgroups_serrano <- as.vector(unique(malawisamples_serrano$genus_species))

#keep only non-malawi groups in P3
BBAA_keep <- BBAA_change_names[BBAA_change_names$P3 %in% outgroups_serrano,]
#length(BBAA_keep$P1) this is the same as before

#get list of Malawi species
malawisamples_onlymalawi <- malawisamples2[malawisamples2$region == "Malawi",]

#keep malawi as P1 and P2
BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% malawisamples_onlymalawi$genus_species,]
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% malawisamples_onlymalawi$genus_species,]
#l <- length(BBAA_keep3$P2)

#remove ruaha blue LVRS cluster species - from P3
BBAA_keep3_noruahalvrs <- BBAA_keep3
BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P3 == "Astatotilapia_sp-Ruaha-blue-Victoria-cluster",]

#remove a.kilossana
BBAA_keep3_noruahalvrs_nokilossana <- BBAA_keep3_noruahalvrs[!BBAA_keep3_noruahalvrs$P3 == "Astatotilapia_sp-kilossana",]
#length(BBAA_keep3_noruahalvrs_nokilossana$P1)

l <- length(BBAA_keep3_noruahalvrs_nokilossana$P2)

colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs_nokilossana, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep4_1 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P1"
BBAA_keep5 <- merge(BBAA_keep4_1, groupings, by = "P1", all.x = TRUE, all.y = FALSE, sort = FALSE)

colnames(BBAA_keep5)[11] <- "Clade_name_P3"
colnames(BBAA_keep5)[12] <- "Clade_name_P2"
colnames(BBAA_keep5)[13] <- "Clade_name_P1"

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
not_significant <- BBAA_keep5[BBAA_keep5$p.value >= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of all significant tests:")
length(significant$P1)
print("total number of all trios:")
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
print("percentage of all significant tests:")
percentsig

#label significant comparisons
BBAA_keep5 <- BBAA_keep5 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))


BBAA_keep5[order(-BBAA_keep5$f4.ratio),]


#add in benthic and pelagic information
BBAA_keep5$group_P1 <- with(BBAA_keep5, ifelse(Clade_name_P1 == "diplotaxodon" | Clade_name_P1 == "rhamphochromis", "pelagic", "benthic"))
BBAA_keep5$group_P2 <- with(BBAA_keep5, ifelse(Clade_name_P2 == "diplotaxodon" | Clade_name_P2 == "rhamphochromis", "pelagic", "benthic"))


library(egg)
library(cowplot)

#################

#pp/bb function

#high bb

all_samples_group_info <- data.frame(malawisamples2$genus_species, malawisamples2$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")

groups <- c("gigliolii","ruaha_blue","other_riverine_haplochromines_burtoni",
            "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "Serr_Pharyng_Sarg_Thora", 
            "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")


group_names_neat <- c(expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia "),bold("sp. 'Ruaha blue'"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia burtoni"))),
                      expression(paste(bold("P3 = "),bolditalic("Haplochromis vanheusdeni"))),
                      expression(paste(bold("P3 = Tanganyika"))),
                      expression(paste(bold("P3 = CSA"))),
                      expression(paste(bold("P3 = "),bolditalic("Pseudocrenilabrus"))),
                      expression(paste(bold("P3 = "),bolditalic("Ctenochromis scatebra"))),
                      expression(paste(bold("P3 = "),bolditalic("Orthochromis"))))         

malawi_names_neat <- c(expression(paste(bold("Mbuna (Mb)"))),
                       expression(paste(bold("Deep Benthic (Db)"))),
                       expression(paste(bold("Shallow Benthic (Sb)"))),
                       expression(paste(bolditalic("Astatotilapia calliptera"), bold(" (Ac)"))),
                       expression(paste(bold("Utaka (Ut)"))))


plots <- list()

for (i in 1:length(groups)){
#for (i in 2:2){   

    group <- groups[i]


    length(unique(BBAA_keep$P1))
    BBAA_keep5$P1 <- factor(BBAA_keep5$P1, levels = unique(BBAA_keep5$P1))
    BBAA_keep5$P2 <- factor(BBAA_keep5$P2, levels = unique(BBAA_keep5$P2))


    highbb <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "benthic",]
    highbb_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "benthic",]
    highbb_count_all_P1 <- as.data.frame(table(highbb_all$P1))

    #length(highbb_count_all_P1$Freq)

    #what is P1?
    highbb_count_P1 <- as.data.frame(table(highbb$P1))
    length(levels(highbb$P1))
    length(levels(highbb_all$P1))
    length(highbb_count_P1$Freq)
    length(highbb_count_all_P1$Freq)

    highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq

    #what is P1?
    highbb_count_P1 <- as.data.frame(table(highbb$P1))
    highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq
    highbb_count_P1$percent_sig <- (highbb_count_P1$Freq/highbb_count_P1$all_Freq)*100
    highbb_count_P1_ordered <- highbb_count_P1[order(-highbb_count_P1$percent_sig),]
    all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]

    highbb_count_P1_ordered_clades <- merge(highbb_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
    highbb_count_P1_ordered_clades2 <- highbb_count_P1_ordered_clades[!duplicated(highbb_count_P1_ordered_clades$Var1),]
    highbb_count_P1_ordered_clades2$clade <- factor(highbb_count_P1_ordered_clades2$clade, levels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'))

    #what is P2?
    highbb_count_all_P2 <- as.data.frame(table(highbb_all$P2))
    highbb_count_P2 <- as.data.frame(table(highbb$P2))
    highbb_count_P2$all_Freq <- highbb_count_all_P2$Freq
    highbb_count_P2$percent_sig <- (highbb_count_P2$Freq/highbb_count_P2$all_Freq)*100

    highbb_count_P2_ordered <- highbb_count_P2[order(-highbb_count_P2$percent_sig),]
    highbb_count_P2_ordered_clades <- merge(highbb_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
    highbb_count_P2_ordered_clades2 <- highbb_count_P2_ordered_clades[!duplicated(highbb_count_P2_ordered_clades$Var1),]
    #highbb_count_P2_ordered

    highbb_count_P2_ordered_clades2$clade <- factor(highbb_count_P2_ordered_clades2$clade, levels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'))


    #P1
    new_table <- highbb_count_P1_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

    new_table$percent <- new_table$sum_Freq/new_table$sum_all_Freq*100
    new_table$percent <- ifelse(new_table$sum_all_Freq != 0, new_table$sum_Freq / new_table$sum_all_Freq * 100, 0)


    new_table <- new_table %>%
        filter(!is.na(clade))
    new_table$clade <- forcats::fct_na_value_to_level(new_table$clade)
    
    #P2
    new_table2 <- highbb_count_P2_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

    new_table2$percent <- new_table2$sum_Freq/new_table2$sum_all_Freq*100
    new_table2$percent <- ifelse(new_table2$sum_all_Freq != 0, new_table2$sum_Freq / new_table2$sum_all_Freq * 100, 0)


    new_table2 <- new_table2 %>%
        filter(!is.na(clade))
    new_table2$clade <- forcats::fct_na_value_to_level(new_table2$clade)

    
    print(new_table)
    print(new_table2)
    
    p1 <- ggplot(new_table, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 1), "%")), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P1 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.1) 
    

    
    p2 <- ggplot(new_table2, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 1), "%")), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table2$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P2 Trio Count")))) +
      theme_light()+
      theme(legend.position = "none")+
      xlim(0, max(new_table2$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.1) +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
    
      #pie charts
      new_table$short_clade <- c("Mb", "Db", "Sb", "Ac", "Ut")

      new_table <- new_table %>% 
          arrange(desc(clade)) %>%
          mutate(prop = sum_Freq / sum(new_table$sum_Freq) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
    
      threshold <- 1

      new_table_filtered <- new_table %>%
          filter(prop > threshold)
    
      #red_palette <- rev(brewer_pal(palette = "Reds")(n = 5))
      red_palette <- c("#880808", "#4A0404", "#D22B2B", "#E3735E", "#AA4A44")
      clade_colors <- c("mbuna" = red_palette[1],
                  "deepbenthic" = red_palette[2],
                  "acalliptera" = red_palette[3],
                  "shallowbenthic" = red_palette[4],
                  "utaka" = red_palette[5])

      pie1 <- ggplot(new_table, aes(x = "", y = prop, fill = clade)) +
          geom_bar(stat = "identity", width = 1, color = "white", size = 0.2) +
          coord_polar("y", start = 0) +
          theme_void() + 
          labs(x = "P1") +
          theme(legend.position = "none") +
          geom_text(data = new_table_filtered, aes(y = ypos, x = 1.15, label = paste(short_clade, "\n", scales::percent(prop / 100))), color = "white", size = 2) +
          #scale_fill_manual(values = rep("red3", nrow(new_table)))
          scale_fill_manual(values = clade_colors)
    
      new_table2$short_clade <- c("Mb", "Db", "Sb", "Ac", "Ut")

      new_table2 <- new_table2 %>% 
          arrange(desc(clade)) %>%
          mutate(prop = sum_Freq / sum(new_table2$sum_Freq) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
    
      new_table2_filtered <- new_table2 %>%
          filter(prop > threshold)
    
      pie2 <- ggplot(new_table2, aes(x = "", y = prop, fill = clade)) +
          geom_bar(stat = "identity", width = 1, color = "white", size = 0.2) +
          coord_polar("y", start = 0) +
          theme_void() + 
          labs(x = "P2") +
          theme(legend.position = "none") +
          geom_text(data = new_table2_filtered, aes(y = ypos, x = 1.15, label = paste(short_clade, "\n", scales::percent(prop / 100))), color = "white", size = 2) +
          #scale_fill_manual(values = rep("red3", nrow(new_table)))
          scale_fill_manual(values = clade_colors)

     #combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(),
     #                plot.margin = margin(r = 2)),
     #     p2 + theme(axis.text.y = element_blank(),
     #                axis.ticks.y = element_blank(),
     #                axis.title.y = element_blank(),
     #               plot.margin = margin(r = 0, l = 2, b = 0)), nrow = 1, widths = c(5,5))

     combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 2)), pie1,
          p2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 2, b = 0)),
                             pie2, nrow = 1, widths = c(5,0.8,5,0.8))
      
     multiplot_with_title <- ggdraw() +
      draw_plot(combinedplot, width = 0, height = 1, x = 0, y = 0) +
      draw_label(group_names_neat[i], size = 12, x = 0.55, y = 0.94)

    
      plots[[group]] <- multiplot_with_title

}

multiplot <- do.call(grid.arrange, c(plots, ncol = 1, nrow = length(groups)))

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bbpp_bethics_nov2024.png", sep = ""),width=19,height=15,units="in",res=500)
print(multiplot)
dev.off()

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023


#################

#pp/bb function

#pelagic

all_samples_group_info <- data.frame(malawisamples2$genus_species, malawisamples2$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")

groups <- c("gigliolii","ruaha_blue","other_riverine_haplochromines_burtoni",
            "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "Serr_Pharyng_Sarg_Thora", 
            "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")


group_names_neat <- c(expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia "),bold("sp. 'Ruaha blue'"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia burtoni"))),
                      expression(paste(bold("P3 = "),bolditalic("Haplochromis vanheusdeni"))),
                      expression(paste(bold("P3 = Tanganyika"))),
                      expression(paste(bold("P3 = CSA"))),
                      expression(paste(bold("P3 = "),bolditalic("Pseudocrenilabrus"))),
                      expression(paste(bold("P3 = "),bolditalic("Ctenochromis scatebra"))),
                      expression(paste(bold("P3 = "),bolditalic("Orthochromis"))))         

malawi_names_neat <- c(expression(paste(bolditalic("Rhamphochromis"))),
                       expression(paste(bolditalic("Diplotaxodon"))))


plots <- list()

for (i in 1:length(groups)){
#for (i in 2:2){   

    group <- groups[i]


    length(unique(BBAA_keep$P1))
    BBAA_keep5$P1 <- factor(BBAA_keep5$P1, levels = unique(BBAA_keep5$P1))
    BBAA_keep5$P2 <- factor(BBAA_keep5$P2, levels = unique(BBAA_keep5$P2))


    highbb <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "pelagic",]
    highbb_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "pelagic",]
    highbb_count_all_P1 <- as.data.frame(table(highbb_all$P1))

    #length(highbb_count_all_P1$Freq)

    #what is P1?
    highbb_count_P1 <- as.data.frame(table(highbb$P1))
    length(levels(highbb$P1))
    length(levels(highbb_all$P1))
    length(highbb_count_P1$Freq)
    length(highbb_count_all_P1$Freq)

    highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq

    #what is P1?
    highbb_count_P1 <- as.data.frame(table(highbb$P1))
    highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq
    highbb_count_P1$percent_sig <- (highbb_count_P1$Freq/highbb_count_P1$all_Freq)*100
    highbb_count_P1_ordered <- highbb_count_P1[order(-highbb_count_P1$percent_sig),]
    all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]

    highbb_count_P1_ordered_clades <- merge(highbb_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
    highbb_count_P1_ordered_clades2 <- highbb_count_P1_ordered_clades[!duplicated(highbb_count_P1_ordered_clades$Var1),]
    highbb_count_P1_ordered_clades2$clade <- factor(highbb_count_P1_ordered_clades2$clade, levels = c('rhamphochromis', 'diplotaxodon'))

    #what is P2?
    highbb_count_all_P2 <- as.data.frame(table(highbb_all$P2))
    highbb_count_P2 <- as.data.frame(table(highbb$P2))
    highbb_count_P2$all_Freq <- highbb_count_all_P2$Freq
    highbb_count_P2$percent_sig <- (highbb_count_P2$Freq/highbb_count_P2$all_Freq)*100

    highbb_count_P2_ordered <- highbb_count_P2[order(-highbb_count_P2$percent_sig),]
    highbb_count_P2_ordered_clades <- merge(highbb_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
    highbb_count_P2_ordered_clades2 <- highbb_count_P2_ordered_clades[!duplicated(highbb_count_P2_ordered_clades$Var1),]
    #highbb_count_P2_ordered

    highbb_count_P2_ordered_clades2$clade <- factor(highbb_count_P2_ordered_clades2$clade, levels = c('rhamphochromis', 'diplotaxodon'))


    #P1
    new_table <- highbb_count_P1_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

    new_table$percent <- new_table$sum_Freq/new_table$sum_all_Freq*100
    new_table$percent <- ifelse(new_table$sum_all_Freq != 0, new_table$sum_Freq / new_table$sum_all_Freq * 100, 0)


    new_table <- new_table %>%
        filter(!is.na(clade))
    new_table$clade <- forcats::fct_na_value_to_level(new_table$clade)
    
    #P2
    new_table2 <- highbb_count_P2_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

    new_table2$percent <- new_table2$sum_Freq/new_table2$sum_all_Freq*100
    new_table2$percent <- ifelse(new_table2$sum_all_Freq != 0, new_table2$sum_Freq / new_table2$sum_all_Freq * 100, 0)


    new_table2 <- new_table2 %>%
        filter(!is.na(clade))
    new_table2$clade <- forcats::fct_na_value_to_level(new_table2$clade)

    
    print(new_table)
    print(new_table2)
    
    p1 <- ggplot(new_table, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 1), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P1 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.1) 
    

    
    p2 <- ggplot(new_table2, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 1), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table2$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P2 Trio Count")))) +
      theme_light()+
      theme(legend.position = "none")+
      xlim(0, max(new_table2$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.1) +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
    
      #pie charts
      new_table$short_clade <- c("Di", "Rh")

      new_table <- new_table %>% 
          arrange(desc(clade)) %>%
          mutate(prop = sum_Freq / sum(new_table$sum_Freq) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
    
      threshold <- 1

      new_table_filtered <- new_table %>%
          filter(prop > threshold)
    
      #red_palette <- rev(brewer_pal(palette = "Reds")(n = 5))
      red_palette <- c("#880808", "#4A0404", "#D22B2B", "#E3735E", "#AA4A44")
      clade_colors <- c("diplotaxodon" = red_palette[1],
                  "rhamphochromis" = red_palette[2])

      pie1 <- ggplot(new_table, aes(x = "", y = prop, fill = clade)) +
          geom_bar(stat = "identity", width = 1, color = "white", size = 0.2) +
          coord_polar("y", start = 0) +
          theme_void() + 
          labs(x = "P1") +
          theme(legend.position = "none") +
          geom_text(data = new_table_filtered, aes(y = ypos, x = 1.15, label = paste(short_clade, "\n", scales::percent(prop / 100))), color = "white", size = 2) +
          #scale_fill_manual(values = rep("red3", nrow(new_table)))
          scale_fill_manual(values = clade_colors)
    
      new_table2$short_clade <- c("Di", "Rh")

      new_table2 <- new_table2 %>% 
          arrange(desc(clade)) %>%
          mutate(prop = sum_Freq / sum(new_table2$sum_Freq) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
    
      new_table2_filtered <- new_table2 %>%
          filter(prop > threshold)
    
      pie2 <- ggplot(new_table2, aes(x = "", y = prop, fill = clade)) +
          geom_bar(stat = "identity", width = 1, color = "white", size = 0.2) +
          coord_polar("y", start = 0) +
          theme_void() + 
          labs(x = "P2") +
          theme(legend.position = "none") +
          geom_text(data = new_table2_filtered, aes(y = ypos, x = 1.15, label = paste(short_clade, "\n", scales::percent(prop / 100))), color = "white", size = 2) +
          #scale_fill_manual(values = rep("red3", nrow(new_table)))
          scale_fill_manual(values = clade_colors)

     combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 2)),
          p2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 15, l = 2, b = 0)), nrow = 1, widths = c(5,5))
    
     #combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(),
     #                plot.margin = margin(r = 2)), pie1,
     #     p2 + theme(axis.text.y = element_blank(),
     #                axis.ticks.y = element_blank(),
     #                axis.title.y = element_blank(),
     #               plot.margin = margin(r = 0, l = 2, b = 0)),
     #                        pie2, nrow = 1, widths = c(5,0.8,5,0.8))
      
     multiplot_with_title <- ggdraw() +
      draw_plot(combinedplot, width = 1, height = 1, x = 0, y = 0) +
      draw_label(group_names_neat[i], size = 12, x = 0.55, y = 0.94)

    
      plots[[group]] <- multiplot_with_title

}

multiplot <- do.call(grid.arrange, c(plots, ncol = 1, nrow = length(groups)))

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bbpp_pelagics_nov2024.png", sep = ""),width=19,height=15,units="in",res=500)
print(multiplot)
dev.off()

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023

#################

#pb/bp function


all_samples_group_info <- data.frame(malawisamples2$genus_species, malawisamples2$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")

groups <- c("gigliolii","ruaha_blue","other_riverine_haplochromines_burtoni",
            "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "Serr_Pharyng_Sarg_Thora", 
            "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")


group_names_neat <- c(expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia "),bold("sp. 'Ruaha blue'"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia burtoni"))),
                      expression(paste(bold("P3 = "),bolditalic("Haplochromis vanheusdeni"))),
                      expression(paste(bold("P3 = Tanganyika"))),
                      expression(paste(bold("P3 = CSA"))),
                      expression(paste(bold("P3 = "),bolditalic("Pseudocrenilabrus"))),
                      expression(paste(bold("P3 = "),bolditalic("Ctenochromis scatebra"))),
                      expression(paste(bold("P3 = "),bolditalic("Orthochromis"))))         

malawi_names_neat <- c(expression(paste(bolditalic("Rhamphochromis"))),
                       expression(paste(bolditalic("Diplotaxodon"))))


plots <- list()

for (i in 1:length(groups)){
#for (i in 2:2){   

    group <- groups[i]


    length(unique(BBAA_keep$P1))
    BBAA_keep5$P1 <- factor(BBAA_keep5$P1, levels = unique(BBAA_keep5$P1))
    BBAA_keep5$P2 <- factor(BBAA_keep5$P2, levels = unique(BBAA_keep5$P2))


    highbb <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic",]
    highbb_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic",]
    highbb_count_all_P1 <- as.data.frame(table(highbb_all$P1))

    #length(highbb_count_all_P1$Freq)

    #what is P1?
    highbb_count_P1 <- as.data.frame(table(highbb$P1))
    length(levels(highbb$P1))
    length(levels(highbb_all$P1))
    length(highbb_count_P1$Freq)
    length(highbb_count_all_P1$Freq)

    highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq

    #what is P1?
    highbb_count_P1 <- as.data.frame(table(highbb$P1))
    highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq
    highbb_count_P1$percent_sig <- (highbb_count_P1$Freq/highbb_count_P1$all_Freq)*100
    highbb_count_P1_ordered <- highbb_count_P1[order(-highbb_count_P1$percent_sig),]
    all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]

    highbb_count_P1_ordered_clades <- merge(highbb_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
    highbb_count_P1_ordered_clades2 <- highbb_count_P1_ordered_clades[!duplicated(highbb_count_P1_ordered_clades$Var1),]
    #highbb_count_P1_ordered_clades2$clade <- factor(highbb_count_P1_ordered_clades2$clade, levels = c('rhamphochromis', 'diplotaxodon'))
    highbb_count_P1_ordered_clades2$clade <- factor(highbb_count_P1_ordered_clades2$clade, levels = c('deepbenthic', 'shallowbenthic', 'utaka', 'mbuna', 'acalliptera'))

    
    #what is P2?
    highbb_count_all_P2 <- as.data.frame(table(highbb_all$P2))
    highbb_count_P2 <- as.data.frame(table(highbb$P2))
    highbb_count_P2$all_Freq <- highbb_count_all_P2$Freq
    highbb_count_P2$percent_sig <- (highbb_count_P2$Freq/highbb_count_P2$all_Freq)*100

    highbb_count_P2_ordered <- highbb_count_P2[order(-highbb_count_P2$percent_sig),]
    highbb_count_P2_ordered_clades <- merge(highbb_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
    highbb_count_P2_ordered_clades2 <- highbb_count_P2_ordered_clades[!duplicated(highbb_count_P2_ordered_clades$Var1),]
    #highbb_count_P2_ordered

    highbb_count_P2_ordered_clades2$clade <- factor(highbb_count_P2_ordered_clades2$clade, levels = c('rhamphochromis', 'diplotaxodon'))


    #P1
    new_table <- highbb_count_P1_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

    new_table$percent <- new_table$sum_Freq/new_table$sum_all_Freq*100
    new_table$percent <- ifelse(new_table$sum_all_Freq != 0, new_table$sum_Freq / new_table$sum_all_Freq * 100, 0)


    new_table <- new_table %>%
        filter(!is.na(clade))
    new_table$clade <- forcats::fct_na_value_to_level(new_table$clade)
    
    #P2
    new_table2 <- highbb_count_P2_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

    new_table2$percent <- new_table2$sum_Freq/new_table2$sum_all_Freq*100
    new_table2$percent <- ifelse(new_table2$sum_all_Freq != 0, new_table2$sum_Freq / new_table2$sum_all_Freq * 100, 0)


    new_table2 <- new_table2 %>%
        filter(!is.na(clade))
    new_table2$clade <- forcats::fct_na_value_to_level(new_table2$clade)

    
    print(new_table)
    print(new_table2)
    
    p1 <- ggplot(new_table, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P1 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.1) 
    

    
    p2 <- ggplot(new_table2, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table2$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P2 Trio Count")))) +
      theme_light()+
      theme(legend.position = "none")+
      xlim(0, max(new_table2$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.1) +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
    
      #pie charts
      new_table$short_clade <- c("Di", "Rh")

      new_table <- new_table %>% 
          arrange(desc(clade)) %>%
          mutate(prop = sum_Freq / sum(new_table$sum_Freq) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
    
      threshold <- 1

      new_table_filtered <- new_table %>%
          filter(prop > threshold)
    
      #red_palette <- rev(brewer_pal(palette = "Reds")(n = 5))
      red_palette <- c("#880808", "#4A0404", "#D22B2B", "#E3735E", "#AA4A44")
      clade_colors <- c("diplotaxodon" = red_palette[1],
                  "rhamphochromis" = red_palette[2])

      pie1 <- ggplot(new_table, aes(x = "", y = prop, fill = clade)) +
          geom_bar(stat = "identity", width = 1, color = "white", size = 0.2) +
          coord_polar("y", start = 0) +
          theme_void() + 
          labs(x = "P1") +
          theme(legend.position = "none") +
          geom_text(data = new_table_filtered, aes(y = ypos, x = 1.15, label = paste(short_clade, "\n", scales::percent(prop / 100))), color = "white", size = 2) +
          #scale_fill_manual(values = rep("red3", nrow(new_table)))
          scale_fill_manual(values = clade_colors)
    
      new_table2$short_clade <- c("Di", "Rh")

      new_table2 <- new_table2 %>% 
          arrange(desc(clade)) %>%
          mutate(prop = sum_Freq / sum(new_table2$sum_Freq) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
    
      new_table2_filtered <- new_table2 %>%
          filter(prop > threshold)
    
      pie2 <- ggplot(new_table2, aes(x = "", y = prop, fill = clade)) +
          geom_bar(stat = "identity", width = 1, color = "white", size = 0.2) +
          coord_polar("y", start = 0) +
          theme_void() + 
          labs(x = "P2") +
          theme(legend.position = "none") +
          geom_text(data = new_table2_filtered, aes(y = ypos, x = 1.15, label = paste(short_clade, "\n", scales::percent(prop / 100))), color = "white", size = 2) +
          #scale_fill_manual(values = rep("red3", nrow(new_table)))
          scale_fill_manual(values = clade_colors)

     combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 2)),
          p2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 15, l = 2, b = 0)), nrow = 1, widths = c(5,5))
    
     #combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(),
     #                plot.margin = margin(r = 2)), pie1,
     #     p2 + theme(axis.text.y = element_blank(),
     #                axis.ticks.y = element_blank(),
     #                axis.title.y = element_blank(),
     #               plot.margin = margin(r = 0, l = 2, b = 0)),
     #                        pie2, nrow = 1, widths = c(5,0.8,5,0.8))
      
     multiplot_with_title <- ggdraw() +
      draw_plot(combinedplot, width = 1, height = 1, x = 0, y = 0) +
      draw_label(group_names_neat[i], size = 12, x = 0.55, y = 0.94)

    
      plots[[group]] <- multiplot_with_title

}

multiplot <- do.call(grid.arrange, c(plots, ncol = 1, nrow = length(groups)))

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bbpp_pelagics_nov2024.png", sep = ""),width=19,height=15,units="in",res=500)
print(multiplot)
dev.off()

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023

#benthic vs pelagic

all_samples_group_info <- data.frame(malawisamples2$genus_species, malawisamples2$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")

groups <- c("gigliolii","ruaha_blue","other_riverine_haplochromines_burtoni",
            "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "Serr_Pharyng_Sarg_Thora", 
            "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")


group_names_neat <- c(expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia "),bold("sp. 'Ruaha blue'"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia burtoni"))),
                      expression(paste(bold("P3 = "),bolditalic("Haplochromis vanheusdeni"))),
                      expression(paste(bold("P3 = Tanganyika"))),
                      expression(paste(bold("P3 = CSA"))),
                      expression(paste(bold("P3 = "),bolditalic("Pseudocrenilabrus"))),
                      expression(paste(bold("P3 = "),bolditalic("Ctenochromis scatebra"))),
                      expression(paste(bold("P3 = "),bolditalic("Orthochromis"))))        

malawi_names_neat1 <- c(expression(paste(bold("Mbuna"))),
                       expression(paste(bold("Deep Benthic"))),
                       expression(paste(bold("Shallow Benthic"))),
                       expression(paste(bolditalic("Astatotilapia calliptera"), bold(""))),
                       expression(paste(bold("Utaka"))))

malawi_names_neat2 <- c(expression(paste(bolditalic("Rhamphochromis"))),
                       expression(paste(bolditalic("Diplotaxodon"))))

plots <- list()

for (i in 1:length(groups)){
#for (i in 2:2){   

    group <- groups[i]

length(unique(BBAA_keep$P1))
BBAA_keep5$P1 <- factor(BBAA_keep5$P1, levels = unique(BBAA_keep5$P1))
BBAA_keep5$P2 <- factor(BBAA_keep5$P2, levels = unique(BBAA_keep5$P2))

highbb <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic",]
highbb_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic",]
highbb_count_all_P1 <- as.data.frame(table(highbb_all$P1))

#what is P1?
highbb_count_P1 <- as.data.frame(table(highbb$P1))
length(levels(highbb$P1))
length(levels(highbb_all$P1))
length(highbb_count_P1$Freq)
length(highbb_count_all_P1$Freq)

highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq

#what is P1?
highbb_count_P1 <- as.data.frame(table(highbb$P1))
highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq
highbb_count_P1$percent_sig <- (highbb_count_P1$Freq/highbb_count_P1$all_Freq)*100
highbb_count_P1_ordered <- highbb_count_P1[order(-highbb_count_P1$percent_sig),]
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]

highbb_count_P1_ordered_clades <- merge(highbb_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
highbb_count_P1_ordered_clades2 <- highbb_count_P1_ordered_clades[!duplicated(highbb_count_P1_ordered_clades$Var1),]
highbb_count_P1_ordered_clades2$clade <- factor(highbb_count_P1_ordered_clades2$clade, levels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'))

#what is P2?
highbb_count_all_P2 <- as.data.frame(table(highbb_all$P2))
highbb_count_P2 <- as.data.frame(table(highbb$P2))
highbb_count_P2$all_Freq <- highbb_count_all_P2$Freq
highbb_count_P2$percent_sig <- (highbb_count_P2$Freq/highbb_count_P2$all_Freq)*100

highbb_count_P2_ordered <- highbb_count_P2[order(-highbb_count_P2$percent_sig),]
highbb_count_P2_ordered_clades <- merge(highbb_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
highbb_count_P2_ordered_clades2 <- highbb_count_P2_ordered_clades[!duplicated(highbb_count_P2_ordered_clades$Var1),]
#highbb_count_P2_ordered

highbb_count_P2_ordered_clades2$clade <- factor(highbb_count_P2_ordered_clades2$clade, levels = c('rhamphochromis', 'diplotaxodon'))

#P1
new_table <- highbb_count_P1_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

new_table$percent <- new_table$sum_Freq/new_table$sum_all_Freq*100
new_table$percent <- ifelse(new_table$sum_all_Freq != 0, new_table$sum_Freq / new_table$sum_all_Freq * 100, 0)


new_table <- new_table %>%
        filter(!is.na(clade))
new_table$clade <- forcats::fct_na_value_to_level(new_table$clade)
    
#P2
new_table2 <- highbb_count_P2_ordered_clades2 %>%
      group_by(clade) %>%
     summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

new_table2$percent <- new_table2$sum_Freq/new_table2$sum_all_Freq*100
 new_table2$percent <- ifelse(new_table2$sum_all_Freq != 0, new_table2$sum_Freq / new_table2$sum_all_Freq * 100, 0)


new_table2 <- new_table2 %>%
        filter(!is.na(clade))
new_table2$clade <- forcats::fct_na_value_to_level(new_table2$clade)

    
#print(new_table)
#print(new_table2)

 p1 <- ggplot(new_table, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P1 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat1) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table$sum_all_Freq)+ max(new_table$sum_all_Freq)*0.18) 

p2 <- ggplot(new_table2, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table2$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P2 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat2) +
      theme_light()+
      theme(legend.position = "none")+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table2$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.18) 

    dummy_plot <- ggplot() + theme_void()

    right <- ggarrange(p2, dummy_plot, ncol=1, nrow=2, heights = c(2.8,1))

    combinedplot <- ggarrange(p1 + theme(axis.ticks.y = element_blank(), plot.margin = margin(r = 2, l = 15)), 
                      right + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = margin(r = 30, l = 2, b = 0)),
                      ncol=2, nrow=1, widths = c(4,4))
      
     multiplot_with_title <- ggdraw() +
      draw_plot(combinedplot, width = 1, height = 1, x = 0, y = 0) +
      draw_label(group_names_neat[i], size = 12, x = 0.55, y = 0.94)

    
      plots[[group]] <- multiplot_with_title

}

multiplot <- do.call(grid.arrange, c(plots, ncol = 1, nrow = length(groups)))

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bp_nov2024.png", sep = ""),width=19,height=15,units="in",res=500)
print(multiplot)
dev.off()

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023


#pelagic vs benthic

all_samples_group_info <- data.frame(malawisamples2$genus_species, malawisamples2$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")

groups <- c("gigliolii","ruaha_blue","other_riverine_haplochromines_burtoni",
            "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "Serr_Pharyng_Sarg_Thora", 
            "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")


group_names_neat <- c(expression(paste(bold("P3 = "),bolditalic("Astatotilapia gigliolii"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia "),bold("sp. 'Ruaha blue'"))),
                      expression(paste(bold("P3 = "),bolditalic("Astatotilapia burtoni"))),
                      expression(paste(bold("P3 = "),bolditalic("Haplochromis vanheusdeni"))),
                      expression(paste(bold("P3 = Tanganyika"))),
                      expression(paste(bold("P3 = CSA"))),
                      expression(paste(bold("P3 = "),bolditalic("Pseudocrenilabrus"))),
                      expression(paste(bold("P3 = "),bolditalic("Ctenochromis scatebra"))),
                      expression(paste(bold("P3 = "),bolditalic("Orthochromis"))))        

malawi_names_neat2 <- c(expression(paste(bold("Mbuna"))),
                       expression(paste(bold("Deep Benthic"))),
                       expression(paste(bold("Shallow Benthic"))),
                       expression(paste(bolditalic("Astatotilapia calliptera"), bold(""))),
                       expression(paste(bold("Utaka"))))

malawi_names_neat1 <- c(expression(paste(bolditalic("Rhamphochromis"))),
                       expression(paste(bolditalic("Diplotaxodon"))))

plots <- list()

for (i in 1:length(groups)){
#for (i in 2:2){   

    group <- groups[i]

length(unique(BBAA_keep$P1))
BBAA_keep5$P1 <- factor(BBAA_keep5$P1, levels = unique(BBAA_keep5$P1))
BBAA_keep5$P2 <- factor(BBAA_keep5$P2, levels = unique(BBAA_keep5$P2))

highbb <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P2 == "benthic" & BBAA_keep5$group_P1 == "pelagic",]
highbb_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$group_P2 == "benthic" & BBAA_keep5$group_P1 == "pelagic",]
highbb_count_all_P1 <- as.data.frame(table(highbb_all$P1))

#what is P1?
highbb_count_P1 <- as.data.frame(table(highbb$P1))
length(levels(highbb$P1))
length(levels(highbb_all$P1))
length(highbb_count_P1$Freq)
length(highbb_count_all_P1$Freq)

highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq

#what is P1?
highbb_count_P1 <- as.data.frame(table(highbb$P1))
highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq
highbb_count_P1$percent_sig <- (highbb_count_P1$Freq/highbb_count_P1$all_Freq)*100
highbb_count_P1_ordered <- highbb_count_P1[order(-highbb_count_P1$percent_sig),]
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]

highbb_count_P1_ordered_clades <- merge(highbb_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
highbb_count_P1_ordered_clades2 <- highbb_count_P1_ordered_clades[!duplicated(highbb_count_P1_ordered_clades$Var1),]
highbb_count_P1_ordered_clades2$clade <- factor(highbb_count_P1_ordered_clades2$clade, levels = c('rhamphochromis', 'diplotaxodon'))

#what is P2?
highbb_count_all_P2 <- as.data.frame(table(highbb_all$P2))
highbb_count_P2 <- as.data.frame(table(highbb$P2))
highbb_count_P2$all_Freq <- highbb_count_all_P2$Freq
highbb_count_P2$percent_sig <- (highbb_count_P2$Freq/highbb_count_P2$all_Freq)*100

highbb_count_P2_ordered <- highbb_count_P2[order(-highbb_count_P2$percent_sig),]
highbb_count_P2_ordered_clades <- merge(highbb_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
highbb_count_P2_ordered_clades2 <- highbb_count_P2_ordered_clades[!duplicated(highbb_count_P2_ordered_clades$Var1),]
#highbb_count_P2_ordered

highbb_count_P2_ordered_clades2$clade <- factor(highbb_count_P2_ordered_clades2$clade, levels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'))

    
#P1
new_table <- highbb_count_P1_ordered_clades2 %>%
      group_by(clade) %>%
      summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

new_table$percent <- new_table$sum_Freq/new_table$sum_all_Freq*100
new_table$percent <- ifelse(new_table$sum_all_Freq != 0, new_table$sum_Freq / new_table$sum_all_Freq * 100, 0)


new_table <- new_table %>%
        filter(!is.na(clade))
new_table$clade <- forcats::fct_na_value_to_level(new_table$clade)
    
#P2
new_table2 <- highbb_count_P2_ordered_clades2 %>%
      group_by(clade) %>%
     summarise(sum_Freq = sum(Freq),
            sum_all_Freq = sum(all_Freq))

new_table2$percent <- new_table2$sum_Freq/new_table2$sum_all_Freq*100
 new_table2$percent <- ifelse(new_table2$sum_all_Freq != 0, new_table2$sum_Freq / new_table2$sum_all_Freq * 100, 0)


new_table2 <- new_table2 %>%
        filter(!is.na(clade))
new_table2$clade <- forcats::fct_na_value_to_level(new_table2$clade)

    
#print(new_table)
#print(new_table2)

p1 <- ggplot(new_table, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P1 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat1) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table$sum_all_Freq)+ max(new_table$sum_all_Freq)*0.18) 

p2 <- ggplot(new_table2, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table2$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P2 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat2) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table2$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.18) 

    dummy_plot <- ggplot() + theme_void()

    left <- ggarrange(p1, dummy_plot, ncol=1, nrow=2, heights = c(2.8,1))

    combinedplot <- ggarrange(left + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = margin(r = 30, l = 2, b = 0)),
                              p2 + theme(axis.ticks.y = element_blank(), plot.margin = margin(r = 2, l = 15)), 
                              ncol=2, nrow=1, widths = c(4,4))
      
     multiplot_with_title <- ggdraw() +
      draw_plot(combinedplot, width = 1, height = 1, x = 0, y = 0) +
      draw_label(group_names_neat[i], size = 12, x = 0.55, y = 0.94)

    
      plots[[group]] <- multiplot_with_title

}

multiplot <- do.call(grid.arrange, c(plots, ncol = 1, nrow = length(groups)))

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_pb_nov2024.png", sep = ""),width=19,height=15,units="in",res=500)
print(multiplot)
dev.off()

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023


p2 <- ggplot(new_table2, aes(y = clade)) +
      geom_bar(aes(x = sum_all_Freq, fill = "all_Freq"), stat = "identity") +
      geom_bar(aes(x = sum_Freq, fill = "Freq"), stat = "identity") +
      #geom_text(aes(label = paste(signif(percent, 1), "%"), x = sum_all_Freq + 20), hjust = -0.1, size = 4, color = ifelse(new_table$percent > 0, "red", "black")) +
      geom_text(data = new_table2, aes(label = ifelse(percent < 0.1 & percent != 0, "<0.1%", paste(signif(percent, 2), "%")), x = sum_all_Freq + 10), hjust = -0.1, size = 4, color = ifelse(new_table2$percent > 0, "red", "black")) +
      scale_fill_manual(values = c("Freq" = "red3", "all_Freq" = "lightblue")) +
      labs(title = "",
           y = "",
           x = "") +
           #x = expression(paste(bold("P2 Trio Count")))) +
      scale_y_discrete(labels = malawi_names_neat2) +
      theme_light()+
      theme(legend.position = "none", axis.text.y = element_text(size = 11)) +
        xlim(0, max(new_table2$sum_all_Freq)+ max(new_table2$sum_all_Freq)*0.18)
p2

new_table2

3227/4800


