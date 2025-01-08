#for dtriosparallel - ran all species comparisons across all chromosomes
#combined the results into a single BBAA file
#read in BBAA file and subset so that only P3 is a non-malawi species and P1 and P2 are both Malawi
#are there any significant p values?
#plot a boxplot of P values for each P3 species -

#install.packages("dplyr")
library(dplyr)
library(ggplot2)














setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/dtools/dtrios_cluster/output/sophie_malawi_nonmalawi_aug2023_acalliptera_pops")
BBAA <- read.table("sophie_malawi_nonmalawi_aug2023_acalliptera_pops_BBAA.txt", sep = "\t", header = TRUE)


#previous length of BBAA file = 5013320 trios

length(BBAA[,1])

#head(BBAA)
BBAA1 <- BBAA[BBAA$P3 == "Astatotilapia_gigliolii_2",]
#BBAA1[order(-BBAA1$f4.ratio),]


malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2023-10-08_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
malawisamples <- malawisamples_regions
length_meta <- length(malawisamples_regions$sequence_id)
#malawisamples_regions[-c(length_meta),]
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

unique_species <- malawisamples_regions[!duplicated(malawisamples_regions$full_name),]
groupings <- data.frame(unique_species$full_name, unique_species$clade_SG)
colnames(groupings)[1] <- "P3"

#get list of P3 species (all non malawi species)
malawisamples_notmalawi <- malawisamples[!malawisamples$region == "Malawi",]
outgroups <- as.vector(unique(malawisamples_notmalawi$genus_species))

#keep only outgroups in P3
BBAA_keep <- BBAA[BBAA$P3 %in% outgroups,]

#only keep malawi species in P1 and P2
malawisamples_onlymalawi <- malawisamples[malawisamples$region == "Malawi",]
BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% malawisamples_onlymalawi$genus_species,]
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% malawisamples_onlymalawi$genus_species,]
l <- length(BBAA_keep3$P1)



#variables:
pvalue <- 5e-2

#malawisamples <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/files/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_regionlabels.tsv", sep = "\t", header = T) #this data frame doesnt include the new species names so can't be used
#new metadata with new region assignments
#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades.tsv", sep = "\t", header = T)
#changed Astatoreochromis straeleni clade to burtoni in metadata:
#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades_Aststr.tsv", sep = "\t", header = T)
#change metadata so that 2 strange ruaha blue samples are relabled as ruaha blue LVRS cluster
malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2023-11-05_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
malawisamples <- malawisamples_regions[-c(611, 612),]

length_meta <- length(malawisamples_regions$sequence_id)
malawisamples_regions[-c(length_meta),]
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

unique_species <- malawisamples_regions[!duplicated(malawisamples_regions$full_name),]
groupings <- data.frame(unique_species$full_name, unique_species$clade_SG)
colnames(groupings)[1] <- "P3"

#get list of P3 species (all non malawi species)
malawisamples_notmalawi <- malawisamples[!malawisamples$region == "Malawi",]
outgroups <- as.vector(unique(malawisamples_notmalawi$genus_species))

#keep only outgroups in P3
BBAA_keep <- BBAA[BBAA$P3 %in% outgroups,]

#only keep malawi species in P1 and P2
malawisamples_onlymalawi <- malawisamples[malawisamples$region == "Malawi",]
BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% malawisamples_onlymalawi$genus_species,]
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% malawisamples_onlymalawi$genus_species,]
l <- length(BBAA_keep3$P1)

#write BBAA with only Malawi P1 and P2 and Outgroup P3
#write.table(BBAA_keep3, "out_sim_wholegenome_dtrios_parallel_allchroms_allsamples_newmetahannesdec2021_rmSRR12700905_rmSRR12700906_combined_split_combined_MalawiP1P2_OutgroupP3_BBAA.txt", quote = FALSE, row.names = FALSE, sep = "\t")

#reorder for easier reading
#BBAA_keep4 <- BBAA_keep3[order(BBAA_keep3$P2),]
#BBAA_keep5 <- BBAA_keep4[order(BBAA_keep4$P3),]

#add P2 and P3 group name
colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep4_1 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P1"
BBAA_keep5 <- merge(BBAA_keep4_1, groupings, by = "P1", all.x = TRUE, all.y = FALSE, sort = FALSE)

colnames(BBAA_keep5)[11] <- "Clade_name_P3"
colnames(BBAA_keep5)[12] <- "Clade_name_P2"
colnames(BBAA_keep5)[13] <- "Clade_name_P1"

#sig before control for number of tests
sig_before <- BBAA_keep5[BBAA_keep5$p.value <= pvalue,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of significant tests before controlling for number of tests:")
length(sig_before$P1)

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of significant tests:")
length(significant$P1)
print("total number of trios:")
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
print("percentage of significant tests:")
percentsig

#label significant comparisons
BBAA_keep5 <- BBAA_keep5 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

significant[order(significant$p.value, decreasing = FALSE),]

#change species order
#P3 order
significant_P3_order <- significant[order(significant$Clade_name_P3),]
species_order_P3 <- as.vector(unique(significant_P3_order$P3))
significant_P3_order$P3 <- factor(significant_P3_order$P3, levels = species_order_P3)

#P2 order
significant_P2_order <- significant[order(significant$Clade_name_P2),]
species_order_P2 <- as.vector(unique(significant_P2_order$P2))
significant_P2_order$P2 <- factor(significant_P2_order$P2, levels = species_order_P2)


#previous results (before error fixed):
#[1] "number of significant tests before controlling for number of tests:"
#688451
#[1] "number of significant tests:"
#112618
#[1] "total number of trios:"
#2046267
#[1] "percentage of significant tests:"
#5.5035828657746


pvalue/l

significant[significant$P3 %in% c("Astatotilapia_gigliolii"),]


#is there a significant difference in the f4-ratio between different ecomorphological groups/clades


significant_noruahaLVRScluster <- significant[!significant$P3 %in% c("Astatotilapia_sp-Ruaha-blue-LVRScluster"),]

P3sigspecies <- unique(significant_noruahaLVRScluster$P3)
#length(P3sigspecies)

#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3.png",width=45,height=8,units="in",res=500)
#par(mfrow = c(2,10))
for (i in 1:length(P3sigspecies)){
#for (i in 1:1){
    sub <- significant_noruahaLVRScluster[significant_noruahaLVRScluster$P3 == P3sigspecies[i],]
    sub$Clade_name_P2 <- factor(sub$Clade_name_P2, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
    boxplot(sub$f4.ratio~sub$Clade_name_P2, xlab = "", ylab = "", main = P3sigspecies[i], las = 1, xaxt = "n")
    axis(side = 1, labels = FALSE)
    text(x = 1:7,
     labels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"),
     xpd = NA,
     y = par("usr")[3],
     srt = 35,
     adj = 1,
     cex = 1.2
    )
}
#dev.off()



#how many Malawi and Outgroup species are there - check this against the data plotted
#length(unique(malawisamples_onlymalawi$genus_species))
#length(unique(malawisamples_notmalawi$genus_species))

#onlycal1 <- BBAA_keep5[BBAA_keep5$P1 == "Corematodus_taeniatus",]
#onlycal2 <- BBAA_keep5[BBAA_keep5$P2 == "Corematodus_taeniatus",]#divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#onlycal <- BBAA_keep5[BBAA_keep5$P1 == "Corematodus_taeniatus" | BBAA_keep5$P2 == "Corematodus_taeniatus",] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#length(onlycal1$P1)
#length(onlycal2$P1)
#length(onlycal$P1)


#non_acallipter_sig_tests <- significant[!(significant$P3 == "Astatotilapia_calliptera") & !(significant$P1 == "Astatotilapia_calliptera") & !(significant$P2 == "Astatotilapia_calliptera"),]
non_acallipter_sig_tests <- significant[!(significant$P1 == "Astatotilapia_calliptera") & !(significant$P2 == "Astatotilapia_calliptera"),]

print("number of significant tests which do not include A.calliptera:")
length(non_acallipter_sig_tests$P1)
#non_acallipter_sig_tests

print("percentage of significant tests which do not include A.calliptera:")
length(non_acallipter_sig_tests$P1)/length(significant$P1)*100

#head(non_acallipter_sig_tests)

#previous results (before error fixed):
#[1] "number of significant tests which do not include A.calliptera:"
#97689
#[1] "percentage of significant tests which do not include A.calliptera:"
#86.7436821822444


#add clade info
cladeinfo <- data.frame(malawisamples$genus, malawisamples$clade, malawisamples$clade_SG, malawisamples$genus_species )
colnames(cladeinfo)[4] <- "genus_species"
cladeinfo2 <- unique(cladeinfo)

#plot barplot of results per P3 outgroup
#transform p values into -1og10 p values
BBAA_keep5$minuslog10p <- -log10(BBAA_keep5$p.value)
library(ggplot2)

#pdf("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_minuslog10p_allP3.pdf",  width = 10, height = 10)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP3.png",width=20,height=20,units="in",res=500)
ggplot(BBAA_keep5, aes(x = P3, y = minuslog10p)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

#plot barplot of results per P3 outgroup
#this time with f4 ratio
#pdf("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_f4ratio_allP3.pdf",  width = 10, height = 10)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3.png",width=20,height=20,units="in",res=500)
ggplot(BBAA_keep5, aes(x = P3, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

###
#updated 05.08.2022 - colour clade/order by clade
#plot stats but this time for each P2 Malawi species and only for signifciant trios
significant_P2_order$minuslog10p <- -log10(significant_P2_order$p.value)


dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP2_significant.png",width=20,height=60,units="in",res=500)
p <- ggplot(significant_P2_order, aes(x = P2, y = minuslog10p)) +
#p <- ggplot(sig_no_calliptera_P2_order, aes(x = P2, y = minuslog10p, fill = Clade_name_P2)) +
    #geom_point() +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_significant.png",width=20,height=60,units="in",res=500)
p <- ggplot(significant_P2_order, aes(x = P2, y = f4.ratio)) +
    #geom_boxplot(aes(fill = Clade_name_P2))+ 
    #geom_point() +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_significant.png",width=20,height=60,units="in",res=500)
p <- ggplot(significant_P2_order, aes(x = P2, y = Z.score)) +
    #geom_boxplot(aes(fill = Clade_name_P2))+ 
    #geom_point() +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

#updated 05.08.2022 - colour clade/order by clade
#sig figures for P3 as well
significant_P3_order$minuslog10p <- -log10(significant_P3_order$p.value)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP3_significant.png",width=20,height=20,units="in",res=500)
p <- ggplot(significant_P3_order, aes(x = P3, y = minuslog10p)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_significant.png",width=20,height=20,units="in",res=500)
p <- ggplot(significant_P3_order, aes(x = P3, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_significant.png",width=20,height=20,units="in",res=500)
p <- ggplot(significant_P3_order, aes(x = P3, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

#updated 05.08.2022 - colour clade/order by clade
#are there clade patterns in f4-ratio for the significant trios?

#sig_no_calliptera2$genus_species <- sig_no_calliptera2$P2
significant_P2_order$genus_species <- significant_P2_order$P2

significant_P2_order_genus <- merge(significant_P2_order, cladeinfo2, by = "genus_species", all.y = FALSE)
#head(sig_no_calliptera_P2_order_genus)
#length(clade_sig_no_calliptera2$P2)
#length(sig_no_calliptera2$P2)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_significant_boxplotperclade.png",width=12,height=6,units="in",res=500)
p <- ggplot(significant_P2_order_genus, aes(x = malawisamples.clade_SG, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

genus_order_nocalliptera <- c("Copadichromis",
                 "Tyrannochromis", "Trematocranus", "Tramitichromis", "Taeniolethrinops", "Taeniochromis", "Stigmatochromis", "Sciaenochromis", "Protomelas", "Nimbochromis", "Naevochromis", "Mylochromis", "Mchenga", "Hemitilapia", "Hemitaeniochromis", "Fossorochromis", "Dimidiochromis", "Cyrtocara", "Ctenopharynx", "Corematodus", "Chilotilapia", "Cheilochromis", "Champsochromis", "Buccochromis", "Aristochromis", "Otopharynx", 
                 "Placidochromis", "Lethrinops", "Aulonocara", "Alticorpus", 
                 "Tropheops", "Pseudotropheus", "Petrotilapia", "Melanochromis", "Maylandia", "Labidochromis", "Labeotropheus", "Genyochromis", "Gephyrochromis", "Cynotilapia", "Cyathochromis", "Chindongo", "Abactochromis",
                "Rhamphochromis",
                "Pallidochromis", "Diplotaxodon", "Astatotilapia")

significant_P2_order_genus$malawisamples.genus <- factor(significant_P2_order_genus$malawisamples.genus, levels = genus_order_nocalliptera)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
p <- ggplot(significant_P2_order_genus, aes(x = malawisamples.genus, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

#zscore
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_significant_boxplotperclade.png",width=12,height=6,units="in",res=500)
p <- ggplot(significant_P2_order_genus, aes(x = malawisamples.clade_SG, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
p <- ggplot(significant_P2_order_genus, aes(x = malawisamples.genus, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P2")
dev.off()

head(significant_P2_order_genus[which(is.na(significant_P2_order_genus$malawisamples.genus)),]) #should be empty
length(unique(significant_P2_order_genus$malawisamples.genus))
length(genus_order_nocalliptera)

#repeat with P3
#sig_no_calliptera2$genus_species <- sig_no_calliptera2$P3
#clade_sig_no_calliptera2 <- merge(sig_no_calliptera2, cladeinfo2, by = "genus_species", all.y = FALSE)

significant_P3_order$genus_species <- significant_P3_order$P3
significant_P3_order_genus <- merge(significant_P3_order, cladeinfo2, by = "genus_species", all.y = FALSE)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_significant_boxplotperclade.png",width=12,height=8,units="in",res=500)
p <- ggplot(significant_P3_order_genus, aes(x = malawisamples.clade_SG, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

nonmalawi_genus_order <- c("Orthochromis", "Pseudocrenilabrus", "Ctenochromis", "Serranochromis", "Pharyngochromis",
                          "Thoracochromis", "Sargochromis", "Gnathochromis", "Petrochromis",
                          "Tropheus", "Pseudosimochromis", "Lobochilotes", "Interochromis", "Limnotilapia",
                          "Haplochromis", "Astatoreochromis", "Astatotilapia", "Pyxichromis", "Pundamilia", 
                          "Lipochromis", "Enterochromis", "Gaurochromis", "Yssichromis", "Neochromis")

significant_P3_order_genus$malawisamples.genus <- factor(significant_P3_order_genus$malawisamples.genus, levels = nonmalawi_genus_order)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
ggplot(significant_P3_order_genus, aes(x = malawisamples.genus, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_significant_boxplotperclade.png",width=12,height=8,units="in",res=500)
p <- ggplot(significant_P3_order_genus, aes(x = malawisamples.clade_SG, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
ggplot(significant_P3_order_genus, aes(x = malawisamples.genus, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired") + xlab("P3")
dev.off()

#head(significant_P3_order_genus[which(is.na(significant_P3_order_genus$malawisamples.genus)),]) #should be empty
#length(unique(significant_P3_order_genus$malawisamples.genus))
#length(unique(significant_P3_order_genus$malawisamples.clade_SG))

nonmalawi_group_order <- c("LVRS", "ruaha_blue_LVRScluster", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")
sig_P2_order_group <- significant_P2_order_genus %>% arrange(factor(Clade_name_P3, levels = nonmalawi_group_order))
sig_P2_order_group$Clade_name_P3 <- factor(sig_P2_order_group$Clade_name_P3, levels = nonmalawi_group_order)

xlabels <- c("LVRS", "Astatotilapia sp. ruaha blue LVRS cluster", "Astatotilapia sp. ruaha blue", "Astatotilapia gigliolii group", "Astatoreochromis group", "Haplochromis vanheusdeni",
                            "Tanganyika Tropheini", "Congo/South Africa Group", "Pseudocrenilabrus group", "Orthochromis group")

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_significant_boxplotperclade_colourP2_grouped.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_P2_order_group, aes(x = Clade_name_P3, y = f4.ratio, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_allP3_significant_boxplotperclade_colourP2_grouped.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_P2_order_group, aes(x = Clade_name_P3, y = Dstatistic, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_significant_boxplotperclade_colourP2_grouped.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_P2_order_group, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

head(significant_P2_order_genus)

#repeat with ruaha blue LVRS removed

significant_P2_order_genus_noruahaLVRS <- significant_P2_order_genus[!(significant_P2_order_genus$P3 == "Astatotilapia_sp-Ruaha-blue-LVRScluster"),]

nonmalawi_group_order_noruahaLVRS <- c("LVRS", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")
sig_P2_order_group_noruahaLVRS <- significant_P2_order_genus_noruahaLVRS %>% arrange(factor(Clade_name_P3, levels = nonmalawi_group_order))
sig_P2_order_group_noruahaLVRS$Clade_name_P3 <- factor(sig_P2_order_group_noruahaLVRS$Clade_name_P3, levels = nonmalawi_group_order)

xlabels <- c("LVRS", "Astatotilapia sp. ruaha blue", "Astatotilapia gigliolii group", "Astatoreochromis group", "Haplochromis vanheusdeni",
                           "Tanganyika Tropheini", "Congo/South Africa Group", "Pseudocrenilabrus group", "Orthochromis group")

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_significant_boxplotperclade_colourP2_noruahaLVRS_grouped.png",width=20,height=5,units="in",res=500)
p <- ggplot(sig_P2_order_group_noruahaLVRS, aes(x = Clade_name_P3, y = f4.ratio, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_allP3_significant_boxplotperclade_colourP2_noruahaLVRS_grouped.png",width=20,height=5,units="in",res=500)
p <- ggplot(sig_P2_order_group_noruahaLVRS, aes(x = Clade_name_P3, y = Dstatistic, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_significant_boxplotperclade_colourP2_noruahaLVRS_grouped.png",width=20,height=5,units="in",res=500)
p <- ggplot(sig_P2_order_group_noruahaLVRS, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

#repeat with P1 and P2 always from different ecomorphological groups

#sig_no_calliptera2$genus_species <- sig_no_calliptera2$P2
sig_P2_order_group_noruahaLVRS$genus_species_P1 <- sig_P2_order_group_noruahaLVRS$P1
cladeinfo3 <- cladeinfo2
colnames(cladeinfo3)[4] <- "genus_species_P1"
sig_P2_order_group_noruahaLVRS_P1group <- merge(sig_P2_order_group_noruahaLVRS, cladeinfo3, by = "genus_species_P1", all.y = FALSE)

#if P1 and P2 groups are the same then remove

sig_P2_order_group_noruahaLVRS_diffmalgroups <- sig_P2_order_group_noruahaLVRS_P1group[as.character(sig_P2_order_group_noruahaLVRS_P1group$Clade_name_P2) < as.character(sig_P2_order_group_noruahaLVRS_P1group$malawisamples.clade.y),]

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_significant_boxplotperclade_colourP2_grouped_P1P2diff.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_P2_order_group_noruahaLVRS_diffmalgroups, aes(x = Clade_name_P3, y = f4.ratio, fill = malawisamples.clade_SG.x, colour = malawisamples.clade_SG.x)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_allP3_significant_boxplotperclade_colourP2_grouped_P1P2diff.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_P2_order_group_noruahaLVRS_diffmalgroups, aes(x = Clade_name_P3, y = Dstatistic, fill = malawisamples.clade_SG.x, colour = malawisamples.clade_SG.x)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_significant_boxplotperclade_colourP2_grouped_P1P2diff.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_P2_order_group_noruahaLVRS_diffmalgroups, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG.x, colour = malawisamples.clade_SG.x)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()



#dev.off()

## update 31.01.2023
#new plots

#all trios where P1 and P2 are Malawi, and P3 = Non-Malawi
#scatterplot with all trios and threshold line for sig trios - d-stat or f4-ratio against z-score

min(significant$Z.score)

#head(BBAA_keep5)
#significant_P3_order$Dstatistic

#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_alltrios_Zscore_vs_Dstat.png",width=20,height=20,units="in",res=500)
plot(BBAA_keep5$Dstatistic, BBAA_keep5$Z.score, type = "p", col = ifelse(BBAA_keep5$significant == TRUE, "red", "black"))
abline(h = min(significant$Z.score))
#dev.off()

#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_alltrios_Zscore_vs_F4ratio.png",width=20,height=20,units="in",res=500)
plot(BBAA_keep5$f4.ratio, BBAA_keep5$Z.score, type = "p", col = ifelse(BBAA_keep5$significant == TRUE, "red", "black"))
abline(h = min(significant$Z.score))
#dev.off()
#need to remove ruahablueLVRS from this plot





#plot percentage of sig trios for all P3 species

total_count <- table(BBAA_keep5$P3)
sig_count <- table(significant$P3)

counts <- data.frame(total_count, sig_count)
counts2 <- counts[-c(3)]

colnames(counts2) <- c("P3", "total_count", "sig_count")
counts2$percentage <- (counts2$sig_count/counts2$total_count)*100

all_samples_group_info <- data.frame(malawisamples$genus_species, malawisamples$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")
all_samples_group_info2 <- all_samples_group_info[!duplicated(all_samples_group_info$P3),]

counts3 <- merge(counts2, all_samples_group_info2, by = "P3", all.x = TRUE, all.y = FALSE, no.dups = TRUE)
counts4 <- counts3[counts3$clade != "mbuna" & counts3$clade != "deepbenthic" & counts3$clade != "shallowbenthic"
                   & counts3$clade != "utaka" & counts3$clade != "acalliptera" & counts3$clade != "diplotaxodon"
                   & counts3$clade != "rhamphochromis",]

nonmalawi_group_order <- c("LVRS", "ruaha_blue_LVRScluster", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")
counts4_order <- counts4 %>% arrange(factor(clade, levels = nonmalawi_group_order))
#head(counts4_order)
#unique(counts4_order$clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_MalawiP1P2_percent_sig_trios_perP3.png",width=20,height=10,units="in",res=500)
par(mar = c(22,5,1,1))
barplot(counts4_order$percentage, col = counts4_order$clade, names = counts4_order$P3, las = 2)
dev.off()

counts4_order$clade <- factor(counts4_order$clade, levels = nonmalawi_group_order)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_MalawiP1P2_percent_sig_trios_perP3_cladesgrouped.png",width=10,height=10,units="in",res=500)
par(mar = c(22,5,1,1))
boxplot(counts4_order$percentage~counts4_order$clade, las = 2, xlab = "", ylab = "percentage of significant trios")
dev.off()


#add in benthic and pelagic information
BBAA_keep5$group_P1 <- with(BBAA_keep5, ifelse(Clade_name_P1 == "diplotaxodon" | Clade_name_P1 == "rhamphochromis", "pelagic", "benthic"))
BBAA_keep5$group_P2 <- with(BBAA_keep5, ifelse(Clade_name_P2 == "diplotaxodon" | Clade_name_P2 == "rhamphochromis", "pelagic", "benthic"))

#for each P3 species/group calculate the count of sig trios where P1 and P2 are benthic or pelagic
#(making bb, pp, bp, and pb trios), and calculate what percentange of these trios are significant

#bb% = bb(sig)/bb(tot)*100
#pp% = pp(sig)/pp(tot)*100
#bp% = bp(sig)/(bp(tot)+pb(tot))*100
#pb% = pb(sig)/(bp(tot)+pb(tot))*100

P3_species <- unique(BBAA_keep5$P3)

stats_result <- as.data.frame(matrix(nrow = length(P3_species), ncol = 6))
colnames(stats_result) <- c("P3", "clade", "bb", "pp", "bp", "pb")
stats_result[,1] <- P3_species

info <- as.data.frame(matrix(nrow = length(P3_species), ncol = 3))

#for (i in 1:2){
for (i in 1:length(P3_species)){
    subset <- BBAA_keep5[BBAA_keep5$P3 == P3_species[i],]
    stats_result[i,2] <- as.character(subset$Clade_name_P3[i])
    bb <- as.numeric((count(subset[which(subset$group_P1 == "benthic" & subset$group_P2 == "benthic" & subset$significant == "TRUE"),])/count(subset[which(subset$group_P1 == "benthic" & subset$group_P2 == "benthic"),]))*100)
    pp <- as.numeric((count(subset[which(subset$group_P1 == "pelagic" & subset$group_P2 == "pelagic" & subset$significant == "TRUE"),])/count(subset[which(subset$group_P1 == "pelagic" & subset$group_P2 == "pelagic"),]))*100)
    bp <- as.numeric((count(subset[which(subset$group_P1 == "benthic" & subset$group_P2 == "pelagic" & subset$significant == "TRUE"),])/(count(subset[which(subset$group_P1 == "benthic" & subset$group_P2 == "pelagic"),])+count(subset[which(subset$group_P1 == "pelagic" & subset$group_P2 == "benthic"),])))*100)
    pb <- as.numeric((count(subset[which(subset$group_P1 == "pelagic" & subset$group_P2 == "benthic" & subset$significant == "TRUE"),])/(count(subset[which(subset$group_P1 == "pelagic" & subset$group_P2 == "benthic"),])+count(subset[which(subset$group_P1 == "benthic" & subset$group_P2 == "pelagic"),])))*100)

    stats_result[i,3] <- bb
    stats_result[i,4] <- pp
    stats_result[i,5] <- bp
    stats_result[i,6] <- pb
    
    
    subset2 <- subset[subset$significant == "TRUE",]
    info[i,1] <- as.character(subset$P3[1])
    info[i,2] <- paste(unique(subset2$group_P1), collapse = ' ')
    info[i,3] <- paste(unique(subset2$group_P2), collapse = ' ')
}

#reorder and group P3 according to distance to malawi
nonmalawi_group_order <- c("LVRS", "ruaha_blue_LVRScluster", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")
stats_result_ordered <- stats_result %>% arrange(factor(clade, levels = nonmalawi_group_order))

#info


#head(stats_result_ordered)
#head(BBAA_keep5)

#bp/pb
bppb_sub <- BBAA_keep5[which(BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic" & BBAA_keep5$significant == "TRUE" | BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic" & BBAA_keep5$significant == "TRUE"),]
bppb_sub[which(bppb_sub$group_P1 == "benthic" & bppb_sub$group_P2 == "pelagic"),]$f4.ratio <- -bppb_sub[which(bppb_sub$group_P1 == "benthic" & bppb_sub$group_P2 == "pelagic"),]$f4.ratio
head(bppb_sub)

bppb_sub2 <- bppb_sub %>% arrange(factor(Clade_name_P3, levels = nonmalawi_group_order))
bppb_sub2$P3 <- factor(bppb_sub2$P3, levels = unique(bppb_sub2$P3))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_benthic_pelagic_f4ratio.png",width=20,height=10,units="in",res=500)
par(mar = c(12,15,1,1))
boxplot(bppb_sub2$f4.ratio ~ bppb_sub2$P3, las = 1, xaxt = "n", xlab = "")
abline(h = 0)
text(x = 1:length(unique(bppb_sub2$P3)),
     labels = unique(bppb_sub2$P3),
     xpd = NA,
     y = par("usr")[3],
     srt = 35,
     adj = 1,
     cex = 0.7
    )
dev.off()

#bb/pp
bbpp_sub <- BBAA_keep5[which(BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "benthic" & BBAA_keep5$significant == "TRUE" | BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "pelagic" & BBAA_keep5$significant == "TRUE"),]
bbpp_sub[which(bbpp_sub$group_P1 == "pelagic" & bbpp_sub$group_P2 == "pelagic"),]$f4.ratio <- -bbpp_sub[which(bbpp_sub$group_P1 == "pelagic" & bbpp_sub$group_P2 == "pelagic"),]$f4.ratio
head(bbpp_sub)

bbpp_sub2 <- bbpp_sub %>% arrange(factor(Clade_name_P3, levels = nonmalawi_group_order))
bbpp_sub2$P3 <- factor(bbpp_sub2$P3, levels = unique(bbpp_sub2$P3))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_benthic_pelagic_f4ratio_bbpp.png",width=20,height=10,units="in",res=500)
par(mar = c(12,15,1,1))
boxplot(bbpp_sub2$f4.ratio ~ bbpp_sub2$P3, las = 1, xaxt = "n", xlab = "")
abline(h = 0)
text(x = 1:length(unique(bbpp_sub2$P3)),
     labels = unique(bbpp_sub2$P3),
     xpd = NA,
     y = par("usr")[3],
     srt = 35,
     adj = 1,
     cex = 0.7
    )
dev.off()





#boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
boundaries <- boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip, portion_rhamp),1)

ydim <- boundaries$stats[nrow(boundaries$stats),]
for (i in 1:nlevels(count_P1_ordered_clades2$clade)){
    if (i %in% boundaries$group == "TRUE" & max(boundaries$out[boundaries$group == i]) > ydim[i]){
        ydim[i] <- max(boundaries$out[boundaries$group == i])
    } else {}
}

text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     y = ydim + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )


#repeat with rhampchromis vs diplotaxodon comparison

#stats_result_rd <- as.data.frame(matrix(nrow = length(P3_species), ncol = 6))
#colnames(stats_result_rd) <- c("P3", "clade", "dd", "rr", "dr", "rd")
#stats_result_rd[,1] <- P3_species

#for (i in 1:length(P3_species)){
#    subset <- BBAA_keep5[BBAA_keep5$P3 == P3_species[i],]
#    stats_result_rd[i,2] <- as.character(subset$Clade_name_P3[i])
#    dd <- as.numeric((count(subset[which(subset$Clade_name_P1 == "diplotaxodon" & subset$Clade_name_P2 == "diplotaxodon" & subset$significant == "TRUE"),])/count(subset[which(subset$Clade_name_P1 == "diplotaxodon" & subset$Clade_name_P2 == "diplotaxodon"),]))*100)
#    rr <- as.numeric((count(subset[which(subset$Clade_name_P1 == "rhamphochromis" & subset$Clade_name_P2 == "rhamphochromis" & subset$significant == "TRUE"),])/count(subset[which(subset$Clade_name_P1 == "rhamphochromis" & subset$Clade_name_P2 == "rhamphochromis"),]))*100)
#    dr <- as.numeric((count(subset[which(subset$Clade_name_P1 == "diplotaxodon" & subset$Clade_name_P2 == "rhamphochromis" & subset$significant == "TRUE"),])/(count(subset[which(subset$Clade_name_P1 == "diplotaxodon" & subset$Clade_name_P2 == "rhamphochromis"),])+count(subset[which(subset$Clade_name_P1 == "rhamphochromis" & subset$Clade_name_P2 == "diplotaxodon"),])))*100)
#    rd <- as.numeric((count(subset[which(subset$Clade_name_P1 == "rhamphochromis" & subset$Clade_name_P2 == "diplotaxodon" & subset$significant == "TRUE"),])/(count(subset[which(subset$Clade_name_P1 == "rhamphochromis" & subset$Clade_name_P2 == "diplotaxodon"),])+count(subset[which(subset$Clade_name_P1 == "diplotaxodon" & subset$Clade_name_P2 == "rhamphochromis"),])))*100)

#    stats_result_rd[i,3] <- dd
#    stats_result_rd[i,4] <- rr
#    stats_result_rd[i,5] <- dr
#    stats_result_rd[i,6] <- rd
#}

#stats_result_rd_ordered <- stats_result_rd %>% arrange(factor(clade, levels = nonmalawi_group_order))
#stats_result_rd_ordered

#add colour information for each clade

#stats_result_ordered$colour1 <- with(stats_result_ordered, ifelse(clade == "LVRS", "darkred",
#                                                                  ifelse(clade == "ruaha_blue_LVRScluster", "darkpurple",
#                                                                       ifelse(clade == "ruaha_blue", "darkblue",
#                                                                             ifelse(clade == "gigliolii", "darkpink",
#                                                                                   ifelse(clade == "other_riverine_haplochromines_burtoni", "darkorange",
#                                                                                         ifelse(clade == "Tanganyika", "darkgreen",
#                                                                                               ifelse(clade == "Serr_Pharyng_Sarg_Thora", "darkbrown",
#                                                                                                     ifelse(clade == "Pseudo_Cteno_Ortho2", "darkyellow",
#                                                                                                           ifelse(clade == "Orthochromis", "darkgrey", ""))))))))))
#stats_result_ordered$colour2 <- with(stats_result_ordered, ifelse(clade == "LVRS", "red",
#                                                                  ifelse(clade == "ruaha_blue_LVRScluster", "purple",
#                                                                       ifelse(clade == "ruaha_blue", "blue",
#                                                                             ifelse(clade == "gigliolii", "pink",
#                                                                                   ifelse(clade == "other_riverine_haplochromines_burtoni", "orange",
#                                                                                         ifelse(clade == "Tanganyika", "green",
#                                                                                               ifelse(clade == "Serr_Pharyng_Sarg_Thora", "brown",
#                                                                                                     ifelse(clade == "Pseudo_Cteno_Ortho2", "yellow",
#                                                                                                           ifelse(clade == "Orthochromis", "grey", ""))))))))))

#stats_result_ordered[stats_result_ordered$bp > 0 & stats_result_ordered$pb > 0 ,]

#plot pb and bp percentages for each P3 species

stats_result_ordered1 <- stats_result_ordered
#stats_result_ordered1 <- stats_result_ordered[stats_result_ordered$P3 == "Orthochromis_indermauri",]
stats_result_ordered2 <- stats_result_ordered1[-c(3:4)]
stats_result_ordered2$bp <- -stats_result_ordered2$bp

library(reshape2)
stats_result_ordered3 <- melt(stats_result_ordered2, id.vars = c("P3", "clade"))
#stats_result_ordered3 <- melt(stats_result_ordered2, id.vars = c("P3", "clade", "colour1", "colour2"))

stats_result_ordered3$P3 <- factor(stats_result_ordered3$P3, levels = unique(stats_result_ordered$P3))

head(stats_result_ordered3)
tail(stats_result_ordered3)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_benthic_pelagic.png",width=20,height=10,units="in",res=500)
ggplot(stats_result_ordered3, aes(P3), ylim=(-100:100)) +
geom_bar(data = subset(stats_result_ordered3, variable == "pb"),
   aes(y = value, fill = clade), stat = "identity", position = "dodge") +
geom_bar(data = subset(stats_result_ordered3, variable == "bp"), 
   aes(y = value, fill = clade), stat = "identity", position = "dodge") +
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.margin = margin(1,1,1,3, "cm")) + ggtitle("BP% vs PB% -ve = Pelagic closer/high BP%, +ve = Benthic closer/high PB%")
dev.off()

#https://stackoverflow.com/questions/38268741/geom-bar-ggplot2-stacked-grouped-bar-plot-with-positive-and-negative-values-p

#plot pp and bb percentages for each P3 species

stats_result_ordered1 <- stats_result_ordered
head(stats_result_ordered1)
#stats_result_ordered1 <- stats_result_ordered[stats_result_ordered$P3 == "Orthochromis_indermauri",]
stats_result_ordered2 <- stats_result_ordered1[-c(5:6)]
stats_result_ordered2$pp <- -stats_result_ordered2$pp

library(reshape2)

stats_result_ordered3 <- melt(stats_result_ordered2, id.vars = c("P3", "clade"))
#stats_result_ordered3 <- melt(stats_result_ordered2, id.vars  %>% = c("P3", "clade", "colour1", "colour2"))

stats_result_ordered3$P3 <- factor(stats_result_ordered3$P3, levels = unique(stats_result_ordered$P3))

head(stats_result_ordered3)
tail(stats_result_ordered3)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_bb_vs_pp.png",width=20,height=10,units="in",res=500)
ggplot(stats_result_ordered3, aes(P3), ylim=(-100:100)) +
geom_bar(data = subset(stats_result_ordered3, variable == "pp"),
   aes(y = value, fill = clade), stat = "identity", position = "dodge") +
geom_bar(data = subset(stats_result_ordered3, variable == "bb"), 
   aes(y = value, fill = clade), stat = "identity", position = "dodge") +
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.margin = margin(1,1,1,3, "cm")) + ggtitle("BB% vs PP%, -ve = difference among pelgics/high PP%, +ve = difference among benthics/high BB%")
dev.off()

#which P1 and P2 species are involved in the high pb/bp and bb/pp plots
#i.e. P3 species orthochromis group, and ctenochromis pectoralis

#bp/pb function

#################


makeplot_pbbp_P1_P2 <- function(highPB_P3_test_groups_selected, sign){

if (sign == "pos"){
    
ortho <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == highPB_P3_test_groups_selected & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
ortho_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == highPB_P3_test_groups_selected & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
count_all_P1 <- as.data.frame(table(ortho_all$P1))

#what is P1?
count_P1 <- as.data.frame(table(ortho$P1))
count_P1$all_Freq <- count_all_P1$Freq
count_P1$percent_sig <- (count_P1$Freq/count_P1$all_Freq)*100
count_P1_ordered <- count_P1[order(-count_P1$percent_sig),]
#count_P1_ordered
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]
count_P1_ordered_clades <- merge(count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P1_ordered_clades2 <- count_P1_ordered_clades[!duplicated(count_P1_ordered_clades$Var1),]
count_P1_ordered_clades2$clade <- factor(count_P1_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))
#head(count_P1_ordered_clades2)

#what is P2?
count_all_P2 <- as.data.frame(table(ortho_all$P2))
count_P2 <- as.data.frame(table(ortho$P2))
count_P2$all_Freq <- count_all_P2$Freq
count_P2$percent_sig <- (count_P2$Freq/count_P2$all_Freq)*100

count_P2_ordered <- count_P2[order(-count_P2$percent_sig),]
count_P2_ordered_clades <- merge(count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P2_ordered_clades2 <- count_P2_ordered_clades[!duplicated(count_P2_ordered_clades$Var1),]

count_P2_ordered_clades2$clade <- factor(count_P2_ordered_clades2$clade, levels = c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))

#calculate portions of sig trios
#P1
total_sig <- sum(count_P1_ordered$Freq)
count_P1_ordered_clades3 <- na.omit(count_P1_ordered_clades2)
count_P1_ordered_clades3_diplo <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "diplotaxodon",]
total_sig_dip <- sum(count_P1_ordered_clades3_diplo$Freq)
count_P1_ordered_clades3_rhampho <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "rhamphochromis",]
total_sig_rhamp <- sum(count_P1_ordered_clades3_rhampho$Freq)
portion_dip <- (total_sig_dip/total_sig)*100
portion_rhamp <- (total_sig_rhamp/total_sig)*100

#P2
total_sig <- sum(count_P2_ordered$Freq)
count_P2_ordered_clades3 <- na.omit(count_P2_ordered_clades2)
count_P2_ordered_clades3_mbuna <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "mbuna",]
total_sig_mbuna <- sum(count_P2_ordered_clades3_mbuna$Freq)
count_P2_ordered_clades3_deepbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic <- sum(count_P2_ordered_clades3_deepbenthic$Freq)
count_P2_ordered_clades3_shallowbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic <- sum(count_P2_ordered_clades3_shallowbenthic$Freq)
count_P2_ordered_clades3_acalliptera <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera <- sum(count_P2_ordered_clades3_acalliptera$Freq)
count_P2_ordered_clades3_utaka <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "utaka",]
total_sig_utaka <- sum(count_P2_ordered_clades3_utaka$Freq)
portion_mbuna <- (total_sig_mbuna/total_sig)*100
portion_deepbenthic <- (total_sig_deepbenthic/total_sig)*100
portion_shallowbenthic <- (total_sig_shallowbenthic/total_sig)*100
portion_acalliptera <- (total_sig_acalliptera/total_sig)*100
portion_utaka <- (total_sig_utaka/total_sig)*100


dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_", highPB_P3_test_groups_selected ,"2.png", sep = ""),width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.8,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
#boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
boundaries <- boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip, portion_rhamp),1)

ydim <- boundaries$stats[nrow(boundaries$stats),]
for (i in 1:nlevels(count_P1_ordered_clades2$clade)){
    if (i %in% boundaries$group == "TRUE" & max(boundaries$out[boundaries$group == i]) > ydim[i]){
        ydim[i] <- max(boundaries$out[boundaries$group == i])
    } else {}
}

text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     y = ydim + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
boundaries2 <- boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_mbuna, portion_deepbenthic, portion_shallowbenthic, portion_acalliptera, portion_utaka),1)

ydim2 <- boundaries2$stats[nrow(boundaries2$stats),]
for (i in 1:nlevels(count_P2_ordered_clades2$clade)){
    if (i %in% boundaries2$group == "TRUE" & max(boundaries2$out[boundaries2$group == i]) > ydim2[i]){
        ydim2[i] <- max(boundaries2$out[boundaries2$group == i])
    } else {}
}
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     y = ydim2 + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()
    
} else if (sign == "neg") {

    
ortho <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == highPB_P3_test_groups_selected & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic",]
ortho_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == highPB_P3_test_groups_selected & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "pelagic",]
count_all_P1 <- as.data.frame(table(ortho_all$P1))

#what is P1?
count_P1 <- as.data.frame(table(ortho$P1))
count_P1$all_Freq <- count_all_P1$Freq
count_P1$percent_sig <- (count_P1$Freq/count_P1$all_Freq)*100
count_P1_ordered <- count_P1[order(-count_P1$percent_sig),]
#count_P1_ordered
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]
count_P1_ordered_clades <- merge(count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P1_ordered_clades2 <- count_P1_ordered_clades[!duplicated(count_P1_ordered_clades$Var1),]
count_P1_ordered_clades2$clade <- factor(count_P1_ordered_clades2$clade, levels = c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))
#head(count_P1_ordered_clades2)

#what is P2?
count_all_P2 <- as.data.frame(table(ortho_all$P2))
count_P2 <- as.data.frame(table(ortho$P2))
count_P2$all_Freq <- count_all_P2$Freq
count_P2$percent_sig <- (count_P2$Freq/count_P2$all_Freq)*100

count_P2_ordered <- count_P2[order(-count_P2$percent_sig),]
count_P2_ordered_clades <- merge(count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P2_ordered_clades2 <- count_P2_ordered_clades[!duplicated(count_P2_ordered_clades$Var1),]

count_P2_ordered_clades2$clade <- factor(count_P2_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))

#calculate portions of sig trios
#P2
total_sig <- sum(count_P2_ordered$Freq)
count_P2_ordered_clades3 <- na.omit(count_P2_ordered_clades2)
count_P2_ordered_clades3_diplo <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "diplotaxodon",]
total_sig_dip <- sum(count_P2_ordered_clades3_diplo$Freq)
count_P2_ordered_clades3_rhampho <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "rhamphochromis",]
total_sig_rhamp <- sum(count_P2_ordered_clades3_rhampho$Freq)
portion_dip <- (total_sig_dip/total_sig)*100
portion_rhamp <- (total_sig_rhamp/total_sig)*100

#P1
total_sig <- sum(count_P1_ordered$Freq)
count_P1_ordered_clades3 <- na.omit(count_P1_ordered_clades2)
count_P1_ordered_clades3_mbuna <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "mbuna",]
total_sig_mbuna <- sum(count_P1_ordered_clades3_mbuna$Freq)
count_P1_ordered_clades3_deepbenthic <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic <- sum(count_P1_ordered_clades3_deepbenthic$Freq)
count_P1_ordered_clades3_shallowbenthic <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic <- sum(count_P1_ordered_clades3_shallowbenthic$Freq)
count_P1_ordered_clades3_acalliptera <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera <- sum(count_P1_ordered_clades3_acalliptera$Freq)
count_P1_ordered_clades3_utaka <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "utaka",]
total_sig_utaka <- sum(count_P1_ordered_clades3_utaka$Freq)
portion_mbuna <- (total_sig_mbuna/total_sig)*100
portion_deepbenthic <- (total_sig_deepbenthic/total_sig)*100
portion_shallowbenthic <- (total_sig_shallowbenthic/total_sig)*100
portion_acalliptera <- (total_sig_acalliptera/total_sig)*100
portion_utaka <- (total_sig_utaka/total_sig)*100

    
dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_", highPB_P3_test_groups_selected ,"2.png", sep = ""),width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(2,0.8))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
#boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
boundaries <- boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip, portion_rhamp),1)

ydim <- boundaries$stats[nrow(boundaries$stats),]
for (i in 1:nlevels(count_P1_ordered_clades2$clade)){
    if (i %in% boundaries$group == "TRUE" & max(boundaries$out[boundaries$group == i]) > ydim[i]){
        ydim[i] <- max(boundaries$out[boundaries$group == i])
    } else {}
}

text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     y = ydim + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
boundaries2 <- boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_mbuna, portion_deepbenthic, portion_shallowbenthic, portion_acalliptera, portion_utaka),1)

ydim2 <- boundaries2$stats[nrow(boundaries2$stats),]
for (i in 1:nlevels(count_P2_ordered_clades2$clade)){
    if (i %in% boundaries2$group == "TRUE" & max(boundaries2$out[boundaries2$group == i]) > ydim2[i]){
        ydim2[i] <- max(boundaries2$out[boundaries2$group == i])
    } else {}
}
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     y = ydim2 + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()
    

}
    
}

makeplot_pbbp_P1_P2("Orthochromis", "pos")
makeplot_pbbp_P1_P2("Pseudo_Cteno_Ortho2", "pos")
makeplot_pbbp_P1_P2("Tanganyika", "pos")
makeplot_pbbp_P1_P2("ruaha_blue", "neg")



#which P1 and P2 species are involved in the high pb/bp and bb/pp plots
#i.e. P3 species orthochromis group, and ctenochromis pectoralis

#bp/pb without function

#################

#pb/bp Orthochromis group
ortho <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == "Orthochromis" & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
ortho_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == "Orthochromis" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
count_all_P1 <- as.data.frame(table(ortho_all$P1))
#length(totaltest_dip$P1)

#what is P1?
count_P1 <- as.data.frame(table(ortho$P1))
count_P1$all_Freq <- count_all_P1$Freq
count_P1$percent_sig <- (count_P1$Freq/count_P1$all_Freq)*100
count_P1_ordered <- count_P1[order(-count_P1$percent_sig),]
#count_P1_ordered
count_P1_ordered_clades <- merge(count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P1_ordered_clades2 <- count_P1_ordered_clades[!duplicated(count_P1_ordered_clades$Var1),]
count_P1_ordered_clades2$clade <- factor(count_P1_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))
#head(count_P1_ordered_clades2)


#what is P2?
count_all_P2 <- as.data.frame(table(ortho_all$P2))
count_P2 <- as.data.frame(table(ortho$P2))
count_P2$all_Freq <- count_all_P2$Freq
count_P2$percent_sig <- (count_P2$Freq/count_P2$all_Freq)*100

count_P2_ordered <- count_P2[order(-count_P2$percent_sig),]
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]
count_P2_ordered_clades <- merge(count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P2_ordered_clades2 <- count_P2_ordered_clades[!duplicated(count_P2_ordered_clades$Var1),]

count_P2_ordered_clades2$clade <- factor(count_P2_ordered_clades2$clade, levels = c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))

#calculate portions of sig trios
#P1
total_sig <- sum(count_P1_ordered$Freq)
count_P1_ordered_clades3 <- na.omit(count_P1_ordered_clades2)
count_P1_ordered_clades3_diplo <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "diplotaxodon",]
total_sig_dip <- sum(count_P1_ordered_clades3_diplo$Freq)
count_P1_ordered_clades3_rhampho <- count_P1_ordered_clades3[count_P1_ordered_clades3$clade == "rhamphochromis",]
total_sig_rhamp <- sum(count_P1_ordered_clades3_rhampho$Freq)
portion_dip <- (total_sig_dip/total_sig)*100
portion_rhamp <- (total_sig_rhamp/total_sig)*100

#P2
total_sig <- sum(count_P2_ordered$Freq)
count_P2_ordered_clades3 <- na.omit(count_P2_ordered_clades2)
count_P2_ordered_clades3_mbuna <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "mbuna",]
total_sig_mbuna <- sum(count_P2_ordered_clades3_mbuna$Freq)
count_P2_ordered_clades3_deepbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic <- sum(count_P2_ordered_clades3_deepbenthic$Freq)
count_P2_ordered_clades3_shallowbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic <- sum(count_P2_ordered_clades3_shallowbenthic$Freq)
count_P2_ordered_clades3_acalliptera <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera <- sum(count_P2_ordered_clades3_acalliptera$Freq)
count_P2_ordered_clades3_utaka <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "utaka",]
total_sig_utaka <- sum(count_P2_ordered_clades3_utaka$Freq)
portion_mbuna <- (total_sig_mbuna/total_sig)*100
portion_deepbenthic <- (total_sig_deepbenthic/total_sig)*100
portion_shallowbenthic <- (total_sig_shallowbenthic/total_sig)*100
portion_acalliptera <- (total_sig_acalliptera/total_sig)*100
portion_utaka <- (total_sig_utaka/total_sig)*100


#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_orthochromis.png",width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.8,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
#boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
boundaries <- boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip, portion_rhamp),1)
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     y = boundaries$stats[nrow(boundaries$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
boundaries2 <- boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_mbuna, portion_deepbenthic, portion_shallowbenthic, portion_acalliptera, portion_utaka),1)
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     y = boundaries2$stats[nrow(boundaries2$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#dev.off()



#################

#pp/bb without function

#pp/bb Ctenochromis pectoralis

cteno <- BBAA_keep5[BBAA_keep5$P3 == "Ctenochromis_pectoralis" & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "pelagic",]
cteno_all <- BBAA_keep5[BBAA_keep5$P3 == "Ctenochromis_pectoralis" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "pelagic",]
cteno_count_all_P1 <- as.data.frame(table(cteno_all$P1))

#what is P1?
cteno_count_P1 <- as.data.frame(table(cteno$P1))
cteno_count_P1$all_Freq <- cteno_count_all_P1$Freq
cteno_count_P1$percent_sig <- (cteno_count_P1$Freq/cteno_count_P1$all_Freq)*100
cteno_count_P1_ordered <- cteno_count_P1[order(-cteno_count_P1$percent_sig),]
#head(cteno_count_P1_ordered)
cteno_count_P1_ordered_clades <- merge(cteno_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
cteno_count_P1_ordered_clades2 <- cteno_count_P1_ordered_clades[!duplicated(cteno_count_P1_ordered_clades$Var1),]
cteno_count_P1_ordered_clades2$clade <- factor(cteno_count_P1_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))


#what is P2?
cteno_count_all_P2 <- as.data.frame(table(cteno_all$P2))
cteno_count_P2 <- as.data.frame(table(cteno$P2))
cteno_count_P2$all_Freq <- cteno_count_all_P2$Freq
cteno_count_P2$percent_sig <- (cteno_count_P2$Freq/cteno_count_P2$all_Freq)*100

cteno_count_P2_ordered <- cteno_count_P2[order(-cteno_count_P2$percent_sig),]
cteno_count_P2_ordered_clades <- merge(cteno_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
cteno_count_P2_ordered_clades2 <- cteno_count_P2_ordered_clades[!duplicated(cteno_count_P2_ordered_clades$Var1),]
#cteno_count_P2_ordered

cteno_count_P2_ordered_clades2$clade <- factor(cteno_count_P2_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))
#head(cteno_count_P2_ordered_clades2)
#cteno_count_P2_ordered_clades2[cteno_count_P2_ordered_clades2$Freq == 1,]
#boxplot(cteno_count_P2_ordered_clades2$percent_sig~cteno_count_P2_ordered_clades2$clade, las = 1)
#rhampho


#calculate portions of sig trios
#P1
total_sig <- sum(cteno_count_P1_ordered$Freq)
cteno_count_P1_ordered_clades3 <- na.omit(cteno_count_P1_ordered_clades2)

cteno_count_P1_ordered_clades3_diplo <- cteno_count_P1_ordered_clades3[cteno_count_P1_ordered_clades3$clade == "diplotaxodon",]
total_sig_dip <- sum(cteno_count_P1_ordered_clades3_diplo$Freq)
cteno_count_P1_ordered_clades3_rhampho <- cteno_count_P1_ordered_clades3[cteno_count_P1_ordered_clades3$clade == "rhamphochromis",]
total_sig_rhamp <- sum(cteno_count_P1_ordered_clades3_rhampho$Freq)

portion_dip <- (total_sig_dip/total_sig)*100
portion_rhamp <- (total_sig_rhamp/total_sig)*100

#P2
total_sig_P2 <- sum(cteno_count_P2_ordered$Freq)
cteno_count_P2_ordered_clades3 <- na.omit(cteno_count_P2_ordered_clades2)

cteno_count_P2_ordered_clades3_diplo <- cteno_count_P2_ordered_clades3[cteno_count_P2_ordered_clades3$clade == "diplotaxodon",]
total_sig_dip_P2 <- sum(cteno_count_P2_ordered_clades3_diplo$Freq)
cteno_count_P2_ordered_clades3_rhampho <- cteno_count_P2_ordered_clades3[cteno_count_P2_ordered_clades3$clade == "rhamphochromis",]
total_sig_rhamp_P2 <- sum(cteno_count_P2_ordered_clades3_rhampho$Freq)

portion_dip_P2 <- (total_sig_dip_P2/total_sig_P2)*100
portion_rhamp_P2 <- (total_sig_rhamp_P2/total_sig_P2)*100


dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bbpp_ctenochromis_pectoralis.png", sep = ""),width=10,height=5,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(2,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
boundaries <- boxplot(cteno_count_P1_ordered_clades2$percent_sig~cteno_count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip, portion_rhamp),1)
text(x = 1:nlevels(cteno_count_P1_ordered_clades2$clade),
     y = boundaries$stats[nrow(boundaries$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(cteno_count_P1_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
boundaries2 <- boxplot(cteno_count_P2_ordered_clades2$percent_sig~cteno_count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip_P2, portion_rhamp_P2),1)
text(x = 1:nlevels(cteno_count_P2_ordered_clades2$clade),
     y = boundaries2$stats[nrow(boundaries2$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(cteno_count_P2_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()






#################

#pp/bb function

#high bb

plot_bbpp <- function(group){
    

highbb <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "benthic",]
highbb_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == group & BBAA_keep5$group_P1 == "benthic" & BBAA_keep5$group_P2 == "benthic",]
highbb_count_all_P1 <- as.data.frame(table(highbb_all$P1))

#what is P1?
highbb_count_P1 <- as.data.frame(table(highbb$P1))
highbb_count_P1$all_Freq <- highbb_count_all_P1$Freq
highbb_count_P1$percent_sig <- (highbb_count_P1$Freq/highbb_count_P1$all_Freq)*100
highbb_count_P1_ordered <- highbb_count_P1[order(-highbb_count_P1$percent_sig),]
#head(highbb_count_P1_ordered)
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
#head(cteno_count_P2_ordered_clades2)
#cteno_count_P2_ordered_clades2[cteno_count_P2_ordered_clades2$Freq == 1,]
#boxplot(cteno_count_P2_ordered_clades2$percent_sig~cteno_count_P2_ordered_clades2$clade, las = 1)
#rhampho


#calculate portions of sig trios

#P1
total_sig <- sum(highbb_count_P2_ordered$Freq)
highbb_count_P1_ordered_clades3 <- na.omit(highbb_count_P1_ordered_clades2)

highbb_count_P1_ordered_clades3_mbuna <- highbb_count_P1_ordered_clades3[highbb_count_P1_ordered_clades3$clade == "mbuna",]
total_sig_mbuna <- sum(highbb_count_P1_ordered_clades3_mbuna$Freq)
highbb_count_P1_ordered_clades3_deepbenthic <- highbb_count_P1_ordered_clades3[highbb_count_P1_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic <- sum(highbb_count_P1_ordered_clades3_deepbenthic$Freq)
highbb_count_P1_ordered_clades3_shallowbenthic <- highbb_count_P1_ordered_clades3[highbb_count_P1_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic <- sum(highbb_count_P1_ordered_clades3_shallowbenthic$Freq)
highbb_count_P1_ordered_clades3_acalliptera <- highbb_count_P1_ordered_clades3[highbb_count_P1_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera <- sum(highbb_count_P1_ordered_clades3_acalliptera$Freq)
highbb_count_P1_ordered_clades3_utaka <- highbb_count_P1_ordered_clades3[highbb_count_P1_ordered_clades3$clade == "utaka",]
total_sig_utaka <- sum(highbb_count_P1_ordered_clades3_utaka$Freq)

portion_mbuna <- (total_sig_mbuna/total_sig)*100
portion_deepbenthic <- (total_sig_deepbenthic/total_sig)*100
portion_shallowbenthic <- (total_sig_shallowbenthic/total_sig)*100
portion_acalliptera <- (total_sig_acalliptera/total_sig)*100
portion_utaka <- (total_sig_utaka/total_sig)*100

#P2
total_sig_P2 <- sum(highbb_count_P2_ordered$Freq)
highbb_count_P2_ordered_clades3 <- na.omit(highbb_count_P2_ordered_clades2)

highbb_count_P2_ordered_clades3_mbuna <- highbb_count_P2_ordered_clades3[highbb_count_P2_ordered_clades3$clade == "mbuna",]
total_sig_mbuna_P2 <- sum(highbb_count_P2_ordered_clades3_mbuna$Freq)
highbb_count_P2_ordered_clades3_deepbenthic <- highbb_count_P2_ordered_clades3[highbb_count_P2_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic_P2 <- sum(highbb_count_P2_ordered_clades3_deepbenthic$Freq)
highbb_count_P2_ordered_clades3_shallowbenthic <- highbb_count_P2_ordered_clades3[highbb_count_P2_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic_P2 <- sum(highbb_count_P2_ordered_clades3_shallowbenthic$Freq)
highbb_count_P2_ordered_clades3_acalliptera <- highbb_count_P2_ordered_clades3[highbb_count_P2_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera_P2 <- sum(highbb_count_P2_ordered_clades3_acalliptera$Freq)
highbb_count_P2_ordered_clades3_utaka <- highbb_count_P2_ordered_clades3[highbb_count_P2_ordered_clades3$clade == "utaka",]
total_sig_utaka_P2 <- sum(highbb_count_P2_ordered_clades3_utaka$Freq)

portion_mbuna_P2 <- (total_sig_mbuna_P2/total_sig_P2)*100
portion_deepbenthic_P2 <- (total_sig_deepbenthic_P2/total_sig_P2)*100
portion_shallowbenthic_P2 <- (total_sig_shallowbenthic_P2/total_sig_P2)*100
portion_acalliptera_P2 <- (total_sig_acalliptera_P2/total_sig_P2)*100
portion_utaka_P2 <- (total_sig_utaka_P2/total_sig_P2)*100


dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bbpp_", group, ".png", sep = ""),width=10,height=5,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(2,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
boundaries <- boxplot(highbb_count_P1_ordered_clades2$percent_sig~highbb_count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)

ydim <- boundaries$stats[nrow(boundaries$stats),]
for (i in 1:nlevels(highbb_count_P1_ordered_clades2$clade)){
    if (i %in% boundaries$group == "TRUE" & max(boundaries$out[boundaries$group == i]) > ydim[i]){
        ydim[i] <- max(boundaries$out[boundaries$group == i])
    } else {}
}

portion <- round(c(portion_mbuna, portion_deepbenthic, portion_shallowbenthic, portion_acalliptera, portion_utaka),1)
text(x = 1:nlevels(highbb_count_P1_ordered_clades2$clade),
     y = ydim + 6, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(highbb_count_P1_ordered_clades2$clade),
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
boundaries2 <- boxplot(highbb_count_P2_ordered_clades2$percent_sig~highbb_count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)

ydim2 <- boundaries2$stats[nrow(boundaries2$stats),]
for (i in 1:nlevels(highbb_count_P2_ordered_clades2$clade)){
    if (i %in% boundaries2$group == "TRUE" & max(boundaries2$out[boundaries2$group == i]) > ydim2[i]){
        ydim2[i] <- max(boundaries2$out[boundaries2$group == i])
    } else {}
}

portion <- round(c(portion_mbuna_P2, portion_deepbenthic_P2, portion_shallowbenthic_P2, portion_acalliptera_P2, portion_utaka_P2),1)
text(x = 1:nlevels(highbb_count_P2_ordered_clades2$clade),
     y = ydim2 + 6, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(highbb_count_P2_ordered_clades2$clade),
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()

}

plot_bbpp("Orthochromis")
plot_bbpp("Serr_Pharyng_Sarg_Thora")
plot_bbpp("LVRS")
plot_bbpp("ruaha_blue")
plot_bbpp("other_riverine_haplochromines_burtoni")
plot_bbpp("other_riverine_haplochromines_vanheusdeni")
plot_bbpp("Tanganyika")
plot_bbpp("Pseudo_Cteno_Ortho2")


#plot f4-ratio

BBAA_keep5_sig <- BBAA_keep5[BBAA_keep5$significant == 'TRUE',]

P3_order_LVRS <- c("LVRS", "ruaha_blue_LVRScluster", "ruaha_blue", "gigliolii",
                   "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", 
                  "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")

BBAA_keep5_sig$Clade_name_P3 <- factor(BBAA_keep5_sig$Clade_name_P3, levels = P3_order_LVRS)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_P1P2Malawi_f4-ratio_sig_trios_P3clade_grouped.png",width=8,height=7,units="in",res=500)
par(mar=c(15, 5, 1, 1))
boxplot(BBAA_keep5_sig$f4.ratio~BBAA_keep5_sig$Clade_name_P3,las = 1, xaxt = "n", xlab = "", ylab = "")
axis(side = 1, labels = FALSE)
text(x = 1:length(P3_order_LVRS),
     labels = P3_order_LVRS,
     xpd = NA,
     y = par("usr")[3],
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_P1P2Malawi_dstat_sig_trios_P3clade_grouped.png",width=8,height=7,units="in",res=500)
par(mar=c(15, 5, 1, 1))
boxplot(BBAA_keep5_sig$Dstatistic~BBAA_keep5_sig$Clade_name_P3,las = 1, xaxt = "n", xlab = "", ylab = "")
axis(side = 1, labels = FALSE)
text(x = 1:length(P3_order_LVRS),
     labels = P3_order_LVRS,
     xpd = NA,
     y = par("usr")[3],
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()


#bp/pb and bb/pp f4-ratio comparisons

#repeat benthic vs pelagic comparisons but this time look at the f4-ratios instead of counts


#Take all trios (not just significant) with two different ecomoph malawi groups and one single P3 species, 
#and plot the f4-ratio for all trios (negative and positive values depending on whether ecomorph group 1 and 2 
#are P1 or P2)





#length(unique(BBAA_keep5$P1))
#length(unique(BBAA_keep5$P3))

#which Malawi species are involved in the significant tests?
#count the number of times a species comes up in a significant test and make a barplot for each species

#head(significant)
P1 <- significant %>% count(P1)
P2 <- significant %>% count(P2)
P3 <- significant %>% count(P3)
colnames(P1)[1] <- "genus_species"
colnames(P2)[1] <- "genus_species"
colnames(P3)[1] <- "genus_species"

malawi <- data.frame(unique(malawisamples_onlymalawi$genus_species))
colnames(malawi)[1] <- "genus_species" 

P1_2 <- merge(malawi, P1, by = "genus_species", all.x = TRUE)
P2_2 <- merge(malawi, P2, by = "genus_species", all.x = TRUE)
P1_2$P2_n <- P2_2$n
colnames(P1_2)[2] <- "P1_n"
P1_2[is.na(P1_2)] <- 0
P1_2$P1plusP2 <- P1_2$P1_n + P1_2$P2_n
#head(P1_2)
#length(malawi$genus_species)
#length(P1_2$genus_species) #these 2 should be the same

#head(P1)
#head(P2)

#plot barplot for each species
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_2$P1plusP2, xlim = c(0,15000), horiz = TRUE, names.arg = P1_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


#compare the number of significant tests to the number of tests overall per species
P1_all <- BBAA_keep5 %>% count(P1)
P2_all <- BBAA_keep5 %>% count(P2)
P3_all <- BBAA_keep5 %>% count(P3)
colnames(P1_all)[1] <- "genus_species"
colnames(P2_all)[1] <- "genus_species"
colnames(P3_all)[1] <- "genus_species"

non_malawi <- data.frame(unique(malawisamples_notmalawi$genus_species))
colnames(non_malawi)[1] <- "genus_species" 

P1_all_2 <- merge(malawi, P1_all, by = "genus_species", all.x = TRUE)
P2_all_2 <- merge(malawi, P2_all, by = "genus_species", all.x = TRUE)
P3_all_2 <- merge(non_malawi, P3_all, by = "genus_species", all.x = TRUE)
P1_all_2$P2_n <- P2_all_2$n
colnames(P1_all_2)[2] <- "P1_n"
P1_all_2[is.na(P1_all_2)] <- 0
P1_all_2$P1plusP2 <- P1_all_2$P1_n + P1_all_2$P2_n

#does the number of trios per species vary a lot? is it higher in species with more significant trios?
#P1
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_all_P1tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_all_2$P1_n, xlim = c(0,25000), horiz = TRUE, names.arg = P1_all_2$genus_species, las = 1 )
abline(v = c(5000,10000,15000,20000,25000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_all_P2tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_all_2$P2_n, xlim = c(0,25000), horiz = TRUE, names.arg = P1_all_2$genus_species, las = 1 )
abline(v = c(5000,10000,15000,20000,25000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_all_P3tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_all_2$n, xlim = c(28252,28534), horiz = TRUE, names.arg = P3_all_2$genus_species, las = 1 )
#abline(v = c(5000,10000,15000,20000,25000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P1andP2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_all_P1P2tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_all_2$P1plusP2, xlim = c(0,25000), horiz = TRUE, names.arg = P1_all_2$genus_species, las = 1 )
abline(v = c(5000,10000,15000,20000,25000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#does the number of all trios correlate with the number of significant trios?
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_correlation_P1tests_perspecies.png",width=10,height=10,units="in",res=500)
plot(P1_all_2$P1_n, P1_2$P1_n)
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_correlation_P2tests_perspecies.png",width=10,height=10,units="in",res=500)
plot(P1_all_2$P2_n, P1_2$P2_n)
dev.off()

#what percentage of trios are significant per species?
P1_2$P1_n_percent <- (P1_2$P1_n/P1_all_2$P1_n)*100
P1_2$P2_n_percent <- (P1_2$P2_n/P1_all_2$P2_n)*100
P1_2$P1plusP2_percent <- (P1_2$P1plusP2/P1_all_2$P1plusP2)*100

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_percentageof_sig_P1P2tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_2$P1plusP2_percent, xlim = c(0,100), horiz = TRUE, names.arg = P1_2$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70,80,90,100), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


max(P3_all_2$n)
min(P3_all_2$n)
table(P3_all_2$n)

#length(P1_2$genus_species)

#which max value to set in each plot
#max(P1_2$P1plusP2)
#max(P1_2$P2_n)
#max(P3_2$n)

#are the species with the highest count of significant trios from the same clade?

P1_2_clade <- merge(P1_2, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_2_clade <- droplevels(P1_2_clade) #remove the outgroup levels

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_boxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(P1plusP2~malawisamples.clade, data = P1_2_clade, las = 2, ylab = "", xlab = "")
dev.off()

#repeat but with just P2 incidence (P2 are the receiving species)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P2tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_2$P2_n, xlim = c(0,7500), horiz = TRUE, names.arg = P1_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#repeat - are the species with the highest count of significant trios from the same clade?
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P2tests_perspecies_boxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(P2_n~malawisamples.clade, data = P1_2_clade, las = 2, ylab = "", xlab = "")
dev.off()

#what about outgroup species - which outgroup species are involved in the significant tests?
not_malawi <- data.frame(unique(malawisamples_notmalawi$genus_species))
colnames(not_malawi)[1] <- "genus_species" 
P3_2 <- merge(not_malawi, P3, by = "genus_species", all.x = TRUE)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_outgroup.png",width=20,height=30,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_2$n, xlim = c(0,7500), horiz = TRUE, names.arg = P3_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


#length(P3_2$genus_species)

#again with outgroups - are the species with the highest count from the same clade?

#add clade info
P3_2_clade <- merge(P3_2, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_2_clade <- droplevels(P3_2_clade) #remove the outgroup levels

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_outgroup_boxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_2_clade, las = 2, ylab = "", xlab = "")
dev.off()


#
#length(unique(malawisamples$genus_species))
#uni <- unique(malawisamples$genus_species)
#whole_genome_species_order <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/inputfiles/whole_genome_species_order.txt")
#length(whole_genome_species_order$V1)
#setdiff(uni, whole_genome_species_order$V1)
#Astatoreochromis_straeleni was missing - why? added it next to Astatotilapia gigliolii for now

#look at P3 Malawi species (before filtering)
#head(BBAA)
testp3 <- BBAA[BBAA$P3 == "Alticorpus_geoffreyi",]
head(testp3)
length(testp3$P1)

testp3_2 <- testp3[!testp3$P2 %in% malawisamples_onlymalawi$genus_species,]
testp3_3 <- testp3_2[!testp3_2$P1 %in% malawisamples_onlymalawi$genus_species,]

length(testp3_2$P1)
length(testp3_3$P1)
head(testp3_3)

#BBAA_keep - only non malawi outgroup P3
#BBAA_keep5 - only Malawi P2 P1

length(unique(BBAA_keep5$P2))
BBAA_keep5_test <- BBAA_keep5[BBAA_keep5$P1 == "Alticorpus_geoffreyi" | BBAA_keep5$P2 == "Alticorpus_geoffreyi",]
#BBAA_keep5_test <- BBAA_keep5[BBAA_keep5$P1 == "Alticorpus_geoffreyi",]
length(unique(BBAA_keep5_test$P2))

head(BBAA_keep5_test)
#head(P1_all_2)
#table(P1_all_2$P1plusP2)
#unique(testp3$P1)

#what are the counts of P1+P2+P3 for all species before filtering?
#should be equal across all species

P1_allBBAA <- BBAA %>% count(P1)
P2_allBBAA <- BBAA %>% count(P2)
P3_allBBAA <- BBAA %>% count(P3)

colnames(P1_allBBAA)[1] <- "genus_species"
colnames(P2_allBBAA)[1] <- "genus_species"
colnames(P3_allBBAA)[1] <- "genus_species"

#non_malawi <- data.frame(unique(malawisamples_notmalawi$genus_species))
#colnames(non_malawi)[1] <- "genus_species" 

#head(P1_allBBAA)
#head(P2_allBBAA)
#head(P3_allBBAA)


allBBAA_all <- merge(P1_allBBAA, P3_allBBAA, by = "genus_species", all.x = TRUE)
allBBAA_all2 <- merge(allBBAA_all, P2_allBBAA, by = "genus_species", all.x = TRUE)
#head(allBBAA_all2)
length(allBBAA_all2$n.y)
allBBAA_all2$sum <- allBBAA_all2$n.x + allBBAA_all2$n.y + allBBAA_all2$n
colnames(allBBAA_all2)[2] <- "P1"
colnames(allBBAA_all2)[3] <- "P3"
colnames(allBBAA_all2)[4] <- "P2"
head(allBBAA_all2)
#barplot(height = as.matrix(allBBAA_all2[,2:5]), beside = TRUE)
#allBBAA_all2[,2:5]
unique(allBBAA_all2$sum)
table(allBBAA_all2$sum)
#reshape(allBBAA_all2, idvar = )

length(P1_allBBAA$n)
length(P2_allBBAA$n)
length(P3_allBBAA$n)

#look at bimodality of f4 ratio
#head(BBAA_keep5)
#split tests into two groups - higher than 0.00125 and lower
BBAA_keep5 <- BBAA_keep5 %>% mutate(bimodal=ifelse(f4.ratio >= 0.00125,T,F))

#keep only a few species that definitely show bimodality at the right spot
bimodalspecies <- c("Neochromis_omnicaeruleus", "Lipochromis_cryptodon", "Haplochromis_vonlinnei", 
                   "Haplochromis_lividus", "Haplochromis_bicolor", "Astatotilapia_sp-Rukwa-large-yellow",
                   "Astatotilapia_sp-Rukwa-blue", "Astatotilapia_latifasciata")
#bimodalspecies <- c("Astatotilapia_sp-Rukwa-blue")

#BBAA_keep5_bimodal <- BBAA_keep5[BBAA_keep5$P3 == bimodalspecies,]
BBAA_keep5_bimodal <- BBAA_keep5[is.element(BBAA_keep5$P3, bimodalspecies),]
#head(BBAA_keep5_bimodal)
BBAA_keep5_bimodal <- droplevels(BBAA_keep5_bimodal) #remove the outgroup levels
levels(unique(BBAA_keep5_bimodal$P3))

BBAA_keep5_bimodal1 <- BBAA_keep5_bimodal[BBAA_keep5_bimodal$bimodal == "TRUE",]
BBAA_keep5_bimodal2 <- BBAA_keep5_bimodal[BBAA_keep5_bimodal$bimodal == "FALSE",]

unique(BBAA_keep5_bimodal1$P1)

#count which P2 species are in each group
BBAA_keep5_bimodal2_P2 <- BBAA_keep5_bimodal2 %>% count(P2)
colnames(BBAA_keep5_bimodal2_P2)[1] <- "genus_species"
BBAA_keep5_bimodal2_P2_2 <- merge(malawi, BBAA_keep5_bimodal2_P2, by = "genus_species", all.x = TRUE)

BBAA_keep5_bimodal1_P2 <- BBAA_keep5_bimodal1 %>% count(P2)
colnames(BBAA_keep5_bimodal1_P2)[1] <- "genus_species"
BBAA_keep5_bimodal1_P2_2 <- merge(malawi, BBAA_keep5_bimodal1_P2, by = "genus_species", all.x = TRUE)

head(BBAA_keep5_bimodal2_P2_2)
head(BBAA_keep5_bimodal1_P2_2)
max(BBAA_keep5_bimodal1_P2_2$n)
max(BBAA_keep5_bimodal2_P2_2$n)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_all_P2tests_perspecies_f4higher0.00125.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(BBAA_keep5_bimodal1_P2_2$n, xlim = c(0,100), horiz = TRUE, names.arg = BBAA_keep5_bimodal1_P2_2$genus_species, las = 1 )
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_numberof_all_P2tests_perspecies_f4lower0.00125.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(BBAA_keep5_bimodal2_P2_2$n, xlim = c(0,15000), horiz = TRUE, names.arg = BBAA_keep5_bimodal2_P2_2$genus_species, las = 1 )
dev.off()










calliptera_p1 <- significant[(significant$P1 == "Astatotilapia_calliptera"),]
calliptera_p2 <- significant[(significant$P2 == "Astatotilapia_calliptera"),]
calliptera_p1p2 <- significant[(significant$P1 == "Astatotilapia_calliptera") | (significant$P2 == "Astatotilapia_calliptera"),]

length(calliptera_p1$P1)
length(calliptera_p2$P1)
length(calliptera_p1p2$P1)

#what percentage of significant tests involving A.calliptera have the species as P1 and how many as P2?
length(calliptera_p2$P1)/(length(calliptera_p1$P1)+length(calliptera_p2$P1))*100
#length(calliptera_p1$P1) + length(calliptera_p2$P1) + 97689 = this should be the sum of all significant tests


#which outgroup species are most involved in A.calliptera P1 tests
#head(calliptera_p1)

P2_calliptera <- calliptera_p1 %>% count(P2)
P3_calliptera <- calliptera_p1 %>% count(P3)
colnames(P2_calliptera)[1] <- "genus_species"
colnames(P3_calliptera)[1] <- "genus_species"

#max(P2_calliptera$n)
length(P2_calliptera$n)
max(P3_calliptera$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P2tests_perspecies_acallipteraP1_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P2_calliptera$n, xlim = c(0,150), horiz = TRUE, names.arg = P2_calliptera$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_acallipteraP1_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_calliptera$n, xlim = c(0,250), horiz = TRUE, names.arg = P3_calliptera$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


#merge with clade information
P2_calliptera_clade <- merge(P2_calliptera, cladeinfo2, by = "genus_species", all.y = FALSE)
P2_calliptera_clade <- droplevels(P2_calliptera_clade) #remove the outgroup levels

P3_calliptera_clade <- merge(P3_calliptera, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_calliptera_clade <- droplevels(P3_calliptera_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P2tests_perspecies_acallipteraP1_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P2_calliptera_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_acallipteraP1_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_calliptera_clade, las = 2, ylab = "", xlab = "")
dev.off()


#what about introgression into A.calliptera? which P3 species are involved in these tests?

P1_calliptera_introgressed <- calliptera_p2 %>% count(P1)
P3_calliptera_introgressed <- calliptera_p2 %>% count(P3)
colnames(P1_calliptera_introgressed)[1] <- "genus_species"
colnames(P3_calliptera_introgressed)[1] <- "genus_species"

max(P1_calliptera_introgressed$n)
max(P3_calliptera_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_acallipteraP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_calliptera_introgressed$n, xlim = c(0,5), horiz = TRUE, names.arg = P1_calliptera_introgressed$genus_species, las = 1 )
abline(v = c(1,2,3,4,5), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_acallipteraP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_calliptera_introgressed$n, xlim = c(0,25), horiz = TRUE, names.arg = P3_calliptera_introgressed$genus_species, las = 1 )
abline(v = c(5,10,15,20), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


rhamph_p1 <- significant[(significant$P1 == "Rhamphochromis_woodi"),]
rhamph_p2 <- significant[(significant$P2 == "Rhamphochromis_woodi"),]
rhamph_p1p2 <- significant[(significant$P1 == "Rhamphochromis_woodi") | (significant$P2 == "Rhamphochromis_woodi"),]

length(rhamph_p1$P1)
length(rhamph_p2$P1)
length(rhamph_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 rhampochromis woodi tests?
P1_rhamph_introgressed <- rhamph_p2 %>% count(P1)
P3_rhamph_introgressed <- rhamph_p2 %>% count(P3)
colnames(P1_rhamph_introgressed)[1] <- "genus_species"
colnames(P3_rhamph_introgressed)[1] <- "genus_species"

max(P1_rhamph_introgressed$n)
max(P3_rhamph_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_rhamphochromiswoodiP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_rhamph_introgressed$n, xlim = c(0,70), horiz = TRUE, names.arg = P1_rhamph_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_rhamphochromiswoodiP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_rhamph_introgressed$n, xlim = c(0,220), horiz = TRUE, names.arg = P3_rhamph_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150,200,250), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


#merge with clade information
P1_rhamph_introgressed_clade <- merge(P1_rhamph_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_rhamph_introgressed_clade <- droplevels(P1_rhamph_introgressed_clade) #remove the outgroup levels

P3_rhamph_introgressed_clade <- merge(P3_rhamph_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_rhamph_introgressed_clade <- droplevels(P3_rhamph_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_rhamphochromiswoodiP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_rhamph_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_rhamphochromiswoodiP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_rhamph_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

head(P3_rhamph_introgressed_clade)

ptoko_p1 <- significant[(significant$P1 == "Pallidochromis_tokolosh"),]
ptoko_p2 <- significant[(significant$P2 == "Pallidochromis_tokolosh"),]
ptoko_p1p2 <- significant[(significant$P1 == "Pallidochromis_tokolosh") | (significant$P2 == "Pallidochromis_tokolosh"),]

length(ptoko_p1$P1)
length(ptoko_p2$P1)
length(ptoko_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 p tokolosh tests?
P1_ptoko_introgressed <- ptoko_p2 %>% count(P1)
P3_ptoko_introgressed <- ptoko_p2 %>% count(P3)
colnames(P1_ptoko_introgressed)[1] <- "genus_species"
colnames(P3_ptoko_introgressed)[1] <- "genus_species"

max(P1_ptoko_introgressed$n)
max(P3_ptoko_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_pallidochromistokoloshP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_ptoko_introgressed$n, xlim = c(0,60), horiz = TRUE, names.arg = P1_ptoko_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_pallidochromistokoloshP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_ptoko_introgressed$n, xlim = c(0,200), horiz = TRUE, names.arg = P3_ptoko_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150,200), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#merge with clade information
P1_ptoko_introgressed_clade <- merge(P1_ptoko_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_ptoko_introgressed_clade <- droplevels(P1_ptoko_introgressed_clade) #remove the outgroup levels

P3_ptoko_introgressed_clade <- merge(P3_ptoko_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_ptoko_introgressed_clade <- droplevels(P3_ptoko_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_pallidochromistokoloshP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_ptoko_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_pallidochromistokoloshP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_ptoko_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

tyran_p1 <- significant[(significant$P1 == "Tyrannochromis_nigriventer"),]
tyran_p2 <- significant[(significant$P2 == "Tyrannochromis_nigriventer"),]
tyran_p1p2 <- significant[(significant$P1 == "Tyrannochromis_nigriventer") | (significant$P2 == "Tyrannochromis_nigriventer"),]

length(tyran_p1$P1)
length(tyran_p2$P1)
length(tyran_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 tyrannochromis nigriventer tests?
P1_tyran_introgressed <- tyran_p2 %>% count(P1)
P3_tyran_introgressed <- tyran_p2 %>% count(P3)
colnames(P1_tyran_introgressed)[1] <- "genus_species"
colnames(P3_tyran_introgressed)[1] <- "genus_species"

max(P1_tyran_introgressed$n)
max(P3_tyran_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_tyrannochromisnigriventerP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_tyran_introgressed$n, xlim = c(0,70), horiz = TRUE, names.arg = P1_tyran_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_tyrannochromisnigriventerP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_tyran_introgressed$n, xlim = c(0,150), horiz = TRUE, names.arg = P3_tyran_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#merge with clade information
P1_tyran_introgressed_clade <- merge(P1_tyran_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_tyran_introgressed_clade <- droplevels(P1_tyran_introgressed_clade) #remove the outgroup levels

P3_tyran_introgressed_clade <- merge(P3_tyran_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_tyran_introgressed_clade <- droplevels(P3_tyran_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_tyrannochromisnigriventerP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_tyran_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_tyrannochromisnigriventerP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_tyran_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

sciaeno_p1 <- significant[(significant$P1 == "Sciaenochromis_sp-nyassae"),]
sciaeno_p2 <- significant[(significant$P2 == "Sciaenochromis_sp-nyassae"),]
sciaeno_p1p2 <- significant[(significant$P1 == "Sciaenochromis_sp-nyassae") | (significant$P2 == "Sciaenochromis_sp-nyassae"),]

length(sciaeno_p1$P1)
length(sciaeno_p2$P1)
length(sciaeno_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 Sciaenochromis_sp-nyassae tests?
P1_sciaeno_introgressed <- sciaeno_p2 %>% count(P1)
P3_sciaeno_introgressed <- sciaeno_p2 %>% count(P3)
colnames(P1_sciaeno_introgressed)[1] <- "genus_species"
colnames(P3_sciaeno_introgressed)[1] <- "genus_species"

max(P1_sciaeno_introgressed$n)
max(P3_sciaeno_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_sciaenochromisspnyassaeP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_sciaeno_introgressed$n, xlim = c(0,80), horiz = TRUE, names.arg = P1_sciaeno_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_sciaenochromisspnyassaeP2_only.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_sciaeno_introgressed$n, xlim = c(0,210), horiz = TRUE, names.arg = P3_sciaeno_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150,200), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#merge with clade information
P1_sciaeno_introgressed_clade <- merge(P1_sciaeno_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_sciaeno_introgressed_clade <- droplevels(P1_sciaeno_introgressed_clade) #remove the outgroup levels

P3_sciaeno_introgressed_clade <- merge(P3_sciaeno_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_sciaeno_introgressed_clade <- droplevels(P3_sciaeno_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_sciaenochromisspnyassaeP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_sciaeno_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_sciaenochromisspnyassaeP2_only_aboxplotperclade.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_sciaeno_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

#sig_no_calliptera <- significant[!(significant$P1 == "Astatotilapia_calliptera"),]
#sig_no_calliptera <- significant2[!(significant2$P1 == "Astatotilapia_calliptera"),]
#sig_no_calliptera2 <- sig_no_calliptera[!(sig_no_calliptera$P2 == "Astatotilapia_calliptera"),]

sig_no_calliptera_P3_order <- significant_P3_order[!(significant_P3_order$P1 == "Astatotilapia_calliptera"),]
sig_no_calliptera_P3_order <- sig_no_calliptera_P3_order[!(sig_no_calliptera_P3_order$P2 == "Astatotilapia_calliptera"),]

sig_no_calliptera_P2_order <- significant_P2_order[!(significant_P2_order$P1 == "Astatotilapia_calliptera"),]
sig_no_calliptera_P2_order <- sig_no_calliptera_P2_order[!(sig_no_calliptera_P2_order$P2 == "Astatotilapia_calliptera"),]


#BBAA_keep5_nocal <- BBAA_keep5[!(BBAA_keep5$P1 == "Astatotilapia_calliptera"),]
BBAA_keep5_nocal <- BBAA_keep5[!(BBAA_keep5$P1 == "Astatotilapia_calliptera"),]
BBAA_keep5_nocal2 <- BBAA_keep5_nocal[!(BBAA_keep5_nocal$P2 == "Astatotilapia_calliptera"),]

#BBAA_keep5_nocal <- BBAA_keep5 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

cladeinfo <- data.frame(malawisamples$genus, malawisamples$clade, malawisamples$clade_SG, malawisamples$genus_species )
colnames(cladeinfo)[4] <- "genus_species"
cladeinfo2 <- unique(cladeinfo)

head(BBAA_keep5_nocal2)

print("number of significant tests:")
length(sig_no_calliptera_P2_order$P1)
print("total number of trios:")
length(BBAA_keep5_nocal2$P1)
percentsig <- length(sig_no_calliptera_P2_order$P1)/length(BBAA_keep5_nocal2$P1) *100
print("percentage of significant tests:")
percentsig

mean(sig_no_calliptera_P2_order$f4.ratio)
max(sig_no_calliptera_P2_order$f4.ratio)

#plot barplot of results per P3 outgroup
#transform p values into -1og10 p values
BBAA_keep5_nocal2$minuslog10p <- -log10(BBAA_keep5_nocal2$p.value)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP3_nocalliptera.png",width=20,height=20,units="in",res=500)
ggplot(BBAA_keep5_nocal2, aes(x = P3, y = minuslog10p)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

#plot barplot of results per P3 outgroup
#this time with f4 ratio
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_nocalliptera.png",width=20,height=20,units="in",res=500)
ggplot(BBAA_keep5_nocal2, aes(x = P3, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

#plot stats but this time for each P2 Malawi species
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP2_nocalliptera.png",width=20,height=60,units="in",res=500)
ggplot(BBAA_keep5_nocal2, aes(x = P2, y = minuslog10p)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera.png",width=20,height=60,units="in",res=500)
ggplot(BBAA_keep5_nocal2, aes(x = P2, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

#updated 05.08.2022 - colour clade/order by clade
#plot stats but this time for each P2 Malawi species and only for signifciant trios
sig_no_calliptera_P2_order$minuslog10p <- -log10(sig_no_calliptera_P2_order$p.value)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP2_nocalliptera_significant.png",width=20,height=60,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order, aes(x = P2, y = minuslog10p)) +
#p <- ggplot(sig_no_calliptera_P2_order, aes(x = P2, y = minuslog10p, fill = Clade_name_P2)) +
    #geom_point() +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera_significant.png",width=20,height=60,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order, aes(x = P2, y = f4.ratio)) +
    #geom_boxplot(aes(fill = Clade_name_P2))+ 
    #geom_point() +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_nocalliptera_significant.png",width=20,height=60,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order, aes(x = P2, y = Z.score)) +
    #geom_boxplot(aes(fill = Clade_name_P2))+ 
    #geom_point() +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

#updated 05.08.2022 - colour clade/order by clade
#sig figures for P3 as well
sig_no_calliptera_P3_order$minuslog10p <- -log10(sig_no_calliptera_P3_order$p.value)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_minuslog10p_allP3_nocalliptera_significant.png",width=20,height=20,units="in",res=500)
p <- ggplot(sig_no_calliptera_P3_order, aes(x = P3, y = minuslog10p)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_nocalliptera_significant.png",width=20,height=20,units="in",res=500)
p <- ggplot(sig_no_calliptera_P3_order, aes(x = P3, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_nocalliptera_significant.png",width=20,height=20,units="in",res=500)
p <- ggplot(sig_no_calliptera_P3_order, aes(x = P3, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

#updated 05.08.2022 - colour clade/order by clade
#are there clade patterns in f4-ratio for the significant trios?

#sig_no_calliptera2$genus_species <- sig_no_calliptera2$P2
sig_no_calliptera_P2_order$genus_species <- sig_no_calliptera_P2_order$P2

sig_no_calliptera_P2_order_genus <- merge(sig_no_calliptera_P2_order, cladeinfo2, by = "genus_species", all.y = FALSE)
#head(sig_no_calliptera_P2_order_genus)
#length(clade_sig_no_calliptera2$P2)
#length(sig_no_calliptera2$P2)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera_significant_boxplotperclade.png",width=10,height=5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.clade_SG, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

genus_order_nocalliptera <- c("Copadichromis",
                 "Tyrannochromis", "Trematocranus", "Tramitichromis", "Taeniolethrinops", "Taeniochromis", "Stigmatochromis", "Sciaenochromis", "Protomelas", "Nimbochromis", "Naevochromis", "Mylochromis", "Mchenga", "Hemitilapia", "Hemitaeniochromis", "Fossorochromis", "Dimidiochromis", "Cyrtocara", "Ctenopharynx", "Corematodus", "Chilotilapia", "Cheilochromis", "Champsochromis", "Buccochromis", "Aristochromis", "Otopharynx", 
                 "Placidochromis", "Lethrinops", "Aulonocara", "Alticorpus", 
                 "Tropheops", "Pseudotropheus", "Petrotilapia", "Melanochromis", "Maylandia", "Labidochromis", "Labeotropheus", "Genyochromis", "Gephyrochromis", "Cynotilapia", "Cyathochromis", "Chindongo", "Abactochromis",
                "Rhamphochromis",
                "Pallidochromis", "Diplotaxodon")

sig_no_calliptera_P2_order_genus$malawisamples.genus <- factor(sig_no_calliptera_P2_order_genus$malawisamples.genus, levels = genus_order_nocalliptera)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.genus, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

#zscore
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_nocalliptera_significant_boxplotperclade.png",width=10,height=5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.clade_SG, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_nocalliptera_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.genus, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()



head(sig_no_calliptera_P2_order_genus[which(is.na(sig_no_calliptera_P2_order_genus$malawisamples.genus)),]) #should be empty
length(unique(sig_no_calliptera_P2_order_genus$malawisamples.genus))
length(genus_order_nocalliptera)

#install.packages("gridExtra")
P3_groups_length

#repeat P2 grouping by clade plot
#but only plotting one P3 clade at a time

P3_groups <- as.vector(unique(sig_no_calliptera_P2_order_genus$Clade_name_P3))
P3_groups_length <- length(P3_groups)

#P3_groups[1]
#subset <- sig_no_calliptera_P2_order_genus[(sig_no_calliptera_P2_order_genus$Clade_name_P3 == P3_groups[2]),]
#subset

for (i in 1:P3_groups_length){
    subset <- sig_no_calliptera_P2_order_genus[(sig_no_calliptera_P2_order_genus$Clade_name_P3 == P3_groups[i]),]
    dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera_significant_boxplotperclade_perP3_", P3_groups[i],".png", sep = ""),width=10,height=5,units="in",res=500)
    p <- ggplot(subset, aes(x = malawisamples.clade_SG, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    ggtitle(P3_groups[i]) + 
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), plot.title = element_text(size = 20))
    p + scale_color_brewer(palette = "Paired") + labs(title = P3_groups[i])
    print(p)
    dev.off()
}

for (i in 1:P3_groups_length){
    subset <- sig_no_calliptera_P2_order_genus[(sig_no_calliptera_P2_order_genus$Clade_name_P3 == P3_groups[i]),]
    dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_nocalliptera_significant_boxplotperclade_perP3_", P3_groups[i],".png", sep = ""),width=10,height=5,units="in",res=500)
    p <- ggplot(subset, aes(x = malawisamples.clade_SG, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P2), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    ggtitle(P3_groups[i]) + 
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), plot.title = element_text(size = 20))
    p + scale_color_brewer(palette = "Paired") + labs(title = P3_groups[i])
    print(p)
    dev.off()
}



#P3_groups

#repeat orginal plot (all P3 species) but colour plot according to P3 group in trio
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera_significant_boxplotperclade_colourP3.png",width=50,height=15,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.clade_SG, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP2_nocalliptera_significant_boxplotperclade_colourP3.png",width=50,height=15,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.clade_SG, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

#plot in grouped boxplot format
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP2_nocalliptera_significant_boxplotperclade_colourP3_grouped.png",width=50,height=30,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.clade_SG, y = f4.ratio, fill = factor(Clade_name_P3))) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot()+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()




nonmalawi_group_order <- c("LVRS", "ruaha_blue_LVRScluster", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")
sig_no_calliptera_P2_order_group <- sig_no_calliptera_P2_order_genus %>% arrange(factor(Clade_name_P3, levels = nonmalawi_group_order))
sig_no_calliptera_P2_order_group$Clade_name_P3 <- factor(sig_no_calliptera_P2_order_group$Clade_name_P3, levels = nonmalawi_group_order)

xlabels <- c("LVRS", "Astatotilapia sp. ruaha blue LVRS cluster", "Astatotilapia sp. ruaha blue", "Astatotilapia gigliolii group", "Astatoreochromis group", "Haplochromis vanheusdeni",
                           "Tanganyika Tropheini", "Congo/South Africa Group", "Pseudocrenilabrus group", "Orthochromis group")

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_nocalliptera_significant_boxplotperclade_colourP2_grouped.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_group, aes(x = Clade_name_P3, y = f4.ratio, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_allP3_nocalliptera_significant_boxplotperclade_colourP2_grouped.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_group, aes(x = Clade_name_P3, y = Dstatistic, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_nocalliptera_significant_boxplotperclade_colourP2_grouped.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_group, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG, colour = malawisamples.clade_SG)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()


#repeat above plot but filter so that P1 and P2 species are always from different ecomorph groups
#sig_no_calliptera_P2_order_group_diffmalgroups

#add in group names for P1 species <- 

#sig_no_calliptera2$genus_species <- sig_no_calliptera2$P2
sig_no_calliptera_P2_order_group$genus_species_P1 <- sig_no_calliptera_P2_order_group$P1
cladeinfo3 <- cladeinfo2
colnames(cladeinfo3)[4] <- "genus_species_P1"
sig_no_calliptera_P2_order_group_P1group <- merge(sig_no_calliptera_P2_order_group, cladeinfo3, by = "genus_species_P1", all.y = FALSE)

#if P1 and P2 groups are the same then remove

sig_no_calliptera_P2_order_group_diffmalgroups <- sig_no_calliptera_P2_order_group_P1group[as.character(sig_no_calliptera_P2_order_group_P1group$Clade_name_P2) < as.character(sig_no_calliptera_P2_order_group_P1group$malawisamples.clade.y),]

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_nocalliptera_significant_boxplotperclade_colourP2_grouped_P1P2diff.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_group_diffmalgroups, aes(x = Clade_name_P3, y = f4.ratio, fill = malawisamples.clade_SG.x, colour = malawisamples.clade_SG.x)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_allP3_nocalliptera_significant_boxplotperclade_colourP2_grouped_P1P2diff.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_group_diffmalgroups, aes(x = Clade_name_P3, y = Dstatistic, fill = malawisamples.clade_SG.x, colour = malawisamples.clade_SG.x)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_nocalliptera_significant_boxplotperclade_colourP2_grouped_P1P2diff.png",width=20,height=8.5,units="in",res=500)
p <- ggplot(sig_no_calliptera_P2_order_group_diffmalgroups, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG.x, colour = malawisamples.clade_SG.x)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_point(position = position_jitterdodge(), alpha=0.5)+
    geom_boxplot(outlier.shape = NA, alpha = 0.7)+ 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #coord_flip() +
    theme(axis.text.y = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 15, angle = 15, vjust=.8, hjust=0.8, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 15, face = 'bold'))
p + scale_fill_brewer(palette = "Paired") + scale_colour_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group") + scale_x_discrete(labels = xlabels) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"))
dev.off()


#head(sig_no_calliptera_P2_order_group_diffmalgroups)

p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG)) +
    #geom_jitter(aes(fill = malawisamples.clade_SG), alpha = 0.9)+
    geom_boxplot(width = 0.5) + 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #geom_point() +
    #coord_flip() +
    theme(axis.text.y = element_text(size = 1), axis.text.x = element_text(size = 1),
    axis.title.y = element_text(size = 1), axis.title.x = element_text(size = 1))
p + scale_color_brewer(palette = "Paired") + theme(panel.background = element_rect(fill = "white", color = "black"),
                                                  panel.grid.major = element_line(color = "gray88"),
                                                  panel.grid.minor = element_line(color = "gray88"))

p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = Clade_name_P3, y = Z.score, fill = malawisamples.clade_SG)) +
    #geom_jitter(aes(fill = malawisamples.clade_SG), alpha = 0.9)+
    geom_boxplot(width = 0.5) + 
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    #geom_point() +
    #coord_flip() +
    theme(axis.text.y = element_text(size = 1), axis.text.x = element_text(size = 1),
    axis.title.y = element_text(size = 1), axis.title.x = element_text(size = 1))
p + scale_color_brewer(palette = "Paired") + theme_bw()

p <- ggplot(sig_no_calliptera_P2_order_genus, aes(x = malawisamples.clade_SG, y = Z.score, fill = Clade_name_P3)) +
    #geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    #geom_jitter(aes(fill = factor(Clade_name_P3))) +
    #geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    geom_boxplot(outlier.shape = NA) +
    #geom_jitter(width = 0.1, alpha = 0.1)+
    #geom_jitter(aes(x = malawisamples.clade_SG, y = Z.score, fill = factor(Clade_name_P3)), alpha = 0.9)+
    #coord_flip() +
    geom_point(position = position_jitterdodge(), alpha=0.3)+
    theme(axis.text.y = element_text(size = 1), axis.text.x = element_text(size = 1),
    axis.title.y = element_text(size = 1), axis.title.x = element_text(size = 1))
p + scale_color_brewer(palette = "Paired")
#dev.off()

head(sig_no_calliptera_P2_order_genus)

#repeat with P3
#sig_no_calliptera2$genus_species <- sig_no_calliptera2$P3
#clade_sig_no_calliptera2 <- merge(sig_no_calliptera2, cladeinfo2, by = "genus_species", all.y = FALSE)

sig_no_calliptera_P3_order$genus_species <- sig_no_calliptera_P3_order$P3
sig_no_calliptera_P3_order_genus <- merge(sig_no_calliptera_P3_order, cladeinfo2, by = "genus_species", all.y = FALSE)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_nocalliptera_significant_boxplotperclade.png",width=20,height=20,units="in",res=500)
p <- ggplot(sig_no_calliptera_P3_order_genus, aes(x = malawisamples.clade_SG, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_allP3_nocalliptera_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
ggplot(sig_no_calliptera_P3_order_genus, aes(x = malawisamples.genus, y = f4.ratio)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_nocalliptera_significant_boxplotperclade.png",width=20,height=20,units="in",res=500)
p <- ggplot(sig_no_calliptera_P3_order_genus, aes(x = malawisamples.clade_SG, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_allP3_nocalliptera_significant_boxplotpergenus.png",width=20,height=60,units="in",res=500)
ggplot(sig_no_calliptera_P3_order_genus, aes(x = malawisamples.genus, y = Z.score)) +
    geom_jitter(aes(colour = Clade_name_P3), alpha = 0.9)+
    geom_boxplot(alpha = 0.35, outlier.shape = NA)+ 
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

#for the species with the highest f4-ratio (Rhamphochromis, diplotaxodon, optopharynx tetrastigma)
#which P3 species have the highest values here?
#head(BBAA_keep5_nocal2)

#species_f4 <- "Diplotaxodon_limnothrissa"
#species_f4 <- "Diplotaxodon_sp-limnothrissa-black-pelvic"
#species_f4 <- "Pallidochromis_tokolosh"
#species_f4 <- "Rhamphochromis_woodi"
species_f4 <- "Rhamphochromis_sp-long-fin"

species_f4_file <- "rhamphochromissplongfin"
diplo_f4 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P2 == species_f4),]
#head(diplo_f4)

#for each P3 outgroup plot the range of f4-ratio values
dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_P3_", species_f4_file, "P2_nocalliptera.png", sep = ""),width=20,height=20,units="in",res=500)
ggplot(diplo_f4, aes(x = P3, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

sig_no_calliptera2_diplo <- sig_no_calliptera2[(sig_no_calliptera2$P2 == species_f4),]
dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_P3_", species_f4_file,"P2_nocalliptera_significant.png", sep = ""),width=20,height=20,units="in",res=500)
ggplot(sig_no_calliptera2_diplo, aes(x = P3, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point() +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

#P1
dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_P1_", species_f4_file, "P2_nocalliptera.png", sep = ""),width=20,height=60,units="in",res=500)
ggplot(diplo_f4, aes(x = P1, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()

sig_no_calliptera2_diplo <- sig_no_calliptera2[(sig_no_calliptera2$P2 == species_f4),]
dev.copy(png, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_P1_", species_f4_file,"P2_nocalliptera_significant.png", sep = ""),width=20,height=60,units="in",res=500)
ggplot(sig_no_calliptera2_diplo, aes(x = P1, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point() +
    coord_flip() +
    theme(axis.text.y = element_text(size = 16))
dev.off()


#head(clade_sig_no_calliptera2)
#plot(clade_sig_no_calliptera2$p.value, clade_sig_no_calliptera2$f4.ratio)

#which Malawi species are involved in the significant tests?
#count the number of times a species comes up in a significant test and make a barplot for each species

#head(significant)
P1_nocal <- sig_no_calliptera2 %>% count(P1)
P2_nocal <- sig_no_calliptera2 %>% count(P2)
P3_nocal <- sig_no_calliptera2 %>% count(P3)
colnames(P1_nocal)[1] <- "genus_species"
colnames(P2_nocal)[1] <- "genus_species"
colnames(P3_nocal)[1] <- "genus_species"

P1_nocal_2 <- merge(malawi, P1_nocal, by = "genus_species", all.x = TRUE)
P2_nocal_2 <- merge(malawi, P2_nocal, by = "genus_species", all.x = TRUE)
P1_nocal_2$P2_nocal_n <- P2_nocal_2$n
colnames(P1_nocal_2)[2] <- "P1_n"
P1_nocal_2[is.na(P1_nocal_2)] <- 0
P1_nocal_2$P1plusP2 <- P1_nocal_2$P1_n + P1_nocal_2$P2_n
#head(P1_nocal_2)
#length(malawi$genus_species)
#length(P1_nocal_2$genus_species) #these 2 should be the same

#head(P1)
#head(P2)

#plot barplot for each species
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_nocal_2$P1plusP2, xlim = c(0,9000), horiz = TRUE, names.arg = P1_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000,9000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#which species are outliers
boxplot(P1_nocal_2$P2_nocal_n)
out <- boxplot.stats(P1_nocal_2$P2_nocal_n)$out
out_ind <- which(P1_nocal_2$P2_nocal_n %in% c(out))
out_ind
P1_nocal_2[out_ind, ]$genus_species

shapiro.test(P1_nocal_2$P2_nocal_n)

#hampel filter
#lower_bound <- median(P1_nocal_2$P2_nocal_n) - 3 * mad(P1_nocal_2$P2_nocal_n, constant = 1)
#lower_bound
#upper_bound <- median(P1_nocal_2$P2_nocal_n) + 3 * mad(P1_nocal_2$P2_nocal_n, constant = 1)
#upper_bound
#outlier_ind <- which(P1_nocal_2$P2_nocal_n < lower_bound | P1_nocal_2$P2_nocal_n > upper_bound)
#outlier_ind

min(P1_nocal_2$P2_nocal_n)
max(P1_nocal_2$P2_nocal_n)
var(P1_nocal_2$P2_nocal_n)

#for each Malawi P2 species, is a high count number due to a higher number of P1 (compatible Malawi sister species)
#or due to a higher number of non-Malawi P3 species
#test this by counting the number of P3 species for every unique P2 species

n_malawi <- length(P2_nocal_2$genus_species)

for (i in 1:n_malawi){
    species <- P2_nocal_2$genus_species[i]
    sub_1species <- sig_no_calliptera2[(sig_no_calliptera2$P2 == paste(species)),]
    P2_nocal_2$unique_P3_n[i] <- length(unique(sub_1species$P3)) 
    P2_nocal_2$unique_P1_n[i] <- length(unique(sub_1species$P1)) 
}

head(P2_nocal_2)
max(P2_nocal_2$unique_P3_n)
max(P2_nocal_2$unique_P1_n)


dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3species_perP2species_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P2_nocal_2$unique_P3_n, xlim = c(0,80), horiz = TRUE, names.arg = P1_nocal_2$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70,80), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1species_perP2species_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P2_nocal_2$unique_P1_n, xlim = c(0,250), horiz = TRUE, names.arg = P1_nocal_2$genus_species, las = 1 )
abline(v = c(50,100,150,200,250), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

max(P2_nocal_2$unique_P3_n)
min(P2_nocal_2$unique_P3_n)
var(P2_nocal_2$unique_P3_n)
shapiro.test(P2_nocal_2$unique_P1_n)

#which max value to set in each plot
max(P1_nocal_2$P1plusP2)
max(P1_nocal_2$P2_n)
max(P3_nocal_2$n)

#are the species with the highest count of significant trios from the same clade?

#add clade info
P1_nocal_2_clade <- merge(P1_nocal_2, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_nocal_2_clade <- droplevels(P1_nocal_2_clade) #remove the outgroup levels

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_boxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(P1plusP2~malawisamples.clade, data = P1_nocal_2_clade, las = 2, ylab = "", xlab = "")
dev.off()

#repeat but with just P2 incidence (P2 are the receiving species)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P2tests_perspecies_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_nocal_2$P2_n, xlim = c(0,7500), horiz = TRUE, names.arg = P1_nocal_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#repeat - are the species with the highest count of significant trios from the same clade?
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P2tests_perspecies_boxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(P2_nocal_n~malawisamples.clade_SG, data = P1_nocal_2_clade, las = 2, ylab = "", xlab = "")
dev.off()

#are any groups significantly different from the others?
head(P1_nocal_2_clade)
#install.packages("rstatix")
#library(rstatix)
res.aov <- aov(P2_nocal_n ~ malawisamples.clade_SG, data = P1_nocal_2_clade)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)
plot(res.aov, 1)
plot(res.aov, 2)
install.packages("car")
library(car)
leveneTest(P2_nocal_n ~ malawisamples.clade_SG, data = P1_nocal_2_clade)

#what about outgroup species - which outgroup species are involved in the significant tests?
P3_nocal_2 <- merge(not_malawi, P3_nocal, by = "genus_species", all.x = TRUE)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_outgroup_nocalliptera.png",width=20,height=30,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_nocal_2$n, xlim = c(0,7500), horiz = TRUE, names.arg = P3_nocal_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

max(P3_nocal_2$n)
min(P3_nocal_2$n)
var(P3_nocal_2$n)

#again with outgroups - are the species with the highest count from the same clade?

#add clade info
P3_nocal_2_clade <- merge(P3_nocal_2, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_nocal_2_clade <- droplevels(P3_nocal_2_clade) #remove the outgroup levels

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1P2tests_perspecies_outgroup_boxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_nocal_2_clade, las = 2, ylab = "", xlab = "")
dev.off()

head(sig_no_calliptera2)


#use all trios data (not just sig results) to plot f4-ratio of two ecomorph groups against each other as P1 and P2
#set P3 to be the same species, and repeat plot for differnt combinations of P1,P2 and P3

library(gridExtra)
library(ggplot2)

#head(BBAA_keep5_nocal2)
#unique(BBAA_keep5_nocal2$Clade_name_P2)

#make dataframe with every P1 and P2 combination of Malawi ecomorph groups
ecomorph_groups <- c("mbuna", "rhamphochromis", "diplotaxodon", "utaka", "shallowbenthic", "deepbenthic")
ecomorph_groups_comb <- t(combn(ecomorph_groups,2))
length(ecomorph_groups_comb[,1])

#get list of unique P3 species
P3_species <- unique(BBAA_keep5_nocal2$P3)

#filter trios so that P3 is fixed for one Non-Malawi species, and P1 and P2 are from 2 different ecomorph malawi groups
#where P1 is group1, P2 is group2 --> positive values
#where P1 is group2, P2 is group 1 --> negative values

#f4-ratio
for (l in 1:length(P3_species)){
    p3species <- P3_species[l]
    BBAA_oneP3 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P3 == p3species),]
    
    p <- list()
    dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_nocalliptera_P1P2ecomorphgroups_comparison_", p3species, ".png", sep = ""),width=10,height=40,units="in",res=500)
    for (i in 1:length(ecomorph_groups_comb[,1])){
        group1 <- ecomorph_groups_comb[i,1]
        group2 <- ecomorph_groups_comb[i,2]
    
        BBAA_oneP3_twoP2 <-BBAA_oneP3[(BBAA_oneP3$Clade_name_P2 == group1 & BBAA_oneP3$Clade_name_P1 == group2),]
        BBAA_oneP3_twoP2_opposite <- BBAA_oneP3[(BBAA_oneP3$Clade_name_P1 == group1 & BBAA_oneP3$Clade_name_P2 == group2),]
        maximum_x <- c(as.numeric(max(BBAA_oneP3_twoP2$f4.ratio)), as.numeric(max(BBAA_oneP3_twoP2_opposite$f4.ratio)))
        BBAA_oneP3_twoP2_opposite$f4.ratio <- BBAA_oneP3_twoP2_opposite$f4.ratio*(-1)
        BBAA_oneP3_twoP2_combined <- rbind(BBAA_oneP3_twoP2, BBAA_oneP3_twoP2_opposite)
        
        
        p[[i]] <- ggplot(BBAA_oneP3_twoP2_combined, aes(f4.ratio, fill = significant)) + 
            xlim(max(maximum_x)*(-1), max(maximum_x)) + geom_vline(xintercept = 0) +
            geom_histogram(binwidth=0.000005) + xlab(paste("P2 = ", group2, " <----------> ", "P2 = ", group1, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))
    }
    do.call(grid.arrange,c(p, ncol = 1, top = paste(p3species)))
    dev.off()
}



#d-stat
for (l in 1:length(P3_species)){
    p3species <- P3_species[l]
    BBAA_oneP3 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P3 == p3species),]
    
    p <- list()
    dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_nocalliptera_P1P2ecomorphgroups_comparison_", p3species, ".png", sep = ""),width=10,height=40,units="in",res=500)
    for (i in 1:length(ecomorph_groups_comb[,1])){
        group1 <- ecomorph_groups_comb[i,1]
        group2 <- ecomorph_groups_comb[i,2]
    
        BBAA_oneP3_twoP2 <-BBAA_oneP3[(BBAA_oneP3$Clade_name_P2 == group1 & BBAA_oneP3$Clade_name_P1 == group2),]
        BBAA_oneP3_twoP2_opposite <- BBAA_oneP3[(BBAA_oneP3$Clade_name_P1 == group1 & BBAA_oneP3$Clade_name_P2 == group2),]
        maximum_x <- c(as.numeric(max(BBAA_oneP3_twoP2$Dstatistic)), as.numeric(max(BBAA_oneP3_twoP2_opposite$Dstatistic)))
        BBAA_oneP3_twoP2_opposite$Dstatistic <- BBAA_oneP3_twoP2_opposite$Dstatistic*(-1)
        BBAA_oneP3_twoP2_combined <- rbind(BBAA_oneP3_twoP2, BBAA_oneP3_twoP2_opposite)
        
        
        p[[i]] <- ggplot(BBAA_oneP3_twoP2_combined, aes(Dstatistic, fill = significant)) + 
            xlim(max(maximum_x)*(-1), max(maximum_x)) + geom_vline(xintercept = 0) +
            geom_histogram(binwidth=0.0005) + xlab(paste("P2 = ", group2, " <----------> ", "P2 = ", group1, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))
    }
    do.call(grid.arrange,c(p, ncol = 1, top = paste(p3species)))
    dev.off()
}





p3species <- P3_species[1]
BBAA_oneP3 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P3 == p3species),]


group1 <- ecomorph_groups_comb[1,1]
group2 <- ecomorph_groups_comb[1,2]


BBAA_oneP3_twoP2 <-BBAA_oneP3[(BBAA_oneP3$Clade_name_P2 == group1 & BBAA_oneP3$Clade_name_P1 == group2),]

#head(BBAA_oneP3_twoP2)
BBAA_oneP3_twoP2_opposite <- BBAA_oneP3[(BBAA_oneP3$Clade_name_P1 == group1 & BBAA_oneP3$Clade_name_P2 == group2),]

maximum_x <- c(as.numeric(max(BBAA_oneP3_twoP2$f4.ratio)), as.numeric(max(BBAA_oneP3_twoP2_opposite$f4.ratio)))

#max <- c(max(BBAA_oneP3_twoP2$f4.ratio), max(BBAA_oneP3_twoP2_opposite$f4.ratio))
BBAA_oneP3_twoP2_opposite$f4.ratio <- BBAA_oneP3_twoP2_opposite$f4.ratio*(-1)
length(BBAA_oneP3_twoP2_opposite$f4.ratio)

BBAA_oneP3_twoP2_combined <- rbind(BBAA_oneP3_twoP2, BBAA_oneP3_twoP2_opposite)

max(BBAA_oneP3_twoP2_combined$f4.ratio)

ggplot(BBAA_oneP3_twoP2_combined, aes(f4.ratio, fill = significant)) + 
    xlim(max(maximum_x)*(-1), max(maximum_x)) + geom_vline(xintercept = 0) +
    geom_histogram(binwidth=0.000005) + xlab(paste(group1, " <----------> ", group2, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))





#max(BBAA_oneP3_twoP2_combined$f4.ratio)
#min(BBAA_oneP3_twoP2_combined$f4.ratio)

#abs(max(BBAA_oneP3_twoP2_combined$f4.ratio))
#abs(min(BBAA_oneP3_twoP2_combined$f4.ratio))


max(BBAA_oneP3_twoP2$f4.ratio)
max(BBAA_oneP3_twoP2_opposite$f4.ratio)
maximum_x <- c(as.numeric(max(BBAA_oneP3_twoP2$f4.ratio)), as.numeric(max(BBAA_oneP3_twoP2_opposite$f4.ratio)))
max
max(max)


#filter trios so that P3 is a unique species
#then make unique plots for every combination of P1 and P2 group in one figure
#make histogram plot with points and colour points according to significance 

#test for one combination of P2 species with one P3 species
#test2 <- test1[(test1$Clade_name_P2 == "mbuna" | test1$Clade_name_P2 == "rhamphochromis" ),]
#change values to negative where P2 is one ecomorph group
#test2$f4.ratio[test2$Clade_name_P2== "mbuna"] <- test2$f4.ratio*(-1)
#head(test2)
#p <- ggplot(test2, aes(f4.ratio, fill = significant)) + 
#        geom_histogram(binwidth=0.000005)
#p + xlab("P2 mbuna <- negative -------- positive -> P2 rhamphochromis") +
#theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
#        panel.grid.minor = element_line(color = "gray88"))

#run for all P3 species, and all combinations of P2 ecomorph groups
for (l in 1:length(P3_species)){
    p3species <- P3_species[l]
    BBAA_oneP3 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P3 == p3species),]
    
    p <- list()
    dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_nocalliptera_P2ecomorphgroups_", p3species, ".png", sep = ""),width=10,height=40,units="in",res=500)
    for (i in 1:length(ecomorph_groups_comb[,1])){
        group1 <- ecomorph_groups_comb[i,1]
        group2 <- ecomorph_groups_comb[i,2]
    
        BBAA_oneP3_twoP2 <- BBAA_oneP3[(BBAA_oneP3$Clade_name_P2 == group1 | BBAA_oneP3$Clade_name_P2 == group2),]
        #BBAA_oneP3_twoP2$f4.ratio[BBAA_oneP3_twoP2$Clade_name_P2== group1] <- BBAA_oneP3_twoP2$f4.ratio*(-1)
        BBAA_oneP3_twoP2$f4.ratio[BBAA_oneP3_twoP2$Clade_name_P2== group1] <- BBAA_oneP3_twoP2$f4.ratio[BBAA_oneP3_twoP2$Clade_name_P2== group1]*(-1)

    
        p[[i]] <- ggplot(BBAA_oneP3_twoP2, aes(f4.ratio, fill = significant)) + 
            geom_histogram(binwidth=0.000005) + xlab(paste(group1, " <----------> ", group2, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))
    }
    do.call(grid.arrange,c(p, ncol = 1, top = paste(p3species)))
    dev.off()
}


#repeat with d-stat
for (l in 1:length(P3_species)){
    p3species <- P3_species[l]
    BBAA_oneP3 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P3 == p3species),]
    
    p <- list()
    dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_nocalliptera_P2ecomorphgroups_", p3species, ".png", sep = ""),width=10,height=40,units="in",res=500)
    for (i in 1:length(ecomorph_groups_comb[,1])){
        group1 <- ecomorph_groups_comb[i,1]
        group2 <- ecomorph_groups_comb[i,2]
    
        BBAA_oneP3_twoP2 <- BBAA_oneP3[(BBAA_oneP3$Clade_name_P2 == group1 | BBAA_oneP3$Clade_name_P2 == group2),]
        #BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1] <- BBAA_oneP3_twoP2$Dstatistic*(-1)
        BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1] <- BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1]*(-1)

        p[[i]] <- ggplot(BBAA_oneP3_twoP2, aes(Dstatistic, fill = significant)) + 
            geom_histogram(binwidth=0.0005) + xlab(paste(group1, " <----------> ", group2, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))
    }
    do.call(grid.arrange,c(p, ncol = 1, top = paste(p3species)))
    dev.off()
}


l <- 1
i <- 1
p3species <- P3_species[l]
    BBAA_oneP3 <- BBAA_keep5_nocal2[(BBAA_keep5_nocal2$P3 == p3species),]
group1 <- ecomorph_groups_comb[i,1]
        group2 <- ecomorph_groups_comb[i,2]
    
    BBAA_oneP3_twoP2 <- BBAA_oneP3[(BBAA_oneP3$Clade_name_P2 == group1 | BBAA_oneP3$Clade_name_P2 == group2),]


    BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1] <- BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1]*(-1)
    


        ggplot(BBAA_oneP3_twoP2, aes(Dstatistic, fill = significant)) + 
            geom_histogram(binwidth=0.0005) + xlab(paste(group1, " <----------> ", group2, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))

        ggplot(BBAA_oneP3_twoP2, aes(Dstatistic, fill = significant)) + 
            geom_histogram(binwidth=0.0005) + xlab(paste(group1, " <----------> ", group2, sep = "")) +
        theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major = element_line(color = "gray88"),
            panel.grid.minor = element_line(color = "gray88"))

BBAA_oneP3_twoP2

length(BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1])
length(BBAA_oneP3_twoP2$Dstatistic[BBAA_oneP3_twoP2$Clade_name_P2== group1]*(-1))

rhamph_p1 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Rhamphochromis_woodi"),]
rhamph_p2 <- sig_no_calliptera2[(sig_no_calliptera2$P2 == "Rhamphochromis_woodi"),]
rhamph_p1p2 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Rhamphochromis_woodi") | (sig_no_calliptera2$P2 == "Rhamphochromis_woodi"),]

length(rhamph_p1$P1)
length(rhamph_p2$P1)
length(rhamph_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 rhampochromis woodi tests?
P1_rhamph_introgressed <- rhamph_p2 %>% count(P1)
P3_rhamph_introgressed <- rhamph_p2 %>% count(P3)
colnames(P1_rhamph_introgressed)[1] <- "genus_species"
colnames(P3_rhamph_introgressed)[1] <- "genus_species"

max(P1_rhamph_introgressed$n)
max(P3_rhamph_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_rhamphochromiswoodiP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_rhamph_introgressed$n, xlim = c(0,70), horiz = TRUE, names.arg = P1_rhamph_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_rhamphochromiswoodiP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_rhamph_introgressed$n, xlim = c(0,220), horiz = TRUE, names.arg = P3_rhamph_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150,200,250), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()


#merge with clade information
P1_rhamph_introgressed_clade <- merge(P1_rhamph_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_rhamph_introgressed_clade <- droplevels(P1_rhamph_introgressed_clade) #remove the outgroup levels

P3_rhamph_introgressed_clade <- merge(P3_rhamph_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_rhamph_introgressed_clade <- droplevels(P3_rhamph_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_rhamphochromiswoodiP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_rhamph_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_rhamphochromiswoodiP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_rhamph_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

#plot genus P1 but sorted by clade
sorted <- P1_rhamph_introgressed_clade[order(P1_rhamph_introgressed_clade$malawisamples.clade),]
genusorder <- unique(sorted$malawisamples.genus)
sorted$malawisamples.genus <- factor(sorted$malawisamples.genus , levels = genusorder)
rmduplicatesgenus = sorted[!duplicated(sorted$malawisamples.genus),]
t <- table(rmduplicatesgenus$malawisamples.clade)
#colours <- c(rep("red", 5), rep("blue", 2), rep("green", 13), rep("yellow",1), rep("purple",21), rep("orange",0))
colours <- c(rep("red", as.numeric(t[1])), rep("blue", as.numeric(t[2])), rep("green", as.numeric(t[3])),
             rep("yellow",as.numeric(t[4])), rep("purple", as.numeric(t[5])), rep("orange", as.numeric(t[6])))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_rhamphochromiswoodiP2_only_aboxplotpergenus_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.genus, data = sorted, las = 2, ylab = "", xlab = "", col = colours)
legend("topleft", legend = c("deep benthic", "diplotaxodon", "mbuna", "rhamphochromis", "shallow benthic", "utaka"),
       col = c("red", "blue", "green", "yellow", "purple", "orange"), lty = 1, cex = 0.4)
dev.off()

ptoko_p1 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Pallidochromis_tokolosh"),]
ptoko_p2 <- sig_no_calliptera2[(sig_no_calliptera2$P2 == "Pallidochromis_tokolosh"),]
ptoko_p1p2 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Pallidochromis_tokolosh") | (sig_no_calliptera2$P2 == "Pallidochromis_tokolosh"),]

length(ptoko_p1$P1)
length(ptoko_p2$P1)
length(ptoko_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 p tokolosh tests?
P1_ptoko_introgressed <- ptoko_p2 %>% count(P1)
P3_ptoko_introgressed <- ptoko_p2 %>% count(P3)
colnames(P1_ptoko_introgressed)[1] <- "genus_species"
colnames(P3_ptoko_introgressed)[1] <- "genus_species"

max(P1_ptoko_introgressed$n)
max(P3_ptoko_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_pallidochromistokoloshP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_ptoko_introgressed$n, xlim = c(0,60), horiz = TRUE, names.arg = P1_ptoko_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_pallidochromistokoloshP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_ptoko_introgressed$n, xlim = c(0,200), horiz = TRUE, names.arg = P3_ptoko_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150,200), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#merge with clade information
P1_ptoko_introgressed_clade <- merge(P1_ptoko_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_ptoko_introgressed_clade <- droplevels(P1_ptoko_introgressed_clade) #remove the outgroup levels

P3_ptoko_introgressed_clade <- merge(P3_ptoko_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_ptoko_introgressed_clade <- droplevels(P3_ptoko_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_pallidochromistokoloshP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_ptoko_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_pallidochromistokoloshP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_ptoko_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()


#plot genus P1 but sorted by clade
sorted <- P1_ptoko_introgressed_clade[order(P1_ptoko_introgressed_clade$malawisamples.clade),]
genusorder <- unique(sorted$malawisamples.genus)
sorted$malawisamples.genus <- factor(sorted$malawisamples.genus , levels = genusorder)
rmduplicatesgenus = sorted[!duplicated(sorted$malawisamples.genus),]
t <- table(rmduplicatesgenus$malawisamples.clade)
#colours <- c(rep("red", 5), rep("blue", 2), rep("green", 13), rep("yellow",1), rep("purple",21), rep("orange",0))
colours <- c(rep("red", as.numeric(t[1])), rep("blue", as.numeric(t[2])), rep("green", as.numeric(t[3])),
             rep("yellow",as.numeric(t[4])), rep("purple", as.numeric(t[5])), rep("orange", as.numeric(t[6])))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_pallidochromistokoloshP2_only_aboxplotpergenus_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.genus, data = sorted, las = 2, ylab = "", xlab = "", col = colours)
legend("topleft", legend = c("deep benthic", "diplotaxodon", "mbuna", "rhamphochromis", "shallow benthic", "utaka"),
       col = c("red", "blue", "green", "yellow", "purple", "orange"), lty = 1, cex = 0.4)
dev.off()

tyran_p1 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Tyrannochromis_nigriventer"),]
tyran_p2 <- sig_no_calliptera2[(sig_no_calliptera2$P2 == "Tyrannochromis_nigriventer"),]
tyran_p1p2 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Tyrannochromis_nigriventer") | (sig_no_calliptera2$P2 == "Tyrannochromis_nigriventer"),]

length(tyran_p1$P1)
length(tyran_p2$P1)
length(tyran_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 tyrannochromis nigriventer tests?
P1_tyran_introgressed <- tyran_p2 %>% count(P1)
P3_tyran_introgressed <- tyran_p2 %>% count(P3)
colnames(P1_tyran_introgressed)[1] <- "genus_species"
colnames(P3_tyran_introgressed)[1] <- "genus_species"

max(P1_tyran_introgressed$n)
max(P3_tyran_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_tyrannochromisnigriventerP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_tyran_introgressed$n, xlim = c(0,70), horiz = TRUE, names.arg = P1_tyran_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_tyrannochromisnigriventerP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_tyran_introgressed$n, xlim = c(0,150), horiz = TRUE, names.arg = P3_tyran_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#merge with clade information
P1_tyran_introgressed_clade <- merge(P1_tyran_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_tyran_introgressed_clade <- droplevels(P1_tyran_introgressed_clade) #remove the outgroup levels

P3_tyran_introgressed_clade <- merge(P3_tyran_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_tyran_introgressed_clade <- droplevels(P3_tyran_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_tyrannochromisnigriventerP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_tyran_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_tyrannochromisnigriventerP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_tyran_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

#plot genus P1 but sorted by clade
sorted <- P1_tyran_introgressed_clade[order(P1_tyran_introgressed_clade$malawisamples.clade),]
genusorder <- unique(sorted$malawisamples.genus)
sorted$malawisamples.genus <- factor(sorted$malawisamples.genus , levels = genusorder)
rmduplicatesgenus = sorted[!duplicated(sorted$malawisamples.genus),]
t <- table(rmduplicatesgenus$malawisamples.clade)
#colours <- c(rep("red", 5), rep("blue", 2), rep("green", 13), rep("yellow",1), rep("purple",21), rep("orange",0))
colours <- c(rep("red", as.numeric(t[1])), rep("blue", as.numeric(t[2])), rep("green", as.numeric(t[3])),
             rep("yellow",as.numeric(t[4])), rep("purple", as.numeric(t[5])), rep("orange", as.numeric(t[6])))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_tyrannochromisnigriventerP2_only_aboxplotpergenus_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.genus, data = sorted, las = 2, ylab = "", xlab = "", col = colours)
legend("topleft", legend = c("deep benthic", "diplotaxodon", "mbuna", "rhamphochromis", "shallow benthic", "utaka"),
       col = c("red", "blue", "green", "yellow", "purple", "orange"), lty = 1, cex = 0.4)
dev.off()


sciaeno_p1 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Sciaenochromis_sp-nyassae"),]
sciaeno_p2 <- sig_no_calliptera2[(sig_no_calliptera2$P2 == "Sciaenochromis_sp-nyassae"),]
sciaeno_p1p2 <- sig_no_calliptera2[(sig_no_calliptera2$P1 == "Sciaenochromis_sp-nyassae") | (sig_no_calliptera2$P2 == "Sciaenochromis_sp-nyassae"),]

length(sciaeno_p1$P1)
length(sciaeno_p2$P1)
length(sciaeno_p1p2$P1)

#which P1 and P3 species are involved in the significant P2 Sciaenochromis_sp-nyassae tests?
P1_sciaeno_introgressed <- sciaeno_p2 %>% count(P1)
P3_sciaeno_introgressed <- sciaeno_p2 %>% count(P3)
colnames(P1_sciaeno_introgressed)[1] <- "genus_species"
colnames(P3_sciaeno_introgressed)[1] <- "genus_species"

max(P1_sciaeno_introgressed$n)
max(P3_sciaeno_introgressed$n)

#plot barplot for each species
#P2
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_sciaenochromisspnyassaeP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_sciaeno_introgressed$n, xlim = c(0,80), horiz = TRUE, names.arg = P1_sciaeno_introgressed$genus_species, las = 1 )
abline(v = c(10,20,30,40,50,60,70), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#P3
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_sciaenochromisspnyassaeP2_only_nocalliptera.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P3_sciaeno_introgressed$n, xlim = c(0,210), horiz = TRUE, names.arg = P3_sciaeno_introgressed$genus_species, las = 1 )
abline(v = c(50,100,150,200), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#merge with clade information
P1_sciaeno_introgressed_clade <- merge(P1_sciaeno_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P1_sciaeno_introgressed_clade <- droplevels(P1_sciaeno_introgressed_clade) #remove the outgroup levels

P3_sciaeno_introgressed_clade <- merge(P3_sciaeno_introgressed, cladeinfo2, by = "genus_species", all.y = FALSE)
P3_sciaeno_introgressed_clade <- droplevels(P3_sciaeno_introgressed_clade) #remove the outgroup levels
#head(P2_calliptera_clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_sciaenochromisspnyassaeP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.clade, data = P1_sciaeno_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P3tests_perspecies_sciaenochromisspnyassaeP2_only_aboxplotperclade_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(25,5,1,1))
boxplot(n~malawisamples.clade_SG, data = P3_sciaeno_introgressed_clade, las = 2, ylab = "", xlab = "")
dev.off()


#plot genus P1 but sorted by clade
sorted <- P1_sciaeno_introgressed_clade[order(P1_sciaeno_introgressed_clade$malawisamples.clade),]
genusorder <- unique(sorted$malawisamples.genus)
sorted$malawisamples.genus <- factor(sorted$malawisamples.genus , levels = genusorder)
rmduplicatesgenus = sorted[!duplicated(sorted$malawisamples.genus),]
t <- table(rmduplicatesgenus$malawisamples.clade)
#colours <- c(rep("red", 5), rep("blue", 2), rep("green", 13), rep("yellow",1), rep("purple",21), rep("orange",0))
colours <- c(rep("red", as.numeric(t[1])), rep("blue", as.numeric(t[2])), rep("green", as.numeric(t[3])),
             rep("yellow",as.numeric(t[4])), rep("purple", as.numeric(t[5])), rep("orange", as.numeric(t[6])))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_numberof_sig_P1tests_perspecies_sciaenochromisspnyassaeP2_only_aboxplotpergenus_nocalliptera.png",width=10,height=10,units="in",res=200)
par(mar=c(10,5,1,1))
boxplot(n~malawisamples.genus, data = sorted, las = 2, ylab = "", xlab = "", col = colours)
legend("topleft", legend = c("deep benthic", "diplotaxodon", "mbuna", "rhamphochromis", "shallow benthic", "utaka"),
       col = c("red", "blue", "green", "yellow", "purple", "orange"), lty = 1, cex = 0.4)
dev.off()

head(P1_sciaeno_introgressed_clade)

tail(sorted)
#table(sorted$malawisamples.genus)

rmduplicatesgenus = sorted[!duplicated(sorted$malawisamples.genus),]
table(rmduplicatesgenus$malawisamples.clade)

t <- table(rmduplicatesgenus$malawisamples.clade)
as.numeric(t[1])

length(rmduplicatesgenus$malawisamples.genus)
length(unique(sorted$malawisamples.genus))

#rmduplicatesgenus

colours <- c(rep("red", as.numeric(t[1])), rep("blue", as.numeric(t[2])), rep("green", as.numeric(t[3])),
             rep("yellow",as.numeric(t[4])), rep("purple", as.numeric(t[5])), rep("orange", as.numeric(t[6])))

par(mar=c(10,5,1,1))
boxplot(n~malawisamples.genus, data = sorted, las = 2, ylab = "", xlab = "", col = colours)
legend(1,58, legend = c("deep benthic", "diplotaxodon", "mbuna", "rhamphochromis", "shallow benthic", "utaka"),
       col = c("red", "blue", "green", "yellow", "purple", "orange"), lty = 1, cex = 0.4)
#head(sorted)

#run with different a.calliptera populations

setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/dtools/dtrios_cluster/output/sophie_malawi_nonmalawi_aug2023_acalliptera_pops")
BBAA <- read.table("sophie_malawi_nonmalawi_aug2023_acalliptera_pops_BBAA.txt", sep = "\t", header = TRUE)

malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2023-11-05_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)

unique_species <- malawisamples_regions[!duplicated(malawisamples_regions$full_name),]
groupings <- data.frame(unique_species$full_name, unique_species$clade_SG)
colnames(groupings)[1] <- "P3"

library(dplyr)

#remove trios with species which is just called "Astatotilapia_calliptera" - this is a mistake
BBAA_2 <- BBAA[!BBAA$P1 == "Astatotilapia_calliptera",]
BBAA_3 <- BBAA_2[!BBAA_2$P2 == "Astatotilapia_calliptera",]
BBAA_4 <- BBAA_3[!BBAA_3$P3 == "Astatotilapia_calliptera",]

#remove trios where there is NaN for stats
BBAA_4_nan_remove <- BBAA_4[!BBAA_4$Dstatistic == "NaN",]

#mark which trios are significant
l <- length(BBAA_4_nan_remove$P2)
pvalue <- 5e-2
significant <- BBAA_4_nan_remove[BBAA_4_nan_remove$p.value <= pvalue/l,]
BBAA_5 <- BBAA_4_nan_remove %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

#add in group info to p1, p2 and p3
victoria <- c("Astatotilapia_flaviijosephi", "Pundamilia_nyererei", "Haplochromis_gracilior", "Thoracochromis_pharyngalis")
calliptera_nonind <- c("Astatotilapia_calliptera_Bua_river", "Astatotilapia_calliptera_Chitimba", "Astatotilapia_calliptera_Chizumulu_island", "Astatotilapia_calliptera_Enukweni", "Astatotilapia_calliptera_Itupi_river", "Astatotilapia_calliptera_Kyela", " Astatotilapia_calliptera_Lake_Itamba", "Astatotilapia_calliptera_Lake_Kingiri", "Astatotilapia_calliptera_Lake_Malombe", "Astatotilapia_calliptera_Lake_Masoko", "Astatotilapia_calliptera_Luwawa", "Astatotilapia_calliptera_Mbaka_river", "Astatotilapia_calliptera_North_Rukuru_river", "Astatotilapia_calliptera_Salima", "Astatotilapia_calliptera_Songwe_river", "Astatotilapia_calliptera_South_Rukuru_river", "Astatotilapia_calliptera_Southwest_arm")
calliptera_ind <- c("Astatotilapia_calliptera_Kitai_Dam", "Astatotilapia_calliptera_Rovuma_river", "Astatotilapia_calliptera_Lake_Chidya", "Astatotilapia_calliptera_Lake_Chilwa")
malawi <- c("Labeotropheus_fuelleborni", "Copadichromis_chrysonotus", "Diplotaxodon_limnothrissa", "Mylochromis_subocularis", "Alticorpus_peterdaviesi", "Rhamphochromis_woodi")
gigliolii <- c("Astatotilapia_gigliolii_1", "Astatotilapia_gigliolii_2", "Astatotilapia_gigliolii_3", "Astatotilapia_gigliolii_4", "Astatotilapia_gigliolii_5")

BBAA_5$P1_group <- "NA"
BBAA_5$P2_group <- "NA"
BBAA_5$P3_group <- "NA"

for (i in 1:nrow(BBAA_5)) {
  # Check if the species is present in any of the vectors
  if (BBAA_5$P1[i] %in% victoria) {
    BBAA_5$P1_group[i] <- "victoria"
  } else if (BBAA_5$P1[i] %in% calliptera_nonind) {
    BBAA_5$P1_group[i] <- "calliptera_nonind"
  } else if (BBAA_5$P1[i] %in% calliptera_ind) {
    BBAA_5$P1_group[i] <- "calliptera_ind"
  } else if (BBAA_5$P1[i] %in% malawi) {
    BBAA_5$P1_group[i] <- "malawi"
  } else if (BBAA_5$P1[i] %in% gigliolii) {
    BBAA_5$P1_group[i] <- "gigliolii"
  } else {
    BBAA_5$P1_group[i] <- "NA" # Optional: Handle cases where there's no match
  }
}

for (i in 1:nrow(BBAA_5)) {
  # Check if the species is present in any of the vectors
  if (BBAA_5$P2[i] %in% victoria) {
    BBAA_5$P2_group[i] <- "victoria"
  } else if (BBAA_5$P2[i] %in% calliptera_nonind) {
    BBAA_5$P2_group[i] <- "calliptera_nonind"
  } else if (BBAA_5$P2[i] %in% calliptera_ind) {
    BBAA_5$P2_group[i] <- "calliptera_ind"
  } else if (BBAA_5$P2[i] %in% malawi) {
    BBAA_5$P2_group[i] <- "malawi"
  } else if (BBAA_5$P2[i] %in% gigliolii) {
    BBAA_5$P2_group[i] <- "gigliolii"
  } else {
    BBAA_5$P2_group[i] <- "NA" # Optional: Handle cases where there's no match
  }
}

for (i in 1:nrow(BBAA_5)) {
  # Check if the species is present in any of the vectors
  if (BBAA_5$P3[i] %in% victoria) {
    BBAA_5$P3_group[i] <- "victoria"
  } else if (BBAA_5$P3[i] %in% calliptera_nonind) {
    BBAA_5$P3_group[i] <- "calliptera_nonind"
  } else if (BBAA_5$P3[i] %in% calliptera_ind) {
    BBAA_5$P3_group[i] <- "calliptera_ind"
  } else if (BBAA_5$P3[i] %in% malawi) {
    BBAA_5$P3_group[i] <- "malawi"
  } else if (BBAA_5$P3[i] %in% gigliolii) {
    BBAA_5$P3_group[i] <- "gigliolii"
  } else {
    BBAA_5$P3_group[i] <- "NA" # Optional: Handle cases where there's no match
  }
}


#Did IOC calliptera have gene flow with giglioli?
#P1 = A.calliptera or other Malawi, P2 = A.calliptera IOC, P3 = Gigliolii

BBAA_6 <- BBAA_5[BBAA_5$P3_group == "gigliolii",]
BBAA_7 <- BBAA_6[BBAA_6$P2_group == "calliptera_ind",]
BBAA_8 <- BBAA_7[BBAA_7$P1_group == "calliptera_nonind" | BBAA_7$P1_group == "malawi",]

#length(BBAA_8$Dstatistic)
summary(BBAA_8$significant) #all tests are significant
max(BBAA_8$Dstatistic)
min(BBAA_8$Dstatistic)
mean(BBAA_8$Dstatistic)

max(BBAA_8$f4.ratio)
min(BBAA_8$f4.ratio)
mean(BBAA_8$f4.ratio)

#BBAA_8[order(-BBAA_8$f4.ratio),]


BBAA_6 <- BBAA_5[BBAA_5$P1_group == "gigliolii",]
BBAA_7 <- BBAA_6[BBAA_6$P2_group == "gigliolii",]
BBAA_8 <- BBAA_7[BBAA_7$P3_group == "calliptera_ind",]
summary(BBAA_8$significant)
BBAA_8[order(-BBAA_8$f4.ratio),]


#is there variation in F4-ratios among A.calliptera populations?
#do the IOC populations have a higher f4-ratio than the other populations?

length(BBAA_4_nan_remove$Dstatistic)
length(significant$Dstatistic)
#not all trios are significant

BBAA_6 <- BBAA_5[BBAA_5$P3_group == "gigliolii",]
BBAA_7 <- BBAA_6[BBAA_6$P1_group == "victoria",]
BBAA_8 <- BBAA_7[!BBAA_7$P2_group == "victoria",]
BBAA_9 <- BBAA_8[!BBAA_8$P2_group == "NA",]
head(BBAA_9)
summary(BBAA_9$significant)
#length(BBAA_9$)
BBAA_10 <- BBAA_9[BBAA_9$P1 == "Astatotilapia_flaviijosephi",]
BBAA_11 <- BBAA_9[BBAA_9$P1 == "Pundamilia_nyererei",]
BBAA_12 <- BBAA_9[BBAA_9$P1 == "Haplochromis_gracilior",]
BBAA_13 <- BBAA_9[BBAA_9$P1 == "Thoracochromis_pharyngalis",]

victoria <- c("Astatotilapia_flaviijosephi", "Pundamilia_nyererei", "Haplochromis_gracilior", "Thoracochromis_pharyngalis")

#head(BBAA_7)


#boxplot(f4.ratio~P3, data = BBAA_7)

library(ggplot2)
library(cowplot)


#ggplot(BBAA_10, aes(x = P3, y = f4.ratio, fill = P2_group)) + 
    #geom_jitter(position = position_jitter(width = 0.1, height = 1), aes(colour = P2_group), alpha = 0.9)+
#    geom_jitter(aes(colour = P2_group)) +
    #geom_boxplot(alpha = 0.5)+
#    coord_flip() +
#    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/p1victoria_p2malawi_acalliptera_acallipteraIOC_p3gigliolii_f4-ratios_allvictoria.png",width=10,height=6,units="in",res=500)
ggplot(BBAA_9, aes(x=P3, y=f4.ratio,, fill=P2_group)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2_group), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/p1victoria_p2malawi_acalliptera_acallipteraIOC_p3gigliolii_f4-ratios_victoriaseparated.png",width=18,height=9,units="in",res=500)
g1 <- ggplot(BBAA_10, aes(x=P3, y=f4.ratio,, fill=P2_group)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2_group), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g2 <- ggplot(BBAA_11, aes(x=P3, y=f4.ratio,, fill=P2_group)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2_group), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g3 <- ggplot(BBAA_12, aes(x=P3, y=f4.ratio,, fill=P2_group)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2_group), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g4 <- ggplot(BBAA_13, aes(x=P3, y=f4.ratio,, fill=P2_group)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2_group), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
plot_grid(g1, g2, g3, g4, labels = c('A', 'B', 'C', 'D'))
dev.off()


#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_acallipterapops 


kitaidam <- BBAA_10[BBAA_10$P2 == "Astatotilapia_calliptera_Kitai_Dam",]
kitaidam
mean(kitaidam$f4.ratio)
max(kitaidam$f4.ratio)
min(kitaidam$f4.ratio)

gigliolii_names <- c(
    expression(paste(bolditalic("Astatotilapia gigliolii "), bold("1"))), 
    expression(paste(bolditalic("Astatotilapia gigliolii "), bold("2"))),
    expression(paste(bolditalic("Astatotilapia gigliolii "), bold("3"))),
    expression(paste(bolditalic("Astatotilapia gigliolii "), bold("4"))),
    expression(paste(bolditalic("Astatotilapia gigliolii "), bold("5"))))


ggplot(BBAA_10, aes(x=P3, y=f4.ratio,, fill=P2_group)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    scale_x_discrete(labels = gigliolii_names) +
    geom_point(position = position_jitterdodge(), aes(colour = P2_group), alpha=1) +
    theme(axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15),
         axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15))+
    theme(legend.position="none")+
    theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(2,2,1,1, "cm"))

#make y axis label larger and more outwards
#make x axis label larger
#add title (P1 = Victoria, P2 = A.calliptera (ind and non ind) + Malawi, P3 = A.gigliolii)

#figure 2b#

my_colors <- c("royalblue", "palegreen3", "red2")


#BBAA_10$label <- ifelse(BBAA_10$P2_group == "calliptera_ind", as.numeric(factor(BBAA_10$P2)), NA)
BBAA_10$label <- ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Kitai_Dam", '1',
                        ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Lake_Chidya", '2',
                              ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Lake_Chilwa", '3',
                                    ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Rovuma_river", '4', ''))))
 

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/p1aflaviijosephi_p2malawi_acalliptera_acallipteraIOC_p3gigliolii_f4-ratios_victoriaseparated_apr2024.png",width=10,height=6,units="in",res=500)
ggplot(BBAA_10, aes(x = P3, y = f4.ratio, fill = P2_group)) + 
  coord_flip() +
  geom_boxplot(alpha = 0.1, outlier.shape = NA, color="gray60", show.legend = FALSE) +
  scale_x_discrete(labels = gigliolii_names) +
  geom_point(position = position_jitterdodge(seed = 1), aes(colour = P2_group), alpha=1, size = 2.2, show.legend = FALSE) +
  geom_text(aes(label = label, colour = P2_group),position = position_jitterdodge(seed = 1), 
            size = 4, hjust = 0.5, vjust = -1.2, show.legend = FALSE) +
  scale_fill_manual(values = my_colors) + 
  scale_colour_manual(values = my_colors) +  
  ylab(expression(paste(bold("f"[4]*"-ratio")))) +
  theme_light()+
  theme(axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  theme(legend.position = "none")
dev.off()

  #theme(panel.background = element_rect(fill = "white", color = "black"),
  #      panel.grid.major = element_line(color = "white"),
  #      panel.grid.minor = element_line(color = "white"), plot.margin = margin(2,2,1,1, "cm"))

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_acallipterapops 


head(BBAA_10)

#head(BBAA_10)

#BBAA_10$label <- with(df, ifelse(col1 > col2, value_if_true, value_if_false))
# Manually assign numeric labels to species within calliptera_ind group
BBAA_10$label <- ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Kitai_Dam", '1',
                        ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Lake_Chidya", '2',
                              ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Lake_Chilwa", '3',
                                    ifelse(BBAA_10$P2 == "Astatotilapia_calliptera_Rovuma_river", '4', ''))))
                        
                                              


unique(BBAA_10$label)
indtest <- BBAA_10[BBAA_10$P2_group == "calliptera_ind",]
unique(indtest$P2)

#of the variation in f4-ratio among IOC populations, 
#is it consistently the same pops which have the highest values?

BBAA_6 <- BBAA_5[BBAA_5$P3_group == "gigliolii",]
BBAA_7 <- BBAA_6[BBAA_6$P1_group == "victoria",]
BBAA_8 <- BBAA_7[!BBAA_7$P2_group == "victoria",]
BBAA_9 <- BBAA_8[!BBAA_8$P2_group == "NA",]
BBAA_10 <- BBAA_9[BBAA_9$P2_group == "calliptera_ind",]

#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/p1victoria_p2acallipteraIOC_p3gigliolii_f4-ratios_allvictoria.png",width=10,height=6,units="in",res=500)
ggplot(BBAA_10, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))
#dev.off()

BBAA_11 <- BBAA_10[BBAA_10$P1 == "Astatotilapia_flaviijosephi",]
BBAA_12 <- BBAA_10[BBAA_10$P1 == "Pundamilia_nyererei",]
BBAA_13 <- BBAA_10[BBAA_10$P1 == "Haplochromis_gracilior",]
BBAA_14 <- BBAA_10[BBAA_10$P1 == "Thoracochromis_pharyngalis",]

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_aug2023_acalliptera_pops/_data/results/plots/p1victoria_p2acallipteraIOC_p3gigliolii_f4-ratios_victoriaseparated.png",width=10,height=6,units="in",res=500)
g1 <- ggplot(BBAA_11, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g2 <- ggplot(BBAA_12, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g3 <- ggplot(BBAA_13, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g4 <- ggplot(BBAA_14, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
plot_grid(g1, g2, g3, g4, labels = c('A', 'B', 'C', 'D'))
dev.off()

#have 4 columns and 1 row

g1 <- ggplot(BBAA_11, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))+
    theme(legend.position="none")
g2 <- ggplot(BBAA_12, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
    theme(legend.position="none")
g3 <- ggplot(BBAA_13, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
    theme(legend.position="none")
g4 <- ggplot(BBAA_14, aes(x=P3, y=f4.ratio,, fill=P2)) + 
    coord_flip() +
    geom_boxplot(alpha = 0.1) +
    geom_point(position = position_jitterdodge(), aes(colour = P2), alpha=1) +
    theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
    theme(legend.position="none")
plot_grid(g1, g2, g3, g4, labels = c('A', 'B', 'C', 'D'), nrow = 1, ncol = 4)

#####
#P1 = victoria, P2 = a.calliptera, P3 = gigliolii
#BBAA_sub <- BBAA_5[BBAA_5$P1_group == "victoria",]
#BBAA_sub2 <- BBAA_sub %>% filter(str_detect(P2, "Astatotilapia_calliptera"))
#BBAA_sub3 <- BBAA_sub2[BBAA_sub2$P3_group == "gigliolii",]
#BBAA_sub3[order(-BBAA_sub3$f4.ratio),]
#(length(BBAA_sub3[BBAA_sub3$significant == "TRUE",]$P1)/length(BBAA_sub3$P1))*100
#100% significant

#boxplot(f4.ratio~P2, data = BBAA_sub3)
#significantly higher f4-ratios for IOC a.calliptera pops, particularly kitai dam

#####
#P1 = a.calliptera nonindian, P2 = a.calliptera IOC, P3 = gigliolii
BBAA_sub <- BBAA_5[BBAA_5$P1_group == "calliptera_nonind",]
BBAA_sub2 <- BBAA_sub[BBAA_sub$P2_group == "calliptera_ind",]
BBAA_sub3 <- BBAA_sub2[BBAA_sub2$P3_group == "gigliolii",]
#BBAA_sub3[order(-BBAA_sub3$f4.ratio),]
(length(BBAA_sub3[BBAA_sub3$significant == "TRUE",]$P1)/length(BBAA_sub3$P1))*100
#100% significant

#####
#P1 = a.calliptera IOC, P2 = a.calliptera nonindian, P3 = gigliolii
BBAA_sub <- BBAA_5[BBAA_5$P1_group == "calliptera_nonind",]
BBAA_sub2 <- BBAA_sub[BBAA_sub$P2_group == "calliptera_nonind",]
BBAA_sub3 <- BBAA_sub2[BBAA_sub2$P3_group == "gigliolii",]
BBAA_sub3[order(-BBAA_sub3$f4.ratio),]
(length(BBAA_sub3[BBAA_sub3$significant == "TRUE",]$P1)/length(BBAA_sub3$P1))*100
#100% significant



#is there variation across gigliolii

#BBAA[order(-BBAA$f4.ratio),][1:50,]
#BBAA[order(-BBAA$f4.ratio),][51:100,]

library(stringr)

BBAA_test <- BBAA %>% filter(str_detect(P3, "Astatotilapia_gigliolii"))
#BBAA_test[order(-BBAA_test$f4.ratio),][1:50,]
#BBAA_test[order(-BBAA_test$f4.ratio),][51:100,]

#Astatotilapia_flaviijosephi
#Astatotilapia_calliptera_Kitai_Dam
#Astatotilapia_gigliolii_5

#BBAA_flav <- BBAA %>% filter(str_detect(P1, "Astatotilapia_flaviijosephi"))
#BBAA_flav2 <- BBAA_flav %>% filter(str_detect(P2, "Astatotilapia_calliptera"))
#BBAA_flav3 <- BBAA_flav2 %>% filter(str_detect(P3, "Astatotilapia_gigliolii_4"))
#BBAA_flav3[order(-BBAA_flav3$f4.ratio),]


#BBAA_flav <- BBAA %>% filter(str_detect(P1, "Astatotilapia_flaviijosephi"))
#BBAA_flav3 <- BBAA_flav %>% filter(str_detect(P3, "Astatotilapia_gigliolii_1"))
#BBAA_flav3[order(-BBAA_flav3$f4.ratio),]

BBAA_flav <- BBAA %>% filter(str_detect(P1, "Astatotilapia_calliptera_Rovuma_river"))
BBAA_flav2 <- BBAA_flav %>% filter(str_detect(P2, "Astatotilapia_calliptera_Kitai_Dam"))
BBAA_flav3 <- BBAA_flav2 %>% filter(str_detect(P3, "Astatotilapia_gigliolii_4"))
BBAA_flav3[order(-BBAA_flav3$f4.ratio),]

#BBAA_flav <- BBAA %>% filter(str_detect(P1, "Astatotilapia_gigliolii"))
#BBAA_flav2 <- BBAA_flav %>% filter(str_detect(P2, "Astatotilapia_gigliolii"))
#BBAA_flav3 <- BBAA_flav2 %>% filter(str_detect(P3, "Astatotilapia_calliptera"))
#BBAA_flav3[order(-BBAA_flav3$f4.ratio),]

#########

#setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/files/")
#BBAA <- read.table("combined_BBAA.txt", sep = "\t", header = TRUE)

setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/dtools/dtrios_cluster/output/sophie_malawi_nonmalawi_may2023")
BBAA <- read.table("sophie_malawi_nonmalawi_may2023_BBAA.txt", sep = "\t", header = TRUE)

#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-24-08_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2023-11-05_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)

unique_species <- malawisamples_regions[!duplicated(malawisamples_regions$full_name),]
groupings <- data.frame(unique_species$full_name, unique_species$clade_SG)
colnames(groupings)[1] <- "P3"
#unique(groupings)

#how many unique Malawi and non-malawi species
malawisamples <- malawisamples_regions[-c(611,612),]
#tail(malawisamples)
species_count <- malawisamples %>% group_by(region) %>%
  summarize(num_species = n_distinct(full_name))
species_count

unique(malawisamples$clade_SG)

#variables:
pvalue <- 5e-2

#malawisamples <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/files/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_regionlabels.tsv", sep = "\t", header = T) #this data frame doesnt include the new species names so can't be used
#new metadata with new region assignments:
#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades.tsv", sep = "\t", header = T)
#changed Astatoreochromis straeleni clade to burtoni in metadata:
#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades_Aststr.tsv", sep = "\t", header = T)
#changed two ruaha blue samples so that they are different species:
#malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades_Aststr.tsv", sep = "\t", header = T)


malawisamples <- malawisamples_regions[-c(611,612),]
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

#get list of P3 species (all Serranochromis group species) - try all other outgroups for now
non_malawi_groups <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_LVRScluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Orthochromis", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano <- malawisamples[malawisamples$clade_SG %in% non_malawi_groups,]
outgroups_serrano <- as.vector(unique(malawisamples_serrano$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))

#keep only serranochromis group in P3
BBAA_keep <- BBAA[BBAA$P3 %in% outgroups_serrano,]

#keep only victoria group and malawi species in P1 and P2
malawisamples_onlymalawi <- malawisamples[malawisamples$region == "Malawi",]
#BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% outgroups_victoria,]
#BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% unique(malawisamples_onlymalawi$genus_species),]

BBAA_keep1 <- BBAA_keep[BBAA_keep$P1 %in% outgroups_victoria & BBAA_keep$P2 %in% unique(malawisamples_onlymalawi$genus_species),]
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria & BBAA_keep$P1 %in% unique(malawisamples_onlymalawi$genus_species),]
BBAA_keep3 <- rbind(BBAA_keep1, BBAA_keep2)
l <- length(BBAA_keep3$P2)

#remove ruaha blue LVRS cluster species - from P3
#BBAA_keep3_noruahalvrs <- BBAA_keep3
BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P3 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",]


#write BBAA with only Malawi P1 and P2 and Outgroup P3
#write.table(BBAA_keep3, "out_sim_wholegenome_dtrios_parallel_allchroms_allsamples_newmetahannesdec2021_rmSRR12700905_rmSRR12700906_combined_split_combined_MalawiP1_LVRSP2_SerranoGroupP3_BBAA.txt", quote = FALSE, row.names = FALSE, sep = "\t")

#reorder for easier reading
BBAA_keep4 <- BBAA_keep3_noruahalvrs[order(BBAA_keep3_noruahalvrs$P2),]
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

#with ruaha blue LVRS cluster:
#[1] "number of all significant tests:"
#301305
#[1] "total number of all trios:"
#1279773
#[1] "percentage of all significant tests:"
#23.5436284403562
#[1] "####################"
#[1] "number of P2=Malawi significant tests:"
#179293
#[1] "total number of all P2=Malawi trios:"
#1121336
#[1] "percentage of P2=Malawi significant tests:"
#15.9892307033753
#14.0097501666311
#[1] "####################"
#[1] "number of P2=Victoria significant tests:"
#122012
#[1] "total number of all P2=Victoria trios:"
#158437
#[1] "percentage of P2=Victoria significant tests:"
#77.0097893800059
#9.53387827372511


head(BBAA_keep7)
unique(malawi_P2$unique_species.clade_SG.y)
unique(malawi_P2$unique_species.clade_SG.x)
length(unique(malawi_P2$unique_species.clade_SG.x))
unique(victoria_P2$unique_species.clade_SG.y)
length(unique(victoria_P2$unique_species.clade_SG.x))


#plot the z scores and P values for each group of P3 species

#plot(significant$Z.score)
#plot(significant$f4.ratio)

library(ggplot2)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2.png",width=20,height=7,units="in",res=500)
p <- ggplot(significant, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) + 
    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2.png",width=20,height=7,units="in",res=500)
d <- ggplot(significant, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)")
dev.off()

#look at the P2 species where orthochromis uvinzae is P3
ortho <- significant[significant$P3 == "Orthochromis_uvinzae",]
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_OuvinzaeP3.png",width=20,height=50,units="in",res=500)
p <- ggplot(ortho, aes(x = P2, y = Z.score, fill = unique_species.clade_SG.y)) + 
    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.y), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_OuvinzaeP3.png",width=20,height=50,units="in",res=500)
p <- ggplot(ortho, aes(x = P2, y = f4.ratio, fill = unique_species.clade_SG.y)) + 
    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.y), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()




#read in trios where P1 and P2 are reversed - Malawi is P1 and Victoria is P2
#make the D-statistic/z-score negative and plot with the other values

#keep only victoria group in P2
#BBAA_keep2_rev <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]

#keep only malawi species in P1
#BBAA_keep3_rev <- BBAA_keep2_rev[BBAA_keep2_rev$P1 %in% malawisamples_onlymalawi$genus_species,]
#l <- length(BBAA_keep3_rev$P2)

#colnames(groupings)[1] <- "P3"
#BBAA_keep6_rev <- merge(BBAA_keep3_rev, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
#colnames(groupings)[1] <- "P2"
#BBAA_keep7_rev <- merge(BBAA_keep6_rev, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)

#make D-stat and Z-score negative
#BBAA_keep7_rev$Dstatistic <- BBAA_keep7_rev$Dstatistic*-1
#BBAA_keep7_rev$Z.score <- BBAA_keep7_rev$Z.score*-1
#BBAA_keep7_rev$f4.ratio <- BBAA_keep7_rev$f4.ratio*-1
#head(BBAA_keep7_rev)

#which tests are significant?
#significant <- BBAA_keep7_rev[BBAA_keep7_rev$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#print("number of significant tests:")
#length(significant$P1)
#print("total number of trios:")
#length(BBAA_keep7_rev$P1)
#percentsig <- length(significant$P1)/length(BBAA_keep7_rev$P1) *100
#print("percentage of significant tests:")
#percentsig

#label significant comparisons
#BBAA_keep8_rev <- BBAA_keep7_rev %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

#concatenate data with the Victoria P1, Malawi P2 data
#BBAA_keep9 <- rbind(BBAA_keep8, BBAA_keep8_rev)
#BBAA_keep9

#new_l <- length(BBAA_keep9$P2)
#significant_comb <- BBAA_keep9[BBAA_keep9$p.value <= pvalue/new_l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#significant_comb

#change species order
#significant_comb2 <- significant_comb[order(significant_comb$unique_species.clade_SG.x),]
#species_order <- as.vector(unique(significant_comb2$P3))
#significant_comb2$P3 <- factor(significant_comb2$P3, levels = species_order)

#results before
#[1] "number of significant tests:"
#121952
#[1] "total number of trios:"
#132279
#[1] "percentage of significant tests:"
#92.1930162762041


#unique(significant_comb$P3)
ruahablue <- significant[significant$P3 == "Astatotilapia_sp-Ruaha-blue",]

max(ruahablue$f4.ratio)
mean(ruahablue$f4.ratio)
min((ruahablue$f4.ratio))
#head(ruahablue %>% arrange(desc(f4.ratio)))
#head(ruahablue %>% arrange(f4.ratio))
max(ruahablue$Dstatistic)
hist(ruahablue$f4.ratio, breaks = 100, xlim = c(0.01,0.10))
plot(ruahablue$Dstatistic)
plot(ruahablue$Z.score)


orthoindo <- significant[significant$P3 == "Orthochromis_indermauri",]
max(orthoindo$f4.ratio)

intero <- significant[significant$P3 == "Interochromis_loocki",]
max(intero$f4.ratio)

newgroups <- significant[significant$unique_species.clade_SG.x == "Orthochromis" | significant$unique_species.clade_SG.x == "Pseudo_Cteno_Ortho2" | significant$unique_species.clade_SG.x == "Serr_Pharyng_Sarg_Thora",]
max(newgroups$f4.ratio)
min(newgroups$f4.ratio[which(newgroups$f4.ratio>0)])


#plot again the z scores and P values for each group of P3 species
#this time with the negative z.scores of the Malawi P1/Victoria P2 tests

#plot(significant$Z.score)
#plot(significant$f4.ratio)

nonmalawi_group_order <- c("LVRS", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Ctenochromis_pectoralis", "Orthochromis")
significant_group_order <-  significant %>% arrange(factor(unique_species.clade_SG.x, levels = nonmalawi_group_order))
species_order <- as.vector(unique(significant_group_order$P3))
significant_group_order$P3 <- factor(significant_group_order$P3, levels = species_order)

library(ggplot2)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse.png",width=15,height=14,units="in",res=500)
p <- ggplot(significant_group_order, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) + 
    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse.png",width=15,height=14,units="in",res=500)
d <- ggplot(significant_group_order, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)")
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse.png",width=15,height=14,units="in",res=500)
d <- ggplot(significant_group_order, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
d + scale_color_brewer(palette = "Paired") + ylab("d statistic")
dev.off()





significant_group_order

######### FINAL PLOT #########

#make points smaller to easier to distinguish each trio
#add in labels without underline (like in previous version)
#add in tree and reorder
#make sure margins are big enough
#add in legend - manually?
#add in percent that are significant


#library(gridExtra)
#library(gtable)
install.packages("egg")
library(egg)
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

all_samples_group_info <- data.frame(malawisamples$genus_species, malawisamples$clade_SG)
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

species_order_neat <- c("Astatotilapia sp. Ruaha blue", "Astatotilapia gigliolii", "Haplochromis sp. kilossana",
"Haplochromis burtoni", "Astatoreochromis straeleni", "Haplochromis vanheusdeni", "Ctenochromis horei", "Gnathochromis pfefferi",
"Interochromis loocki", "Limnotilapia dardennii", "Lobochilotes labiatus", "Petrochromis fasciolatus", "Petrochromis orthognathus",
"Pseudosimochromis babaulti", "Pseudosimochromis curvifrons", "Tropheus annectens", "Tropheus sp. black", "Pharyngochromis acuticeps 2", "Pharyngochromis acuticeps 1",
"Sargochromis carlottae", "Serranochromis angusticeps", "Serranochromis macrocephalus", "Serranochromis robustus",
"Serranochromis sp. checkerboard", "Thoracochromis brauschi", "Orthochromis indermauri", "Orthochromis sp. red cheek",
"Pseudocrenilabrus multicolor", "Pseudocrenilabrus philander", "Ctenochromis pectoralis", "Orthochromis malagaraziensis",
"Orthochromis mazimeroensis", "Orthochromis uvinzae")

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

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_plusnonsig.png",width=23,height=14,units="in",res=500)
#par(mar = c(10,10,10,10))
d <- ggplot() +
    geom_jitter(data = significant_group_order, aes(x = P3, y = Dstatistic), alpha = 0, show.legend = FALSE, size = sizepoints)+ #plot transparent to get order correct
    #geom_vline(xintercept = 0, color = "black") + 
    geom_jitter(data = not_significant_group_order, aes(x = P3, y = Dstatistic), color = "lightgrey", alpha = 0.3, show.legend = FALSE, size = sizepoints)+
    geom_jitter(data = significant_group_order, aes(x = P3, y = Dstatistic, colour = unique_species.clade_SG.x), alpha = 1, show.legend = FALSE, size = sizepoints)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold')) +
    #scale_color_brewer(palette = "Paired") + 
    ylab("D-statistic") + xlab("P3 Non-Malawi Group") +
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
    theme(axis.text.y = element_blank(), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_blank(), axis.title.x = element_text(size = 25, face = 'bold')) +
    #scale_color_brewer(palette = "Paired") + 
    ylab("F4-ratio") + xlab("") +
    scale_color_manual(values = custom_colours) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(2,5,1,1, "cm"))

d3 <- ggplot() +
    annotate("text", x = max(significant_group_order$f4.ratio) + 0.01, y = unique(significant_group_order$P3),
             label = text_labels$percentage, size = 6, color = "black", hjust = 0) + 
    coord_cartesian(clip = "off") +
    theme_void()

#arrange plots so that they are the same width (excluding the yaxis)
ggarrange(d + theme(axis.ticks.y = element_blank(),
                     plot.margin = margin(r = 15)),
          d2 + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                    plot.margin = margin(r = 0, l = 15)),
          d3 + theme(plot.margin = margin(r = 50, l = 1)),
          nrow = 1, widths = c(5,5,0.15))

dev.off()


#https://www.r-bloggers.com/2019/05/the-small-multiples-plot-how-to-combine-ggplot2-plots-with-one-shared-axis/

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dtrios_nov2022/plots_may2023



ldev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablue_colmalawi_2.png",width=30,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_noneg, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

#get species y axis labels with space instead of underscore
#rev(species_order)

#xlabels_nonmalawi <- c(
#            'Orthochromis mazimeroensis', 
#            'Orthochromis malagaraziensis', 
#            'Orthochromis uvinzae', 
#            'Pseudocrenilabrus multicolor', 
#            'Pseudocrenilabrus philander',
#            'Ctenochromis pectoralis',
#            'Orthochromis indermauri', 
#            'Orthochromis sp-red-cheek',    
#            'Serranochromis angusticeps',
#            'Serranochromis macrocephalus', 
#            'Serranochromis robustus', 
#            'Pharyngochromis acuticeps',
#            'Thoracochromis brauschi', 
#            'Sargochromis carlottae', 
#            'Serranochromis sp-checkerboard',    
#            'Gnathochromis pfefferi', 
#            'Petrochromis orthognathus', 
#            'Ctenochromis horei', 
#            'Tropheus annectens',
#            'Tropheus sp-black', 
#            'Pseudosimochromis babaulti', 
#            'Pseudosimochromis curvifrons', 
#            'Lobochilotes labiatus',
#            'Petrochromis fasciolatus',    
#            'Interochromis loocki',
#            'Limnotilapia dardennii',
#            'Haplochromis vanheusdeni',
#            'Haplochromis burtoni', 
#            'Astatoreochromis straeleni', 
#            'Haplochromis sp-kilossana',
#            'Astatotilapia gigliolii')
#xlabels_nonmalawi_rev <- rev(xlabels_nonmalawi)

#update dec 2022 - species order has changed

#get order from species_order
#species_order

xlabels_nonmalawi_rev <- c('Astatotilapia sp-Ruaha-blue LVRScluster',
                           'Astatotilapia sp-Ruaha-blue',
                           'Astatotilapia gigliolii',
                           'Haplochromis sp-kilossana',
                           'Astatoreochromis straeleni',
                           'Haplochromis burtoni',
                           'Haplochromis vanheusdeni',
                           'Interochromis loocki',
                           'Limnotilapia dardennii',
                           'Gnathochromis pfefferi',
                           'Pseudosimochromis babaulti',
                           'Petrochromis orthognathus',
                           'Pseudosimochromis curvifrons',
                           'Ctenochromis horei',
                           'Petrochromis fasciolatus',
                           'Lobochilotes labiatus',
                           'Tropheus annectens',
                           'Tropheus sp-black',
                           'Thoracochromis brauschi',
                           'Serranochromis macrocephalus',
                           'Serranochromis sp-checkerboard',
                           'Serranochromis robustus',
                           'Serranochromis angusticeps',
                           'Sargochromis carlottae',
                           'Pharyngochromis acuticeps',
                           'Orthochromis sp-red-cheek',
                           'Orthochromis indermauri',
                           'Pseudocrenilabrus philander',
                           'Pseudocrenilabrus multicolor',
                           'Ctenochromis pectoralis',
                           'Orthochromis uvinzae',
                           'Orthochromis malagaraziensis',
                           'Orthochromis mazimeroensis')
xlabels_nonmalawi <- rev(xlabels_nonmalawi_rev)

xlabels_nonmalawi_rev_no_ruahaLVRS <- c('Astatotilapia sp-Ruaha-blue',
                           'Astatotilapia gigliolii',
                           'Haplochromis sp-kilossana',
                           'Astatoreochromis straeleni',
                           'Haplochromis burtoni',
                           'Haplochromis vanheusdeni',
                           'Interochromis loocki',
                           'Limnotilapia dardennii',
                           'Gnathochromis pfefferi',
                           'Pseudosimochromis babaulti',
                           'Petrochromis orthognathus',
                           'Pseudosimochromis curvifrons',
                           'Ctenochromis horei',
                           'Petrochromis fasciolatus',
                           'Lobochilotes labiatus',
                           'Tropheus annectens',
                           'Tropheus sp-black',
                           'Thoracochromis brauschi',
                           'Serranochromis macrocephalus',
                           'Serranochromis sp-checkerboard',
                           'Serranochromis robustus',
                           'Serranochromis angusticeps',
                           'Sargochromis carlottae',
                           'Pharyngochromis acuticeps',
                           'Orthochromis sp-red-cheek',
                           'Orthochromis indermauri',
                           'Pseudocrenilabrus philander',
                           'Pseudocrenilabrus multicolor',
                           'Ctenochromis pectoralis',
                           'Orthochromis uvinzae',
                           'Orthochromis malagaraziensis',
                           'Orthochromis mazimeroensis')
xlabels_nonmalawi_no_ruahaLVRS <- rev(xlabels_nonmalawi_rev_no_ruahaLVRS)

xlabels_nonmalawi_rev_no_ruaha <- c('Astatotilapia gigliolii',
                           'Haplochromis sp-kilossana',
                           'Astatoreochromis straeleni',
                           'Haplochromis burtoni',
                           'Haplochromis vanheusdeni',
                           'Interochromis loocki',
                           'Limnotilapia dardennii',
                           'Gnathochromis pfefferi',
                           'Pseudosimochromis babaulti',
                           'Petrochromis orthognathus',
                           'Pseudosimochromis curvifrons',
                           'Ctenochromis horei',
                           'Petrochromis fasciolatus',
                           'Lobochilotes labiatus',
                           'Tropheus annectens',
                           'Tropheus sp-black',
                           'Thoracochromis brauschi',
                           'Serranochromis macrocephalus',
                           'Serranochromis sp-checkerboard',
                           'Serranochromis robustus',
                           'Serranochromis angusticeps',
                           'Sargochromis carlottae',
                           'Pharyngochromis acuticeps',
                           'Orthochromis sp-red-cheek',
                           'Orthochromis indermauri',
                           'Pseudocrenilabrus philander',
                           'Pseudocrenilabrus multicolor',
                           'Ctenochromis pectoralis',
                           'Orthochromis uvinzae',
                           'Orthochromis malagaraziensis',
                           'Orthochromis mazimeroensis')
xlabels_nonmalawi_no_ruaha <- rev(xlabels_nonmalawi_rev_no_ruaha)


#remove ruaha blue for f4-ratio

#significant_comb2_group_order
significant_group_order_noruaha <- significant_group_order[!significant_group_order$P3 %in% c("Astatotilapia_sp-Ruaha-blue", "Astatotilapia_sp-Ruaha-blue-LVRScluster"),]

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablue.png",width=19,height=14,units="in",res=500)
d <- ggplot(significant_group_order_noruaha, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablue.png",width=19,height=14,units="in",res=500)
d <- ggplot(significant_group_order_noruaha, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablue.png",width=19,height=14,units="in",res=500)
d <- ggplot(significant_group_order_noruaha, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablue_legend.png",width=10,height=5,units="in",res=500)
#d <- ggplot(significant_comb2_group_order_noruaha, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
#    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
#    coord_flip() +
#    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
#         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
#d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
#theme(panel.background = element_rect(fill = "white", color = "black"),
#        panel.grid.major = element_line(color = "gray88"),
#        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
#dev.off()

#Ortho <- significant_comb2_group_order_noruaha_noneg[significant_comb2_group_order_noruaha_noneg$P3 == "Orthochromis_uvinzae",] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
#Ortho[order(Ortho$f4.ratio),]


#repeat above plots but colour points according to which Malawi ecomorph group P2 is

#remove the negative values again
#significant_comb2_group_order_noruaha_noneg <- significant_comb2_group_order_noruaha 

significant_group_order_noruaha_noneg <- significant_group_order_noruaha[significant_group_order_noruaha$f4.ratio > 0,] 

#width = 60 (previous is 19)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablue_colmalawi_2.png",width=30,height=14,units="in",res=500)
d <- ggplot(significant_group_order_noruaha_noneg, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablue_colmalawi_2.png",width=30,height=14,units="in",res=500)
d <- ggplot(significant_group_order_noruaha_noneg, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablue_colmalawi_2.png",width=30,height=14,units="in",res=500)
d <- ggplot(significant_group_order_noruaha_noneg, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()


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

#only orthochromis
#head(significant_comb2_group_order_noruaha_noneg)

ortho <- significant_group_order_noruaha_noneg[significant_group_order_noruaha_noneg$unique_species.clade_SG.x == "Orthochromis",]

p1 <- ggplot(ortho, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(ortho, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(ortho, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Orthochromis.png",width=20,height=10,units="in",res=1000)
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


ortho1 <- significant_group_order_noruaha_noneg[significant_group_order_noruaha_noneg$P3 == "Orthochromis_malagaraziensis",]

p1 <- ggplot(ortho1, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(ortho1, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(ortho1, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Orthochromis_malagaraziensis.png",width=20,height=10,units="in",res=1000)
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

ortho2 <- significant_group_order_noruaha_noneg[significant_group_order_noruaha_noneg$P3 == "Orthochromis_mazimeroensis",]

p1 <- ggplot(ortho2, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(ortho2, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(ortho2, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Orthochromis_mazimeroensis.png",width=20,height=10,units="in",res=1000)
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

ortho3 <- significant_group_order_noruaha_noneg[significant_group_order_noruaha_noneg$P3 == "Orthochromis_uvinzae",]

p1 <- ggplot(ortho3, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(ortho3, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(ortho3, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Orthochromis_uvinzae.png",width=20,height=10,units="in",res=1000)
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

#supplementary figure {result2_gigliolii_f4-ratio_variance}
gigliolii <- significant_group_order_noruaha_noneg[significant_group_order_noruaha_noneg$P3 == "Astatotilapia_gigliolii",]

p1 <- ggplot(gigliolii, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(gigliolii, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(gigliolii, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_gigliolii.png",width=20,height=10,units="in",res=1000)
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

kilossana <- significant_group_order_noruaha_noneg[significant_group_order_noruaha_noneg$P3 == "Haplochromis_sp-kilossana",]

p1 <- ggplot(kilossana, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.x = element_text(size = 15, face = 'bold'), axis.text.y = element_text(size = 13), axis.title.x = element_text(size = 20, face = 'bold'), axis.title.y = element_text(size = 20, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p2 <- ggplot(kilossana, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)
p3 <- ggplot(kilossana, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE, size = 0.2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x = element_text(size = 20, face = 'bold'), axis.text.x = element_text(size = 15, face = 'bold')) + scale_color_manual(values = custom_colours_malawi)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Hkilossana.png",width=20,height=10,units="in",res=1000)
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

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_ruaha_blue.png",width=20,height=10,units="in",res=1000)
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
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_ruaha_blue_legend.png",width=15,height=10,units="in",res=1000)
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



head(significant_group_order)
unique(ortho$P3)

#test other species 

### Supplementary Figure {result2_Pmulticolor_f4-ratio_variance} ####
#not using as a supplementary figure for now

pmulticolor <- significant_group_order[significant_group_order$P3 == "Pseudocrenilabrus_multicolor",]
head(pmulticolor)
p <- list()
p[[1]]  <- ggplot(pmulticolor, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE)
p[[2]]  <- ggplot(pmulticolor, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
p[[3]]  <- ggplot(pmulticolor, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Pseudocrenilabrus_multicolor.png",width=20,height=30,units="in",res=500)
do.call(grid.arrange,c(p, ncol = 3))
dev.off()




limno <- significant_group_order[significant_group_order$P3 == "Limnotilapia_dardennii",]
p <- list()
p[[1]]  <- ggplot(limno, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE)
p[[2]]  <- ggplot(limno, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
p[[3]]  <- ggplot(limno, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Limnotilapia_dardennii.png",width=20,height=30,units="in",res=500)
do.call(grid.arrange,c(p, ncol = 3))
dev.off()



intero <- significant_group_order[significant_group_order$P3 == "Interochromis_loocki",]
p <- list()
p[[1]]  <- ggplot(intero, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE)
p[[2]]  <- ggplot(intero, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
p[[3]]  <- ggplot(intero, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Interochromis_loocki.png",width=20,height=30,units="in",res=500)
do.call(grid.arrange,c(p, ncol = 3))
dev.off()




cteno <- significant_group_order[significant_group_order$P3 == "Ctenochromis_pectoralis",]
p <- list()
p[[1]]  <- ggplot(cteno, aes(y = P1, x = Dstatistic)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE)
p[[2]]  <- ggplot(cteno, aes(y = P1, x = Z.score)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
p[[3]]  <- ggplot(cteno, aes(y = P1, x = f4.ratio)) + geom_jitter(aes(colour = unique_species.clade_SG.y), show.legend = FALSE) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_groupbyLVRS_Ctenochromis_pectoralis.png",width=20,height=30,units="in",res=500)
do.call(grid.arrange,c(p, ncol = 3))
dev.off()





max(ortho$f4.ratio)

unique(significant_comb2_group_order$unique_species.clade_SG.x)

#look at f4
ruahablue <- significant_comb2_group_order[significant_comb2_group_order$P3 == "Astatotilapia_sp-Ruaha-blue",]
head(ruahablue)
max(ruahablue$f4.ratio)
min(ruahablue$f4.ratio)
mean(ruahablue$f4.ratio)

#are all the malawi groups/species/genera represented in the significant results?
#how many species are there from each group?
#head(significant_comb2_group_order)

library(tidyr)
library(dplyr)

significant_comb2_group_order_noruaha_noneg_dataframe <- data.frame(unique(significant_comb2_group_order_noruaha_noneg[c("P3", "P2")]))
significant_comb2_group_order_noruaha_noneg_groups <- data.frame(unique(significant_comb2_group_order_noruaha_noneg[c("P3", "unique_species.clade_SG.y")]))

#head(significant_comb2_group_order_noruaha_noneg2)

#significant_comb2_group_order_noruaha_noneg_dataframe <- data.frame(significant_comb2_group_order_noruaha_noneg2$P3, significant_comb2_group_order_noruaha_noneg2$P2)
head(significant_comb2_group_order_noruaha_noneg_dataframe)

#significant_comb2_group_order_noruaha_noneg_groups <- data.frame(significant_comb2_group_order_noruaha_noneg2$P3, significant_comb2_group_order_noruaha_noneg2$unique_species.clade_SG.y)
#head(significant_comb2_group_order_noruaha_noneg_groups)


table1 <- significant_comb2_group_order_noruaha_noneg_groups %>%
  # count number of rows for each combination of server_id and protocol
  group_by(P3, unique_species.clade_SG.y) %>%
  tally() %>%
  # pivot the protocol names over the columns
  pivot_wider(names_from=unique_species.clade_SG.y, values_from=n) %>%
  # replace NA values in all columns with 0
  mutate()
#table1
table1_2 <- as.data.frame(table1)

table1_2[table1_2 == "1"] <- "yes"
table1_2[is.na(table1_2)] <- "no"
table1_2 


table2 <- significant_comb2_group_order_noruaha_noneg_dataframe %>%
  # count number of rows for each combination of server_id and protocol
  group_by(P3, P2) %>%
  tally() %>%
  # pivot the protocol names over the columns
  pivot_wider(names_from=P2, values_from=n) %>%
  # replace NA values in all columns with 0
  mutate()

#significant_comb2_group_order %>%
#group_by(P1, P2) %>%
#mutate(unique = n()) %>%
#ungroup()



#is there a significant difference in the f4-ratio between different ecomorphological groups/clades (benthic vs pelagic)


significant_comb2_group_order_noruahaLVRScluster <- significant_comb2_group_order[!significant_comb2_group_order$P3 %in% c("Astatotilapia_sp-Ruaha-blue-LVRScluster"),]
significant_comb2_group_order_noruahaLVRScluster_noneg <- significant_comb2_group_order_noruahaLVRScluster[significant_comb2_group_order_noruahaLVRScluster$f4.ratio > 0,] 

#head(significant_comb2_group_order_noruahaLVRScluster_noneg)
#head(significant_comb2_group_order_noruaha_noneg)


P3sigspecies <- unique(significant_comb2_group_order_noruahaLVRScluster_noneg$P3)
#length(P3sigspecies)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_f4ratio_malawigrouped_perP3.png",width=45,height=8,units="in",res=500)
par(mfrow = c(2,10))
for (i in 1:length(P3sigspecies)){
#for (i in 1:1){
    sub <- significant_comb2_group_order_noruahaLVRScluster_noneg[significant_comb2_group_order_noruahaLVRScluster_noneg$P3 == P3sigspecies[i],]
    sub$unique_species.clade_SG.y <- factor(sub$unique_species.clade_SG.y, levels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"))
    boxplot(sub$f4.ratio~sub$unique_species.clade_SG.y, xlab = "", ylab = "", main = P3sigspecies[i], las = 1, xaxt = "n")
    axis(side = 1, labels = FALSE)
    text(x = 1:7,
     labels = c("diplotaxodon", "rhamphochromis", "acalliptera", "mbuna", "shallowbenthic", "deepbenthic", "utaka"),
     xpd = NA,
     y = par("usr")[3],
     srt = 35,
     adj = 1,
     cex = 1.2
    )
}
dev.off()




#kilo <- significant_comb2_group_order_noruaha_noneg[significant_comb2_group_order_noruaha_noneg$P3 == "Haplochromis_sp-kilossana",]

#sort(kilo$f4.ratio, decreasing = TRUE)
#kilosort <- kilo[order(kilo$f4.ratio),]
#kilosort
#kilo2 <- kilo[kilo$P1 == "Haplochromis_sp-kilossana",]

#unique(kilo$P1)
#plot(f4.ratio~P1, data = kilo)

#write.table(kilosort, "/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/subset_dsuite.csv", sep = ",", quote = FALSE, row.names = FALSE)



#repeat again but colour by victoria P1 species 

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablue_colLVRS.png",width=60,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_noneg, aes(x = P3, y = f4.ratio, fill = P1)) +
    geom_jitter(aes(colour = P1), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablue_colLVRS.png",width=60,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_noneg, aes(x = P3, y = Z.score, fill = P1)) +
    geom_jitter(aes(colour = P1), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablue_colLVRS.png",width=60,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_noneg, aes(x = P3, y = Dstatistic, fill = P1)) +
    geom_jitter(aes(colour = P1), alpha = 0.9, show.legend = TRUE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()


#repeat plots but only keep 1 victoria species
#unique(malawisamples_victoria$full_name)
victoria_species <- "Yssichromis_sp-plumbus"

significant_comb2_1vic <- significant_comb2[significant_comb2$P1 %in% victoria_species | significant_comb2$P2 %in% victoria_species,]

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_1vicspecies_", victoria_species, ".png", sep = ""),width=15,height=14,units="in",res=500)
p <- ggplot(significant_comb2_1vic, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) + 
    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
p + scale_color_brewer(palette = "Paired")
dev.off()

dev.copy(png,paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_1vicspecies_", victoria_species, ".png", sep = ""),width=15,height=14,units="in",res=500)
d <- ggplot(significant_comb2_1vic, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)")
dev.off()


victoria_species <- "Pundamilia_pundamilia"
significant_comb2_1vic <- significant_comb2[significant_comb2$P1 %in% victoria_species | significant_comb2$P2 %in% victoria_species,]

test <- significant_comb2_1vic[significant_comb2_1vic$P3 %in% "Serranochromis_sp-checkerboard",]
plot(test$Z.score)

#test2 <- significant_comb2_1vic[significant_comb2_1vic$P3 %in% "Serranochromis_sp-checkerboard",]
#plot(test2$Z.score)

significant_comb$unique_species.clade_SG.x <- factor(significant_comb$unique_species.clade_SG.x, levels = c("gigliolii", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis", "Tanganyika", "ruaha_blue", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni"))


significant_comb2 <- significant_comb[order(significant_comb$unique_species.clade_SG.x),]

species_order <- as.vector(unique(significant_comb2$P3))
#species_order
significant_comb2$P3 <- factor(significant_comb2$P3, levels = species_order)

#p <- ggplot(significant_comb2, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) + 
#    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.x), alpha = 0.9)+
#    #geom_boxplot(alpha = 0.5)+
#    coord_flip() +
#    theme(axis.text.y = element_text(size = 1), axis.text.x = element_text(size = 1),
#         axis.title.y = element_text(size = 1), axis.title.x = element_text(size = 1))
#p + scale_color_brewer(palette = "Paired")

#length(BBAA_keep6[BBAA_keep6$p.value == 0,]$P1)
#BBAA_keep6[order(BBAA_keep6$f4.ratio),]

#d <- ggplot(significant, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG)) +
#    geom_jitter(aes(colour = unique_species.clade_SG), alpha = 0.9)+
#    #geom_boxplot(alpha = 0.5)+
#    coord_flip() +
#    theme(axis.text.y = element_text(size = 1), axis.text.x = element_text(size = 1))
#d + scale_color_brewer(palette = "Paired")

non_malawi_groups_noortho <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_LVRScluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples[malawisamples$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples[malawisamples$clade_SG %in% "Orthochromis",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA[BBAA$P1 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
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
par(mar=c(10,15,1,1))
boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "P3")
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P2, horizontal=TRUE, las=2, ylab = "")

## supplementary figure {result2_p1nonmalawi_p2victoria_p3orthochromis}

par(mar=c(10,15,10,10))
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4-ratio_P1nonmalawi_P2victoria_P3orthochromis.png",width=10,height=8,units="in",res=500)
ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Non-Malawi species") +
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
dev.off()


par(mar=c(10,15,10,10))
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4-ratio_P1victoria_P2ruahablueLVRS_P3ruahablue.png",width=10,height=8,units="in",res=500)
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "")
ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Victoria species") +
  theme_bw()
dev.off()

### how many trios are there the other way around - P1 = victoria, P2 = non-malawi, P3 = Orthochromis
non_malawi_groups_noortho <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_LVRScluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples[malawisamples$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples[malawisamples$clade_SG %in% "Orthochromis",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA[BBAA$P2 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
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


significant

non_malawi_groups_noortho <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_LVRScluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples[malawisamples$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples[malawisamples$clade_SG %in% "Serr_Pharyng_Sarg_Thora",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA[BBAA$P1 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
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


# repeat but P2 = Malawi

non_malawi_groups_noortho <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_LVRScluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples[malawisamples$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples[malawisamples$clade_SG %in% "Orthochromis",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "mbuna" | malawisamples$clade_SG == "deepbenthic" |
                                        malawisamples$clade_SG == "shallowbenthic" | malawisamples$clade_SG == "acalliptera" |
                                        malawisamples$clade_SG == "utaka" | malawisamples$clade_SG == "diplotaxodon" |
                                        malawisamples$clade_SG == "rhamphochromis",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA[BBAA$P1 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
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

head(BBAA_keep5)

#BBAA_keep5 %>% arrange(desc(f4.ratio))
#head(BBAA_keep5 %>% arrange(f4.ratio))

BBAA_keep5_sub <- BBAA_keep5
BBAA_keep5_sub_species <- unique(BBAA_keep5_sub$P1)
BBAA_keep5_sub$P1 <- factor(BBAA_keep5_sub$P1, levels = BBAA_keep5_sub_species)


hist(BBAA_keep5_sub$f4.ratio)
par(mar=c(10,15,1,1))
boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "")




#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "LVRS",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA[BBAA$P1 %in% outgroups_victoria,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% "Astatotilapia_sp-Ruaha-blue-LVRScluster",]

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

#### supplementary figure {result2_P1victoria_P2ruahablueLVRS_P3ruahablue} ####

#hist(BBAA_keep5_sub$f4.ratio)
par(mar=c(10,15,10,10))
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4-ratio_P1victoria_P2ruahablueLVRS_P3ruahablue.png",width=10,height=8,units="in",res=500)
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=2, ylab = "")
ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Victoria species") +
  theme_bw()
dev.off()


#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=1, ylab = "", xlab = "", border = NA, xaxt='n', yaxt = "n", frame = FALSE)
#grid(nx=16, ny=16)
#boxplot(BBAA_keep5_sub$f4.ratio~BBAA_keep5_sub$P1, horizontal=TRUE, las=1, ylab = "", xlab = "F4-ratio")
#title(ylab = "P1 Victoria species", line = 10)


#boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
#        ylab = "Miles Per Gallon", border = NA, 
#        xaxt='n', yaxt = "n", frame = FALSE)
#grid(nx=16, ny=16)
#boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
#        ylab = "Miles Per Gallon", add = TRUE, ann = FALSE)

ggplot(data = BBAA_keep5_sub, aes(x=f4.ratio, y=P1)) +
  geom_boxplot() +
  xlab("F4-ratio") +
  ylab("P1 Victoria species") +
  theme_bw()


BBAA_keep5_sub

# repeat but P2 = Malawi

non_malawi_groups_noortho <- c("Serr_Pharyng_Sarg_Thora", "ruaha_blue_LVRScluster", "ruaha_blue", "Pseudo_Cteno_Ortho2", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni", "Tanganyika", "gigliolii", "Ctenochromis_pectoralis")
malawisamples_serrano_noortho <- malawisamples[malawisamples$clade_SG %in% non_malawi_groups_noortho,]
outgroups_serrano_noortho <- as.vector(unique(malawisamples_serrano_noortho$genus_species))

malawisamples_ortho <- malawisamples[malawisamples$clade_SG %in% "Orthochromis",]
outgroups_ortho <- as.vector(unique(malawisamples_ortho$genus_species))

#get list of P2 species (all LVRS species)
malawisamples_victoria <- malawisamples[malawisamples$clade_SG == "mbuna" | malawisamples$clade_SG == "deepbenthic" |
                                        malawisamples$clade_SG == "shallowbenthic" | malawisamples$clade_SG == "acalliptera" |
                                        malawisamples$clade_SG == "utaka" | malawisamples$clade_SG == "diplotaxodon" |
                                        malawisamples$clade_SG == "rhamphochromis",]
outgroups_victoria <- as.vector(unique(malawisamples_victoria$genus_species))


#keep only non-malawi group in P1
BBAA_keep <- BBAA[BBAA$P1 %in% outgroups_serrano_noortho,]

#keep only victoria group and malawi species in P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P2 %in% outgroups_victoria,]
#BBAA_keep3 <- rbind(BBAA_keep, BBAA_keep2)

#keep only orthochromis as P3
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P3 %in% outgroups_ortho,]
#BBAA_keep5 <- rbind(BBAA_keep3, BBAA_keep4)

BBAA_keep3_noruahalvrs <- BBAA_keep3[!BBAA_keep3$P1 == "Astatotilapia_sp-Ruaha-blue-LVRScluster",]


colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3_noruahalvrs, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
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

head(BBAA_keep5)





#plot percentage of sig trios for all P3 species

total_count <- table(BBAA_keep5$P3)
sig_count <- table(significant$P3)

counts <- data.frame(total_count, sig_count)
counts2 <- counts[-c(3)]

colnames(counts2) <- c("P3", "total_count", "sig_count")
counts2$percentage <- (counts2$sig_count/counts2$total_count)*100

all_samples_group_info <- data.frame(malawisamples$genus_species, malawisamples$clade_SG)
colnames(all_samples_group_info) <- c("P3", "clade")
all_samples_group_info2 <- all_samples_group_info[!duplicated(all_samples_group_info$P3),]
#head(all_samples_group_info2)

counts3 <- merge(counts2, all_samples_group_info2, by = "P3", all.x = TRUE, all.y = FALSE, no.dups = TRUE)
counts4 <- counts3[counts3$clade != "mbuna" & counts3$clade != "deepbenthic" & counts3$clade != "shallowbenthic"
                   & counts3$clade != "utaka" & counts3$clade != "acalliptera" & counts3$clade != "diplotaxodon"
                   & counts3$clade != "rhamphochromis" & counts3$clade != "LVRS",]
#head(counts4)

nonmalawi_group_order <- c("ruaha_blue_LVRScluster", "ruaha_blue", "gigliolii", "other_riverine_haplochromines_burtoni", "other_riverine_haplochromines_vanheusdeni",
                           "Tanganyika", "Serr_Pharyng_Sarg_Thora", "Pseudo_Cteno_Ortho2", "Orthochromis")
counts4_order <- counts4 %>% arrange(factor(clade, levels = nonmalawi_group_order))
head(counts4_order)
#unique(counts4_order$clade)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_LVRSP1_MalawiP2_percent_sig_trios_perP3.png",width=20,height=10,units="in",res=500)
par(mar = c(22,5,1,1))
barplot(counts4_order$percentage, col = counts4_order$clade, names = counts4_order$P3, las = 2)
dev.off()

#counts4_order$clade <- factor(counts4_order$clade, levels = nonmalawi_group_order)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_LVRSP1_MalawiP2_percent_sig_trios_perP3_cladesgrouped.png",width=10,height=10,units="in",res=500)
par(mar = c(22,5,1,1))
boxplot(counts4_order$percentage~counts4_order$clade, las = 2, xlab = "", ylab = "percentage of significant trios")
dev.off()

#Ctenochromis pectoralis has few sig trios - which are the P2 species that are still significant?
cteno <- significant[significant$P3 == "Ctenochromis_pectoralis",]
#head(cteno)
#table(cteno$unique_species.clade_SG.y)
#table(cteno$P1)
#table(cteno$P2)


#ruaha blue also has surprisingly few sig trios - which are the P2 species that are still significant?
ruaha <- significant[significant$P3 == "Astatotilapia_sp-Ruaha-blue",]
head(ruaha)
#table(ruaha$unique_species.clade_SG.y)
#table(ruaha$P1)
#table(ruaha$P2)

ruaha <- BBAA_keep8[BBAA_keep8$P3 == "Astatotilapia_sp-Ruaha-blue" & BBAA_keep8$significant == "TRUE",]
head(ruaha)
#which are the P1 and P2 species?


#which are the P1 and P2 species?

P3_species <- "Ctenochromis_pectoralis"
#P3_species <- "Astatotilapia_sp-Ruaha-blue"
#P3_species <- "Pseudocrenilabrus_multicolor"
#P3_species <- "Interochromis_loocki"
#P3_species <- "Limnotilapia_dardennii"

ruaha <- BBAA_keep8[BBAA_keep8$P3 == P3_species & BBAA_keep8$significant == "TRUE",]
ruaha_all <- BBAA_keep8[BBAA_keep8$P3 == P3_species,]
ruaha_all_P1 <- as.data.frame(table(ruaha_all$P1))

#what is P1?
count_P1 <- as.data.frame(table(ruaha$P1))
count_P1$all_Freq <- ruaha_all_P1$Freq
count_P1$percent_sig <- (count_P1$Freq/count_P1$all_Freq)*100
count_P1_ordered <- count_P1[order(-count_P1$percent_sig),]
count_P1_ordered_clades <- merge(count_P1_ordered, all_samples_group_info, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P1_ordered_clades2 <- count_P1_ordered_clades[!duplicated(count_P1_ordered_clades$Var1),]
count_P1_ordered_clades2$clade <- factor(count_P1_ordered_clades2$clade, levels = c("LVRS"))
#count_P1_ordered_clades2[order(-count_P1_ordered_clades2$percent_sig),]

#what is P2?
count_all_P2 <- as.data.frame(table(ruaha_all$P2))
count_P2 <- as.data.frame(table(ruaha$P2))
count_P2$all_Freq <- count_all_P2$Freq
count_P2$percent_sig <- (count_P2$Freq/count_P2$all_Freq)*100

count_P2_ordered <- count_P2[order(-count_P2$percent_sig),]
head(count_P2_ordered)
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]
count_P2_ordered_clades <- merge(count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P2_ordered_clades2 <- count_P2_ordered_clades[!duplicated(count_P2_ordered_clades$Var1),]

count_P2_ordered_clades2$clade <- factor(count_P2_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis", "mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))
#tail(count_P2_ordered_clades2)


#P2
total_sig <- sum(count_P2_ordered$Freq)
count_P2_ordered_clades3 <- na.omit(count_P2_ordered_clades2)
count_P2_ordered_clades3_mbuna <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "mbuna",]
total_sig_mbuna <- sum(count_P2_ordered_clades3_mbuna$Freq)
count_P2_ordered_clades3_deepbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic <- sum(count_P2_ordered_clades3_deepbenthic$Freq)
count_P2_ordered_clades3_shallowbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic <- sum(count_P2_ordered_clades3_shallowbenthic$Freq)
count_P2_ordered_clades3_acalliptera <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera <- sum(count_P2_ordered_clades3_acalliptera$Freq)
count_P2_ordered_clades3_utaka <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "utaka",]
total_sig_utaka <- sum(count_P2_ordered_clades3_utaka$Freq)
count_P2_ordered_clades3_diplotaxodon <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "diplotaxodon",]
total_sig_diplotaxodon <- sum(count_P2_ordered_clades3_diplotaxodon$Freq)
count_P2_ordered_clades3_rhamphochromis <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "rhamphochromis",]
total_sig_rhamphochromis <- sum(count_P2_ordered_clades3_rhamphochromis$Freq)
portion_mbuna <- (total_sig_mbuna/total_sig)*100
portion_deepbenthic <- (total_sig_deepbenthic/total_sig)*100
portion_shallowbenthic <- (total_sig_shallowbenthic/total_sig)*100
portion_acalliptera <- (total_sig_acalliptera/total_sig)*100
portion_utaka <- (total_sig_utaka/total_sig)*100
portion_diplotaxodon <- (total_sig_diplotaxodon/total_sig)*100
portion_rhamphochromis <- (total_sig_rhamphochromis/total_sig)*100


#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_orthochromis.png",width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.5,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
#boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
text(x = 1:1,
     labels = c('LVRS'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
boundaries2 <- boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_mbuna, portion_deepbenthic, portion_shallowbenthic, portion_acalliptera, portion_utaka),1)
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     y = boundaries2$stats[nrow(boundaries2$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:7,
     labels = c('diplotaxodon', 'rhamphochromis', 'mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#dev.off()


#which are the P1 and P2 species?

ruaha <- BBAA_keep8[BBAA_keep8$P3 == "Astatotilapia_sp-Ruaha-blue" & BBAA_keep8$significant == "TRUE",]
ruaha_all <- BBAA_keep8[BBAA_keep8$P3 == "Astatotilapia_sp-Ruaha-blue",]
ruaha_all_P1 <- as.data.frame(table(ruaha_all$P1))
#length(totaltest_dip$P1)

#what is P1?
count_P1 <- as.data.frame(table(ruaha$P1))
count_P1$all_Freq <- count_all_P1$Freq
count_P1$percent_sig <- (count_P1$Freq/count_P1$all_Freq)*100
count_P1_ordered <- count_P1[order(-count_P1$percent_sig),]
#count_P1_ordered
count_P1_ordered_clades <- merge(count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P1_ordered_clades2 <- count_P1_ordered_clades[!duplicated(count_P1_ordered_clades$Var1),]
count_P1_ordered_clades2$clade <- factor(count_P1_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))
#head(count_P1_ordered_clades2)


#what is P2?
count_all_P2 <- as.data.frame(table(ortho_all$P2))
count_P2 <- as.data.frame(table(ortho$P2))
count_P2$all_Freq <- count_all_P2$Freq
count_P2$percent_sig <- (count_P2$Freq/count_P2$all_Freq)*100

count_P2_ordered <- count_P2[order(-count_P2$percent_sig),]
all_samples_group_info_mal <- all_samples_group_info[all_samples_group_info$clade == "mbuna" | all_samples_group_info$clade == "deepbenthic" | all_samples_group_info$clade == "shallowbenthic" | all_samples_group_info$clade == "utaka" | all_samples_group_info$clade == "rhamphochromis" | all_samples_group_info$clade == "diplotaxodon" | all_samples_group_info$clade == "acalliptera", ]
count_P2_ordered_clades <- merge(count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
count_P2_ordered_clades2 <- count_P2_ordered_clades[!duplicated(count_P2_ordered_clades$Var1),]

count_P2_ordered_clades2$clade <- factor(count_P2_ordered_clades2$clade, levels = c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))


#P2
total_sig <- sum(count_P2_ordered$Freq)
count_P2_ordered_clades3 <- na.omit(count_P2_ordered_clades2)
count_P2_ordered_clades3_mbuna <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "mbuna",]
total_sig_mbuna <- sum(count_P2_ordered_clades3_mbuna$Freq)
count_P2_ordered_clades3_deepbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "deepbenthic",]
total_sig_deepbenthic <- sum(count_P2_ordered_clades3_deepbenthic$Freq)
count_P2_ordered_clades3_shallowbenthic <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "shallowbenthic",]
total_sig_shallowbenthic <- sum(count_P2_ordered_clades3_shallowbenthic$Freq)
count_P2_ordered_clades3_acalliptera <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "acalliptera",]
total_sig_acalliptera <- sum(count_P2_ordered_clades3_acalliptera$Freq)
count_P2_ordered_clades3_utaka <- count_P2_ordered_clades3[count_P2_ordered_clades3$clade == "utaka",]
total_sig_utaka <- sum(count_P2_ordered_clades3_utaka$Freq)
portion_mbuna <- (total_sig_mbuna/total_sig)*100
portion_deepbenthic <- (total_sig_deepbenthic/total_sig)*100
portion_shallowbenthic <- (total_sig_shallowbenthic/total_sig)*100
portion_acalliptera <- (total_sig_acalliptera/total_sig)*100
portion_utaka <- (total_sig_utaka/total_sig)*100


#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_orthochromis.png",width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.8,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
#boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
boundaries <- boxplot(count_P1_ordered_clades2$percent_sig~count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_dip, portion_rhamp),1)
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     y = boundaries$stats[nrow(boundaries$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P1_ordered_clades2$clade),
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
boundaries2 <- boxplot(count_P2_ordered_clades2$percent_sig~count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,105))
axis(side = 1, labels = FALSE)
portion <- round(c(portion_mbuna, portion_deepbenthic, portion_shallowbenthic, portion_acalliptera, portion_utaka),1)
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     y = boundaries2$stats[nrow(boundaries2$stats),] + 5, 
     labels = paste(portion, "%", sep = ""))
text(x = 1:nlevels(count_P2_ordered_clades2$clade),
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
#dev.off()



#conclusion from this (23.08.2022) is that removing A.calliptera as P1 or P2 doesnt make much of a difference, 
#the main patterns of the plot still hold

#head(significant_comb2_group_order_noruaha)
significant_comb2_group_order_noruaha_nocalliptera <- significant_comb2_group_order_noruaha[!(significant_comb2_group_order_noruaha$P2 == "Astatotilapia_calliptera"),]
significant_comb2_group_order_noruaha_nocalliptera2 <- significant_comb2_group_order_noruaha_nocalliptera[!(significant_comb2_group_order_noruaha_nocalliptera$P1 == "Astatotilapia_calliptera"),]
length(significant_comb2_group_order_noruaha$P2)
length(significant_comb2_group_order_noruaha_nocalliptera$P2)
length(significant_comb2_group_order_noruaha_nocalliptera2$P2)


dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablue_nocalliptera.png",width=19,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_nocalliptera, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablue_nocalliptera.png",width=19,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_nocalliptera, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablue_nocalliptera.png",width=19,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruaha_nocalliptera, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruaha) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()


#repeat but keep ruaha blue and remove ruaha blue LVRS, and keep calliptera for now
significant_comb2_group_order_noruahalvrs <- significant_comb2_group_order[!(significant_comb2_group_order$P3 == "Astatotilapia_sp-Ruaha-blue-LVRScluster"),]
#head(significant_comb2_group_order_noruahalvrs)
#unique(significant_comb2_group_order_noruahalvrs$unique_species.clade_SG.x)


dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()


#repeat with A.calliptera removed

significant_comb2_group_order_noruahalvrs_nocalliptera <- significant_comb2_group_order_noruahalvrs[!(significant_comb2_group_order_noruahalvrs$P2 == "Astatotilapia_calliptera" | significant_comb2_group_order_noruahalvrs$P1 == "Astatotilapia_calliptera"),]

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS_nocalliptera.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs_nocalliptera, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS_nocalliptera.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs_nocalliptera, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS_nocalliptera.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs_nocalliptera, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.x)) +
    geom_jitter(aes(colour = unique_species.clade_SG.x), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()


#colour plots according to the Malawi ecomorph group

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_f4ratio_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS_colourMalawi.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs, aes(x = P3, y = f4.ratio, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("F4-ratio (ancestry proportion)") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS_colourMalawi.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs, aes(x = P3, y = Z.score, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("Z-score") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_dstat_LVRSP1_MalawiP2_plusreverse_noruahablueLVRS_colourMalawi.png",width=14,height=14,units="in",res=500)
d <- ggplot(significant_comb2_group_order_noruahalvrs, aes(x = P3, y = Dstatistic, fill = unique_species.clade_SG.y)) +
    geom_jitter(aes(colour = unique_species.clade_SG.y), alpha = 0.9, show.legend = FALSE)+
    #geom_boxplot(alpha = 0.5)+
    coord_flip() +
    theme(axis.text.y = element_text(size = 23, face = 'bold'), axis.text.x = element_text(size = 25, face = 'bold'),
         axis.title.y = element_text(size = 25, face = 'bold'), axis.title.x = element_text(size = 25, face = 'bold'))
d + scale_color_brewer(palette = "Paired") + ylab("D-statistic") + xlab("P3 Non-Malawi Group")+ scale_x_discrete(labels = xlabels_nonmalawi_rev_no_ruahaLVRS) +
theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray88"),
        panel.grid.minor = element_line(color = "gray88"), plot.margin = margin(0,1,0,0, "cm"))
dev.off()


#read in all trios (no filtering) to explore the most significant trios

#variables:
pvalue <- 5e-2

#malawisamples <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/files/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_regionlabels.tsv", sep = "\t", header = T) #this data frame doesnt include the new species names so can't be used
#new metadata with new region assignments
malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades.tsv", sep = "\t", header = T)
malawisamples <- malawisamples_regions[-c(612),]
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

BBAA_keep3 <- BBAA

#reorder for easier reading
BBAA_keep4 <- BBAA_keep3[order(BBAA_keep3$P2),]
BBAA_keep5 <- BBAA_keep4[order(BBAA_keep4$P3),]

BBAA_keep6 <- merge(BBAA_keep5, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)

#which tests are significant?
significant <- BBAA_keep6[BBAA_keep6$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of significant tests:")
length(significant$P1)
print("total number of trios:")
length(BBAA_keep6$P1)
percentsig <- length(significant$P1)/length(BBAA_keep6$P1) *100
print("percentage of significant tests:")
percentsig

#label significant comparisons
BBAA_keep6 <- BBAA_keep6 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

#significant[order(significant$p.value, decreasing = FALSE),]

significant[order(significant$f4.ratio, decreasing = TRUE),]

malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-01-07_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_hannesdec2021changes_rmexludesamples_rmSRR12700905_rmSRR12700906_newclades.tsv", sep = "\t", header = T)
malawisamples <- malawisamples_regions[-c(612),]
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

#get list of P3 species (all non malawi species)
malawisamples_notmalawi <- malawisamples[!malawisamples$region == "Malawi",]
malawisamples_malawi <- malawisamples[malawisamples$region == "Malawi",]

length(unique(malawisamples_notmalawi$genus_species))
length(unique(malawisamples_malawi$genus_species))

#rerun dsuite but with each ruaha blue sample set as a different species/P3
#this is to test whether different ruaha blue populations have different signals of excess allele sharing

setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/dtools/dtrios_cluster/output/sophie_malawi_nonmalawi_ruahablue_april2023")
BBAA <- read.table("sophie_malawi_nonmalawi_ruahablue_april2023_BBAA.txt", sep = "\t", header = TRUE)
length(BBAA[,1])

#need to add in the negatives stats as well here (if P1 = Malawi, P2 = victoria)?

#variables:
pvalue <- 5e-2

malawisamples_regions <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/metadata/cichlid_callset_mt_2022-24-08_callset202103_malawi2perspecies_newalignments.tsv", sep = "\t", header = T)
malawisamples <- malawisamples_regions[-c(612),]
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

unique_species <- malawisamples_regions[!duplicated(malawisamples_regions$full_name),]
groupings <- data.frame(unique_species$full_name, unique_species$clade_SG)
colnames(groupings)[1] <- "P3"

#define P1, P2 and P3 species
P3_nonmalawi <- c("Astatotilapia_gigliolii", "Astatotilapia_sp-Ruaha-blue1", "Astatotilapia_sp-Ruaha-blue2", "Astatotilapia_sp-Ruaha-blue3", "Astatotilapia_sp-Ruaha-blue4", "Astatotilapia_sp-Ruaha-blue5", "Astatotilapia_sp-Ruaha-blue6", "Astatotilapia_sp-Ruaha-blue7", "Astatotilapia_sp-Ruaha-blue8", "Astatotilapia_sp-Ruaha-blue9", "Astatotilapia_sp-Ruaha-blue10", "Astatotilapia_sp-Ruaha-blue11", "Astatotilapia_sp-Ruaha-blue12", "Astatotilapia_sp-Ruaha-blue13", "Astatotilapia_sp-Ruaha-blue14", "Astatotilapia_sp-Ruaha-blue15", "Astatotilapia_sp-Ruaha-blue16")
P2_malawi <- c("Rhamphochromis_woodi", "Alticorpus_peterdaviesi", "Mylochromis_subocularis", "Diplotaxodon_limnothrissa", "Copadichromis_chrysonotus", "Labeotropheus_fuelleborni", "Astatotilapia_calliptera")
P1_victoria <- c("Astatotilapia_sp-Ruaha-yellow", "Astatotilapia_flaviijosephi", "Pundamilia_nyererei", "Haplochromis_gracilior", "Thoracochromis_pharyngalis")

#subset P3
BBAA_keep <- BBAA[BBAA$P3 %in% P3_nonmalawi,]

#subset P1
BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% P1_victoria,]

#subset P2
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% P2_malawi,]

#add P2 and P3 group name
colnames(groupings)[1] <- "P3"
BBAA_keep4 <- merge(BBAA_keep3, groupings, by = "P3", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P2"
BBAA_keep4_1 <- merge(BBAA_keep4, groupings, by = "P2", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(groupings)[1] <- "P1"
BBAA_keep5 <- merge(BBAA_keep4_1, groupings, by = "P1", all.x = TRUE, all.y = FALSE, sort = FALSE)
colnames(BBAA_keep5)[11] <- "Clade_name_P3"
colnames(BBAA_keep5)[12] <- "Clade_name_P2"
colnames(BBAA_keep5)[13] <- "Clade_name_P1"

l <- length(BBAA_keep5$P1)

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
print("number of significant tests:")
length(significant$P1)
print("total number of trios:")
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
print("percentage of significant tests:")
percentsig

#label significant comparisons
BBAA_keep5 <- BBAA_keep5 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))

head(BBAA_keep5)

BBAA %>% arrange(desc(f4.ratio))

#BBAA_keep[BBAA_keep$P2 == "Astatotilapia_sp-Ruaha-blue2",]
BBAA2 <- BBAA[BBAA$P2 == "Astatotilapia_sp-Ruaha-blue11" | BBAA$P1 == "Astatotilapia_sp-Ruaha-blue11" | BBAA$P3 == "Astatotilapia_sp-Ruaha-blue11",]
BBAA3 <- BBAA2[BBAA2$P3 == "Astatotilapia_sp-Ruaha-blue3" & BBAA2$P1 == "Astatotilapia_sp-Ruaha-yellow",]
BBAA3 %>% arrange(desc(f4.ratio))



BBAA2 <- BBAA[BBAA$P2 == "Astatotilapia_sp-Ruaha-blue6",]
BBAA3 <- BBAA2[BBAA2$P1 == "Astatotilapia_gigliolii" & BBAA2$P3 == "Astatotilapia_sp-Ruaha-yellow",]
BBAA3 %>% arrange(desc(f4.ratio))

P1_victoria

#for each ruaha blue sample plot stats for each P1 species
P1 <- "Astatotilapia_flaviijosephi"
significant1 <- significant[!significant$P3 == "Astatotilapia_gigliolii",]
significant2 <- significant1[significant1$P1 == P1,]

#significant2 <- significant[significant$P1 == P1,]
ggplot(significant2, aes(y = P3, x = f4.ratio)) +
geom_jitter(aes(colour = Clade_name_P2))

#ruaha blue 1 and 11 are the LVRS cluster samples

#for each ruaha blue sample plot stats for each P1 species
P2 <- "Rhamphochromis_woodi"
significant1 <- significant[!significant$P3 == "Astatotilapia_gigliolii",]
significant2 <- significant1[significant1$P2 == P2,]

#significant2 <- significant[significant$P1 == P1,]
ggplot(significant2, aes(y = P3, x = f4.ratio)) +
geom_jitter(aes(colour = P1))

#ruaha blue 1 and 11 are the LVRS cluster samples

#for each ruaha blue sample plot stats for each P1 species
P3 <- "Rhamphochromis_woodi"
significant1 <- significant[!significant$P3 == "Astatotilapia_gigliolii",]
significant2 <- significant1[significant1$P3 == P3,]

#significant2 <- significant[significant$P1 == P1,]
ggplot(significant2, aes(y = P1, x = f4.ratio)) +
geom_jitter(aes(colour = P2))




#ortho <- significant[significant$P3 == "Orthochromis_uvinzae",]
#dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_zscore_LVRSP1_MalawiP2_OuvinzaeP3.png",width=20,height=50,units="in",res=500)
#p <- ggplot(ortho, aes(x = P2, y = Z.score, fill = unique_species.clade_SG.y)) + 
#    geom_jitter(position = position_jitter(width = 0.3, height = 0.2), aes(colour = unique_species.clade_SG.y), alpha = 0.9)+
#    #geom_boxplot(alpha = 0.5)+
#    coord_flip() +
#    theme(axis.text.y = element_text(size = 17), axis.text.x = element_text(size = 20),
#         axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))
#p + scale_color_brewer(palette = "Paired")
#dev.off()





#run with larger test region and with all species comparisons
setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/inputfiles/test_dtriosparallel")
BBAA <- read.table("out_sim_wholegenome_dtrios_parallel_chr1_test_200000lines_combined_BBAA.txt", header = TRUE)

#variables:
pvalue <- 5e-2

malawisamples <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/files/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_regionlabels.tsv", sep = "\t", header = T)
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")

#get list of P3 species (all non malawi species)
malawisamples_notmalawi <- malawisamples[!malawisamples$region == "Malawi",]
outgroups <- as.vector(unique(malawisamples_notmalawi$genus_species))

#or use the test outgroups:
#outgroups <- c("Pharyngochromis_acuticeps", "Serranochromis_angusticeps", "Serranochromis_macrocephalus", "Serranochromis_robustus")

#keep only outgroups in P3
BBAA_keep <- BBAA[BBAA$P3 %in% outgroups,]
l <- length(BBAA_keep$P1)

#only keep malawi species in P1 and P2
malawisamples_onlymalawi <- malawisamples[malawisamples$region == "Malawi",]
BBAA_keep2 <- BBAA_keep[BBAA_keep$P1 %in% malawisamples_onlymalawi$genus_species,]
BBAA_keep3 <- BBAA_keep2[BBAA_keep2$P2 %in% malawisamples_onlymalawi$genus_species,]

#reorder for easier reading
BBAA_keep4 <- BBAA_keep3[order(BBAA_keep3$P2),]
BBAA_keep5 <- BBAA_keep4[order(BBAA_keep4$P3),]

#which tests are significant?
significant <- BBAA_keep5[BBAA_keep5$p.value <= pvalue/l,] #divide p by the number of tests being made (bonferroni correction) to account for the number of tests
length(significant$P1)
length(BBAA_keep5$P1)
percentsig <- length(significant$P1)/length(BBAA_keep5$P1) *100
percentsig

#label significant comparisons
BBAA_keep5 <- BBAA_keep5 %>% mutate(significant=ifelse(p.value <= pvalue/l,T,F))


#investigate which trios are significant
significant[!(significant$P3 == "Astatotilapia_calliptera") & (significant$P1 == "Astatotilapia_calliptera") & !(significant$P2 == "Astatotilapia_calliptera"),]
#all significant tests involve A.calliptera


#plot barplot of results per P3 outgroup
#transform p values into -1og10 p values
BBAA_keep5$minuslog10p <- -log10(BBAA_keep5$p.value)
library(ggplot2)

#pdf("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_minuslog10p_allP3.pdf",  width = 10, height = 10)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_minuslog10p_allP3.png",width=20,height=20,units="in",res=500)
ggplot(BBAA_keep5, aes(x = P3, y = minuslog10p)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip()
dev.off()

#plot barplot of results per P3 outgroup
#this time with f4 ratio
#pdf("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_f4ratio_allP3.pdf",  width = 10, height = 10)
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_f4ratio_allP3.png",width=20,height=20,units="in",res=500)
ggplot(BBAA_keep5, aes(x = P3, y = f4.ratio)) +
    geom_boxplot()+ 
    geom_point(aes(color = significant)) +
    coord_flip()
dev.off()


#which species are involved in the significant tests?
#count the number of times a species comes up in a significant test and make a barplot for each species

#head(significant)
P1 <- significant %>% count(P1)
P2 <- significant %>% count(P2)
P3 <- significant %>% count(P3)
colnames(P1)[1] <- "genus_species"
colnames(P2)[1] <- "genus_species"

malawi <- data.frame(unique(malawisamples_onlymalawi$genus_species))
colnames(malawi)[1] <- "genus_species" 

P1_2 <- merge(malawi, P1, by = "genus_species", all.x = TRUE)
P2_2 <- merge(malawi, P2, by = "genus_species", all.x = TRUE)
P1_2$P2_n <- P2_2$n
colnames(P1_2)[2] <- "P1_n"
P1_2[is.na(P1_2)] <- 0
P1_2$P1plusP2 <- P1_2$P1_n + P1_2$P2_n
#head(P1_2)
#length(malawi$genus_species)
#length(P1_2$genus_species) #these 2 should be the same

#head(P1)
#head(P2)

#plot barplot for each species
dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/testplots/dtrios_parallel_chr1_test_200000lines_numberof_sig_P1P2tests_perspecies.png",width=20,height=60,units="in",res=500)
par(mar = c(10,22,2,2))
barplot(P1_2$P1plusP2, xlim = c(0,10000), horiz = TRUE, names.arg = P1_2$genus_species, las = 1 )
abline(v = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000), col = adjustcolor("blue", alpha = 0.1), lwd = 2)
dev.off()

#do similar thing but for P3 tests - which nonmalawi species are involved here?








#setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/inputfiles")
setwd("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios/_data/results/test")

BBAA <- read.table("wholegenome_sets_test_sim_wholegenome_dtrios_chr18_test_wholechr18_smallsets_BBAA.txt", header = TRUE)
#BBAA <- read.table("out_sim_wholegenome_dtrios_parallel_chr1_test_10000lines_combined_BBAA.txt", header = TRUE)

outgroups <- c("Pharyngochromis_acuticeps", "Serranochromis_angusticeps", "Serranochromis_macrocephalus", "Serranochromis_robustus")

#keep only outgroups in P3
BBAA_keep <- BBAA[BBAA$P3 %in% outgroups,]
length(BBAA_keep$P1)

#keep only malawi in P1 and P2
#subsetting nonmalawis just from P1 also removes from P2 - why is this?
BBAA_keep2 <- BBAA_keep[!BBAA_keep$P1 %in% outgroups,]

BBAA_keep3 <- BBAA_keep2[order(BBAA_keep2$P2),]
BBAA_keep4 <- BBAA_keep3[order(BBAA_keep3$P3),]
#head(BBAA_keep4)

#which tests are significant?
BBAA_keep4[BBAA_keep4$p.value <= 5e-2,]

#plot barplot of results per P3 outgroup
#transform p values into -1og10 p values
BBAA_keep4$minuslog10p <- -log10(BBAA_keep4$p.value)
#head(BBAA_keep4)

library(ggplot2)
ggplot(BBAA_keep4, aes(x = P3, y = minuslog10p)) + geom_boxplot() + geom_point()







###########
#incorrect plot - this one still had non-malawi species in P1 and P2
#########


outgroups <- c("Pharyngochromis_acuticeps", "Serranochromis_angusticeps", "Serranochromis_macrocephalus", "Serranochromis_robustus")

#keep only outgroups in P3
BBAA_keep <- BBAA[BBAA$P3 %in% outgroups,]
length(BBAA_keep$P1)

#keep only malawi in P1 and P2
#subsetting nonmalawis just from P1 also removes from P2 - why is this?
BBAA_keep2 <- BBAA_keep[!BBAA_keep$P1 %in% outgroups,]
BBAA_keep2_2 <- BBAA_keep2[!BBAA_keep$P2 %in% outgroups,]

BBAA_keep3 <- BBAA_keep2_2[order(BBAA_keep2_2$P2),]
BBAA_keep4 <- BBAA_keep3[order(BBAA_keep3$P3),]
#head(BBAA_keep4)

#only keep malawi species in P1 and P2
malawisamples <- read.table("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/files/metadata/cichlid_callset_mt_2021-03-16_callset202103_malawi2perspecies_newalignments_rmCICHM16429765_rmILBCDS5438980_CALO_anc_samp_regionlabels.tsv", sep = "\t", header = T)
malawisamples$genus_species <- paste(malawisamples$genus, malawisamples$species, sep = "_")
BBAA_keep5 <- BBAA_keep4[BBAA_keep4$P1 %in% malawisamples$genus_species,]
BBAA_keep6 <- BBAA_keep5[BBAA_keep4$P2 %in% malawisamples$genus_species,]

#which tests are significant?
BBAA_keep6_noNA <- BBAA_keep6[complete.cases(BBAA_keep6), ]
significant <- BBAA_keep6_noNA[BBAA_keep6_noNA$p.value <= 5e-2,]

#plot barplot of results per P3 outgroup
#transform p values into -1og10 p values
BBAA_keep6_noNA$minuslog10p <- -log10(BBAA_keep6_noNA$p.value)

library(ggplot2)
ggplot(BBAA_keep6_noNA, aes(x = P3, y = minuslog10p)) + geom_boxplot() + geom_point()



#################

#pb/bp Pseudo group
pseudo <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == "Pseudo_Cteno_Ortho2" & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
pseudo_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == "Pseudo_Cteno_Ortho2" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
pseudo_count_all_P1 <- as.data.frame(table(pseudo_all$P1))

#what is P1?
pseudo_count_P1 <- as.data.frame(table(pseudo$P1))
pseudo_count_P1$all_Freq <- pseudo_count_all_P1$Freq
pseudo_count_P1$percent_sig <- (pseudo_count_P1$Freq/pseudo_count_P1$all_Freq)*100
pseudo_count_P1_ordered <- pseudo_count_P1[order(-pseudo_count_P1$percent_sig),]
#pseudo_count_P1_ordered
pseudo_count_P1_ordered_clades <- merge(pseudo_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
pseudo_count_P1_ordered_clades2 <- pseudo_count_P1_ordered_clades[!duplicated(pseudo_count_P1_ordered_clades$Var1),]
pseudo_count_P1_ordered_clades2$clade <- factor(pseudo_count_P1_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))

#only diplotaxodon

pseudo_count_all_P2 <- as.data.frame(table(pseudo_all$P2))
pseudo_count_P2 <- as.data.frame(table(pseudo$P2))
pseudo_count_P2$all_Freq <- pseudo_count_all_P2$Freq
pseudo_count_P2$percent_sig <- (pseudo_count_P2$Freq/pseudo_count_P2$all_Freq)*100

pseudo_count_P2_ordered <- pseudo_count_P2[order(-pseudo_count_P2$percent_sig),]
pseudo_count_P2_ordered_clades <- merge(pseudo_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
pseudo_count_P2_ordered_clades2 <- pseudo_count_P2_ordered_clades[!duplicated(pseudo_count_P2_ordered_clades$Var1),]

pseudo_count_P2_ordered_clades2$clade <- factor(pseudo_count_P2_ordered_clades2$clade, levels = c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))
#boxplot(pseudo_count_P2_ordered_clades2$percent_sig~pseudo_count_P2_ordered_clades2$clade, las = 1)


dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_pseudogroup.png",width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.8,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
boxplot(pseudo_count_P1_ordered_clades2$percent_sig~pseudo_count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
axis(side = 1, labels = FALSE)
text(x = 1:2,
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
boxplot(pseudo_count_P2_ordered_clades2$percent_sig~pseudo_count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
axis(side = 1, labels = FALSE)
text(x = 1:5,
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()

#################

#pb/bp Tanganyika

tan <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == "Tanganyika" & BBAA_keep5$significant == "TRUE" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
tan_all <- BBAA_keep5[BBAA_keep5$Clade_name_P3 == "Tanganyika" & BBAA_keep5$group_P1 == "pelagic" & BBAA_keep5$group_P2 == "benthic",]
tan_count_all_P1 <- as.data.frame(table(tan_all$P1))

#what is P1?
tan_count_P1 <- as.data.frame(table(tan$P1))
tan_count_P1$all_Freq <- tan_count_all_P1$Freq
tan_count_P1$percent_sig <- (tan_count_P1$Freq/tan_count_P1$all_Freq)*100
tan_count_P1_ordered <- tan_count_P1[order(-tan_count_P1$percent_sig),]
#tan_count_P1_ordered
tan_count_P1_ordered_clades <- merge(tan_count_P1_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
tan_count_P1_ordered_clades2 <- tan_count_P1_ordered_clades[!duplicated(tan_count_P1_ordered_clades$Var1),]
tan_count_P1_ordered_clades2$clade <- factor(tan_count_P1_ordered_clades2$clade, levels = c("diplotaxodon", "rhamphochromis"))

#mainly diplotaxodon

tan_count_all_P2 <- as.data.frame(table(tan_all$P2))
tan_count_P2 <- as.data.frame(table(tan$P2))
tan_count_P2$all_Freq <- tan_count_all_P2$Freq
tan_count_P2$percent_sig <- (tan_count_P2$Freq/tan_count_P2$all_Freq)*100

tan_count_P2_ordered <- tan_count_P2[order(-tan_count_P2$percent_sig),]
tan_count_P2_ordered_clades <- merge(tan_count_P2_ordered, all_samples_group_info_mal, by.x = "Var1", by.y = "P3", all.y = FALSE, all.x = TRUE)
tan_count_P2_ordered_clades2 <- tan_count_P2_ordered_clades[!duplicated(tan_count_P2_ordered_clades$Var1),]

tan_count_P2_ordered_clades2$clade <- factor(tan_count_P2_ordered_clades2$clade, levels = c("mbuna", "deepbenthic", "shallowbenthic", "acalliptera", "utaka"))
#boxplot(tan_count_P2_ordered_clades2$percent_sig~tan_count_P2_ordered_clades2$clade, las = 1)

dev.copy(png,"/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dtrios_nov2022/_data/results/plots/dtrios_parallel_whole_genome_sig_vs_total_comparison_high_bppb_tanganyika.png",width=10,height=7,units="in",res=500)
layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.8,2))
par(mar=c(0.5, 0.5, 0.2, 0.2),
     oma = c(10, 4, 0.2, 0.2))
boxplot(tan_count_P1_ordered_clades2$percent_sig~tan_count_P1_ordered_clades2$clade, las = 1, xaxt = "n", xlab = "", ylim = c(0,100))
axis(side = 1, labels = FALSE)
text(x = 1:2,
     labels = c('diplotaxodon', 'rhamphochromis'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
boxplot(tan_count_P2_ordered_clades2$percent_sig~tan_count_P2_ordered_clades2$clade, las = 1, ylab = "", yaxt = "n", xaxt = "n", xlab = "", ylim = c(0,100))
axis(side = 1, labels = FALSE)
text(x = 1:5,
     labels = c('mbuna', 'deepbenthic', 'shallowbenthic', 'acalliptera', 'utaka'),
     xpd = NA,
     y = par("usr")[3] - 1.5,
     srt = 35,
     adj = 1,
     cex = 1.2
    )
dev.off()

