#07.10.2022
#plot dinvestigate results
#updated june 2023

#the following trios were run:
#LVRS    malawi  Serr_Pharyng_Sarg_Thora Outgroup
#LVRS    malawi  Orthochromis Outgroup
#LVRS    malawi  gigliolii Outgroup
#LVRS    malawi  ruaha_blue Outgroup
#LVRS    malawi  Pseudocrenilabrus group Outgroup
#

#run trios on two different window sizes:  100 SNPs and 1000 SNPs

#inversions removed:
#chr2    9800000 32540000
#chr9    12000000 28900000
#chr10   11300000 29320000
#chr11   6789301 28721782
#chr13   9780000 29600000
#chr20   2157743 4792622

#output stats:
#D = can give overinflated values in small regions when Ne is low, also inaccurate in regions of low recombination and divergence
#f_d = modified version of D more suitable for small windows thus better at identifying introgressed loci (Martin et al. 2015 MBE)
    #accuracy depends on the timing of gene flow
#f_dM = similar to fd, under the null hypothesis of 0 introgression is symmetrically distributed around 0 (Malinsky et al. 2015).
    #gives positive values for introgression between P3 and P2 and negative vlaues for between P3 and P1. 
    #Simon Martin claims it underestimates the admixture proportion.
#d_f = also accurate in small regions and quantifies the fraction of introgression (Pfeifer & Kapan, 2019)
    #less sensistive to variation in the time of gene flow than fd


library(RColorBrewer)
library(dplyr)
library(tidyr)
library(scales)
library(cowplot)
library(ggplot2)
library(bedr)
library(reshape2)

#move plots and tables to local comp:
#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dinvestigate/plots_june2023



chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

lineweight <- 5

dinvestigate_plot_line <- function(output_folder, window, window_short){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    
    
for (p in 1:length(chroms)){
    
    chr <- chroms[p]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)
    
    if (output_folder == "P1_LVRS"){
        cols <- c("blue", "red", "darkgreen", "darkorange", "purple")
        legend_text <- c("gigliolii", "orthochromis", "ruaha_blue", "Pseudo_Cteno_Ortho2", "Serr_Pharyng_Sarg_Thora")
    } else if (output_folder == "P1_ruahablue"){
        cols <- c("lightblue", "pink", "green", "blue", "red", "darkgreen")
        legend_text <- c("P1 ruaha blue LVRS/P3 gigliolii", "P1 ruaha blue LVRS/P3 orthochromis", "P1 ruaha blue LVRS/P3 Serr_Pharyng_Sarg_Thora", "P1 ruaha blue/P3 gigliolii", "P1 ruaha blue/P3 orthochromis", "P1 ruaha blue/P3 Serr_Pharyng_Sarg_Thora")
    }
    
    pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_",chr,"_line.pdf", sep = ""),width=500,height=130)
    par(mfrow=c(4,1))
    #par(mar=c(4,4,0,0))
    
    #Dstat
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {    
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$D,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("D (ABBA-BABA)", side=2, line=15, cex=15)
      }else {
            lines(data$windowStart/1000000, data$D,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        abline(h=0)
        #legend(1, 1, legend=c("gigliolii", "orthochromis", "ruaha_blue", "ruaha_blue_LVRScluster", "Serr_Pharyng_Sarg_Thora"),
        #   col=cols, lty=1, cex=10, lwd = 30)
    }

    #Fd
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {    
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$f_d,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("fd", side=2, line=15, cex=15)
      }else {
            lines(data$windowStart/1000000, data$f_d,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        abline(h=0)
        abline(h=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), col = "gray89")
        legend(1, 1, legend=legend_text,
           col=cols, lty=1, cex=10, lwd = 30)
    }

    #Fdm
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {   
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$f_dM,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("fdm", side=2, line=15, cex=15)
      }else {
            lines(data$windowStart/1000000, data$f_dM,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        abline(h=0)
        abline(h=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), col = "gray89")
        #legend(1, 1, legend=c("gigliolii", "orthochromis", "ruaha_blue", "ruaha_blue_LVRScluster", "Serr_Pharyng_Sarg_Thora"),
        #   col=cols, lty=1, cex=10, lwd = 30)
    }
    
    #df
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {   
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$d_f,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("df", side=2, line=15, cex=15)
      }else {
            lines(data$windowStart/1000000, data$d_f,type="l",xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        abline(h=0)
        abline(h=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), col = "gray89")
        #legend(1, 1, legend=c("gigliolii", "orthochromis", "ruaha_blue", "ruaha_blue_LVRScluster", "Serr_Pharyng_Sarg_Thora"),
        #   col=cols, lty=1, cex=10, lwd = 30)
    }
    dev.off()
}

}    


#repeat above but with no lines connecting points

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

lineweight <- 5

dinvestigate_plot_point <- function(output_folder, window, window_short, width){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))

for (p in 1:length(chroms)){
#for (p in 1:1){
    
    chr <- chroms[p]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)

    if (output_folder == "P1_LVRS"){
        cols <- c("blue", "red", "darkgreen", "darkorange", "purple")
        legend_text <- c("gigliolii", "orthochromis", "ruaha_blue", "Pseudo_Cteno_Ortho2", "Serr_Pharyng_Sarg_Thora")
    } else if (output_folder == "P1_ruahablue"){
        cols <- c("lightblue", "pink", "green", "blue", "red", "darkgreen")
        legend_text <- c("P1 ruaha blue LVRS/P3 gigliolii", "P1 ruaha blue LVRS/P3 orthochromis", "P1 ruaha blue LVRS/P3 Serr_Pharyng_Sarg_Thora", "P1 ruaha blue/P3 gigliolii", "P1 ruaha blue/P3 orthochromis", "P1 ruaha blue/P3 Serr_Pharyng_Sarg_Thora")
    }

    pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_",chr,"_point.pdf", sep = ""),width=width,height=130)
    par(mfrow=c(4,1))
    #par(mar=c(4,4,0,0))
    
    #Dstat
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {    
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$D,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("D (ABBA-BABA)", side=2, line=15, cex=15)
        abline(h=0)
      }else {
            lines(data$windowStart/1000000, data$D,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        #legend(1, 1, legend=c("gigliolii", "orthochromis", "ruaha_blue", "ruaha_blue_LVRScluster", "Serr_Pharyng_Sarg_Thora"),
        #   col=cols, lty=1, cex=10, lwd = 30)
    }

    #Fd
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {    
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$f_d,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("fd", side=2, line=15, cex=15)
        abline(h=0)
        abline(h=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), col = "gray89")
      }else {
            lines(data$windowStart/1000000, data$f_d,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        legend(1, 1, legend=legend_text,
           col=cols, lty=1, cex=10, lwd = 30)
    }

    #Fdm
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {   
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$f_dM,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("fdm", side=2, line=15, cex=15)
        abline(h=0)
        abline(h=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), col = "gray89")
      }else {
            lines(data$windowStart/1000000, data$f_dM,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        #legend(1, 1, legend=c("gigliolii", "orthochromis", "ruaha_blue", "ruaha_blue_LVRScluster", "Serr_Pharyng_Sarg_Thora"),
        #   col=cols, lty=1, cex=10, lwd = 30)
    }
    
    #df
    for (i in 1:l) {
      data <- read.table(filenames[i],as.is=T,header=T)
      if(i==1) {   
        par(mar=c(10,40,5,5))
        plot(data$windowStart/1000000, data$d_f,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          axis(2,cex.axis=10)
          mtext("df", side=2, line=15, cex=15)
        abline(h=0)
        abline(h=c(-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), col = "gray89")
      }else {
            lines(data$windowStart/1000000, data$d_f,type="p", pch = 16, cex = 7, xlab="",ylab="", col = cols[i], lwd=lineweight, ylim = c(-1,1), xaxs="i", yaxs="i", xlim = c(0, max(data$windowStart/1000000)))
          #axis(2,cex.axis=1.2)
      }
        #legend(1, 1, legend=c("gigliolii", "orthochromis", "ruaha_blue", "ruaha_blue_LVRScluster", "Serr_Pharyng_Sarg_Thora"),
        #   col=cols, lty=1, cex=10, lwd = 30)
    }
    dev.off()
}

}


#run dinvestigate plot - with lines connecting points

dinvestigate_plot_line(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100")
dinvestigate_plot_line(output_folder = "P1_LVRS", window = "1000snpwindow_1000_100", window_short = "1000_1000")
dinvestigate_plot_line(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50")

#dinvestigate_plot_line(output_folder = "P1_ruahablue", window = "100snpwindow_100_50", window_short = "100_50")
#dinvestigate_plot_line(output_folder = "P1_ruahablue", window = "50snpwindow_50_25", window_short = "50_25")

#run dinvestigate plot - no lines

dinvestigate_plot_point(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", width = 800)
dinvestigate_plot_point(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", width = 500)
dinvestigate_plot_point(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", width = 1000)

#dinvestigate_plot_point(output_folder = "P1_ruahablue", window = "100snpwindow_100_50", window_short = "100_50", width = 800)
#dinvestigate_plot_point(output_folder = "P1_ruahablue", window = "50snpwindow_50_25", window_short = "50_25", width = 1000)

#mark which windows are in the 95th percentile

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

lineweight <- 5

library(gridExtra)


#output_folder = "P1_LVRS"
#window = "100snpwindow_100_50"
#window_short = "100_50"

dinvestigate_plot_sigd <- function(output_folder, window, window_short){
    

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

#pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigd.pdf", sep = ""),width=1100,height=50)
pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigfdm.pdf", sep = ""),width=1100,height=50)
q <- list()
#par(mfrow=c(22,1))

for (p in 1:length(chroms)){
#for (p in 1:2){
    
    chr <- chroms[p]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)
        
    data <- read.table(filenames[1],as.is=T,header=T)
    
    #extract species name
    sub1 <- gsub('_localFstats_dinvestigate_chr1_hybridswarm_[0-9]+snpwindow_[0-9]+_[0-9]+.txt', '', filenames[1])
    sub2 <- gsub('./LVRS_malawi_', '', sub1)
        
    #quant95 <- quantile(data$D, 0.95)
    #quant05 <- quantile(data$D, 0.05)
    quant95 <- quantile(data$f_dM, 0.95)
    quant05 <- quantile(data$f_dM, 0.05)
      #subset <- data[(data$D >= quant95 | data$D <= quant05),]
    data$loc <- paste(data$chr, data$windowStart, data$windowEnd, sep = "_")
    #data$sig <- (data$D >= quant95 | data$D <= quant05)
    #data$sig <- (data$D >= quant95)
    data$sig <- (data$f_dM >= quant95)
    data$sig <- as.numeric(data$sig)
    subsetdata <- data[,c(1,2,3,8,9)]
    colnames(subsetdata)[5] <- sub2
    
    #Dstat sig windows
    for (i in 2:l) {
        data <- read.table(filenames[i],as.is=T,header=T)
        sub1 <- gsub('_localFstats_dinvestigate_chr[0-9]+_hybridswarm_[0-9]+snpwindow_[0-9]+_[0-9]+.txt', '', filenames[i])
        sub2 <- gsub('./LVRS_malawi_', '', sub1)
        
        #quant95 <- quantile(data$D, 0.95)
        #quant05 <- quantile(data$D, 0.05)
        quant95 <- quantile(data$f_dM, 0.95)
        quant05 <- quantile(data$f_dM, 0.05)  
        data$loc <- paste(data$chr, data$windowStart, data$windowEnd, sep = "_")
        #data$sig <- (data$D >= quant95 | data$D <= quant05)
        #data$sig <- (data$D >= quant95)
        data$sig <- (data$f_dM >= quant95)
        data$sig <- as.numeric(data$sig)        
        subsetdata_precom <- data[,c(1,2,3,8,9)]
        colnames(subsetdata_precom)[5] <- sub2
        subsetdata <- merge(subsetdata, subsetdata_precom, by = c("loc", "chr", "windowStart", "windowEnd"), all = TRUE, sort = TRUE)
    }
    
    #change values for plotting
    subsetdata[6][subsetdata[6] == 1] <- 3
    subsetdata[7][subsetdata[7] == 1] <- 4
    subsetdata[8][subsetdata[8] == 1] <- 5
    subsetdata[9][subsetdata[9] == 1] <- 6    
    
    #how many sig windows
    gig <- length(subsetdata[which(subsetdata[,5] == 1),]$chr)
    orth <- length(subsetdata[which(subsetdata[,6] == 3),]$chr)
    ruaha <- length(subsetdata[which(subsetdata[,7] == 4),]$chr)
    Pseudo_Cteno_Ortho2 <- length(subsetdata[which(subsetdata[,8] == 5),]$chr)
    serr <- length(subsetdata[which(subsetdata[,9] == 6),]$chr)
    
    #subsetdata1 <- subsetdata[c(1, 20, 500, 200, 1000),]
    subsetdata1 <- subsetdata
    subsetdata2 <- subsetdata1[order(subsetdata1$windowStart),]
    #subsetdata2 <- subsetdata1
    subsetdata2[is.na(subsetdata2)] <- 2
    rownames(subsetdata2) <- subsetdata2$loc
    subsetdata3 <- subsetdata2[,-c(1,2,3,4)]
    
    subsetdata3t <- as.data.frame(t(subsetdata3))
    order <- colnames(subsetdata3t)
    subsetdata3t$species <- rownames(subsetdata3t)
    
    subsetdata4 <- subsetdata3t %>%
      # convert data to long format
      gather(key="window", value="value", -species) %>%
      # rename columns
      setNames(c("species", "window", "value")) %>%
      # convert window to factor
      mutate(window=factor(window))
    
    subsetdata4$window <- factor(subsetdata4$window, levels = order)
    
    #pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_",chr,"_sigd.pdf", sep = ""),width=1100,height=10)
    #par(mar=c(5,5,5,5))
    #par(mar=c(5,5,5,5), cex.axis = 2, cex.lab = 2, xaxs = "i",yaxs = "i")
    q[[p]] <- ggplot(subsetdata4, aes(x=window, y=species, fill=value))+
        geom_tile() + theme_grey(base_size = 8) + coord_equal() +
        scale_fill_gradientn(colours = c("grey90", "blue", "white", "red", "green", "orange", "purple"), values = rescale(c(0,1,2,3,4,5,6))) +
        theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust = 1 ))
    #print(p)
    #dev.off()
}
do.call(grid.arrange,c(q, ncol = 1, top = paste("total number of sig windows: gigliolii = ", gig, " , orthochromis = ", orth, " , ruaha blue= ", ruaha, " , Pseudo_Cteno_Ortho2 = ", Pseudo_Cteno_Ortho2, " , CSA = ", serr, sep = "")))
dev.off()

}

#edit this so that plots are arranged to the left - may need serious rejigging (https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html)



#dinvestigate_plot_sigd(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100")
#dinvestigate_plot_sigd(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000")
#dinvestigate_plot_sigd(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50")

#make same plot but with window coordinates plotted along axis

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

dinvestigate_plot_sigd_cont <- function(output_folder, window, window_short){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm/files/", output_folder, "/", sep = ""))    

q <- list()
pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigfdm_cont.pdf", sep = ""),width=100,height=66)
#pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigd_cont.pdf", sep = ""),width=100,height=66)

    
allsums <- matrix(,nrow = length(chroms), ncol = 5)    
    
for (a in 1:length(chroms)){    
    
    chr <- chroms[a]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    data <- read.table(filenames[1],as.is=T,header=T)
    data$windowsize <- data$windowEnd - data$windowStart
    quant95 <- data[which(data$f_dM >= quantile(data$f_dM, 0.95)),]
    #quant95 <- data[which(data$D >= quantile(data$D, 0.95)),]
    allsums[a,1] <- sum(quant95$windowsize)

    data2 <- read.table(filenames[2],as.is=T,header=T)
    data2$windowsize <- data2$windowEnd - data2$windowStart
    quant952 <- data2[which(data2$f_dM >= quantile(data2$f_dM, 0.95)),]
    #quant952 <- data2[which(data2$D >= quantile(data2$D, 0.95)),]
    allsums[a,2] <- sum(quant952$windowsize)

    data3 <- read.table(filenames[3],as.is=T,header=T)
    data3$windowsize <- data3$windowEnd - data3$windowStart
    quant953 <- data3[which(data3$f_dM >= quantile(data3$f_dM, 0.95)),]
    #quant953 <- data3[which(data3$D >= quantile(data3$D, 0.95)),]
    allsums[a,3] <- sum(quant953$windowsize)

    data4 <- read.table(filenames[4],as.is=T,header=T)
    data4$windowsize <- data4$windowEnd - data4$windowStart
    quant954 <- data4[which(data4$f_dM >= quantile(data4$f_dM, 0.95)),]
    #quant954 <- data4[which(data4$D >= quantile(data4$D, 0.95)),]
    allsums[a,4] <- sum(quant954$windowsize)

    data5 <- read.table(filenames[5],as.is=T,header=T)
    data5$windowsize <- data5$windowEnd - data5$windowStart
    quant955 <- data5[which(data5$f_dM >= quantile(data5$f_dM, 0.95)),]
    #quant955 <- data5[which(data5$D >= quantile(data5$D, 0.95)),]
    allsums[a,5] <- sum(quant955$windowsize)

    total_sums <- colSums(allsums)
    percentage <- (total_sums/sum(accessible$accessible.inversion))*100
    
    #get inversion dimensions
    if (!is.na(match(chr, inversions$V1))){
        n <- match(chr, inversions$V1)
        invstart <- inversions$V2[n]
        invend <- inversions$V3[n]
    } else {
        invstart <- 0
        invend <- 0
    }
    
    #find the significant windows and then plot their coordinates in the plot
    plot <- ggplot() + 
        geom_segment(data = quant95, aes(x=windowStart, y=1, xend=windowEnd, yend=1), color="blue", size=15) +
        geom_segment(data = quant952, aes(x=windowStart, y=2, xend=windowEnd, yend=2), color="red", size=15) +
        geom_segment(data = quant953, aes(x=windowStart, y=3, xend=windowEnd, yend=3), color="green", size=15) +
        geom_segment(data = quant954, aes(x=windowStart, y=4, xend=windowEnd, yend=4), color="orange", size=15) +
        geom_segment(data = quant955, aes(x=windowStart, y=5, xend=windowEnd, yend=5), color="purple", size=15) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major.x = element_line(color = "gray88"),
            panel.grid.minor.x = element_line(color = "gray88"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    #scale_y_continuous(labels = c("gigliolii", "Orthochromis", "ruaha blue", "ruaha blue LVRS cluster", "Serr_Pharyng_Sarg_Thora")) +
    scale_y_continuous(labels = c("", "", "", "", "")) +
    scale_x_continuous(limits = c(0,accessible[a,2]), expand = c(0, 0)) +
    labs(x = chr, y = "")  
    plot + annotate("rect", xmin = invstart, xmax = invend, ymin = 0, ymax = 5, alpha = 0.5, fill = "grey")
    
    
    empty <- ggplot() + theme_void()
    row <- plot_grid(plot, empty, rel_widths = c(accessible[a,2], max(accessible[,2] - accessible[a,2])))
    q[[a]] <- row

    #print(p)
    #dev.off()

}

title <- ggdraw() + 
    #draw_label(
    #"dinvestigate sig fdm - gigliolii = blue, orthochromis = red, ruaha blue = green, ruaha blue LVRS cluster = orange, Serr_Pharyng_Sarg_Thora = purple",
    draw_label(paste(
    "dinvestigate sig fdm - gigliolii ", round(percentage[1], 2), "% = blue, orthochromis ", round(percentage[2], 2), 
        "% = red, ruaha blue ", round(percentage[3], 2), "% = green, ruaha blue LVRS cluster ", round(percentage[4], 2), 
        "% = orange, Serr_Pharyng_Sarg_Thora ", round(percentage[5], 2), "% = purple", sep = ""),
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 60
    )    
    
print(plot_grid(title, plotlist = q, align = "v", nrow = 23, ncol = 1))
#do.call(grid.arrange,c(q, ncol = 1))
dev.off()
    
}
    
#chr23 is missing - fix this and check other plots dont have this problem
#add in inversion locations in background - code not working

#need to signal which regions are accessible here as well? inversions removed
#add vertical lines indicating where windows overlap


#dinvestigate_plot_sigd_cont(output_folder = "P1_LVRS", window = "100snpwindow_100_50", window_short = "100_50")
dinvestigate_plot_sigd_cont(output_folder = "P1_LVRS", window = "1000snpwindow_1000_250", window_short = "1000_250")
#dinvestigate_plot_sigd_cont(output_folder = "P1_LVRS", window = "50snpwindow_50_25", window_short = "50_25")

#edit selected windows in 2 ways:
# 1. take the maximum negative d/fdm value (across whole genome) and select all windows in the positive distribution 
#    with values higher than this value
# 2. take the most negative 5 or 0.5 percent values (across whole genome) and find the negative value that is the 
#    threshold between these values and the rest of the negative distribution. on the positive side select all windows
#    in the positive distribution with values higher than this value. 

#to do - remove the non-accessible regions from the plot and percentage
#      - plot overall fdm/d distributions
#      - add markings to define neighbouring windows
#      - add more variables to the function - d/fdm and sig window thresholds


chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

#output_folder = "P1_LVRS"
#window = "100snpwindow_100_50"
#window_short = "100_50"
#threshold <- 0.05

dinvestigate_plot_sigd_cont <- function(output_folder, window, window_short){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

q <- list()
q_2 <- list()
q_3 <- list()
#pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigd_cont.pdf", sep = ""),width=100,height=66)

###########

chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)

    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
data_quant0.05 <- quantile(data$f_dM, 0.05)
data_quant0.005 <- quantile(data$f_dM, 0.005)
data_min <- min(data$f_dM)

data2_quant0.05 <- quantile(data2$f_dM, 0.05)
data2_quant0.005 <- quantile(data2$f_dM, 0.005)
data2_min <- min(data2$f_dM)

data3_quant0.05 <- quantile(data3$f_dM, 0.05)
data3_quant0.005 <- quantile(data3$f_dM, 0.005)
data3_min <- min(data3$f_dM)

data4_quant0.05 <- quantile(data4$f_dM, 0.05)
data4_quant0.005 <- quantile(data4$f_dM, 0.005)
data4_min <- min(data4$f_dM)

data5_quant0.05 <- quantile(data5$f_dM, 0.05)
data5_quant0.005 <- quantile(data5$f_dM, 0.005)
data5_min <- min(data5$f_dM)

#####

allsums <- matrix(,nrow = length(chroms), ncol = 5)
allsums2 <- matrix(,nrow = length(chroms), ncol = 5)    
allsums3 <- matrix(,nrow = length(chroms), ncol = 5)    

#for (a in 3:3){  
for (a in 1:length(chroms)){    
    
    chr <- chroms[a]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    data <- read.table(filenames[1],as.is=T,header=T)
    data$windowsize <- data$windowEnd - data$windowStart
    quant95 <- data[which(data$f_dM >= abs(data_quant0.05)),]
    quant95[nrow(quant95) + 1, 1] <- "chr" #add dummy row becuase some dataframes are empty in the end plot
    quant95[nrow(quant95), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    quant99.5 <- data[which(data$f_dM >= abs(data_quant0.005)),]
    quant99.5[nrow(quant99.5) + 1, 1] <- "chr"
    quant99.5[nrow(quant99.5), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    above_min <- data[which(data$f_dM >= abs(data_min)),]
    above_min[nrow(above_min) + 1, 1] <- "chr"
    above_min[nrow(above_min), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    allsums[a,1] <- sum(quant95$windowsize)
    allsums2[a,1] <- sum(quant99.5$windowsize)
    allsums3[a,1] <- sum(above_min$windowsize)

    ##########
    data2 <- read.table(filenames[2],as.is=T,header=T)
    data2$windowsize <- data2$windowEnd - data2$windowStart    
    quant952 <- data2[which(data2$f_dM >= abs(data2_quant0.05)),]
    quant952[nrow(quant952) + 1, 1] <- "chr"
    quant952[nrow(quant952), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    quant99.52 <- data2[which(data2$f_dM >= abs(data2_quant0.005)),]
    quant99.52[nrow(quant99.52) + 1, 1] <- "chr"
    quant99.52[nrow(quant99.52), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    above_min2 <- data2[which(data2$f_dM >= abs(data2_min)),]
    above_min2[nrow(above_min2) + 1, 1] <- "chr"
    above_min2[nrow(above_min2), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    allsums[a,2] <- sum(quant952$windowsize)
    allsums2[a,2] <- sum(quant99.52$windowsize)
    allsums3[a,2] <- sum(above_min2$windowsize)
    
    ##########
    data3 <- read.table(filenames[3],as.is=T,header=T)
    data3$windowsize <- data3$windowEnd - data3$windowStart
    quant953 <- data3[which(data3$f_dM >= abs(data3_quant0.05)),]
    quant953[nrow(quant953) + 1, 1] <- "chr"
    quant953[nrow(quant953), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    quant99.53 <- data3[which(data3$f_dM >= abs(data3_quant0.005)),]
    quant99.53[nrow(quant99.53) + 1, 1] <- "chr"
    quant99.53[nrow(quant99.53), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    above_min3 <- data2[which(data3$f_dM >= abs(data3_min)),]
    above_min3[nrow(above_min3) + 1, 1] <- "chr"
    above_min3[nrow(above_min3), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    allsums[a,3] <- sum(quant953$windowsize)
    allsums2[a,3] <- sum(quant99.53$windowsize)
    allsums3[a,3] <- sum(above_min3$windowsize)
    
    ##########
    data4 <- read.table(filenames[4],as.is=T,header=T)
    data4$windowsize <- data4$windowEnd - data4$windowStart
    quant954 <- data4[which(data4$f_dM >= abs(data4_quant0.05)),]
    quant954[nrow(quant954) + 1, 1] <- "chr" 
    quant954[nrow(quant954), 2:8] <- as.numeric(c(0,1,0,0,0,0,0)) 
    
    quant99.54 <- data4[which(data4$f_dM >= abs(data4_quant0.005)),]
    quant99.54[nrow(quant99.54) + 1, 1] <- "chr" 
    quant99.54[nrow(quant99.54), 2:8] <- as.numeric(c(0,1,0,0,0,0,0)) 
    
    above_min4 <- data4[which(data4$f_dM >= abs(data4_min)),]
    above_min4[nrow(above_min4) + 1, 1] <- "chr" 
    above_min4[nrow(above_min4), 2:8] <- as.numeric(c(0,1,0,0,0,0,0)) 
    
    allsums[a,4] <- sum(quant954$windowsize)
    allsums2[a,4] <- sum(quant99.54$windowsize)
    allsums3[a,4] <- sum(above_min4$windowsize)

    ##########
    data5 <- read.table(filenames[5],as.is=T,header=T)
    data5$windowsize <- data5$windowEnd - data5$windowStart
    quant955 <- data5[which(data5$f_dM >= abs(data5_quant0.05)),]
    quant955[nrow(quant955) + 1, 1] <- "chr"
    quant955[nrow(quant955), 2:8] <- as.numeric(c(0,1,0,0,0,0,0))
    
    quant99.55 <- data5[which(data5$f_dM >= abs(data5_quant0.005)),]
    quant99.55[nrow(quant99.55) + 1, 1] <- "chr" 
    quant99.55[nrow(quant99.55), 2:8] <- as.numeric(c(0,1,0,0,0,0,0)) 
    
    above_min5 <- data5[which(data5$f_dM >= abs(data5_min)),]
    above_min5[nrow(above_min5) + 1, 1] <- "chr" 
    above_min5[nrow(above_min5), 2:8] <- as.numeric(c(0,1,0,0,0,0,0)) 
    
    allsums[a,5] <- sum(quant955$windowsize)
    allsums2[a,5] <- sum(quant99.55$windowsize)
    allsums3[a,5] <- sum(above_min5$windowsize)

    #print(length(quant95$chr))
    #print(length(quant952$chr))
    #print(length(quant953$chr))
    #print(length(quant954$chr))
    #print(length(quant955$chr))
    
    total_sums <- colSums(allsums)
    percentage <- (total_sums/sum(accessible$accessible.inversion))*100
    
    total_sums2 <- colSums(allsums2)
    percentage2 <- (total_sums2/sum(accessible$accessible.inversion))*100
    
    total_sums3 <- colSums(allsums3)
    percentage3 <- (total_sums3/sum(accessible$accessible.inversion))*100
    
    
    #get inversion dimensions
    if (!is.na(match(chr, inversions$V1))){
        n <- match(chr, inversions$V1)
        invstart <- inversions$V2[n]
        invend <- inversions$V3[n]
    } else {
        invstart <- 0
        invend <- 0
    }
    
    #find the significant windows and then plot their coordinates in the plot
    #0.5 negative quantile
    plot <- ggplot() + 
        geom_segment(data = quant95, aes(x=windowStart, y=1, xend=windowEnd, yend=1), color="blue", size=15) +
        geom_segment(data = quant952, aes(x=windowStart, y=2, xend=windowEnd, yend=2), color="red", size=15) +
        geom_segment(data = quant953, aes(x=windowStart, y=3, xend=windowEnd, yend=3), color="green", size=15) +
        geom_segment(data = quant954, aes(x=windowStart, y=4, xend=windowEnd, yend=4), color="orange", size=15) +
        geom_segment(data = quant955, aes(x=windowStart, y=5, xend=windowEnd, yend=5), color="purple", size=15) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major.x = element_line(color = "gray88"),
            panel.grid.minor.x = element_line(color = "gray88"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    scale_y_continuous(labels = c("", "", "", "", "")) +
    scale_x_continuous(limits = c(0,accessible[a,2]), expand = c(0, 0)) +
    labs(x = chr, y = "")  
    #plot + annotate("rect", xmin = invstart, xmax = invend, ymin = 0, ymax = 5, alpha = 0.5, fill = "grey")
    
    empty <- ggplot() + theme_void()
    row <- plot_grid(plot, empty, rel_widths = c(accessible[a,2], max(accessible[,2] - accessible[a,2])))
    q[[a]] <- row
    
    #scale_color_manual(values = c("red3", "seagreen4", "hotpink2", "royalblue2", "purple3")) +
    #0.05 negative quantile
    plot_2 <- ggplot() + 
        geom_segment(data = quant99.5, aes(x=windowStart, y=1, xend=windowEnd, yend=1), color="red3", size=15) +
        geom_segment(data = quant99.52, aes(x=windowStart, y=2, xend=windowEnd, yend=2), color="seagreen4", size=15) +
        geom_segment(data = quant99.53, aes(x=windowStart, y=3, xend=windowEnd, yend=3), color="hotpink2", size=15) +
        geom_segment(data = quant99.54, aes(x=windowStart, y=4, xend=windowEnd, yend=4), color="royalblue2", size=15) +
        geom_segment(data = quant99.55, aes(x=windowStart, y=5, xend=windowEnd, yend=5), color="purple3", size=15) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major.x = element_line(color = "gray88"),
            panel.grid.minor.x = element_line(color = "gray88"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    scale_y_continuous(labels = c("", "", "", "", "")) +
    scale_x_continuous(limits = c(0,accessible[a,2]), expand = c(0, 0)) +
    labs(x = chr, y = "")  
    #plot + annotate("rect", xmin = invstart, xmax = invend, ymin = 0, ymax = 5, alpha = 0.5, fill = "grey")
    
    empty_2 <- ggplot() + theme_void()
    row_2 <- plot_grid(plot_2, empty_2, rel_widths = c(accessible[a,2], max(accessible[,2] - accessible[a,2])))
    q_2[[a]] <- row_2
    
    #min negative
    plot_3 <- ggplot() + 
        geom_segment(data = above_min, aes(x=windowStart, y=1, xend=windowEnd, yend=1), color="blue", size=15) +
        geom_segment(data = above_min2, aes(x=windowStart, y=2, xend=windowEnd, yend=2), color="red", size=15) +
        geom_segment(data = above_min3, aes(x=windowStart, y=3, xend=windowEnd, yend=3), color="green", size=15) +
        geom_segment(data = above_min4, aes(x=windowStart, y=4, xend=windowEnd, yend=4), color="orange", size=15) +
        geom_segment(data = above_min5, aes(x=windowStart, y=5, xend=windowEnd, yend=5), color="purple", size=15) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major.x = element_line(color = "gray88"),
            panel.grid.minor.x = element_line(color = "gray88"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    scale_y_continuous(labels = c("", "", "", "", "")) +
    scale_x_continuous(limits = c(0,accessible[a,2]), expand = c(0, 0)) +
    labs(x = chr, y = "")  
    #plot + annotate("rect", xmin = invstart, xmax = invend, ymin = 0, ymax = 5, alpha = 0.5, fill = "grey")
    
    empty_3 <- ggplot() + theme_void()
    row_3 <- plot_grid(plot_3, empty_3, rel_widths = c(accessible[a,2], max(accessible[,2] - accessible[a,2])))
    q_3[[a]] <- row_3

}

#0.5 negative quantile
title <- ggdraw() + 
    draw_label(paste(
    "dinvestigate sig fdm - gigliolii ", round(percentage[1], 2), "% = blue, orthochromis ", round(percentage[2], 2), 
        "% = red,Pseudo_Cteno_Ortho2 ", round(percentage[3], 2), "% = green, ruaha blue ", round(percentage[4], 2), 
        "% = orange, Serr_Pharyng_Sarg_Thora ", round(percentage[5], 2), "% = purple", sep = ""),
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 60
    )    

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigfdm_0.5percent_cont.pdf", sep = ""),width=100,height=66)
print(plot_grid(title, plotlist = q, align = "v", nrow = 23, ncol = 1))
dev.off()
    
#supplementary figure {result3_dinvestigate_sig_windows_50snps_0.5percent}
    
#0.05 negative quantile
title_2 <- ggdraw() + 
    draw_label("Dinvestigate windows 50snps with significant positive FdM values, where P1 = Victoria, P2 = Malawi and P3 = A.gigliolii (red), Orthochromis (green), Pseudocrenilabrus (pink), A.sp. Ruaha blue (blue), or CSA (purple)", fontface = 'bold', x = 0, hjust = 0, size = 60
    #draw_label(paste(
    #"dinvestigate sig fdm - gigliolii ", round(percentage2[1], 2), "% = blue, orthochromis ", round(percentage2[2], 2), 
    #    "% = red, Pseudo_Cteno_Ortho2 ", round(percentage2[3], 2), "% = green, ruaha blue ", round(percentage2[4], 2), 
    #    "% = orange, Serr_Pharyng_Sarg_Thora ", round(percentage2[5], 2), "% = purple", sep = ""),
    #fontface = 'bold',
    #x = 0,
    #hjust = 0,
    #size = 60
    )    

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigfdm_0.05percent_cont.pdf", sep = ""),width=100,height=66)
print(plot_grid(title_2, plotlist = q_2, align = "v", nrow = 23, ncol = 1))
dev.off()
    
#min negative
title_3 <- ggdraw() + 
    draw_label(paste(
    "dinvestigate sig fdm - gigliolii ", round(percentage3[1], 2), "% = blue, orthochromis ", round(percentage3[2], 2), 
        "% = red, Pseudo_Cteno_Ortho2 ", round(percentage3[3], 2), "% = green, ruaha blue ", round(percentage3[4], 2), 
        "% = orange, Serr_Pharyng_Sarg_Thora ", round(percentage3[5], 2), "% = purple", sep = ""),
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 60
    )    

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sigfdm_minnegative_cont.pdf", sep = ""),width=100,height=66)
print(plot_grid(title_3, plotlist = q_3, align = "v", nrow = 23, ncol = 1))
dev.off()
    
}
    
#chr23 is missing - fix this and check other plots dont have this problem
#add in inversion locations in background - code not working

#need to signal which regions are accessible here as well? inversions removed
#add vertical lines indicating where windows overlap


#dinvestigate_plot_sigd_cont(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100")
#dinvestigate_plot_sigd_cont(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000")
dinvestigate_plot_sigd_cont(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50")

#copy plots over with:
rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dinvestigate/plots_june2023

#read in all accessible region files and remove inversions

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#for (i in 1:1){
for (i in 1:(length(chroms))){

    chr <- chroms[i]
    accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible.bed", sep = ""), sep = "\t", header = FALSE)
    inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

    colnames(inversions) <- c("chr", "start", "end")
    sub_inversions <- inversions[which(inversions$chr == chr),] #subset file to only include inversion info on current chr

    colnames(accessible_regions) <- c("chr", "start", "end")

    if (dim(sub_inversions)[1] == 0){
        #write file as is
        write.table(accessible_regions, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
    } else {
        removed_inversions <- bedr.subtract.region(accessible_regions, sub_inversions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
        write.table(removed_inversions, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
    }

}

#inv_test <- accessible_regions[which(accessible_regions$start >=9800000 & accessible_regions$start <=32540000),]
#length(inv_test$chr)

#removed_inversions <- bedr.subtract.region(accessible_regions, sub_inversions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
#head(removed_inversions)
#length(removed_inversions$chr)

#inv_test2 <- test[which(test$start >=9800000 & test$start <=32540000),]
#length(inv_test2$chr)


#edit selected windows in 2 ways:
# 1. take the maximum negative d/fdm value (across whole genome) and select all windows in the positive distribution 
#    with values higher than this value
# 2. take the most negative 5 or 0.5 percent values (across whole genome) and find the negative value that is the 
#    threshold between these values and the rest of the negative distribution. on the positive side select all windows
#    in the positive distribution with values higher than this value. 
  

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

#output_folder = "P1_LVRS"
#window = "50snpwindow_50_25"
#window_short = "50_25"
#sig <- "quant"
#quant <- 0.05
#name <- "5percent"
#stat <- "d"


dinvestigate_plot_sigd_cont_onlyaccessible <- function(output_folder, window, window_short, sig, quant, name, stat){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

q <- list()

###########

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}   
    
#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)
    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

data_dist <- data
data_dist2 <- data2
data_dist3 <- data3
data_dist4 <- data4
data_dist5 <- data5

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
    sig_threshold2 <- quantile(data2[,stat_col_num], quant)
    sig_threshold3 <- quantile(data3[,stat_col_num], quant)
    sig_threshold4 <- quantile(data4[,stat_col_num], quant)
    sig_threshold5 <- quantile(data5[,stat_col_num], quant)
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
    sig_threshold2 <- min(data2[,stat_col_num])
    sig_threshold3 <- min(data3[,stat_col_num])
    sig_threshold4 <- min(data4[,stat_col_num])
    sig_threshold5 <- min(data5[,stat_col_num])
}

#####

allsums <- matrix(,nrow = length(chroms), ncol = 5)   

#read through each chromosome, select sig windows and plot
#for (a in 1:1){  
for (a in 1:length(chroms)){    
        
    chr <- chroms[a]
    #print(chr)
    accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", header = FALSE)
    #accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible.bed", sep = ""), sep = "\t", header = FALSE)
    colnames(accessible_regions) <- c("chr", "start", "end")
    
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    
    ##########
    data <- read.table(filenames[1],as.is=T,header=T)
    data_sig <- data[which(data[,stat_col_num] >= abs(sig_threshold)),] #select significant windows
    data_sig <- rbind(c(chr, 0,1,0,0,0,0), data_sig) #add dummy row becuase some dataframes are empty in the end plot
    colnames(data_sig) <- colnames(data)
    #data_sig[nrow(data_sig) + 1, 1] <- chr 
    #data_sig[nrow(data_sig), 2:7] <- as.numeric(c(0,1,0,0,0,0))
    
    data_sig_bed <- data_sig[,1:3] #remove non-accessible regions from windows
    colnames(data_sig_bed) <- c("chr", "start", "end")
    data_sig_bed_accessible <- bedr.join.region(data_sig_bed, accessible_regions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
    colnames(data_sig_bed_accessible) <- c("chr", "windowStart", "windowEnd", "new_chr", "new_windowStart", "new_windowEnd")
    data_sig_merge <- merge(data_sig_bed_accessible, data_sig, by = c("chr", "windowStart", "windowEnd"), sort = FALSE, all.x = TRUE, all.y = FALSE)
    data_sig_merge$new_windowsize <- as.numeric(data_sig_merge$new_windowEnd) - as.numeric(data_sig_merge$new_windowStart)
    data_sig_merge$new_windowStart <- as.numeric(data_sig_merge$new_windowStart)
    data_sig_merge$new_windowEnd <- as.numeric(data_sig_merge$new_windowEnd)
    
    allsums[a,1] <- sum(data_sig_merge$new_windowsize)

    ##########
    data2 <- read.table(filenames[2],as.is=T,header=T) 
    data_sig2 <- data2[which(data2[,stat_col_num] >= abs(sig_threshold2)),]
    data_sig2 <- rbind(c(chr, 0,1,0,0,0,0), data_sig2)
    colnames(data_sig2) <- colnames(data2)
    #data_sig2[nrow(data_sig2) + 1, 1] <- chr
    #data_sig2[nrow(data_sig2), 2:7] <- as.numeric(c(0,1,0,0,0,0))
    
    data_sig2_bed <- data_sig2[,1:3]
    colnames(data_sig2_bed) <- c("chr", "start", "end")
    data_sig2_bed_accessible <- bedr.join.region(data_sig2_bed, accessible_regions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
    colnames(data_sig2_bed_accessible) <- c("chr", "windowStart", "windowEnd", "new_chr", "new_windowStart", "new_windowEnd")
    data_sig2_merge <- merge(data_sig2_bed_accessible, data_sig2, by = c("chr", "windowStart", "windowEnd"), sort = FALSE, all.x = TRUE, all.y = FALSE)
    data_sig2_merge$new_windowsize <- as.numeric(data_sig2_merge$new_windowEnd) - as.numeric(data_sig2_merge$new_windowStart)
    data_sig2_merge$new_windowStart <- as.numeric(data_sig2_merge$new_windowStart)
    data_sig2_merge$new_windowEnd <- as.numeric(data_sig2_merge$new_windowEnd)

    allsums[a,2] <- sum(data_sig2_merge$new_windowsize)
    
    ##########
    data3 <- read.table(filenames[3],as.is=T,header=T)
    data_sig3 <- data3[which(data3[,stat_col_num] >= abs(sig_threshold3)),]
    data_sig3 <- rbind(c(chr, 0,1,0,0,0,0), data_sig3)
    colnames(data_sig3) <- colnames(data3)
    #data_sig3[nrow(data_sig3) + 1, 1] <- chr
    #data_sig3[nrow(data_sig3), 2:7] <- as.numeric(c(0,1,0,0,0,0))
    
    data_sig3_bed <- data_sig3[,1:3]
    colnames(data_sig3_bed) <- c("chr", "start", "end")
    data_sig3_bed_accessible <- bedr.join.region(data_sig3_bed, accessible_regions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
    colnames(data_sig3_bed_accessible) <- c("chr", "windowStart", "windowEnd", "new_chr", "new_windowStart", "new_windowEnd")
    data_sig3_merge <- merge(data_sig3_bed_accessible, data_sig3, by = c("chr", "windowStart", "windowEnd"), sort = FALSE, all.x = TRUE, all.y = FALSE)
    data_sig3_merge$new_windowsize <- as.numeric(data_sig3_merge$new_windowEnd) - as.numeric(data_sig3_merge$new_windowStart)
    data_sig3_merge$new_windowStart <- as.numeric(data_sig3_merge$new_windowStart)
    data_sig3_merge$new_windowEnd <- as.numeric(data_sig3_merge$new_windowEnd)
    
    allsums[a,3] <- sum(data_sig3_merge$new_windowsize)
    
    ##########
    data4 <- read.table(filenames[4],as.is=T,header=T)
    data_sig4 <- data4[which(data4[,stat_col_num] >= abs(sig_threshold4)),]
    data_sig4 <- rbind(c(chr, 0,1,0,0,0,0), data_sig4)
    colnames(data_sig4) <- colnames(data4)
    #data_sig4[nrow(data_sig4) + 1, 1] <- chr
    #data_sig4[nrow(data_sig4), 2:7] <- as.numeric(c(0,1,0,0,0,0)) 
    
    data_sig4_bed <- data_sig4[,1:3]
    colnames(data_sig4_bed) <- c("chr", "start", "end")
    data_sig4_bed_accessible <- bedr.join.region(data_sig4_bed, accessible_regions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
    colnames(data_sig4_bed_accessible) <- c("chr", "windowStart", "windowEnd", "new_chr", "new_windowStart", "new_windowEnd")
    data_sig4_merge <- merge(data_sig4_bed_accessible, data_sig4, by = c("chr", "windowStart", "windowEnd"), sort = FALSE, all.x = TRUE, all.y = FALSE)
    data_sig4_merge$new_windowsize <- as.numeric(data_sig4_merge$new_windowEnd) - as.numeric(data_sig4_merge$new_windowStart)
    data_sig4_merge$new_windowStart <- as.numeric(data_sig4_merge$new_windowStart)
    data_sig4_merge$new_windowEnd <- as.numeric(data_sig4_merge$new_windowEnd)
    
    allsums[a,4] <- sum(data_sig4_merge$new_windowsize)

    ##########
    data5 <- read.table(filenames[5],as.is=T,header=T)
    data_sig5 <- data5[which(data5[,stat_col_num] >= abs(sig_threshold5)),]
    data_sig5 <- rbind(c(chr, 0,1,0,0,0,0), data_sig5)
    colnames(data_sig5) <- colnames(data5)
    #data_sig5[nrow(data_sig5) + 1, 1] <- chr
    #data_sig5[nrow(data_sig5), 2:7] <- as.numeric(c(0,1,0,0,0,0))
    
    data_sig5_bed <- data_sig5[,1:3]
    colnames(data_sig5_bed) <- c("chr", "start", "end")
    data_sig5_bed_accessible <- bedr.join.region(data_sig5_bed, accessible_regions, check.chr = FALSE, check.sort = FALSE, check.valid = FALSE, verbose = FALSE)
    colnames(data_sig5_bed_accessible) <- c("chr", "windowStart", "windowEnd", "new_chr", "new_windowStart", "new_windowEnd")
    data_sig5_merge <- merge(data_sig5_bed_accessible, data_sig5, by = c("chr", "windowStart", "windowEnd"), sort = FALSE, all.x = TRUE, all.y = FALSE)
    data_sig5_merge$new_windowsize <- as.numeric(data_sig5_merge$new_windowEnd) - as.numeric(data_sig5_merge$new_windowStart)
    data_sig5_merge$new_windowStart <- as.numeric(data_sig5_merge$new_windowStart)
    data_sig5_merge$new_windowEnd <- as.numeric(data_sig5_merge$new_windowEnd)
    
    allsums[a,5] <- sum(data_sig5_merge$new_windowsize)
    
    #print(length(data_sig_merge$chr))
    #print(length(data_sig2_merge$chr))
    #print(length(data_sig3_merge$chr))
    #print(length(data_sig4_merge$chr))
    #print(length(data_sig5_merge$chr))
    
    total_sums <- colSums(allsums)
    percentage <- (total_sums/sum(accessible$accessible.inversion))*100
    
    #get inversion dimensions
    if (!is.na(match(chr, inversions$V1))){
        n <- match(chr, inversions$V1)
        invstart <- inversions$V2[n]
        invend <- inversions$V3[n]
    } else {
        invstart <- 0
        invend <- 0
    }
    
    #find the significant windows and then plot their coordinates in the plot
    plot <- ggplot() + 
        geom_segment(data = data_sig_merge, aes(x=new_windowStart, y=1, xend=new_windowEnd, yend=1), color="blue", size=15) +
        geom_segment(data = data_sig2_merge, aes(x=new_windowStart, y=2, xend=new_windowEnd, yend=2), color="red", size=15) +
        geom_segment(data = data_sig3_merge, aes(x=new_windowStart, y=3, xend=new_windowEnd, yend=3), color="green", size=15) +
        geom_segment(data = data_sig4_merge, aes(x=new_windowStart, y=4, xend=new_windowEnd, yend=4), color="orange", size=15) +
        geom_segment(data = data_sig5_merge, aes(x=new_windowStart, y=5, xend=new_windowEnd, yend=5), color="purple", size=15) + 
    theme(panel.background = element_rect(fill = "white", color = "black"),panel.grid.major.x = element_line(color = "gray88"),
            panel.grid.minor.x = element_line(color = "gray88"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    scale_y_continuous(labels = c("", "", "", "", "")) +
    scale_x_continuous(limits = c(0,accessible[a,2]), expand = c(0, 0)) +
    labs(x = chr, y = "")  
    #plot + annotate("rect", xmin = invstart, xmax = invend, ymin = 0, ymax = 5, alpha = 0.5, fill = "grey")
    
    empty <- ggplot() + theme_void()
    row <- plot_grid(plot, empty, rel_widths = c(accessible[a,2], max(accessible[,2] - accessible[a,2])))
    q[[a]] <- row
    
}

title <- ggdraw() + 
    draw_label(paste(
    "dinvestigate sig ", stat, " - gigliolii ", round(percentage[1], 2), "% = blue, orthochromis ", round(percentage[2], 2), 
        "% = red, Pseudo_Cteno_Ortho2 ", round(percentage[3], 2), "% = green, ruaha blue ", round(percentage[4], 2), 
        "% = orange, Serr_Pharyng_Sarg_Thora ", round(percentage[5], 2), "% = purple", sep = ""),
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 60
    )    

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_allchr_sig", stat, "_", name, "_cont_accessible.pdf", sep = ""),width=200,height=66)
print(plot_grid(title, plotlist = q, align = "v", nrow = 23, ncol = 1))
dev.off()

}

#chr23 is missing - fix this and check other plots dont have this problem
#add vertical lines indicating where windows overlap
#plot overall fdm/d distributions



#stat = fdm

dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.05, name = "5percent", stat = "fdm")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.005, name = "0.5percent", stat = "fdm")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "min", quant = 0, name = "minnegative", stat = "fdm")

dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.05, name = "5percent", stat = "fdm")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.005, name = "0.5percent", stat = "fdm")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "min", quant = 0, name = "minnegative", stat = "fdm")

dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.05, name = "5percent", stat = "fdm")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.005, name = "0.5percent", stat = "fdm")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "min", quant = 0, name = "minnegative", stat = "fdm")


#stat = d

dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.05, name = "5percent", stat = "d")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.005, name = "0.5percent", stat = "d")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "min", quant = 0, name = "minnegative", stat = "d")

dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.05, name = "5percent", stat = "d")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.005, name = "0.5percent", stat = "d")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "min", quant = 0, name = "minnegative", stat = "d")

dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.05, name = "5percent", stat = "d")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.005, name = "0.5percent", stat = "d")
dinvestigate_plot_sigd_cont_onlyaccessible(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "min", quant = 0, name = "minnegative", stat = "d")


#get stats and distributions of d and fdm values
#what are the threshold values for sig windows, how many sig windows are there etc

summarystats <- data.frame(matrix(nrow = 90, ncol = 12))
colnames(summarystats) <- c("P1", "window", "sig_selection", "threshold_percent", "stat", "P3", "threshold_value", 
                            "n_total_windows", "n_sig_windows", "percent_sig_windows", "mean_windowsize", "mean_sig_windowsize")

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#test
output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_50"
sig = "quant"
quant = 0.05
name = "5percent"
stat = "fdm"
row = 1

dinvestigate_info_fill <- function(output_folder, window, window_short, sig, quant, name, stat, row){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}       
    
summarystats[row:(row+4),1] <- output_folder
summarystats[row:(row+4),2] <- window_short
summarystats[row:(row+4),3] <- sig
summarystats[row:(row+4),4] <- quant
summarystats[row:(row+4),5] <- stat
summarystats[row:(row+4),6] <- c("gigliolii", "orthochromis", "Pseudo_Cteno_Ortho2", "ruaha_blue", "Serr_Pharyng_Sarg_Thora")

#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)
    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

data$windowsize <- as.numeric(data$windowEnd) - as.numeric(data$windowStart)
data2$windowsize <- as.numeric(data2$windowEnd) - as.numeric(data2$windowStart)
data3$windowsize <- as.numeric(data3$windowEnd) - as.numeric(data3$windowStart)
data4$windowsize <- as.numeric(data4$windowEnd) - as.numeric(data4$windowStart)
data5$windowsize <- as.numeric(data5$windowEnd) - as.numeric(data5$windowStart)

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
    sig_threshold2 <- quantile(data2[,stat_col_num], quant)
    sig_threshold3 <- quantile(data3[,stat_col_num], quant)
    sig_threshold4 <- quantile(data4[,stat_col_num], quant)
    sig_threshold5 <- quantile(data5[,stat_col_num], quant)
    
    summarystats[row,7] <- sig_threshold
    summarystats[row+1,7] <- sig_threshold2
    summarystats[row+2,7] <- sig_threshold3
    summarystats[row+3,7] <- sig_threshold4
    summarystats[row+4,7] <- sig_threshold5
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
    sig_threshold2 <- min(data2[,stat_col_num])
    sig_threshold3 <- min(data3[,stat_col_num])
    sig_threshold4 <- min(data4[,stat_col_num])
    sig_threshold5 <- min(data5[,stat_col_num])
    
    summarystats[row,7] <- sig_threshold
    summarystats[row+1,7] <- sig_threshold2
    summarystats[row+2,7] <- sig_threshold3
    summarystats[row+3,7] <- sig_threshold4
    summarystats[row+4,7] <- sig_threshold5
}

#total number of windows
summarystats[row,8] <- length(data$chr)
summarystats[row+1,8] <- length(data2$chr)
summarystats[row+2,8] <- length(data3$chr)
summarystats[row+3,8] <- length(data4$chr)
summarystats[row+4,8] <- length(data5$chr)

#subset to get sig windows
sig_threshold_sub <- data[which(data[,stat_col_num] >= abs(sig_threshold)),]
sig_threshold_sub2 <- data2[which(data2[,stat_col_num] >= abs(sig_threshold2)),]
sig_threshold_sub3 <- data3[which(data3[,stat_col_num] >= abs(sig_threshold3)),]
sig_threshold_sub4 <- data4[which(data4[,stat_col_num] >= abs(sig_threshold4)),]
sig_threshold_sub5 <- data5[which(data5[,stat_col_num] >= abs(sig_threshold5)),]

#total number of sig windows
summarystats[row,9] <- length(sig_threshold_sub$chr)
summarystats[row+1,9] <- length(sig_threshold_sub2$chr)
summarystats[row+2,9] <- length(sig_threshold_sub3$chr)
summarystats[row+3,9] <- length(sig_threshold_sub4$chr)
summarystats[row+4,9] <- length(sig_threshold_sub5$chr)

#percentage of sig windows
summarystats[row,10] <- (length(sig_threshold_sub$chr)/length(data$chr))*100
summarystats[row+1,10]  <- (length(sig_threshold_sub2$chr)/length(data2$chr))*100
summarystats[row+2,10]  <- (length(sig_threshold_sub3$chr)/length(data3$chr))*100
summarystats[row+3,10] <- (length(sig_threshold_sub4$chr)/length(data4$chr))*100
summarystats[row+4,10] <- (length(sig_threshold_sub5$chr)/length(data5$chr))*100

#mean window size of all windows
summarystats[row,11] <- mean(data$windowsize)
summarystats[row+1,11] <- mean(data2$windowsize)
summarystats[row+2,11] <- mean(data3$windowsize)
summarystats[row+3,11] <- mean(data4$windowsize)
summarystats[row+4,11] <- mean(data5$windowsize)    
    
#mean window size of sig windows
summarystats[row,12] <- mean(sig_threshold_sub$windowsize)
summarystats[row+1,12] <- mean(sig_threshold_sub2$windowsize)
summarystats[row+2,12] <- mean(sig_threshold_sub3$windowsize)
summarystats[row+3,12] <- mean(sig_threshold_sub4$windowsize)
summarystats[row+4,12] <- mean(sig_threshold_sub5$windowsize)

#return the dataframe
summarystats <<- summarystats
    
}


dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.05, name = "0.5percent", stat = "fdm", row = 1)


#stat = fdm

dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.05, name = "5percent", stat = "fdm", row = 1)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.005, name = "0.5percent", stat = "fdm", row = 6)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "min", quant = 0, name = "minnegative", stat = "fdm", row = 11)

dinvestigate_info_fill(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.05, name = "5percent", stat = "fdm", row = 16)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.005, name = "0.5percent", stat = "fdm", row = 21)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "min", quant = 0, name = "minnegative", stat = "fdm", row = 26)

dinvestigate_info_fill(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.05, name = "5percent", stat = "fdm", row = 31)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.005, name = "0.5percent", stat = "fdm", row = 36)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "min", quant = 0, name = "minnegative", stat = "fdm", row = 41)

#stat = d

dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.05, name = "5percent", stat = "d", row = 46)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "quant", quant = 0.005, name = "0.5percent", stat = "d", row = 51)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", sig = "min", quant = 0, name = "minnegative", stat = "d", row = 56)

dinvestigate_info_fill(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.05, name = "5percent", stat = "d", row = 61)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "quant", quant = 0.005, name = "0.5percent", stat = "d", row = 66)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", sig = "min", quant = 0, name = "minnegative", stat = "d", row = 71)

dinvestigate_info_fill(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.05, name = "5percent", stat = "d", row = 76)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "quant", quant = 0.005, name = "0.5percent", stat = "d", row = 81)
dinvestigate_info_fill(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", sig = "min", quant = 0, name = "minnegative", stat = "d", row = 86)


#head(summarystats)
summarystats[which(summarystats$threshold_percent == 0.005 & summarystats$window == "50_50" & summarystats$stat == "fdm"),]


#summarystats[which(summarystats$n_sig_windows == 1100),] 

write.table(summarystats, "/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/dinvestigate_hybridswarm_d_fdm_summary_stats.csv", row.names = FALSE, quote = FALSE, sep = "\t")
write.table(summarystats, "/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/dinvestigate_hybridswarm_d_fdm_summary_stats_2.csv", row.names = FALSE, quote = FALSE, sep = ";")

#make plots of distributions and thresholds     
 
#test
output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_50"
stat = "fdm"

#dinvestigate_dist_plot <- function(output_folder, window, window_short, stat){

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}       

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)
    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

sig0.5_threshold <- quantile(data[,stat_col_num], 0.05)
sig0.5_threshold2 <- quantile(data2[,stat_col_num], 0.05)
sig0.5_threshold3 <- quantile(data3[,stat_col_num], 0.05)
sig0.5_threshold4 <- quantile(data4[,stat_col_num], 0.05)
sig0.5_threshold5 <- quantile(data5[,stat_col_num], 0.05)

sig0.05_threshold <- quantile(data[,stat_col_num], 0.005)
sig0.05_threshold2 <- quantile(data2[,stat_col_num], 0.005)
sig0.05_threshold3 <- quantile(data3[,stat_col_num], 0.005)
sig0.05_threshold4 <- quantile(data4[,stat_col_num], 0.005)
sig0.05_threshold5 <- quantile(data5[,stat_col_num], 0.005)

sigmin_threshold <- min(data[,stat_col_num])
sigmin_threshold2 <- min(data2[,stat_col_num])
sigmin_threshold3 <- min(data3[,stat_col_num])
sigmin_threshold4 <- min(data4[,stat_col_num])
sigmin_threshold5 <- min(data5[,stat_col_num])

### Main figure 3 ###    
    
pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", 
          output_folder, "_", window_short, "_allchr_", stat, "_distribution_thresholds.pdf", sep = ""),width=6,height=13)
par(mfrow=c(5,1), mai = c(0.3, 0.7, 0.2, 0.1))
    
hist(data[,stat_col_num], breaks = 150, main = "A. gigliolii", xaxt='n', xlab = "")
min <- plyr::round_any(min(data[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data[,stat_col_num]), 0.1, f = ceiling)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#abline(v = sig0.5_threshold, col = "blue")
#abline(v = abs(sig0.5_threshold), col = "blue")
abline(v = mean(data[,stat_col_num]), col = "blue")
abline(v = sig0.05_threshold, col = "red")
abline(v = abs(sig0.05_threshold), col = "red")
#abline(v = sigmin_threshold, col = "green")
#abline(v = abs(sigmin_threshold), col = "green")

hist(data2[,stat_col_num], breaks = 150, main = "Orthochromis", xaxt='n', xlab = "")
min <- plyr::round_any(min(data2[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data2[,stat_col_num]), 0.1, f = ceiling)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#abline(v = sig0.5_threshold2, col = "blue")
#abline(v = abs(sig0.5_threshold2), col = "blue")
abline(v = mean(data2[,stat_col_num]), col = "blue")
abline(v = sig0.05_threshold2, col = "red")
abline(v = abs(sig0.05_threshold2), col = "red")
#abline(v = sigmin_threshold2, col = "green")
#abline(v = abs(sigmin_threshold2), col = "green")

hist(data3[,stat_col_num], breaks = 150, main = "A. sp. Ruaha blue", xaxt='n', xlab = "")
min <- plyr::round_any(min(data3[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data3[,stat_col_num]), 0.1, f = ceiling)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#abline(v = sig0.5_threshold3, col = "blue")
#abline(v = abs(sig0.5_threshold3), col = "blue")
#abline(v = abs(sig0.5_threshold3), col = "blue")
#abline(v = abs(sig0.5_threshold3), col = "blue")
abline(v = mean(data3[,stat_col_num]), col = "blue")
abline(v = sig0.05_threshold3, col = "red")
abline(v = abs(sig0.05_threshold3), col = "red")
#abline(v = sigmin_threshold3, col = "green")
#abline(v = abs(sigmin_threshold3), col = "green")

hist(data4[,stat_col_num], breaks = 150, main = "Pseudocrenilabrus", xaxt='n', xlab = "")
min <- plyr::round_any(min(data4[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data4[,stat_col_num]), 0.1, f = ceiling)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#abline(v = sig0.5_threshold4, col = "blue")
#abline(v = abs(sig0.5_threshold4), col = "blue")
abline(v = mean(data4[,stat_col_num]), col = "blue")
abline(v = sig0.05_threshold4, col = "red")
abline(v = abs(sig0.05_threshold4), col = "red")
#abline(v = sigmin_threshold4, col = "green")
#abline(v = abs(sigmin_threshold4), col = "green")

hist(data5[,stat_col_num], breaks = 150, main = "CSA", xaxt='n', xlab = "")
min <- plyr::round_any(min(data5[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data5[,stat_col_num]), 0.1, f = ceiling)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#abline(v = sig0.5_threshold5, col = "blue")
#abline(v = abs(sig0.5_threshold5), col = "blue")
abline(v = mean(data5[,stat_col_num]), col = "blue")
abline(v = sig0.05_threshold5, col = "red")
abline(v = abs(sig0.05_threshold5), col = "red")
#abline(v = sigmin_threshold5, col = "green")
#abline(v = abs(sigmin_threshold5), col = "green")

#mtext(paste("Histogram ", stat, " windows. Blue = 5% quantile threshold, red = 0.5% quantile threshold, green = minimum negative value threshold", sep = ""), side = 3, line = -1.5, outer = TRUE)    
    
dev.off()

#}

#tidy up plots for main figure
#combine with the autocorrelation plot


#version 2

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", 
          output_folder, "_", window_short, "_allchr_", stat, "_distribution_thresholds_version2.pdf", sep = ""),width=10,height=6)
par(mfrow=c(3,2), mai = c(0.3, 0.7, 0.2, 0.1))

#expression(paste(bolditalic("Astatotilapia gigliolii"), bold(" group"))), 


max_y <- 4000
#par(mar=c(4,4.7,2,1))
par(mar=c(2,3.5,2,1))
hist(data[,stat_col_num], breaks = 150, main = expression(paste(bolditalic("Astatotilapia gigliolii"), bold(" group"))), xaxt='n', xlab = "", ylab = "", ylim = c(0, max_y), col = "slategray3", border = "slategray3", las = 1)
min <- plyr::round_any(min(data[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data[,stat_col_num]), 0.1, f = ceiling)
axis(side = 2, at = c(0,500,1000,1500,2000,2500,3000,3500,4000), las = 1)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#title(ylab="Frequency", line=3.7, cex.lab=1.2)
#title(xlab = "FdM", line=2.5, cex.lab=1.2)
abline(v = 0, col = "grey89", lwd = 1)
abline(v = mean(data[,stat_col_num]), col = "navyblue", lwd = 2)
abline(v = sig0.05_threshold, col = "red3", lwd = 2)
abline(v = abs(sig0.05_threshold), col = "red3", lwd = 2)

max_y <- 7000
#par(mar=c(4,4.7,2,1))
par(mar=c(2,3.5,2,1))
hist(data2[,stat_col_num], breaks = 150, main = expression(paste(bolditalic("Orthochromis"), bold(" group"))), xaxt='n', xlab = "", ylab = "", ylim = c(0, max_y), col = "slategray3", border = "slategray3", las = 1)
min <- plyr::round_any(min(data2[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data2[,stat_col_num]), 0.1, f = ceiling)
axis(side = 2, at = c(0,1000,2000,3000,4000, 5000, 6000, 7000), las = 1)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#title(ylab="Frequency", line=3.7, cex.lab=1.2)
#title(xlab = "FdM", line=2.5, cex.lab=1.2)
abline(v = 0, col = "grey89", lwd = 1)
abline(v = mean(data2[,stat_col_num]), col = "navyblue", lwd = 2)
abline(v = sig0.05_threshold2, col = "red3", lwd = 2)
abline(v = abs(sig0.05_threshold2), col = "red3", lwd = 2)

max_y <- 5000
#par(mar=c(4,4.7,2,1))
par(mar=c(2,3.5,2,1))
hist(data3[,stat_col_num], breaks = 150, main = expression(paste(bolditalic("Astatotilapia"), bold(" sp. 'Ruaha blue'"))), xaxt='n', xlab = "", ylab = "", ylim = c(0, max_y), col = "slategray3", border = "slategray3", las = 1)
min <- plyr::round_any(min(data3[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data3[,stat_col_num]), 0.1, f = ceiling)
axis(side = 2, at = c(0,500,1000, 1500,2000,2500,3000,3500,4000,4500, 5000), las = 1)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#title(ylab="Frequency", line=3.7, cex.lab=1.2)
#title(xlab = "FdM", line=2.5, cex.lab=1.2)
abline(v = 0, col = "grey89", lwd = 1)
abline(v = mean(data3[,stat_col_num]), col = "navyblue", lwd = 2)
abline(v = sig0.05_threshold3, col = "red3", lwd = 2)
abline(v = abs(sig0.05_threshold3), col = "red3", lwd = 2)

max_y <- 3000
#par(mar=c(4,4.7,2,1))
par(mar=c(2,3.5,2,1))
hist(data4[,stat_col_num], breaks = 150, main = expression(paste(bolditalic("Pseudocrenilabrus"), bold(" group"))), xaxt='n', xlab = "", ylab = "", ylim = c(0, max_y), col = "slategray3", border = "slategray3", las = 1)
min <- plyr::round_any(min(data4[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data4[,stat_col_num]), 0.1, f = ceiling)
axis(side = 2, at = c(0,500,1000, 1500,2000,2500,3000), las = 1)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#title(ylab="Frequency", line=3.7, cex.lab=1.2)
#title(xlab = "FdM", line=2.5, cex.lab=1.2)
abline(v = 0, col = "grey89", lwd = 1)
abline(v = mean(data4[,stat_col_num]), col = "navyblue", lwd = 2)
abline(v = sig0.05_threshold4, col = "red3", lwd = 2)
abline(v = abs(sig0.05_threshold4), col = "red3", lwd = 2)

max_y <- 8000
#par(mar=c(4,4.7,2,1))
par(mar=c(2,3.5,2,1))
hist(data5[,stat_col_num], breaks = 150, main = expression(paste(bold("CSA group"))), xaxt='n', xlab = "", ylab = "", ylim = c(0, max_y), col = "slategray3", border = "slategray3", las = 1)
min <- plyr::round_any(min(data5[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data5[,stat_col_num]), 0.1, f = ceiling)
axis(side = 2, at = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000), las = 1)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#title(ylab="Frequency", line=3.7, cex.lab=1.2)
#title(xlab = "FdM", line=2.5, cex.lab=1.2 )
abline(v = 0, col = "grey89", lwd = 1)
abline(v = mean(data5[,stat_col_num]), col = "navyblue", lwd = 2)
abline(v = sig0.05_threshold5, col = "red3", lwd = 2)
abline(v = abs(sig0.05_threshold5), col = "red3", lwd = 2)

dev.off()

#move files from cluster to local computer:

#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dinvestigate/plots_june2023




max_y <- 8000
par(mar=c(2,3.5,2,1))
hist(data5[,stat_col_num], breaks = 150, main = "CSA", xaxt='n', xlab = "", ylab = "", ylim = c(0, max_y), col = "slategray3", border = "slategray3", las = 1)
min <- plyr::round_any(min(data5[,stat_col_num]), 0.1, f = floor)
max <- plyr::round_any(max(data5[,stat_col_num]), 0.1, f = ceiling)
axis(side = 2, at = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000), las = 1)
axis(side=1,at=round(seq(from = min,to = max, by = 0.1),1), labels=round(seq(from = min,to = max, by = 0.1),1))
#title(ylab="Frequency", line=3.7, cex.lab=1.2)
#title(xlab = "FdM", line=2.5, cex.lab=1.2 )
abline(v = 0, col = "grey89", lwd = 1)
abline(v = mean(data5[,stat_col_num]), col = "navyblue", lwd = 2)
abline(v = sig0.05_threshold5, col = "red3", lwd = 2)
abline(v = abs(sig0.05_threshold5), col = "red3", lwd = 2)



dinvestigate_dist_plot(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "fdm")
dinvestigate_dist_plot(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "fdm")
dinvestigate_dist_plot(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", stat = "fdm")

dinvestigate_dist_plot(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "d")
dinvestigate_dist_plot(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "d")
dinvestigate_dist_plot(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", stat = "d")


round(mean(data[,stat_col_num]),3)
round(mean(data2[,stat_col_num]),3)
round(mean(data3[,stat_col_num]),3)
round(mean(data4[,stat_col_num]),3)
round(mean(data5[,stat_col_num]),3)



#test whether there is a significant skew in normal distribution for each P3 group
mean(data[,stat_col_num])
median(data[,stat_col_num])
#data[,stat_col_num]
length(data[,stat_col_num])

#data order:
#data = A. gigliolii
#data2 = Orthochromis
#data3 = ruaha blue
#data4 = Pseudocrenilabrus
#data5 = CSA

#Kolmogorov-Smirnov Test for Normality:
ks.test(data[,stat_col_num], "pnorm", mean(data[,stat_col_num]), sd(data[,stat_col_num]))
ks.test(data2[,stat_col_num], "pnorm", mean(data2[,stat_col_num]), sd(data2[,stat_col_num]))
ks.test(data3[,stat_col_num], "pnorm", mean(data3[,stat_col_num]), sd(data3[,stat_col_num]))
ks.test(data4[,stat_col_num], "pnorm", mean(data4[,stat_col_num]), sd(data4[,stat_col_num]))
ks.test(data5[,stat_col_num], "pnorm", mean(data5[,stat_col_num]), sd(data5[,stat_col_num]))

#t.test(data[,stat_col_num], mu = 0)
#wilcox.test(data[,stat_col_num], mu = 0)

#shapiro test doesnt work because the sample size is too big (must be between 3 and 5000)

max(data5[,stat_col_num])

min <- plyr::round_any(min(data5[,stat_col_num]), 0.05, f = floor)
min(data5[,stat_col_num])
max <- plyr::round_any(max(data5[,stat_col_num]), 0.05, f = ceiling)

round(seq(from = min,to = max, by = 0.05),2)


#filenames

#test_data_distributions <- data_dist3
#test_data_distributions$windowsize <- as.numeric(test_data_distributions$windowEnd) - as.numeric(test_data_distributions$windowStart)



#head(test_data_distributions)
#tail(data_dist)

#head(data2)
#sig_threshold1 <- quantile(test_data_distributions[,stat_col_num], 0.05)
#sig_threshold2 <- quantile(test_data_distributions[,stat_col_num], 0.005)
#sig_threshold3 <- min(test_data_distributions[,stat_col_num])
#sig_threshold1
#sig_threshold2
#sig_threshold3

#plot distribution of values
#hist(test_data_distributions$D, breaks = 200)
#abline(v = sig_threshold1, col = "blue")
#abline(v = abs(sig_threshold1), col = "blue")
#abline(v = sig_threshold2, col = "red")
#abline(v = abs(sig_threshold2), col = "red")
#abline(v = sig_threshold3, col = "green")
#abline(v = abs(sig_threshold3), col = "green")

#sig_threshold1_sub <- test_data_distributions[which(test_data_distributions$D >= abs(sig_threshold1)),]
#head(sig_threshold1_sub)
#mean(sig_threshold1_sub$windowsize)
#(length(sig_threshold1_sub$chr)/length(test_data_distributions$chr))*100
#sig_threshold2_sub <- test_data_distributions[which(test_data_distributions$D >= abs(sig_threshold2)),]
#mean(sig_threshold2_sub$windowsize)
#(length(sig_threshold2_sub$chr)/length(test_data_distributions$chr))*100
#sig_threshold3_sub <- test_data_distributions[which(test_data_distributions$D >= abs(sig_threshold3)),]
#mean(sig_threshold3_sub$windowsize)
#(length(sig_threshold3_sub$chr)/length(test_data_distributions$chr))*100


#test <- data_sig_merge[,4:6]
#test2 <- data_sig5_merge[,4:6]
#colnames(test) <- c("chr", "start", "end")
#colnames(test2) <- c("chr", "start", "end")
#test <- bedr.sort.region(test)
#test2 <- bedr.sort.region(test2)
#test_overlap <- bedr.join.region(test, test2, check.chr = FALSE)
#test_overlap

#test[which(test$start == 1870265),]
#test2[which(test2$start == 1870265),]
#test_overlap

#test_overlap[which(test_overlap$end.b > 1),]

#install.packages("data.table")

#library(data.table)

#test_ver2 <- setDT(test)
#setkey(test_ver2, chr, start, end)
#test2_ver2 <- setDT(test2)
#setkey(test2_ver2, chr, start, end)
#over <- foverlaps(test, test2)
#over2 <- data.table(
#    chr = over$chr,
#    start = over[, ifelse(start > i.start, start, i.start)],
#    end = over[, ifelse(end < i.end, end, i.end)])
#over2[which(over2$end > 1),]


#test to see if there is a correlation along windows for the fdm values
#i.e. is the value of one window correlated with the window before it (or n number of times before it)?

#Correlation coefficient values of 1, -1 and 0 indicate a perfect positive correlation, 
#a perfect negative correlation and no correlation respectively between a window and its neighbouring 
#upstream window

#Lags refer to the time intervals between the current observation and the lagged observations. 
#For example, a lag of 1 indicates the correlation between the current observation and the one immediately before it.
#A lag of 2 indicates the correlation between the current observation and the one two time periods before it,
#and so on.

#A lag of 0 in the context of an autocorrelation function (ACF) means that you are computing the correlation between 
#the time series and itself at the same time point. In other words, it represents the correlation of the series with
#itself at no time lag, which will always be perfect (correlation strength of 1) because any time series is perfectly
#correlated with itself at the same time.



chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_50"
sig <- "quant"
quant <- 0.05
name <- "5percent"
stat <- "fdm"


#dinvestigate_autocorrelation <- function(output_folder, window, window_short, sig, quant, name, stat){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

q <- list()

###########

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}   
    
#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)
    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

data_dist <- data
data_dist2 <- data2
data_dist3 <- data3
data_dist4 <- data4
data_dist5 <- data5

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
    sig_threshold2 <- quantile(data2[,stat_col_num], quant)
    sig_threshold3 <- quantile(data3[,stat_col_num], quant)
    sig_threshold4 <- quantile(data4[,stat_col_num], quant)
    sig_threshold5 <- quantile(data5[,stat_col_num], quant)
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
    sig_threshold2 <- min(data2[,stat_col_num])
    sig_threshold3 <- min(data3[,stat_col_num])
    sig_threshold4 <- min(data4[,stat_col_num])
    sig_threshold5 <- min(data5[,stat_col_num])
}

#####

allsums <- matrix(,nrow = length(chroms), ncol = 5)   

autocorrelation_data <- data.frame(matrix(,nrow = length(chroms)*5, ncol = 7))
colnames(autocorrelation_data) <- c("P3", "Stat", "Window_size", "No_windows", "Chr", "Significant_lags", "Autocorrelation_significant_lags")

autocorrelation_data[1:22,1] <- "gigliolii"
autocorrelation_data[23:44,1] <- "orthochromis"
autocorrelation_data[45:66,1] <- "ruaha_blue"
autocorrelation_data[67:88,1] <- "Pseudo_Cteno_Ortho2"
autocorrelation_data[89:110,1] <- "Serr_Pharyng_Sarg_Thora"
autocorrelation_data[,2] <- stat
autocorrelation_data[,3] <- window_short

#read through each chromosome, select sig windows and plot
#for (a in 1:1){  
for (a in 1:length(chroms)){    
        
    chr <- chroms[a]
    #print(chr)
    accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", header = FALSE)
    #accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible.bed", sep = ""), sep = "\t", header = FALSE)
    colnames(accessible_regions) <- c("chr", "start", "end")
    
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    
    ##########
    data <- read.table(filenames[1],as.is=T,header=T)
    data$sig <- ifelse(data[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    autocorrelation_data[a,4] <- length(data$chr) #number of windows
    autocorrelation_data[a,5] <- chr
    
    #get sig autocorrelation lags
    acf_result <- acf(data$f_dM, plot = FALSE)
    significant_autocorrelation <- abs(acf_result$acf) > 2 / sqrt(length(data$f_dM))
    autocorrelation_data[a,6] <- toString(which(significant_autocorrelation)-1)
    autocorrelation_data[a,7] <- toString(round(acf_result$acf[which(significant_autocorrelation== TRUE)], digits = 3))
    
    ##########
    data2 <- read.table(filenames[2],as.is=T,header=T) 
    data2$sig <- ifelse(data2[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    autocorrelation_data[a+22,4] <- length(data2$chr) #number of windows
    autocorrelation_data[a+22,5] <- chr
    
    #get sig autocorrelation lags
    acf_result <- acf(data2$f_dM, plot = FALSE)
    significant_autocorrelation <- abs(acf_result$acf) > 2 / sqrt(length(data2$f_dM))
    autocorrelation_data[a+22,6] <- toString(which(significant_autocorrelation)-1)
    autocorrelation_data[a+22,7] <- toString(round(acf_result$acf[which(significant_autocorrelation== TRUE)], digits = 3))

    
    ##########
    data3 <- read.table(filenames[3],as.is=T,header=T)
    data3$sig <- ifelse(data3[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    autocorrelation_data[a+44,4] <- length(data3$chr) #number of windows
    autocorrelation_data[a+44,5] <- chr
    
    #get sig autocorrelation lags
    acf_result <- acf(data3$f_dM, plot = FALSE)
    significant_autocorrelation <- abs(acf_result$acf) > 2 / sqrt(length(data3$f_dM))
    autocorrelation_data[a+44,6] <- toString(which(significant_autocorrelation)-1)
    autocorrelation_data[a+44,7] <- toString(round(acf_result$acf[which(significant_autocorrelation== TRUE)], digits = 3))

    
    ##########
    data4 <- read.table(filenames[4],as.is=T,header=T)
    data4$sig <- ifelse(data4[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    autocorrelation_data[a+66,4] <- length(data4$chr) #number of windows
    autocorrelation_data[a+66,5] <- chr
    
    #get sig autocorrelation lags
    acf_result <- acf(data4$f_dM, plot = FALSE)
    significant_autocorrelation <- abs(acf_result$acf) > 2 / sqrt(length(data4$f_dM))
    autocorrelation_data[a+66,6] <- toString(which(significant_autocorrelation)-1)
    autocorrelation_data[a+66,7] <- toString(round(acf_result$acf[which(significant_autocorrelation== TRUE)], digits = 3))


    ##########
    data5 <- read.table(filenames[5],as.is=T,header=T)
    data5$sig <- ifelse(data5[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    autocorrelation_data[a+88,4] <- length(data5$chr) #number of windows
    autocorrelation_data[a+88,5] <- chr
    
    #get sig autocorrelation lags
    acf_result <- acf(data5$f_dM, plot = FALSE)
    significant_autocorrelation <- abs(acf_result$acf) > 2 / sqrt(length(data5$f_dM))
    autocorrelation_data[a+88,6] <- toString(which(significant_autocorrelation)-1)
    autocorrelation_data[a+88,7] <- toString(round(acf_result$acf[which(significant_autocorrelation== TRUE)], digits = 3))

    #save table
    write.table(autocorrelation_data, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/dinvestigate_hybridswarm_", output_folder, "_autocorrelation_", stat, "_", window_short, ".tsv", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE)

}


#}

#inversions not removed from this data - ideally would need to remove then and split the chromosome into parts so that the windows before and after the inversion are not touching

head(autocorrelation_data)

#get autocorrelation values of all lags - not just significant ones - add all up and divide by the number of chr
#calculate the average window size for each P3 group 


chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_50"
sig <- "quant"
quant <- 0.05
name <- "5percent"
stat <- "fdm"


#dinvestigate_autocorrelation_decay <- function(output_folder, window, window_short, sig, quant, name, stat){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

q <- list()

###########

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}   
    
#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)
    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

data_dist <- data
data_dist2 <- data2
data_dist3 <- data3
data_dist4 <- data4
data_dist5 <- data5

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
    sig_threshold2 <- quantile(data2[,stat_col_num], quant)
    sig_threshold3 <- quantile(data3[,stat_col_num], quant)
    sig_threshold4 <- quantile(data4[,stat_col_num], quant)
    sig_threshold5 <- quantile(data5[,stat_col_num], quant)
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
    sig_threshold2 <- min(data2[,stat_col_num])
    sig_threshold3 <- min(data3[,stat_col_num])
    sig_threshold4 <- min(data4[,stat_col_num])
    sig_threshold5 <- min(data5[,stat_col_num])
}

#####

allsums <- matrix(,nrow = length(chroms), ncol = 5)   

autocorrelation_data <- data.frame(matrix(,nrow = length(chroms)*5, ncol = 34))
colnames(autocorrelation_data) <- c("P3", "Stat", "chr", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")

autocorrelation_data[1:22,1] <- "gigliolii"
autocorrelation_data[23:44,1] <- "orthochromis"
autocorrelation_data[45:66,1] <- "ruaha_blue"
autocorrelation_data[67:88,1] <- "Pseudo_Cteno_Ortho2"
autocorrelation_data[89:110,1] <- "Serr_Pharyng_Sarg_Thora"
autocorrelation_data[,2] <- stat
autocorrelation_data[,4:34] <- 0


#read through each chromosome, select sig windows and plot
#for (a in 1:1){  
for (a in 1:length(chroms)){    
        
    chr <- chroms[a]
    #print(chr)
    accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", header = FALSE)
    #accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible.bed", sep = ""), sep = "\t", header = FALSE)
    colnames(accessible_regions) <- c("chr", "start", "end")
    
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    
    ##########
    data <- read.table(filenames[1],as.is=T,header=T)
    data$sig <- ifelse(data[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    acf_result <- acf(data$f_dM, plot = FALSE)
    autocorrelation_data[a,3] <- chr
    for (m in 1:length(acf_result$acf)){
        if (m <= 31){
            autocorrelation_data[a,m+3] <- round(acf_result$acf[m], digits = 3)
        } else {}
    }
    
    ##########
    data2 <- read.table(filenames[2],as.is=T,header=T) 
    data2$sig <- ifelse(data2[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    acf_result <- acf(data2$f_dM, plot = FALSE)
    autocorrelation_data[a+22,3] <- chr
    for (m in 1:length(acf_result$acf)){
        if (m <= 31){
            autocorrelation_data[a+22,m+3] <- round(acf_result$acf[m], digits = 3)
        } else {}
    }
    
    ##########
    data3 <- read.table(filenames[3],as.is=T,header=T)
    data3$sig <- ifelse(data3[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    acf_result <- acf(data3$f_dM, plot = FALSE)
    autocorrelation_data[a+44,3] <- chr
    for (m in 1:length(acf_result$acf)){
        if (m <= 31){
            autocorrelation_data[a+44,m+3] <- round(acf_result$acf[m], digits = 3)
        } else {}
    }
    
    ##########
    data4 <- read.table(filenames[4],as.is=T,header=T)
    data4$sig <- ifelse(data4[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    acf_result <- acf(data4$f_dM, plot = FALSE)
    autocorrelation_data[a+66,3] <- chr
    for (m in 1:length(acf_result$acf)){
        if (m <= 31){
            autocorrelation_data[a+66,m+3] <- round(acf_result$acf[m], digits = 3)
        } else {}
    }

    ##########
    data5 <- read.table(filenames[5],as.is=T,header=T)
    data5$sig <- ifelse(data5[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    acf_result <- acf(data5$f_dM, plot = FALSE)
    autocorrelation_data[a+88,3] <- chr
    for (m in 1:length(acf_result$acf)){
        if (m <= 31){
            autocorrelation_data[a+88,m+3] <- round(acf_result$acf[m], digits = 3)
        } else {}
    }
    
    #save table
    #write.table(autocorrelation_data, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/dinvestigate_hybridswarm_", output_folder, "_autocorrelation_", stat, "_", window_short, ".tsv", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE)

}

#get average autocorrelation values for each lag and P3 group

#autocorrelation_data_average <- data.frame(matrix(,nrow = 5, ncol = 33))
#colnames(autocorrelation_data_average) <- c("P3", "Stat", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")

#autocorrelation_data_average[1,1] <- "gigliolii"
#autocorrelation_data_average[2,1] <- "orthochromis"
#autocorrelation_data_average[3,1] <- "ruaha_blue"
#autocorrelation_data_average[4,1] <- "Pseudo_Cteno_Ortho2"
#autocorrelation_data_average[5,1] <- "Serr_Pharyng_Sarg_Thora"
#autocorrelation_data_average[,2] <- stat
#autocorrelation_data_average[,3:33] <- 0


#get average window sizes for each P3 group
data$window_size <- data$windowEnd - data$windowStart
data2$window_size <- data2$windowEnd - data2$windowStart
data3$window_size <- data3$windowEnd - data3$windowStart
data4$window_size <- data4$windowEnd - data4$windowStart
data5$window_size <- data5$windowEnd - data5$windowStart
datamean <- round(mean(data$window_size), digits= 0)
data2mean <- mean(data2$window_size, digits= 0)
data3mean <- mean(data3$window_size, digits= 0)
data4mean <- mean(data4$window_size, digits= 0)
data5mean <- mean(data5$window_size, digits= 0)

#head(autocorrelation_data)

#autocorrelation_data_test <- autocorrelation_data[1:22,]
#averages <- apply(autocorrelation_data_test[, -(1:3)], 2, function(col) mean(col[col != 0], na.rm = TRUE))
#averages

autocorrelation_data[autocorrelation_data == 0] <- NA
melted_data <- melt(autocorrelation_data, id.vars = c("P3", "Stat", "chr"), variable.name = "column")
melted_data$lag_corrected <- melted_data$column

#multiple lag by the average window size for the P3 group                  
melted_data <- melted_data %>%
  mutate(lag_corrected = as.factor(ifelse(P3 == "gigliolii", as.numeric(as.character(lag_corrected)) * datamean, as.character(lag_corrected))))
melted_data <- melted_data %>%
  mutate(lag_corrected = as.factor(ifelse(P3 == "orthochromis", as.numeric(as.character(lag_corrected)) * data2mean, as.character(lag_corrected))))
melted_data <- melted_data %>%
  mutate(lag_corrected = as.factor(ifelse(P3 == "ruaha_blue", as.numeric(as.character(lag_corrected)) * data3mean, as.character(lag_corrected))))
melted_data <- melted_data %>%
  mutate(lag_corrected = as.factor(ifelse(P3 == "Pseudo_Cteno_Ortho2", as.numeric(as.character(lag_corrected)) * data4mean, as.character(lag_corrected))))
melted_data <- melted_data %>%
  mutate(lag_corrected = as.factor(ifelse(P3 == "Serr_Pharyng_Sarg_Thora", as.numeric(as.character(lag_corrected)) * data5mean, as.character(lag_corrected))))

#get mean and standard deviations for each P3 and lag                  
group_summary <- melted_data %>%
  group_by(P3, column, lag_corrected) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))   

### Main figure 3 ###

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_", stat, "_", name, "_", "autocorrelation_decay.pdf", sep = ""),width=15,height=5)
p <- ggplot(group_summary, aes(x = as.numeric(as.character(lag_corrected)), y = mean, color = P3)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(x = "", y = "Autocorrelation value") +
    scale_color_manual(values = c("red3", "seagreen4", "hotpink2", "royalblue2", "purple3")) +
  scale_x_continuous(name = "lag x mean window size", breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, 250000, 275000, 300000, 325000, 350000, 375000, 400000, 425000, 450000, 475000, 500000, 525000, 550000, 575000, 600000, 625000, 650000, 675000, 700000, 725000)) +
  guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 30, size = 7)) +
  theme_bw()
print(p)
dev.off()
                  


#}

#inversions not removed from this data - ideally would need to remove then and split the chromosome into parts so that the windows before and after the inversion are not touching


acf_result

head(group_summary)

p <- ggplot(group_summary, aes(x = as.numeric(as.character(lag_corrected)), y = mean, color = P3)) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(x = "", y = "Autocorrelation value") +
    scale_color_manual(values = c("red3", "seagreen4", "hotpink2", "royalblue2", "purple3")) +
  scale_x_continuous(name = "lag x mean window size", breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, 250000, 275000, 300000, 325000, 350000, 375000, 400000, 425000, 450000, 475000, 500000, 525000, 550000, 575000, 600000, 625000, 650000, 675000, 700000, 725000)) +
  guides(color = FALSE) +
  theme(axis.text.x = element_text(angle = 30, size = 7)) +
  theme_bw()
print(p)

head(data2)
length(data4$chr)

datamean
data2mean
data3mean
data4mean
data5mean

dinvestigate_autocorrelation(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")
#dinvestigate_autocorrelation(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")
#dinvestigate_autocorrelation(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")

head(autocorrelation_data)

#dinvestigate_autocorrelation_decay(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")
#dinvestigate_autocorrelation_decay(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")
dinvestigate_autocorrelation_decay(output_folder = "P1_LVRS", window = "1000snpwindow_1000_1000", window_short = "1000_1000", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")

#define function to record occurrences of sig windows next to each other

adjacent_windows <- function(data){
    
# Initialize variables to count the occurrences and track the length of the current string
count_adjacent_sig <- 0
current_string_length <- 0

# Create an empty list to store the lengths of each group of adjacent significant rows
sig_group_lengths_2_or_more <- list()

# Loop through the rows of the dataframe (starting from the second row)
for (i in 2:nrow(data)) {
  # Check if the current row and the previous row both have 'Yes' in the 'sig' column
  if (data$sig[i] == "Yes" && data$sig[i - 1] == "Yes") {
    # If it's the start of a new string, set the current_string_length to 2 (for the two adjacent rows)
    if (current_string_length == 0) {
      current_string_length <- 2
    } else {
      # If it's not the start of a new string, increment the current_string_length
      current_string_length <- current_string_length + 1
    }
  } else {
    # If there is a break in the string, update the count and store the current_string_length
    if (current_string_length >= 2) {
      count_adjacent_sig <- count_adjacent_sig + 1
      sig_group_lengths_2_or_more[[length(sig_group_lengths_2_or_more) + 1]] <- current_string_length
    }
    # Reset the current_string_length
    current_string_length <- 0
  }
}

# Check if there was an ongoing string of significant rows at the end of the dataframe
if (current_string_length >= 2) {
  count_adjacent_sig <- count_adjacent_sig + 1
  sig_group_lengths_2_or_more[[length(sig_group_lengths_2_or_more) + 1]] <- current_string_length
}
    
# Print the count of instances with a string of significant rows
#count_adjacent_sig

#breakdown of sig strings and their lengths
#sig_group_lengths_2_or_more

#total number of significant windows
total_significant_rows <- sum(data$sig == "Yes")

# Calculate the total number of significant rows contained within these strings
lengths_values <- unlist(sig_group_lengths_2_or_more)
lengths_values_table <- table(lengths_values)
total_sig_rows_in_strings <- sum(lengths_values)

#percentage of windows in a string of 2 or more sig windows
#percent_windows_over_2_length <- round(total_sig_rows_in_strings/total_significant_rows*100, 1)
    
return(list(total_significant_rows = total_significant_rows,
            total_sig_rows_in_strings = total_sig_rows_in_strings,
            lengths_values_table = lengths_values_table))
}

chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

library(ggplot2)
library(tidyr)

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_50"
sig <- "quant"
quant <- 0.005
name <- "0.5percent"
stat <- "fdm"

#0.05 quant = 5percent
#0.005 quant = 0.5percent

dinvestigate_adjacent_sig_windows <- function(output_folder, window, window_short, sig, quant, name, stat){

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

q <- list()

###########

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}   
    
#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)
data2 <- read.table(filenames[2],as.is=T,header=T)
data3 <- read.table(filenames[3],as.is=T,header=T)
data4 <- read.table(filenames[4],as.is=T,header=T)
data5 <- read.table(filenames[5],as.is=T,header=T)

#for (a in 1:1){  
for (t in 2:length(chroms)){    
    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
    data2_a <- read.table(filenames[2],as.is=T,header=T)
    data2 <- rbind(data2, data2_a)
    data3_a <- read.table(filenames[3],as.is=T,header=T)
    data3 <- rbind(data3, data3_a)
    data4_a <- read.table(filenames[4],as.is=T,header=T)
    data4 <- rbind(data4, data4_a)
    data5_a <- read.table(filenames[5],as.is=T,header=T)
    data5 <- rbind(data5, data5_a)
}

data_dist <- data
data_dist2 <- data2
data_dist3 <- data3
data_dist4 <- data4
data_dist5 <- data5

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
    sig_threshold2 <- quantile(data2[,stat_col_num], quant)
    sig_threshold3 <- quantile(data3[,stat_col_num], quant)
    sig_threshold4 <- quantile(data4[,stat_col_num], quant)
    sig_threshold5 <- quantile(data5[,stat_col_num], quant)
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
    sig_threshold2 <- min(data2[,stat_col_num])
    sig_threshold3 <- min(data3[,stat_col_num])
    sig_threshold4 <- min(data4[,stat_col_num])
    sig_threshold5 <- min(data5[,stat_col_num])
}

#####

allsums <- matrix(,nrow = length(chroms), ncol = 5)   

adjacent_data <- data.frame(matrix(,nrow = 5, ncol = 28))
#colnames(adjacent_data) <- c("P3", "Stat", "total_sig_windows", "sig_windows_adj", "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7", "adj_8", "adj_9", "adj_10", "adj_11", "adj_12", "adj_13", "adj_14", "adj_15", "adj_16", "adj_17", "adj_18", "adj_19", "adj_20", "adj_21", "adj_22", "adj_23", "adj_24", "adj_25", "adj_26", "adj_27", "adj_28", "adj_29", "adj_30", "adj_31", "adj_32", "adj_33", "adj_34", "adj_35", "adj_36", "adj_37", "adj_38", "adj_48", "adj_50", "adj_74")
colnames(adjacent_data) <- c("P3", "Stat", "total_sig_windows", "sig_windows_adj", "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7", "adj_8", "adj_9", "adj_10", "adj_11", "adj_12", "adj_13", "adj_14", "adj_15", "adj_16", "adj_17", "adj_18", "adj_19", "adj_20", "adj_21", "adj_22", "adj_23", "adj_24", "adj_25")


adjacent_data[1,1] <- "gigliolii"
adjacent_data[2,1] <- "orthochromis"
adjacent_data[3,1] <- "Pseudo_Cteno_Ortho2"
adjacent_data[4,1] <- "ruaha_blue"
adjacent_data[5,1] <- "Serr_Pharyng_Sarg_Thora"
adjacent_data[,2] <- stat
adjacent_data[,3:28] <- 0

#read through each chromosome, select sig windows and plot
#for (a in 1:1){  
for (a in 1:length(chroms)){    
        
    chr <- chroms[a]
    
    #get inversion dimensions
    if (!is.na(match(chr, inversions$V1))){
        n <- match(chr, inversions$V1)
        invstart <- inversions$V2[n]
        invend <- inversions$V3[n]
    } else {
        invstart <- 0
        invend <- 0
    }
    
    #remove windows which are found in inversion regions
    
    
    #print(chr)
    accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", header = FALSE)
    #accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible.bed", sep = ""), sep = "\t", header = FALSE)
    colnames(accessible_regions) <- c("chr", "start", "end")
    
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    
    ##########
    data <- read.table(filenames[1],as.is=T,header=T)
    data$sig <- ifelse(data[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    data_noinversion <- data[!(data$windowStart >= invstart & data$windowEnd <= invend) & #remove windows completely in inversion
                   !(data$windowStart <= invend & data$windowEnd >= invstart), ] #remove windows which partially overlap with the inversion
    adjacent_windows_result <- adjacent_windows(data_noinversion)
    
    adjacent_data[1,3] <- adjacent_data[1,3] + adjacent_windows_result$total_significant_rows
    if(adjacent_windows_result$total_sig_rows_in_strings > 0){
        adjacent_data[1,4] <- adjacent_data[1,4] + adjacent_windows_result$total_sig_rows_in_strings
        #for (p in 1:length(adjacent_windows_result$lengths_values_table)){
        #    adjacent_data[1,as.numeric(names(adjacent_windows_result$lengths_values_table))[p]+3] <- adjacent_data[1,as.numeric(names(adjacent_windows_result$lengths_values_table))[p]+3] + adjacent_windows_result$lengths_values_table[p]
        #}
        for (p in 1:length(adjacent_windows_result$lengths_values_table)) {
            col_name <- paste0("adj_", names(adjacent_windows_result$lengths_values_table)[p])
            adjacent_data[1, col_name] <- adjacent_data[1, col_name] + adjacent_windows_result$lengths_values_table[p]
        }
    }
    
    ##########
    data2 <- read.table(filenames[2],as.is=T,header=T) 
    data2$sig <- ifelse(data2[,stat_col_num] >= abs(sig_threshold2), "Yes", "No")
    data2_noinversion <- data2[!(data2$windowStart >= invstart & data2$windowEnd <= invend) & #remove windows completely in inversion
                   !(data2$windowStart <= invend & data2$windowEnd >= invstart), ] #remove windows which partially overlap with the inversion
    adjacent_windows_result2 <- adjacent_windows(data2_noinversion)
    #print(adjacent_windows_result2$total_significant_rows)
    
    adjacent_data[2,3] <- adjacent_data[2,3] + adjacent_windows_result2$total_significant_rows
    if(adjacent_windows_result2$total_sig_rows_in_strings > 0){
        adjacent_data[2,4] <- adjacent_data[2,4] + adjacent_windows_result2$total_sig_rows_in_strings
        #for (p in 1:length(adjacent_windows_result2$lengths_values_table)){
        #    adjacent_data[2,as.numeric(names(adjacent_windows_result2$lengths_values_table))[p]+3] <- adjacent_data[2,as.numeric(names(adjacent_windows_result2$lengths_values_table))[p]+3] + adjacent_windows_result2$lengths_values_table[p]
        #}
        for (p in 1:length(adjacent_windows_result2$lengths_values_table)) {
            col_name <- paste0("adj_", names(adjacent_windows_result2$lengths_values_table)[p])
            adjacent_data[2, col_name] <- adjacent_data[2, col_name] + adjacent_windows_result2$lengths_values_table[p]
        }
    }
    
    ##########
    data3 <- read.table(filenames[3],as.is=T,header=T)
    data3$sig <- ifelse(data3[,stat_col_num] >= abs(sig_threshold3), "Yes", "No")
    data3_noinversion <- data3[!(data3$windowStart >= invstart & data3$windowEnd <= invend) & #remove windows completely in inversion
                   !(data3$windowStart <= invend & data3$windowEnd >= invstart), ] #remove windows which partially overlap with the inversion
    adjacent_windows_result3 <- adjacent_windows(data3_noinversion)
    
    adjacent_data[3,3] <- adjacent_data[3,3] + adjacent_windows_result3$total_significant_rows
    if(adjacent_windows_result3$total_sig_rows_in_strings > 0){
        adjacent_data[3,4] <- adjacent_data[3,4] + adjacent_windows_result3$total_sig_rows_in_strings
        #for (p in 1:length(adjacent_windows_result3$lengths_values_table)){
        #    adjacent_data[3,as.numeric(names(adjacent_windows_result3$lengths_values_table))[p]+3] <- adjacent_data[3,as.numeric(names(adjacent_windows_result3$lengths_values_table))[p]+3] + adjacent_windows_result3$lengths_values_table[p]
        #}
        for (p in 1:length(adjacent_windows_result3$lengths_values_table)) {
            col_name <- paste0("adj_", names(adjacent_windows_result3$lengths_values_table)[p])
            adjacent_data[3, col_name] <- adjacent_data[3, col_name] + adjacent_windows_result3$lengths_values_table[p]
        }
    }
    
    ##########
    data4 <- read.table(filenames[4],as.is=T,header=T)
    data4$sig <- ifelse(data4[,stat_col_num] >= abs(sig_threshold4), "Yes", "No")
    data4_noinversion <- data4[!(data4$windowStart >= invstart & data4$windowEnd <= invend) & #remove windows completely in inversion
                   !(data4$windowStart <= invend & data4$windowEnd >= invstart), ] #remove windows which partially overlap with the inversion
    adjacent_windows_result4 <- adjacent_windows(data4_noinversion)
    
    adjacent_data[4,3] <- adjacent_data[4,3] + adjacent_windows_result4$total_significant_rows
    if(adjacent_windows_result4$total_sig_rows_in_strings > 0){
        adjacent_data[4,4] <- adjacent_data[4,4] + adjacent_windows_result4$total_sig_rows_in_strings
        #for (p in 1:length(adjacent_windows_result4$lengths_values_table)){
        #    adjacent_data[4,as.numeric(names(adjacent_windows_result4$lengths_values_table))[p]+3] <- adjacent_data[4,as.numeric(names(adjacent_windows_result4$lengths_values_table))[p]+3] + adjacent_windows_result4$lengths_values_table[p]
        #}
        for (p in 1:length(adjacent_windows_result4$lengths_values_table)) {
            col_name <- paste0("adj_", names(adjacent_windows_result4$lengths_values_table)[p])
            adjacent_data[4, col_name] <- adjacent_data[4, col_name] + adjacent_windows_result4$lengths_values_table[p]
        }
    }    

    ##########
    data5 <- read.table(filenames[5],as.is=T,header=T)
    data5$sig <- ifelse(data5[,stat_col_num] >= abs(sig_threshold5), "Yes", "No")
    data5_noinversion <- data5[!(data5$windowStart >= invstart & data5$windowEnd <= invend) & #remove windows completely in inversion
                   !(data5$windowStart <= invend & data5$windowEnd >= invstart), ] #remove windows which partially overlap with the inversion
    adjacent_windows_result5 <- adjacent_windows(data5_noinversion)
    
    adjacent_data[5,3] <- adjacent_data[5,3] + adjacent_windows_result5$total_significant_rows
    if(adjacent_windows_result5$total_sig_rows_in_strings > 0){
        adjacent_data[5,4] <- adjacent_data[5,4] + adjacent_windows_result5$total_sig_rows_in_strings
        #for (p in 1:length(adjacent_windows_result5$lengths_values_table)){
        #    adjacent_data[5,as.numeric(names(adjacent_windows_result5$lengths_values_table))[p]+3] <- adjacent_data[5,as.numeric(names(adjacent_windows_result5$lengths_values_table))[p]+3] + adjacent_windows_result5$lengths_values_table[p]
        #}
        for (p in 1:length(adjacent_windows_result5$lengths_values_table)) {
            col_name <- paste0("adj_", names(adjacent_windows_result5$lengths_values_table)[p])
            adjacent_data[5, col_name] <- adjacent_data[5, col_name] + adjacent_windows_result5$lengths_values_table[p]
        }
    }

}

#save table
write.table(adjacent_data, paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/dinvestigate_hybridswarm_", output_folder, "_adjacent_windows_", stat, "_", window_short, "_", sig, "_", name,".tsv", sep = ""), quote = FALSE, sep = "\t", row.names = FALSE)


#make bar plot of data
# Select only the columns starting with "adj"
adj_data <- adjacent_data %>%
  select(P3, starts_with("adj"))

# Reshape the data to long format
adj_data_long <- adj_data %>%
  gather(key = "Adj_Column", value = "Value", -P3)
#head(adj_data_long)

custom_order <- c(
  "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7",
  "adj_8", "adj_9", "adj_10", "adj_11", "adj_12",
  "adj_13", "adj_14", "adj_15", "adj_16", "adj_17",
  "adj_18", "adj_19", "adj_20", "adj_21", "adj_22", "adj_23", "adj_24", "adj_25"
)

adj_data_long$Adj_Column <- factor(adj_data_long$Adj_Column, levels = custom_order)

pdf(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/dinvestigate_hybridswarm_", output_folder, "_", window_short, "_", stat, "_", name, "adjacent_sig_windows.pdf", sep = ""),width=15,height=7)
# Plot the grouped bar chart
plot1 <- ggplot(adj_data_long, aes(x = Adj_Column, y = Value, fill = P3)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = paste(stat, window, name, "- significant windows adjacent to n number of other significant windows", sep = " "),
       x = "adj", y = "Value") +
  theme_minimal()
print(plot1)
dev.off()

}

#calculate what percentage of windows are adjacent to another

#move plots and tables to local comp:
#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/plots/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dinvestigate/plots_june2023
#rsync -avh hsnode2:/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/_data/results/ ~/Dropbox/PhD/Project/Malawi_Outgroup_callset_2021/analyses/dinvestigate/tables_june2023


#adjacent_data

dinvestigate_adjacent_sig_windows(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")
#dinvestigate_adjacent_sig_windows(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "fdm", sig = "quant", quant = 0.05, name = "5percent")

#dinvestigate_adjacent_sig_windows(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "fdm", sig = "quant", quant = 0.005, name = "0.5percent")
#dinvestigate_adjacent_sig_windows(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "fdm", sig = "quant", quant = 0.005, name = "0.5percent")

#dinvestigate_adjacent_sig_windows(output_folder = "P1_LVRS", window = "50snpwindow_50_50", window_short = "50_50", stat = "fdm", sig = "min", quant = 0, name = "minnegative")
#dinvestigate_adjacent_sig_windows(output_folder = "P1_LVRS", window = "100snpwindow_100_100", window_short = "100_100", stat = "fdm", sig = "min", quant = 0, name = "minnegative")



adjacent_data
#head(inversions)
#head(accessible_regions)

#what is the CSA window which has many adjacent sig windows
data_dist5$sig <- ifelse(data_dist5[,stat_col_num] >= abs(sig_threshold5), "Yes", "No")
tail(data_dist5)

runs <- rle(data_dist5$sig)
true_runs <- runs$values == "Yes" & runs$lengths == 15
#true_runs
true_run_indices <- which(true_runs)
true_run_indices
row_numbers <- unlist(sapply(true_run_indices, function(i) seq(sum(runs$lengths[1:i - 1]) + 1, sum(runs$lengths[1:i]))))
#row_numbers[1] - 10
new_row_numbers <- c(26632:26666)
window <- data_dist5[new_row_numbers,]
window
plot(window$f_dM)

#does the serranochromis haplotype have a sig fdm?

window = "50snpwindow_50_50"
chr <- chroms[18]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
data5 <- read.table(filenames[5],as.is=T,header=T)
#data5 <- read.table(filenames[1],as.is=T,header=T)
#head(data5)

#whole haplotype:
#LS420036.1 35276054 35299846
#shorter haplotype:
#LS420036.1 35276717 35285829

#large haplo size = 23792
#35299846 - 35276054

#small haplo size = 9112
#35285829 - 35276717

data5$sig <- ifelse(data5[,stat_col_num] >= abs(sig_threshold5), "Yes", "No")
data5_ver2 <- data5[(data5$windowStart >= 35276054 & data5$windowEnd <= 35299846), ]
head(data5_ver2)


#what percentage of sig windows are shared between different P3 groups tested?
#is it higher than what would be expected by chance given that 
#e.g. orthochromis covers 5% of the genome and ruaha blue covers 7%? - what is the expected random overlap here?

dev.off()



#difference in candidate windows in sections 6 and 8 - why is this
#fixed error now

#method 1
chroms <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
           "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr22", "chr23")

summarystats <- data.frame(matrix(nrow = 90, ncol = 12))
colnames(summarystats) <- c("P1", "window", "sig_selection", "threshold_percent", "stat", "P3", "threshold_value", 
                            "n_total_windows", "n_sig_windows", "percent_sig_windows", "mean_windowsize", "mean_sig_windowsize")

#test
output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_25"
sig = "quant"
quant = 0.005
name = "0.5percent"
stat = "fdm"
row = 1

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}       
    
summarystats[row:(row+4),1] <- output_folder
summarystats[row:(row+4),2] <- window_short
summarystats[row:(row+4),3] <- sig
summarystats[row:(row+4),4] <- quant
summarystats[row:(row+4),5] <- stat
summarystats[row:(row+4),6] <- c("gigliolii", "orthochromis", "Pseudo_Cteno_Ortho2", "ruaha_blue", "Serr_Pharyng_Sarg_Thora")

#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)

for (t in 2:length(chroms)){    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
}

data$windowsize <- as.numeric(data$windowEnd) - as.numeric(data$windowStart)

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
 
    summarystats[row,7] <- sig_threshold
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
    
    summarystats[row,7] <- sig_threshold
}

#total number of windows
summarystats[row,8] <- length(data$chr)

#subset to get sig windows
sig_threshold_sub <- data[which(data[,stat_col_num] >= abs(sig_threshold)),]

#total number of sig windows
summarystats[row,9] <- length(sig_threshold_sub$chr)


#percentage of sig windows
summarystats[row,10] <- (length(sig_threshold_sub$chr)/length(data$chr))*100

#mean window size of all windows
summarystats[row,11] <- mean(data$windowsize)

#mean window size of sig windows
summarystats[row,12] <- mean(sig_threshold_sub$windowsize)

#return the dataframe
summarystats <<- summarystats


mean(sig_threshold_sub$windowsize)
length(sig_threshold_sub$chr)

#method 2

#get max length of all chromosomes
accessible <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/accessible_genome_length.tsv", sep = "\t", header = TRUE)
inversions <- read.csv("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo/_data/inversions_info/inversions.bed", sep = "\t", header = FALSE)

lineweight <- 5

output_folder = "P1_LVRS"
window = "50snpwindow_50_50"
window_short = "50_50"
sig <- "quant"
quant <- 0.005
name <- "0.5percent"
stat <- "fdm"

setwd(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/dsuite/dinvestigate_hybridswarm_june2023/files/", output_folder, "/", sep = ""))    

q <- list()

###########

if (stat == "fdm"){
    stat_col_num <- 6
} else if (stat == "d"){
    stat_col_num <- 4
}   
    
#read in all data files and combine into one file in order to calculate sig thresholds based on genome wide d/fdm distributions
chr <- chroms[1]
filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)    
data <- read.table(filenames[1],as.is=T,header=T)

#for (a in 2:2){  
for (t in 2:length(chroms)){    
    
    chr <- chroms[t]
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)   
    data_a <- read.table(filenames[1],as.is=T,header=T)
    data <- rbind(data, data_a)
}

data_dist <- data

#for each data file find the negative threshold for the 5% and 0.5% quantile, and also the most negative value
if (sig == "quant"){
    sig_threshold <- quantile(data[,stat_col_num], quant)
} else if (sig == "min"){
    sig_threshold <- min(data[,stat_col_num])
}

length(data$chr)

sig_threshold_sub <- data[which(data[,stat_col_num] >= abs(sig_threshold)),]

#total number of sig windows
length(sig_threshold_sub$chr)

(length(sig_threshold_sub$chr)/length(data$chr))*100

#####

allsums <- matrix(,nrow = length(chroms), ncol = 5)   

adjacent_data <- data.frame(matrix(,nrow = 5, ncol = 28))
#colnames(adjacent_data) <- c("P3", "Stat", "total_sig_windows", "sig_windows_adj", "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7", "adj_8", "adj_9", "adj_10", "adj_11", "adj_12", "adj_13", "adj_14", "adj_15", "adj_16", "adj_17", "adj_18", "adj_19", "adj_20", "adj_21", "adj_22", "adj_23", "adj_24", "adj_25", "adj_26", "adj_27", "adj_28", "adj_29", "adj_30", "adj_31", "adj_32", "adj_33", "adj_34", "adj_35", "adj_36", "adj_37", "adj_38", "adj_48", "adj_50", "adj_74")
colnames(adjacent_data) <- c("P3", "Stat", "total_sig_windows", "sig_windows_adj", "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7", "adj_8", "adj_9", "adj_10", "adj_11", "adj_12", "adj_13", "adj_14", "adj_15", "adj_16", "adj_17", "adj_18", "adj_19", "adj_20", "adj_21", "adj_22", "adj_23", "adj_24", "adj_25")


adjacent_data[1,1] <- "gigliolii"
adjacent_data[2,1] <- "orthochromis"
adjacent_data[3,1] <- "ruaha_blue"
adjacent_data[4,1] <- "Pseudo_Cteno_Ortho2"
adjacent_data[5,1] <- "Serr_Pharyng_Sarg_Thora"
adjacent_data[,2] <- stat
adjacent_data[,3:28] <- 0

#for (a in 1:1){
for (a in 1:length(chroms)){    
        
    chr <- chroms[a]
    
    #get inversion dimensions
    if (!is.na(match(chr, inversions$V1))){
        n <- match(chr, inversions$V1)
        invstart <- inversions$V2[n]
        invend <- inversions$V3[n]
    } else {
        invstart <- 0
        invend <- 0
    }
    
    #remove windows which are found in inversion regions
    
    #print(chr)
    accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible_inversions_removed.bed", sep = ""), sep = "\t", header = FALSE)
    #accessible_regions <- read.csv(paste("/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/distance_in_windows/_data/inputfiles/accessible_bed/all_sites.Malawicallsetfeb2021.sf_stringent1.", chr, ".accessible.bed", sep = ""), sep = "\t", header = FALSE)
    colnames(accessible_regions) <- c("chr", "start", "end")
    
    filenames <- list.files(pattern=paste("*",chr,"_hybridswarm_", window, sep = ""), full.names = TRUE)
    l <- length(filenames)      
    
    ##########
    data <- read.table(filenames[1],as.is=T,header=T)
    data$sig <- ifelse(data[,stat_col_num] >= abs(sig_threshold), "Yes", "No")
    data_noinversion <- data[!(data$windowStart >= invstart & data$windowEnd <= invend) & #remove windows completely in inversion
                   !(data$windowStart <= invend & data$windowEnd >= invstart), ] #remove windows which partially overlap with the inversion
    adjacent_windows_result <- adjacent_windows(data_noinversion)
    
    adjacent_data[1,3] <- adjacent_data[1,3] + adjacent_windows_result$total_significant_rows
    if(adjacent_windows_result$total_sig_rows_in_strings > 0){
        adjacent_data[1,4] <- adjacent_data[1,4] + adjacent_windows_result$total_sig_rows_in_strings
        for (p in 1:length(adjacent_windows_result$lengths_values_table)) {
            col_name <- paste0("adj_", names(adjacent_windows_result$lengths_values_table)[p])
            adjacent_data[1, col_name] <- adjacent_data[1, col_name] + adjacent_windows_result$lengths_values_table[p]
        }
    }
}



adjacent_windows_result
adjacent_data

head(data_noinversion)

#define function to record occurrences of sig windows next to each other

adjacent_windows <- function(data){
    
# Initialize variables to count the occurrences and track the length of the current string
count_adjacent_sig <- 0
current_string_length <- 0

# Create an empty list to store the lengths of each group of adjacent significant rows
sig_group_lengths_2_or_more <- list()

# Loop through the rows of the dataframe (starting from the second row)
for (i in 2:nrow(data)) {
  # Check if the current row and the previous row both have 'Yes' in the 'sig' column
  if (data$sig[i] == "Yes" && data$sig[i - 1] == "Yes") {
    # If it's the start of a new string, set the current_string_length to 2 (for the two adjacent rows)
    if (current_string_length == 0) {
      current_string_length <- 2
    } else {
      # If it's not the start of a new string, increment the current_string_length
      current_string_length <- current_string_length + 1
    }
  } else {
    # If there is a break in the string, update the count and store the current_string_length
    if (current_string_length >= 2) {
      count_adjacent_sig <- count_adjacent_sig + 1
      sig_group_lengths_2_or_more[[length(sig_group_lengths_2_or_more) + 1]] <- current_string_length
    }
    # Reset the current_string_length
    current_string_length <- 0
  }
}

# Check if there was an ongoing string of significant rows at the end of the dataframe
if (current_string_length >= 2) {
  count_adjacent_sig <- count_adjacent_sig + 1
  sig_group_lengths_2_or_more[[length(sig_group_lengths_2_or_more) + 1]] <- current_string_length
}
    
# Print the count of instances with a string of significant rows
#count_adjacent_sig

#breakdown of sig strings and their lengths
#sig_group_lengths_2_or_more

#total number of significant windows
total_significant_rows <- sum(data$sig == "Yes")

# Calculate the total number of significant rows contained within these strings
lengths_values <- unlist(sig_group_lengths_2_or_more)
lengths_values_table <- table(lengths_values)
total_sig_rows_in_strings <- sum(lengths_values)

#percentage of windows in a string of 2 or more sig windows
#percent_windows_over_2_length <- round(total_sig_rows_in_strings/total_significant_rows*100, 1)
    
return(list(total_significant_rows = total_significant_rows,
            total_sig_rows_in_strings = total_sig_rows_in_strings,
            lengths_values_table = lengths_values_table))
}

sum(data_noinversion$sig == "Yes")

sig_threshold_sub <- data_noinversion[which(data_noinversion[,stat_col_num] >= abs(sig_threshold)),]

#total number of sig windows
length(sig_threshold_sub$chr)


