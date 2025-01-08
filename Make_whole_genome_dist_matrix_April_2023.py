#!/usr/bin/env python
# coding: utf-8

# In[4]:


#create NJ trees for whole genome
#with ancestral sample and inversions removed

import pandas as pd
import skbio
import numpy as np
import re, os
#from pypopgen3.modules import diversity
jn = os.path.join
import sys
sys.path.append("/data/antwerpen/grp/asvardal/hs_tools")
import pypopgen3
from pypopgen3.modules import diversity as dv


# In[5]:


ana_dir = "/scratch/antwerpen/grp/asvardal/projects/cichlid/analyses/2019CallsetInvestigation_Sophie/analyses/callsetFeb2021/phylo"


# In[5]:


#create a distance matrix for the whole genome (sum of all chromosome dist matrices)
#for each chr divide the distance matrix by the size of the accessible genome size before adding to the wg matrix

CHROMOSOMES = [f'chr{i}' for i in range(1,24) if i!= 21]
accessible = pd.read_csv(f"{ana_dir}/_data/inversions_info/accessible_genome_length.tsv", sep='\t')

#create empty dataframe for the whole genome distance matrix
whole_genome = pd.DataFrame(np.zeros((622,622)))

for chrom in CHROMOSOMES:
    pairwise_differences = pd.read_csv(f"{ana_dir}/_data/results/distancematrix/Malawicallsetfeb2021.pass.snps.biallelic.anc_samp.noninversion.{chrom}.allsamples.allsites.distance.dist.april2023.dist",
                sep='\t',header=None)
    chr_num = int(chrom.split('chr')[1]) - 1
    #print(chr_num)
    
    #divide by the size of the accesible genome
    #chr 21 is missing so is excluded from the loop:
    if chr_num <= 19:
        accessible_number = accessible["accessible-inversion"][chr_num]
    elif chr_num >= 21:
        chr_num2 = chr_num - 1
        accessible_number = accessible["accessible-inversion"][chr_num2]

    #add the chr distance matrix to the whole genome matrix
    whole_genome = whole_genome + pairwise_differences/accessible_number  

    
whole_genome.to_csv(f"{ana_dir}/_data/results/wholegenomedist_april2023.csv")


# In[28]:


#version 2 - hannes correction
#22.06.2024

#create a distance matrix for the whole genome
#sum all matrices together
#then divide by the sum of the accessible genome size

CHROMOSOMES = [f'chr{i}' for i in range(1,24) if i!= 21]
accessible = pd.read_csv(f"{ana_dir}/_data/inversions_info/accessible_genome_length.tsv", sep='\t')

#create empty dataframe for the whole genome distance matrix
whole_genome = pd.DataFrame(np.zeros((622,622)))

for chrom in CHROMOSOMES:
    pairwise_differences = pd.read_csv(f"{ana_dir}/_data/results/distancematrix/Malawicallsetfeb2021.pass.snps.biallelic.anc_samp.noninversion.{chrom}.allsamples.allsites.distance.dist.april2023.dist",
                sep='\t',header=None)
    chr_num = int(chrom.split('chr')[1]) - 1
    
    #sum all matrices together
    whole_genome = whole_genome + pairwise_differences
    
#divide by the sum of the accesible genome size
whole_genome = whole_genome/accessible['accessible-inversion'].sum()
    
whole_genome.to_csv(f"{ana_dir}/_data/results/wholegenomedist_june2024.csv")


# In[27]:


whole_genome


# In[19]:


whole_genome = whole_genome/accessible['accessible-inversion'].sum()
whole_genome


# In[ ]:




