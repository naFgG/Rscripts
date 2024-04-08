rm(list=ls())
source("utilities.R")
library(reshape2)
library(ggalluvial)
library(ggrepel)

args <- commandArgs(T)
# args <- c("asv_table.xls", "taxonomy.xls", "metadata.xls", "sankey.pdf")
'''
asv_table.xls: rownames=ASV, colnames=Sample
taxonomy.xls: ASV -> Tax
metadata.xls: Sample -> Group
'''


asv <- read.table(args[1], sep="\t", header=T, check.names=F, quote="")
asv$ASV <- factor(asv$ASV, levels=asv$ASV)

tax <- read.table(args[2], sep="\t", header=T, check.names=F, quote="")
tax$Tax <- factor(tax$Tax, levels=unique(tax$Tax))

meta <- read.table(args[3], sep="\t", header=T, check.names=F, quote="")
meta$Sample <- factor(meta$Sample, levels=meta$Sample)
meta$Group <- factor(meta$Group, levels=unique(meta$Group))

asv <- melt(asv)
asv <- merge(asv, tax, by="ASV", sort=F)
colnames(asv)[2: 3] <- c("Sample", "Abundance")
df <- merge(asv, meta, by="Sample", sort=F)

df.plot <- melt(df, id="Abundance")
colnames(df.plot)[2: 3] <- c("category", "info")
category <- summary(df.plot$category)
df.plot$flow <- rep(1: category[1], length(category))

p <- ggplot(df.plot, aes(x=category, y=Abundance, stratum=info, alluvium=flow, fill=info)) +
  #> 画出柱形图
  geom_stratum(fill="white") + 
  #> 画出连线
  geom_flow() + 
  geom_text(stat='stratum', aes(label=after_stat(stratum))) + 
  labs(x="", y="") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), 
        axis.text.y=element_blank(), 
        axis.text.x=element_text(color="black"),
        legend.position="none") +
  scale_fill_manual(values=c(color_sample[1: dim(meta)[1]], 
                             color_otu[1: length(unique(tax$ASV))], 
                             color_phylum[1: length(unique(tax$Tax))], 
                             color_group[1: length(unique(meta$Group))])) +
  scale_x_discrete(limits=c('Tax', 'ASV', 'Sample', 'Group'))

pdf(args[4])
p
dev.off()
