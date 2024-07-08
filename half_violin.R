rm(list=ls())
library(gghalves)
library(dplyr)
library(tidyverse)
library(reshape2)

args <- commandArgs(T)
# args <- c("sample.group.xls", "phylum.xls", "plot.pdf")

meta <- read.table(args[1], sep="\t", header=T, quote="", check.names=F, comment.char="")
meta$Group <- factor(meta$Group, levels=unique(meta$Group))
taxon <- read.table(args[2], sep="\t", header=T, quote="", check.names=F)
taxon <- taxon %>% filter(Taxonomy=="Firmicutes" | Taxonomy=="Bacteroidota")
taxon$Taxonomy <- factor(taxon$Taxonomy, levels=taxon$Taxonomy)

df <- melt(taxon)
colnames(df) <- c("tax", "Sample", "abundance")
df <- merge(df, meta, by="Sample")

p <- ggplot() + 
  geom_half_violin(data=df, aes(x=Group, y=abundance, split=tax, fill=tax), 
                   position="identity") + 
  labs(x="", y="Relative abundance") +
  theme_bw() +
  theme(panel.grid=element_blank(), 
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=10, color="black"),
        legend.position="inside",
        legend.position.inside=c(0.99, 0.99),
        legend.justification=c("right", "top")) +
  scale_fill_manual(values=c("#3275A0", "#E6842E")) +
  guides(fill=guide_legend(title=NULL))

pdf(args[3])
p
dev.off()


