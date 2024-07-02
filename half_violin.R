rm(list=ls())
library(gghalves)
library(dplyr)
library(tidyverse)
library(reshape2)

meta <- read.table(样本-信息表, sep="\t", header=T, quote="", check.names=F, comment.char="")
colnames(meta)[1] <- "Sample"
meta$Group <- factor(meta$Group, levels=unique(meta$Group))
phylum <- read.table(丰度表, sep="\t", header=T, quote="", check.names=F)
phylum <- phylum %>% filter(Taxonomy=="Firmicutes" | Taxonomy=="Bacteroidota")
phylum$Taxonomy <- factor(phylum$Taxonomy, levels=phylum$Taxonomy)

ave <- phylum %>% 
  column_to_rownames("Taxonomy") %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("Sample")
ave <- merge(ave, meta, "Sample", sort=F) %>% column_to_rownames("Sample")
ave <- ave %>% 
  group_by(Group) %>% 
  summarise(across(everything(), ~mean(.x, na.rm=T))) %>%
  column_to_rownames("Group")

df <- melt(phylum)
colnames(df) <- c("tax", "Sample", "abundance")
df <- merge(df, meta, by="Sample")

# 两种不同的样式
p <- ggplot() + 
  geom_half_violin(data=df, aes(x=Group, y=abundance, split=tax, fill=tax), 
                   position="identity", scale="width", bw="nrd") + 
  labs(x="", y="Relative abundance") +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))
ggsave('half_volin2.png', p, width=4, height=3)

p <- ggplot() + 
  geom_half_violin(data=df, aes(x=Group, y=abundance, split=tax, fill=tax), 
                   position="identity") + 
  labs(x="", y="Relative abundance") +
  theme_classic() +
  guides(fill=guide_legend(title=NULL))
ggsave('half_volin1.png', p, width=4, height=3)


