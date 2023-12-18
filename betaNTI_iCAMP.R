rm(list=ls())
library(iCAMP)
library(treeio)
library(reshape2)
library(tidyverse)

mapping <- read.table("16S/mapping.txt", sep="\t", check.names=F, header=T, quote="", row.names=1)
mapping$Group <- factor(mapping$Group, levels=unique(mapping$Group))
tree <- read.tree("16S/rep_phylo.tre")
asv <- read.table("16S/feature_54samples_filtered_16S.xls", sep="\t", header=T, row.names=1, check.names=F)
t_asv <- t(asv)

pd.big <- pdist.big(tree=tree, wd="icamp_test", nworker=2)
bNTI <- bNTI.big(t_asv, 
                 meta.group=mapping, 
                 pd.desc=pd.big$pd.file, 
                 pd.spname=pd.big$tip.label, 
                 pd.wd=pd.big$pd.wd, 
                 spname.check=T, 
                 nworker=2, memo.size.GB=16, 
                 weighted=T, 
                 exclude.consp=F, 
                 rand=10, 
                 output.dtail=T, 
                 RC=F, 
                 trace=T)

bNTI.nti <- bNTI$bNTI
bNTI.nti[upper.tri(bNTI.nti)] <- diag(bNTI.nti) <- NA
bNTI.nti <- bNTI.nti %>% 
  as_tibble(rownames="sample") %>% 
  melt(id.vars="sample") %>% 
  dplyr::filter(value!="NA")

mapping <- cbind(sample=rownames(mapping), mapping)
bNTI.nti <- bNTI.nti %>% left_join(mapping, by="sample") %>% 
  left_join(mapping, by=c("variable"="sample")) %>% 
  dplyr::filter(Group.x==Group.y)
plot.nti <- bNTI.nti[, c("Group.x", "value")]

library(ggplot2)
mycol <- c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17")
pdf('icamp_test/betaNTI.pdf', width=6, height=5)
ggplot(plot.nti, aes(x=Group.x, y=value, color=Group.x)) + 
  geom_boxplot(size=1) + 
  geom_jitter(size=2) + 
  geom_hline(yintercept=c(-2, 2), linetype=2, color="grey50") +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        axis.text.x=element_text(color="black", size=11, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(color="black", size=11), 
        axis.title.y=element_text(color="black", size=13)) +
  scale_y_continuous(limits=c(-5, 3)) + 
  scale_color_manual(values=mycol) +
  guides(color="none") + 
  labs(x="", y=expression(beta*NTI)) +
  geom_smooth(method="loess", se=T, aes(group=1), color="#C86B8C", fill="#EEC6CF")
dev.off()

