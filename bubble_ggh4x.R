rm(list=ls())
library(ggh4x)
library(ggplot2)
library(reshape2)
library(dplyr)

args <- commandArgs(T)
# args <- c("data.xls", "taxon.xls", "meta.xls", "dotplot.pdf")

col_x <- c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17",
           "#666666", "#1B9E77", "#7570B3", "#66A61E", "#E6AB02", "#A6761D",
           "#A6CEE3", "#B2DF8A", "#FB9A99", "#E31A1C", "#FF7F00", "#6A3D9A")
col_y <- c("#8DA0CB", "#4DAF4A", "#984EA3", "#c6c386", "#999999", "#66C2A5",
           "#FC8D62", "#A6D854", "#FFD92F", "#BEBADA", "#FB8072", "#80B1D3",
           "#FDB462", "#BC80BD", "#B3B3B3", "#33A02C", "#B3DE69", "#4038b0",
           "#ee7576", "#e94749", "#E78AC3", "#ff0000", "#A65628", "#d80172")

df <- read.table(args[1], sep="\t", header=T, quote="", check.names=F)
tax <- read.table(args[2], sep="\t", header=T, quote="", check.names=F, stringsAsFactor=T)
meta <- read.table(args[3], sep="\t", header=T, quote="", check.names=F)

df <- melt(df)
colnames(df) <- c(colnames(tax)[1], colnames(meta)[1], "abundance")
df.plot <- merge(df, meta, by="Sample", sort=F) %>% merge(., tax, by=colnames(tax)[1], sort=F)
colnames(df.plot)[1] <- "taxon1"

mystrips <- strip_nested(
  background_x=elem_list_rect(fill=col_x),
  by_layer_x=F,
  background_y=elem_list_rect(fill=col_y),
 # text_y=elem_list_text(angle=0),
  by_layer_y=F)

p <- ggplot(df.plot, aes(x=Sample, y=taxon1, size=abundance, color=Phylum)) +
  geom_point(alpha=0.7) +
  labs(x="", y="") + 
  facet_nested(Phylum + Genus ~ Group2 + Group1, scales="free", strip=mystrips) +
  theme_bw() + 
  theme(panel.spacing=unit(0, "lines"),
        axis.text=element_text(color="black")) + 
  scale_color_manual(values=col_y[1: length(unique(tax$Phylum))]) +
  guides(color=F, size=guide_legend(title=NULL))

pdf(args[4])
p
dev.off()

